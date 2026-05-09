namespace FsKame

open System
open System.Threading
open System.Threading.Channels
open FSharp.Control
open Fabulous
open FsKame.WorkFlow
open Microsoft.Maui.ApplicationModel
open Microsoft.Maui.Devices
open Microsoft.Maui.Graphics
open Microsoft.Maui.Storage

module Update =
    let private minLogFontSize = 10.
    let private maxLogFontSize = 22.
    let private logFontStep = 1.

    let private clampLogFontSize value =
        min maxLogFontSize (max minLogFontSize value)

    let private sources model =
        KnowledgeSources.selectedSources model.pdfDocuments

    let private isRealtimeActive model =
        model.bundle.IsSome || model.sessionState <> RTOpenAI.WebRTC.State.Disconnected

    let private canMutateDocuments model =
        not model.isBusy && not (isRealtimeActive model)

    let private canChangeSourceSelection model =
        not model.isBusy && not (isRealtimeActive model)

    let private documentMutationBlocked model action =
        if model.isBusy then
            Some $"{action} is unavailable while another operation is running."
        elif isRealtimeActive model then
            Some $"{action} is unavailable while realtime is connected."
        else
            None

    let private sourceConfigBlocked model action =
        if model.isBusy then
            Some $"{action} is unavailable while another operation is running."
        elif isRealtimeActive model then
            Some $"{action} is unavailable while realtime is connected."
        else
            None

    let private documentFileTypes =
        FilePickerFileType(
            dict
                [ DevicePlatform.iOS,
                  [ "com.adobe.pdf"; "net.daringfireball.markdown"; "public.plain-text" ] :> seq<string>
                  DevicePlatform.MacCatalyst,
                  [ "com.adobe.pdf"; "net.daringfireball.markdown"; "public.plain-text" ] :> seq<string>
                  DevicePlatform.Android, [ "application/pdf"; "text/markdown"; "text/plain" ] :> seq<string>
                  DevicePlatform.WinUI, [ ".pdf"; ".md"; ".markdown" ] :> seq<string> ]
        )

    let private saveSettings model =
        Settings.setOpenAiKey model.openAiKey
        Settings.setOracleModel model.oracleModel
        Settings.setRetrievalMode model.retrievalMode
        Settings.setLogExpansions model.logExpansions
        Settings.setLogChunks model.logChunks
        Settings.setUseLexicalFilter model.useLexicalFilter

    let private savePdfLibraryWithLog docs log =
        let saveLog =
            match Settings.setPdfLibrary docs with
            | Ok path -> $"Saved document library manifest: {docs.Length} document(s) at {path}."
            | Error error -> $"Document library manifest was not saved: {error}"

        saveLog :: log |> List.truncate C.MAX_LOG

    let private postSources model =
        match model.bundle with
        | Some bundle ->
            let flags =
                {| logExpansions = model.logExpansions
                   logChunks = model.logChunks
                   useLexicalFilter = model.useLexicalFilter |}

            bundle.flow.PostToAgent(Ag_SourcesUpdated(model.retrievalMode, sources model, flags))
        | None -> ()

    let private pickAndCopyDocuments existing =
        async {
            try
                let opts =
                    PickOptions(PickerTitle = "Select PDFs or Markdown files", FileTypes = documentFileTypes)

                let tsk () =
                    FilePicker.Default.PickMultipleAsync(opts)

                let! results = MainThread.InvokeOnMainThreadAsync<FileResult seq>(tsk) |> Async.AwaitTask
                let! docs = PdfLibrary.copyNewDocuments existing results
                return Ok docs
            with ex ->
                return Error ex
        }

    let private processDocuments report (docs: PdfDocumentSource list) =
        async {
            try
                let! results = PdfLibrary.processDocuments report docs
                return Ok results
            with ex ->
                return Error ex
        }

    let private deleteDocumentAndIndexes (doc: PdfDocumentSource) =
        async {
            try
                let! removedFile = PdfLibrary.deleteStoredDocument doc
                let! removedIndexCount, indexErrors = KnowledgeSources.clearPersistedIndexes ()

                return
                    Ok
                        { id = doc.id
                          displayName = doc.displayName
                          removedFile = removedFile
                          removedIndexCount = removedIndexCount
                          indexErrors = indexErrors }
            with ex ->
                return Error ex
        }

    let private applyProcessingResult (result: PdfProcessResult) (doc: PdfDocumentSource) : PdfDocumentSource =
        if doc.id <> result.id then
            doc
        else
            match result.error with
            | None ->
                { doc with
                    selected = true
                    status = Ready
                    chunkCount = result.chunkCount
                    error = None }
            | Some err ->
                { doc with
                    selected = false
                    status = Failed
                    chunkCount = 0
                    error = Some err }

    let private retryDocs ids (docs: PdfDocumentSource list) : PdfDocumentSource list =
        docs
        |> List.map (fun doc ->
            if Set.contains doc.id ids then
                { doc with
                    selected = false
                    status = Processing
                    chunkCount = 0
                    error = None }
            else
                doc)

    let private startParams model =
        { apiKey = model.openAiKey
          oracleModel = model.oracleModel
          retrievalMode = model.retrievalMode
          sources = sources model
          mailbox = model.mailbox
          logExpansions = model.logExpansions
          logChunks = model.logChunks
          useLexicalFilter = model.useLexicalFilter
          toolCatalog = ToolCatalog.empty }

    let init () =
        { currentPage = Main
          mailbox = Channel.CreateBounded<Msg>(100)
          bundle = None
          sessionState = RTOpenAI.WebRTC.State.Disconnected
          openAiKey = Settings.openAiKey ()
          oracleModel = Settings.oracleModel ()
          retrievalMode = Settings.retrievalMode ()
          pdfDocuments = Settings.pdfLibrary ()
          log = [ "FsKame ready. Add PDFs, then connect." ]
          logFontSize = 12.
          hideSecrets = true
          isBusy = false
          logExpansions = Settings.logExpansions ()
          logChunks = Settings.logChunks ()
          useLexicalFilter = Settings.useLexicalFilter () },
        Cmd.none

    let update msg model =
        match msg with
        | OpenAiKeyChanged value ->
            match sourceConfigBlocked model "Changing OpenAI key" with
            | Some msg ->
                { model with
                    log = msg :: model.log |> List.truncate C.MAX_LOG },
                Cmd.none
            | None -> { model with openAiKey = value }, Cmd.none
        | OracleModelChanged value ->
            match sourceConfigBlocked model "Changing oracle model" with
            | Some msg ->
                { model with
                    log = msg :: model.log |> List.truncate C.MAX_LOG },
                Cmd.none
            | None -> { model with oracleModel = value }, Cmd.none
        | RetrievalModeChanged mode ->
            match sourceConfigBlocked model "Changing retrieval mode" with
            | Some msg ->
                { model with
                    log = msg :: model.log |> List.truncate C.MAX_LOG },
                Cmd.none
            | None -> { model with retrievalMode = mode }, Cmd.none
        | LogExpansionsToggled value ->
            match sourceConfigBlocked model "Changing retrieval logging" with
            | Some msg ->
                { model with
                    log = msg :: model.log |> List.truncate C.MAX_LOG },
                Cmd.none
            | None ->
                let model = { model with logExpansions = value }
                saveSettings model
                postSources model
                model, Cmd.none
        | LogChunksToggled value ->
            match sourceConfigBlocked model "Changing chunk logging" with
            | Some msg ->
                { model with
                    log = msg :: model.log |> List.truncate C.MAX_LOG },
                Cmd.none
            | None ->
                let model = { model with logChunks = value }
                saveSettings model
                postSources model
                model, Cmd.none
        | UseLexicalFilterToggled value ->
            match sourceConfigBlocked model "Changing lexical filter" with
            | Some msg ->
                { model with
                    log = msg :: model.log |> List.truncate C.MAX_LOG },
                Cmd.none
            | None ->
                let model = { model with useLexicalFilter = value }
                saveSettings model
                postSources model
                model, Cmd.none
        | Settings_Show ->
            match sourceConfigBlocked model "Opening settings" with
            | Some msg ->
                { model with
                    log = msg :: model.log |> List.truncate C.MAX_LOG },
                Cmd.none
            | None -> { model with currentPage = Settings }, Cmd.none
        | Settings_Close ->
            saveSettings model
            { model with currentPage = Main }, Cmd.none
        | ToggleSecretVisibility ->
            match sourceConfigBlocked model "Changing secret visibility" with
            | Some msg ->
                { model with
                    log = msg :: model.log |> List.truncate C.MAX_LOG },
                Cmd.none
            | None ->
                { model with
                    hideSecrets = not model.hideSecrets },
                Cmd.none
        | PickPdfs ->
            match documentMutationBlocked model "Adding documents" with
            | Some msg ->
                { model with
                    log = msg :: model.log |> List.truncate C.MAX_LOG },
                Cmd.none
            | None ->
                { model with isBusy = true },
                Cmd.OfAsync.either pickAndCopyDocuments model.pdfDocuments PickPdfsCompleted EventError
        | PickPdfsCompleted(Ok docs) ->
            let pdfDocuments = model.pdfDocuments @ docs

            let msg =
                if List.isEmpty docs then
                    "No new documents selected."
                else
                    $"Processing {docs.Length} new document(s)."

            let log = msg :: model.log |> List.truncate C.MAX_LOG

            let model =
                { model with
                    pdfDocuments = pdfDocuments
                    log = log }

            let model =
                { model with
                    log = savePdfLibraryWithLog model.pdfDocuments model.log }

            if List.isEmpty docs then
                { model with isBusy = false }, Cmd.none
            else
                let report msg =
                    model.mailbox.Writer.TryWrite(Log_Append msg) |> ignore

                model, Cmd.OfAsync.either (processDocuments report) docs PdfProcessingCompleted EventError
        | PickPdfsCompleted(Error ex) ->
            { model with
                isBusy = false
                log = $"Document picker failed: {ex.Message}" :: model.log |> List.truncate C.MAX_LOG },
            Cmd.none
        | PdfProcessingCompleted(Ok results) ->
            let pdfDocuments =
                results
                |> List.fold (fun docs result -> docs |> List.map (applyProcessingResult result)) model.pdfDocuments

            let readyCount = results |> List.filter (fun r -> r.error.IsNone) |> List.length
            let failedCount = results.Length - readyCount

            let log =
                $"Document processing complete: {readyCount} ready, {failedCount} failed."
                :: model.log
                |> List.truncate C.MAX_LOG

            let model =
                { model with
                    pdfDocuments = pdfDocuments
                    isBusy = false
                    log = log }

            let model =
                { model with
                    log = savePdfLibraryWithLog model.pdfDocuments model.log }

            postSources model
            model, Cmd.none
        | PdfProcessingCompleted(Error ex) ->
            { model with
                isBusy = false
                log =
                    $"Document processing failed: {ex.Message}" :: model.log
                    |> List.truncate C.MAX_LOG },
            Cmd.none
        | PdfSelectionChanged(id, selected) ->
            if not (canChangeSourceSelection model) then
                { model with
                    log =
                        "Changing selected sources is unavailable while realtime is connected or another operation is running."
                        :: model.log
                        |> List.truncate C.MAX_LOG },
                Cmd.none
            else
                let pdfDocuments =
                    model.pdfDocuments
                    |> List.map (fun doc ->
                        if doc.id = id && PdfDocuments.canSelect doc then
                            { doc with selected = selected }
                        else
                            doc)

                let model =
                    { model with
                        pdfDocuments = pdfDocuments }

                let model =
                    { model with
                        log = savePdfLibraryWithLog model.pdfDocuments model.log }

                postSources model
                model, Cmd.none
        | RetryPdfProcessing id ->
            match documentMutationBlocked model "Retrying document processing" with
            | Some msg ->
                { model with
                    log = msg :: model.log |> List.truncate C.MAX_LOG },
                Cmd.none
            | None ->
                let retry =
                    model.pdfDocuments
                    |> List.filter (fun doc -> doc.id = id && doc.status = Failed)

                if List.isEmpty retry then
                    model, Cmd.none
                else
                    let ids = retry |> List.map _.id |> Set.ofList

                    let model =
                        { model with
                            pdfDocuments = retryDocs ids model.pdfDocuments
                            isBusy = true }

                    let report msg =
                        model.mailbox.Writer.TryWrite(Log_Append msg) |> ignore

                    let model =
                        { model with
                            log = savePdfLibraryWithLog model.pdfDocuments model.log }

                    model, Cmd.OfAsync.either (processDocuments report) retry PdfProcessingCompleted EventError
        | DeletePdf id ->
            match documentMutationBlocked model "Deleting documents" with
            | Some msg ->
                { model with
                    log = msg :: model.log |> List.truncate C.MAX_LOG },
                Cmd.none
            | None ->
                match model.pdfDocuments |> List.tryFind (fun doc -> doc.id = id) with
                | None -> model, Cmd.none
                | Some doc ->
                    { model with isBusy = true },
                    Cmd.OfAsync.either deleteDocumentAndIndexes doc DeletePdfCompleted EventError
        | DeletePdfCompleted(Ok result) ->
            let pdfDocuments =
                model.pdfDocuments |> List.filter (fun doc -> doc.id <> result.id)

            let fileMsg =
                if result.removedFile then
                    $"Deleted document: {result.displayName}."
                else
                    $"Removed document library entry: {result.displayName}."

            let indexMsg =
                if result.removedIndexCount = 0 then
                    "No persisted FsColbert indexes needed removal."
                else
                    $"Removed {result.removedIndexCount} persisted FsColbert index file(s)."

            let log =
                [ yield fileMsg; yield indexMsg; yield! result.indexErrors ] @ model.log
                |> List.truncate C.MAX_LOG

            let model =
                { model with
                    pdfDocuments = pdfDocuments
                    isBusy = false
                    log = log }

            let model =
                { model with
                    log = savePdfLibraryWithLog model.pdfDocuments model.log }

            postSources model
            model, Cmd.none
        | DeletePdfCompleted(Error ex) ->
            { model with
                isBusy = false
                log = $"Document delete failed: {ex.Message}" :: model.log |> List.truncate C.MAX_LOG },
            Cmd.none
        | ApplySources ->
            match sourceConfigBlocked model "Applying sources" with
            | Some msg ->
                { model with
                    log = msg :: model.log |> List.truncate C.MAX_LOG },
                Cmd.none
            | None ->
                saveSettings model
                postSources model

                let count = sources model |> List.length

                { model with
                    log =
                        $"Configured {count} document source(s)." :: model.log
                        |> List.truncate C.MAX_LOG },
                Cmd.none
        | StartStop ->
            match model.bundle with
            | None ->
                saveSettings model

                { model with
                    isBusy = true
                    log = "Starting realtime FsKame flow..." :: model.log },
                Cmd.OfAsync.either Connect.start (startParams model) StartCompleted EventError
            | Some bundle ->
                { model with isBusy = true }, Cmd.OfAsync.either Connect.stop bundle StopCompleted EventError
        | StartCompleted(Ok bundle) ->
            { model with
                bundle = Some bundle
                isBusy = false
                log = "Realtime flow started." :: model.log },
            Cmd.none
        | StartCompleted(Error ex) ->
            { model with
                isBusy = false
                log = $"Start failed: {ex.Message}" :: model.log },
            Cmd.none
        | StopCompleted(Ok()) ->
            { model with
                bundle = None
                isBusy = false
                log = "Realtime flow stopped." :: model.log },
            Cmd.none
        | StopCompleted(Error ex) ->
            { model with
                isBusy = false
                log = $"Stop failed: {ex.Message}" :: model.log },
            Cmd.none
        | WebRTC_StateChanged state -> { model with sessionState = state }, Cmd.none
        | Log_Append text ->
            { model with
                log = text :: model.log |> List.truncate C.MAX_LOG },
            Cmd.none
        | Log_Clear -> { model with log = [] }, Cmd.none
        | LogFont_Increase ->
            { model with
                logFontSize = clampLogFontSize (model.logFontSize + logFontStep) },
            Cmd.none
        | LogFont_Decrease ->
            { model with
                logFontSize = clampLogFontSize (model.logFontSize - logFontStep) },
            Cmd.none
        | EventError ex ->
            { model with
                isBusy = false
                log = ex.Message :: model.log |> List.truncate C.MAX_LOG },
            Cmd.none

    let subscribeMailbox model =
        let background dispatch =
            let cts = new CancellationTokenSource()

            let comp =
                async {
                    let reader = model.mailbox.Reader.ReadAllAsync(cts.Token) |> AsyncSeq.iter dispatch

                    match! Async.Catch reader with
                    | Choice1Of2 _ -> ()
                    | Choice2Of2(:? OperationCanceledException) -> ()
                    | Choice2Of2 ex -> dispatch (EventError ex)
                }

            Async.Start(comp, cts.Token)

            { new IDisposable with
                member _.Dispose() =
                    cts.Cancel()
                    cts.Dispose() }

        [ [ "mailbox" ], background ]

    let internal statusText model =
        match model.sessionState with
        | RTOpenAI.WebRTC.State.Connected -> "Connected"
        | RTOpenAI.WebRTC.State.Connecting -> "Connecting"
        | _ -> "Disconnected"

    let internal statusColor model =
        match model.sessionState with
        | RTOpenAI.WebRTC.State.Connected -> Colors.SeaGreen
        | RTOpenAI.WebRTC.State.Connecting -> Colors.DarkOrange
        | _ -> Colors.DimGray
