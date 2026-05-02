namespace FsKame

open System
open System.Threading
open System.Threading.Channels
open FSharp.Control
open Fabulous
open FsKame.WorkFlow
open Microsoft.Maui.ApplicationModel
open Microsoft.Maui.Graphics
open Microsoft.Maui.Storage

module Update =
    let private sources model =
        KnowledgeSources.selectedSources model.pdfDocuments

    let private saveSettings model =
        Settings.setOpenAiKey model.openAiKey
        Settings.setOracleModel model.oracleModel
        Settings.setPdfLibrary model.pdfDocuments

    let private postSources model =
        match model.bundle with
        | Some bundle -> bundle.flow.PostToAgent(Ag_SourcesUpdated(sources model))
        | None -> ()

    let private pickAndCopyPdfs existing =
        async {
            try
                let opts =
                    PickOptions(PickerTitle = "Select PDFs", FileTypes = FilePickerFileType.Pdf)

                let tsk () =
                    FilePicker.Default.PickMultipleAsync(opts)

                let! results = MainThread.InvokeOnMainThreadAsync<FileResult seq>(tsk) |> Async.AwaitTask
                let! docs = PdfLibrary.copyNewPdfs existing results
                return Ok docs
            with ex ->
                return Error ex
        }

    let private processPdfs (docs: PdfDocumentSource list) =
        async {
            try
                let! results = PdfLibrary.processPdfs docs
                return Ok results
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
          sources = sources model
          mailbox = model.mailbox }

    let init () =
        { currentPage = Main
          mailbox = Channel.CreateBounded<Msg>(100)
          bundle = None
          sessionState = RTOpenAI.WebRTC.State.Disconnected
          openAiKey = Settings.openAiKey ()
          oracleModel = Settings.oracleModel ()
          pdfDocuments = Settings.pdfLibrary ()
          log = [ "FsKame ready. Add PDFs, then connect." ]
          hideSecrets = true
          isBusy = false },
        Cmd.none

    let update msg model =
        match msg with
        | OpenAiKeyChanged value -> { model with openAiKey = value }, Cmd.none
        | OracleModelChanged value -> { model with oracleModel = value }, Cmd.none
        | Settings_Show -> { model with currentPage = Settings }, Cmd.none
        | Settings_Close ->
            saveSettings model
            { model with currentPage = Main }, Cmd.none
        | ToggleSecretVisibility ->
            { model with
                hideSecrets = not model.hideSecrets },
            Cmd.none
        | PickPdfs ->
            { model with isBusy = true },
            Cmd.OfAsync.either pickAndCopyPdfs model.pdfDocuments PickPdfsCompleted EventError
        | PickPdfsCompleted(Ok docs) ->
            let pdfDocuments = model.pdfDocuments @ docs

            let msg =
                if List.isEmpty docs then
                    "No new PDFs selected."
                else
                    $"Processing {docs.Length} new PDF(s)."

            let log = msg :: model.log |> List.truncate C.MAX_LOG

            let model =
                { model with
                    pdfDocuments = pdfDocuments
                    isBusy = false
                    log = log }

            Settings.setPdfLibrary model.pdfDocuments

            if List.isEmpty docs then
                model, Cmd.none
            else
                model, Cmd.OfAsync.either processPdfs docs PdfProcessingCompleted EventError
        | PickPdfsCompleted(Error ex) ->
            { model with
                isBusy = false
                log = $"PDF picker failed: {ex.Message}" :: model.log |> List.truncate C.MAX_LOG },
            Cmd.none
        | PdfProcessingCompleted(Ok results) ->
            let pdfDocuments =
                results
                |> List.fold (fun docs result -> docs |> List.map (applyProcessingResult result)) model.pdfDocuments

            let readyCount = results |> List.filter (fun r -> r.error.IsNone) |> List.length
            let failedCount = results.Length - readyCount

            let log =
                $"PDF processing complete: {readyCount} ready, {failedCount} failed."
                :: model.log
                |> List.truncate C.MAX_LOG

            let model =
                { model with
                    pdfDocuments = pdfDocuments
                    log = log }

            Settings.setPdfLibrary model.pdfDocuments
            postSources model
            model, Cmd.none
        | PdfProcessingCompleted(Error ex) ->
            { model with
                log = $"PDF processing failed: {ex.Message}" :: model.log |> List.truncate C.MAX_LOG },
            Cmd.none
        | PdfSelectionChanged(id, selected) ->
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

            Settings.setPdfLibrary model.pdfDocuments
            postSources model
            model, Cmd.none
        | RetryPdfProcessing id ->
            let retry =
                model.pdfDocuments
                |> List.filter (fun doc -> doc.id = id && doc.status = Failed)

            if List.isEmpty retry then
                model, Cmd.none
            else
                let ids = retry |> List.map _.id |> Set.ofList

                let model =
                    { model with
                        pdfDocuments = retryDocs ids model.pdfDocuments }

                Settings.setPdfLibrary model.pdfDocuments
                model, Cmd.OfAsync.either processPdfs retry PdfProcessingCompleted EventError
        | ApplySources ->
            saveSettings model
            postSources model

            let count = sources model |> List.length

            { model with
                log = $"Configured {count} PDF source(s)." :: model.log |> List.truncate C.MAX_LOG },
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
        | EventError ex ->
            { model with
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
