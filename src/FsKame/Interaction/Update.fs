namespace FsKame

open System
open System.Diagnostics
open System.Threading
open System.Threading.Channels
open FSharp.Control
open Fabulous
open FsKame.WorkFlow
open Microsoft.Extensions.AI
open Microsoft.Maui.ApplicationModel
open Microsoft.Maui.Devices
open Microsoft.Maui.Graphics
open Microsoft.Maui.Storage
open OpenAI.Chat
#if ANDROID
open Android.Util
#endif

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
        Settings.setActiveUseCaseId model.activeUseCase.id

        model.modelRoleOverrides
        |> Map.iter (fun role modelId -> Settings.setModelRoleModelId model.activeUseCase.id role modelId)

        Settings.setUseCaseRetrievalMode model.activeUseCase.id model.retrievalMode
        Settings.setLogExpansions model.logExpansions
        Settings.setLogChunks model.logChunks
        Settings.setUseCaseUseLexicalFilter model.activeUseCase.id model.useLexicalFilter
        Settings.setUseCaseElaborateIndexKeywords model.activeUseCase.id model.elaborateIndexKeywords
        Settings.setUseHybridPdfParsing model.useHybridPdfParsing

        model.useCaseSettings
        |> Map.iter (fun key value -> Settings.setUseCaseSetting model.activeUseCase.id key value)

    let private savePdfLibraryWithLog docs log =
        let saveLog =
            match Settings.setPdfLibrary docs with
            | Ok path -> $"Saved document library manifest: {docs.Length} document(s) at {path}."
            | Error error -> $"Document library manifest was not saved: {error}"

        saveLog :: log |> List.truncate C.MAX_LOG

    let private processingReport (model: Model) msg =
        let text = $"PDF processing: {msg}"
        Debug.WriteLine text
        Console.WriteLine text
#if ANDROID
        Log.Info("FsKame", text) |> ignore
#endif
        model.mailbox.Writer.TryWrite(Log_Append msg) |> ignore

    let private createChatClient (key: string) (modelId: string) : IChatClient =
        let client = OpenAI.OpenAIClient(key)
        client.GetResponsesClient().AsIChatClient(modelId)

    let private useCaseModelRoleOverrides (definition: FsKame.QA.UseCaseDefinition) =
        FsKame.QA.ModelRole.all
        |> List.map (fun role ->
            let fallback = (FsKame.QA.UseCaseDefinition.model role definition).modelId
            role, Settings.modelRoleModelId definition.id role fallback)
        |> Map.ofList

    let private composeUseCase model =
        UseCaseComposer.withHostOverrides
            model.modelRoleOverrides
            model.retrievalMode
            model.useLexicalFilter
            model.elaborateIndexKeywords
            model.activeUseCase

    let private keywordOptions model =
        if not model.elaborateIndexKeywords then
            FsKame.QA.KnowledgeSources.KeywordGenerationOptions.disabled
        else
            let useCase = composeUseCase model

            let keywordModel = FsKame.QA.UseCaseDefinition.model FsKame.QA.Keyword useCase

            model.openAiKey
            |> Text.notEmpty
            |> Option.map (fun key ->
                { FsKame.QA.KnowledgeSources.KeywordGenerationOptions.defaults with
                    client = Some(createChatClient key keywordModel.modelId)
                    modelId = keywordModel.modelId
                    useCaseProfile = useCase.profile
                    useCaseFingerprint = FsKame.QA.UseCaseDefinition.fingerprint useCase })
            |> Option.defaultValue
                { FsKame.QA.KnowledgeSources.KeywordGenerationOptions.defaults with
                    client = None
                    modelId = keywordModel.modelId
                    useCaseProfile = useCase.profile
                    useCaseFingerprint = FsKame.QA.UseCaseDefinition.fingerprint useCase }

    let private withKeywordCancellation token (options: FsKame.QA.KnowledgeSources.KeywordGenerationOptions) =
        { options with
            cancellationToken = Some token }

    let private postSources model =
        match model.bundle with
        | Some bundle ->
            let flags =
                {| logExpansions = model.logExpansions
                   logChunks = model.logChunks
                   useLexicalFilter = model.useLexicalFilter
                   elaborateIndexKeywords = model.elaborateIndexKeywords
                   useHybridPdfParsing = model.useHybridPdfParsing |}

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

    let private processDocuments
        report
        (cancellationToken: CancellationToken)
        keywordOptions
        useHybridPdfParsing
        (docs: PdfDocumentSource list)
        =
        async {
            try
                let parserName = if useHybridPdfParsing then "Hybrid" else "Legacy"

                report $"Starting document processing command for {docs.Length} document(s); parser={parserName}."
                cancellationToken.ThrowIfCancellationRequested()

                let! results = PdfLibrary.processDocuments report keywordOptions useHybridPdfParsing docs
                report $"Document processing command completed for {docs.Length} document(s)."
                return Ok results
            with
            | :? OperationCanceledException as ex ->
                report "Document processing was canceled."
                return Error(ex :> exn)
            | ex -> return Error ex
        }

    let private installPrebuiltDocuments docs =
        async {
            try
                let! installed, logs = PdfLibrary.installPrebuiltDocuments docs
                return Ok(installed, logs)
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

    let private failProcessingDocuments error (docs: PdfDocumentSource list) : PdfDocumentSource list =
        docs
        |> List.map (fun doc ->
            match doc.status with
            | Processing
            | Queued ->
                { doc with
                    selected = false
                    status = Failed
                    chunkCount = 0
                    error = Some error }
            | Ready
            | Failed -> doc)

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

    let private disposeDocumentProcessingCancellation (model: Model) =
        model.documentProcessingCancellation |> Option.iter _.Dispose()

    let private documentProcessingCommand report (cts: CancellationTokenSource) (model: Model) docs =
        let keywordOptions = keywordOptions model |> withKeywordCancellation cts.Token

        Cmd.OfAsync.either
            (processDocuments report cts.Token keywordOptions model.useHybridPdfParsing)
            docs
            PdfProcessingCompleted
            EventError

    let private startParams model =
        let useCase = composeUseCase model

        { apiKey = model.openAiKey
          useCase = useCase
          useCasePlugin = model.useCasePlugin
          useCaseSettings = model.useCaseSettings
          retrievalMode = model.retrievalMode
          sources = sources model
          mailbox = model.mailbox
          logExpansions = model.logExpansions
          logChunks = model.logChunks
          useLexicalFilter = model.useLexicalFilter
          elaborateIndexKeywords = model.elaborateIndexKeywords
          useHybridPdfParsing = model.useHybridPdfParsing }

    let init () =
        let docs = Settings.pdfLibrary ()

        let loadedUseCase, useCaseLogs =
            UseCaseHost.loadActive (Some(Settings.activeUseCaseId ()))

        Settings.setActiveUseCaseId loadedUseCase.definition.id

        let modelRoleOverrides = useCaseModelRoleOverrides loadedUseCase.definition

        let retrievalMode =
            loadedUseCase.definition.runtime.retrievalMode
            |> UseCaseComposer.fromQaRetrievalMode
            |> Settings.useCaseRetrievalMode loadedUseCase.definition.id

        let initialLog =
            ($"FsKame ready with use case: {loadedUseCase.definition.displayName}. Add PDFs, then connect."
             :: useCaseLogs)
            |> List.truncate C.MAX_LOG

        { currentPage = Main
          mailbox = Channel.CreateBounded<Msg>(100)
          bundle = None
          sessionState = RTOpenAI.WebRTC.State.Disconnected
          openAiKey = Settings.openAiKey ()
          activeUseCase = loadedUseCase.definition
          useCasePlugin = loadedUseCase.plugin
          useCaseSettings = Settings.useCaseSettings loadedUseCase.definition.id loadedUseCase.definition.settingsFacets
          modelRoleOverrides = modelRoleOverrides
          retrievalMode = retrievalMode
          pdfDocuments = docs
          log = initialLog
          logFontSize = 12.
          hideSecrets = true
          isBusy = false
          documentProcessingCancellation = None
          logExpansions = Settings.logExpansions ()
          logChunks = Settings.logChunks ()
          useLexicalFilter =
            Settings.useCaseUseLexicalFilter
                loadedUseCase.definition.id
                loadedUseCase.definition.runtime.useLexicalFilter
          elaborateIndexKeywords =
            Settings.useCaseElaborateIndexKeywords
                loadedUseCase.definition.id
                loadedUseCase.definition.runtime.elaborateIndexKeywords
          useHybridPdfParsing = Settings.useHybridPdfParsing () },
        Cmd.OfAsync.either installPrebuiltDocuments docs PrebuiltDocumentsInstalled EventError

    let update msg model =
        match msg with
        | OpenAiKeyChanged value ->
            match sourceConfigBlocked model "Changing OpenAI key" with
            | Some msg ->
                { model with
                    log = msg :: model.log |> List.truncate C.MAX_LOG },
                Cmd.none
            | None -> { model with openAiKey = value }, Cmd.none
        | ModelRoleModelChanged(role, value) ->
            match sourceConfigBlocked model $"Changing {FsKame.QA.ModelRole.storageName role} model" with
            | Some msg ->
                { model with
                    log = msg :: model.log |> List.truncate C.MAX_LOG },
                Cmd.none
            | None ->
                let modelRoleOverrides = model.modelRoleOverrides |> Map.add role value

                { model with
                    modelRoleOverrides = modelRoleOverrides },
                Cmd.none
        | UseCaseSettingChanged(key, value) ->
            match sourceConfigBlocked model $"Changing {key}" with
            | Some msg ->
                { model with
                    log = msg :: model.log |> List.truncate C.MAX_LOG },
                Cmd.none
            | None ->
                { model with
                    useCaseSettings = model.useCaseSettings |> Map.add key value },
                Cmd.none
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
        | ElaborateIndexKeywordsToggled value ->
            match sourceConfigBlocked model "Changing index elaboration" with
            | Some msg ->
                { model with
                    log = msg :: model.log |> List.truncate C.MAX_LOG },
                Cmd.none
            | None ->
                let model =
                    { model with
                        elaborateIndexKeywords = value }

                saveSettings model
                postSources model
                model, Cmd.none
        | UseHybridPdfParsingToggled value ->
            match sourceConfigBlocked model "Changing PDF parser" with
            | Some msg ->
                { model with
                    log = msg :: model.log |> List.truncate C.MAX_LOG },
                Cmd.none
            | None ->
                let parserName = if value then "Hybrid" else "Legacy"

                let model =
                    { model with
                        useHybridPdfParsing = value
                        log =
                            $"PDF parser set to {parserName}. Reprocess documents to rebuild indexes with this parser."
                            :: model.log
                            |> List.truncate C.MAX_LOG }

                saveSettings model
                postSources model
                model, Cmd.none
        | PrebuiltDocumentsInstalled(Ok(docs, logs)) ->
            let model =
                { model with
                    pdfDocuments = docs
                    log = (logs @ model.log) |> List.truncate C.MAX_LOG }

            let model =
                { model with
                    log = savePdfLibraryWithLog model.pdfDocuments model.log }

            postSources model
            model, Cmd.none
        | PrebuiltDocumentsInstalled(Error ex) ->
            { model with
                log =
                    $"Prebuilt document installation failed: {ex.Message}" :: model.log
                    |> List.truncate C.MAX_LOG },
            Cmd.none
        | Settings_Show ->
            if model.isBusy then
                { model with
                    log =
                        "Opening settings is unavailable while another operation is running."
                        :: model.log
                        |> List.truncate C.MAX_LOG },
                Cmd.none
            else
                { model with currentPage = Settings }, Cmd.none
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
                let report msg = processingReport model msg
                let cts = new CancellationTokenSource()

                { model with
                    documentProcessingCancellation = Some cts },
                documentProcessingCommand report cts model docs
        | PickPdfsCompleted(Error ex) ->
            { model with
                isBusy = false
                log = $"Document picker failed: {ex.Message}" :: model.log |> List.truncate C.MAX_LOG },
            Cmd.none
        | PdfProcessingCompleted(Ok results) ->
            disposeDocumentProcessingCancellation model

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
                    documentProcessingCancellation = None
                    log = log }

            let model =
                { model with
                    log = savePdfLibraryWithLog model.pdfDocuments model.log }

            postSources model
            model, Cmd.none
        | PdfProcessingCompleted(Error ex) ->
            disposeDocumentProcessingCancellation model

            let canceled =
                match ex with
                | :? OperationCanceledException -> true
                | _ -> false

            let error =
                if canceled then
                    "Document processing canceled."
                else
                    $"Document processing failed: {ex.Message}"

            let pdfDocuments = failProcessingDocuments error model.pdfDocuments

            let log = error :: model.log |> List.truncate C.MAX_LOG

            let model =
                { model with
                    pdfDocuments = pdfDocuments
                    isBusy = false
                    documentProcessingCancellation = None
                    log = log }

            { model with
                log = savePdfLibraryWithLog model.pdfDocuments model.log },
            Cmd.none
        | CancelPdfProcessing ->
            match model.documentProcessingCancellation with
            | None ->
                { model with
                    log =
                        "No document processing operation is running." :: model.log
                        |> List.truncate C.MAX_LOG },
                Cmd.none
            | Some cts ->
                cts.Cancel()

                { model with
                    log = "Canceling document processing..." :: model.log |> List.truncate C.MAX_LOG },
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

                    let report msg = processingReport model msg
                    let cts = new CancellationTokenSource()

                    let model =
                        { model with
                            log = savePdfLibraryWithLog model.pdfDocuments model.log }

                    { model with
                        documentProcessingCancellation = Some cts },
                    documentProcessingCommand report cts model retry
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
            disposeDocumentProcessingCancellation model

            { model with
                isBusy = false
                documentProcessingCancellation = None
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
