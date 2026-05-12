namespace FsKame.WorkFlow

open System
open System.IO
open System.Threading
open FsKame
open Microsoft.Extensions.AI
open Microsoft.Maui.Storage
open OpenAI.Chat
open RTFlow
open RTFlow.Functions

module QaAgent =
    type State =
        { bus: WBus<FlowMsg, AgentMsg>
          apiKey: string
          oracleModel: string
          session: FsKame.QA.IQaOrchestrator option
          retrievalMode: FsKame.RetrievalMode
          sources: KnowledgeSource list }

    type SourceFlags =
        { logExpansions: bool
          logChunks: bool
          useLexicalFilter: bool
          elaborateIndexKeywords: bool }

    let private defaultFlags =
        { logExpansions = false
          logChunks = false
          useLexicalFilter = true
          elaborateIndexKeywords = true }

    let private createClient (key: string) (modelId: string) : IChatClient =
        let client = OpenAI.OpenAIClient(key)
        client.GetResponsesClient().AsIChatClient(modelId)

    let private toQaMode (mode: FsKame.RetrievalMode) : FsKame.QA.RetrievalMode =
        match mode with
        | InternalDocumentIndex -> FsKame.QA.RetrievalMode.InternalDocumentIndex
        | FsColbertWithFallback -> FsKame.QA.RetrievalMode.FsColbertWithFallback

    let private toQaSource (source: KnowledgeSource) : FsKame.QA.KnowledgeSource =
        let kind =
            match source.kind with
            | Pdf -> FsKame.QA.KnowledgeSourceKind.Pdf
            | Markdown -> FsKame.QA.KnowledgeSourceKind.Markdown
            | Json -> FsKame.QA.KnowledgeSourceKind.Json

        { FsKame.QA.KnowledgeSource.kind = kind
          location = source.location
          enabled = source.enabled }

    let private toWorkflowSource (source: FsKame.QA.KnowledgeSource) : KnowledgeSource =
        let kind =
            match source.kind with
            | FsKame.QA.KnowledgeSourceKind.Pdf -> Pdf
            | FsKame.QA.KnowledgeSourceKind.Markdown -> Markdown
            | FsKame.QA.KnowledgeSourceKind.Json -> Json

        { kind = kind
          location = source.location
          enabled = source.enabled }

    let private toWorkflowChunk (chunk: FsKame.QA.SourceChunk) : SourceChunk =
        { source = toWorkflowSource chunk.source
          index = chunk.index
          text = chunk.text
          score = chunk.score }

    let private toQaJudgement (judgement: RealtimeJudgement) : FsKame.QA.RealtimeJudgement =
        { FsKame.QA.RealtimeJudgement.turnKind = judgement.turnKind
          topicContinuity = judgement.topicContinuity
          memoryAction = judgement.memoryAction
          needsExternalContext = judgement.needsExternalContext
          confidence = judgement.confidence
          riskFlags =
            { FsKame.QA.RiskFlags.memoryMutation = judgement.riskFlags.memoryMutation
              sensitive = judgement.riskFlags.sensitive
              conflictLikely = judgement.riskFlags.conflictLikely } }

    let private storageRoot () = FileSystem.AppDataDirectory

    let private createSession (st: State) flags : FsKame.QA.IQaOrchestrator =
        let clients: FsKame.QA.QaModelClients =
            if String.IsNullOrWhiteSpace st.apiKey then
                FsKame.QA.QaModelClients.none
            else
                let small = createClient st.apiKey C.NANO_MODEL
                let large = createClient st.apiKey st.oracleModel

                { queryExpansion = Some small
                  toolPlanner = Some small
                  answerGenerator = Some large }

        let storageRoot = storageRoot ()

        let options =
            { FsKame.QA.QaSessionOptions.create storageRoot with
                toolProviderDirectory = Some(Path.Combine(storageRoot, "tool-providers"))
                clients = clients
                answerModelId = st.oracleModel
                keywordModelId = C.NANO_MODEL
                elaborateIndexKeywords = flags.elaborateIndexKeywords
                logTimings = true
                logExpansions = flags.logExpansions
                logChunks = flags.logChunks
                useLexicalFilter = flags.useLexicalFilter
                report = fun msg -> st.bus.PostToAgent(Ag_Log msg) }

        new FsKame.QA.QaSession(options) :> FsKame.QA.IQaOrchestrator

    let private createContextProvider st flags mode sources : FsKame.QA.IQaContextProvider =
        let qaMode = toQaMode mode
        let qaSources = sources |> List.map toQaSource

        let queryExpansionClient =
            if String.IsNullOrWhiteSpace st.apiKey then
                None
            else
                Some(createClient st.apiKey C.NANO_MODEL)

        let options =
            { FsKame.QA.FsColbertContextProviderOptions.create (storageRoot ()) qaMode qaSources with
                queryExpansionClient = queryExpansionClient
                disposeQueryExpansionClient = true
                keywordModelId = C.NANO_MODEL
                elaborateIndexKeywords = flags.elaborateIndexKeywords
                logExpansions = flags.logExpansions
                logChunks = flags.logChunks
                useLexicalFilter = flags.useLexicalFilter
                report = fun msg -> st.bus.PostToAgent(Ag_Log msg) }

        new FsKame.QA.FsColbertContextProvider(options) :> FsKame.QA.IQaContextProvider

    let private ensureSession (st: State) =
        match st.session with
        | Some session -> st, session
        | None ->
            let session = createSession st defaultFlags
            { st with session = Some session }, session

    let private createMemoryContext
        (request: MemoryRequest)
        (chunks: SourceChunk list)
        (inventory: KnowledgeSource list)
        =
        let decision =
            DurableMemory.createSupervisorDecision request.snapshot request.realtimeJudgement

        { requestId = request.requestId
          snapshot = request.snapshot
          supervisorDecision = decision
          context = chunks
          inventory = inventory
          memories = []
          conflicts = []
          cards = []
          timedOut = false
          createdAt = DateTimeOffset.UtcNow }

    let private answerRequest (st: State) (request: MemoryRequest) =
        async {
            let _st, session = ensureSession st

            try
                let qaRequest: FsKame.QA.QaTurnRequest =
                    { turnId = request.snapshot.turnId
                      question = request.snapshot.text
                      realtimeJudgement = request.realtimeJudgement |> Option.map toQaJudgement
                      deadline = Some request.deadline }

                let! answer = session.AnswerAsync(qaRequest, request.cancellationToken) |> Async.AwaitTask

                let chunks: SourceChunk list = answer.context |> List.map toWorkflowChunk
                let inventory: KnowledgeSource list = answer.inventory |> List.map toWorkflowSource
                let context = createMemoryContext request chunks inventory

                request.completion.TrySetResult context |> ignore
                st.bus.PostToAgent(Ag_ContextReady(request.snapshot, chunks, inventory))

                let candidate =
                    { turnId = request.snapshot.turnId
                      revision = request.snapshot.revision
                      source = answer.model
                      answer = answer.answer
                      context = chunks
                      isFinal = true
                      createdAt = answer.createdAt }

                st.bus.PostToAgent(Ag_ResponseReady(request.snapshot, Some candidate))
            with
            | :? OperationCanceledException ->
                request.completion.TrySetCanceled request.cancellationToken |> ignore
                st.bus.PostToAgent(Ag_ResponseReady(request.snapshot, None))
            | ex ->
                st.bus.PostToAgent(Ag_Log $"QA request failed: {ex.Message}")
                request.completion.TrySetException ex |> ignore
                st.bus.PostToAgent(Ag_ResponseReady(request.snapshot, None))
        }

    let private update (st: State) (msg: AgentMsg) =
        async {
            match msg with
            | Ag_SourcesUpdated(mode, sources, flags) ->
                match st.session with
                | Some session -> do! (session :> IAsyncDisposable).DisposeAsync().AsTask() |> Async.AwaitTask
                | None -> ()

                let flags: SourceFlags =
                    { logExpansions = flags.logExpansions
                      logChunks = flags.logChunks
                      useLexicalFilter = flags.useLexicalFilter
                      elaborateIndexKeywords = flags.elaborateIndexKeywords }

                let session = createSession st flags

                let provider = createContextProvider st flags mode sources

                let! errors = session.ConfigureAsync([ provider ], CancellationToken.None) |> Async.AwaitTask

                for err in errors do
                    st.bus.PostToAgent(Ag_Log err)

                return
                    { st with
                        session = Some session
                        retrievalMode = mode
                        sources = sources }
            | Ag_MemoryRequested request ->
                answerRequest st request |> Async.Start
                return st
            | Ag_FlowDone _ ->
                match st.session with
                | Some session -> do! (session :> IAsyncDisposable).DisposeAsync().AsTask() |> Async.AwaitTask
                | None -> ()

                return { st with session = None }
            | _ -> return st
        }

    let start apiKey oracleModel bus =
        let st0 =
            { bus = bus
              apiKey = apiKey
              oracleModel = oracleModel
              session = None
              retrievalMode = FsColbertWithFallback
              sources = [] }

        bus.AgentBus.RunAsync("qa", st0, update) |> FlowUtils.catch bus.PostToFlow
