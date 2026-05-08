namespace FsKame.WorkFlow

open System
open System.Threading
open FsKame
open RTFlow
open RTFlow.Functions

module MemoryAgent =
    type State =
        { bus: WBus<FlowMsg, AgentMsg>
          retrievalMode: RetrievalMode
          retrieval: KnowledgeSources.RetrievalIndex
          logExpansions: bool
          logChunks: bool
          useLexicalFilter: bool
          inFlight: Map<string, MemoryRequest> }

    let private loadSources (st: State) mode (sources: KnowledgeSource list) =
        async {
            st.bus.PostToAgent(
                Ag_Log $"Loading {sources.Length} knowledge source(s) with {RetrievalModes.displayName mode}."
            )

            let report msg = st.bus.PostToAgent(Ag_Log msg)

            let! retrieval, errors =
                match mode with
                | InternalDocumentIndex -> KnowledgeSources.loadInternalIndex sources
                | FsColbertWithFallback -> KnowledgeSources.loadIndex report sources

            for err in errors do
                st.bus.PostToAgent(Ag_Log err)

            KnowledgeSources.disposeIndex st.retrieval
            st.bus.PostToAgent(Ag_Log $"Loaded {retrieval.chunks.Length} source chunk(s).")

            return
                { st with
                    retrievalMode = mode
                    retrieval = retrieval }
        }

    let private remainingTimeout (request: MemoryRequest) =
        let remaining = request.deadline - DateTimeOffset.UtcNow

        if remaining <= TimeSpan.Zero then
            TimeSpan.Zero
        else
            remaining

    let private createContext
        (request: MemoryRequest)
        (retrieval: KnowledgeSources.RetrievalIndex)
        (chunks: SourceChunk list)
        timedOut
        : MemoryContext =
        { requestId = request.requestId
          snapshot = request.snapshot
          context = chunks
          inventory = retrieval.sources
          timedOut = timedOut
          createdAt = DateTimeOffset.UtcNow }

    let private chunkKey (chunk: SourceChunk) =
        chunk.source.location.ToLowerInvariant(), chunk.index

    let private sourceEquals left right =
        String.Equals(left.location, right.location, StringComparison.OrdinalIgnoreCase)

    let private tryNeighbor (retrieval: KnowledgeSources.RetrievalIndex) offset (chunk: SourceChunk) =
        retrieval.chunks
        |> List.tryFind (fun candidate ->
            sourceEquals candidate.source chunk.source
            && candidate.index = chunk.index + offset)
        |> Option.map (fun neighbor ->
            { neighbor with
                score = max neighbor.score (chunk.score * 0.85f) })

    let private contextCandidates (retrieval: KnowledgeSources.RetrievalIndex) (ranked: SourceChunk list) =
        let seedCount = min C.REALTIME_MEMORY_NEIGHBOR_SEEDS ranked.Length

        let expanded =
            ranked
            |> List.truncate seedCount
            |> List.collect (fun chunk ->
                [ tryNeighbor retrieval -1 chunk; Some chunk; tryNeighbor retrieval 1 chunk ]
                |> List.choose id)

        ranked @ expanded
        |> List.fold
            (fun (seen, chunks) chunk ->
                let key = chunkKey chunk

                if Set.contains key seen then
                    seen, chunks
                else
                    Set.add key seen, chunk :: chunks)
            (Set.empty, [])
        |> snd
        |> List.rev
        |> List.sortByDescending (fun chunk -> chunk.score)
        |> List.truncate C.REALTIME_MEMORY_MAX_CONTEXT_CHUNKS

    let private startMemoryResolution (st: State) (request: MemoryRequest) =
        let work =
            async {
                st.bus.PostToAgent(Ag_MemoryJobStarted request.requestId)

                let report msg = st.bus.PostToAgent(Ag_Log msg)
                let timeout = remainingTimeout request

                use timeoutCts = new CancellationTokenSource()

                if timeout = TimeSpan.Zero then
                    timeoutCts.Cancel()
                else
                    timeoutCts.CancelAfter timeout

                use linkedCts =
                    CancellationTokenSource.CreateLinkedTokenSource(request.cancellationToken, timeoutCts.Token)

                try
                    let ranking =
                        KnowledgeSources.rank
                            None
                            st.logExpansions
                            st.logChunks
                            st.useLexicalFilter
                            report
                            request.snapshot.text
                            C.REALTIME_MEMORY_CANDIDATE_CHUNKS
                            st.retrieval

                    let rankingTask = Async.StartAsTask(ranking, cancellationToken = linkedCts.Token)

                    let! ranked = rankingTask.WaitAsync(timeout, linkedCts.Token) |> Async.AwaitTask
                    let chunks = contextCandidates st.retrieval ranked

                    let context = createContext request st.retrieval chunks false
                    st.bus.PostToAgent(Ag_MemoryReady(request, context))
                    st.bus.PostToAgent(Ag_MemoryJobFinished(request.requestId, Ok()))
                with
                | :? OperationCanceledException when request.cancellationToken.IsCancellationRequested ->
                    st.bus.PostToAgent(Ag_MemoryRequestCanceled request.requestId)
                    st.bus.PostToAgent(Ag_MemoryJobFinished(request.requestId, Error "Canceled"))
                | :? TimeoutException ->
                    let context = createContext request st.retrieval [] true
                    st.bus.PostToAgent(Ag_MemoryReady(request, context))
                    st.bus.PostToAgent(Ag_MemoryJobFinished(request.requestId, Error "Timed out"))
                | :? OperationCanceledException ->
                    let context = createContext request st.retrieval [] true
                    st.bus.PostToAgent(Ag_MemoryReady(request, context))
                    st.bus.PostToAgent(Ag_MemoryJobFinished(request.requestId, Error "Timed out"))
                | ex ->
                    st.bus.PostToAgent(Ag_MemoryRequestFailed(request.requestId, ex.Message))
                    st.bus.PostToAgent(Ag_MemoryJobFinished(request.requestId, Error ex.Message))
            }

        Async.Start work

    let private completeReadyRequest (st: State) (request: MemoryRequest) (context: MemoryContext) =
        match st.inFlight |> Map.tryFind request.requestId with
        | None ->
            st.bus.PostToAgent(Ag_Log $"Ignoring late memory result for request {request.requestId}.")
            st
        | Some current when not (obj.ReferenceEquals(current.completion, request.completion)) ->
            st.bus.PostToAgent(Ag_Log $"Ignoring superseded memory result for request {request.requestId}.")
            st
        | Some current ->
            let completed =
                if current.cancellationToken.IsCancellationRequested then
                    current.completion.TrySetCanceled current.cancellationToken
                else
                    current.completion.TrySetResult context

            if completed && not current.cancellationToken.IsCancellationRequested then
                if context.timedOut then
                    st.bus.PostToAgent(
                        Ag_Log
                            $"Memory request {request.requestId} reached its realtime deadline; answering with fast context."
                    )

                st.bus.PostToAgent(Ag_ContextReady(current.snapshot, context.context, context.inventory))
                st.bus.PostToAgent(Ag_OracleRequested(current, context))

            { st with
                inFlight = st.inFlight |> Map.remove request.requestId }

    let private cancelRequest (st: State) requestId =
        match st.inFlight |> Map.tryFind requestId with
        | None -> st
        | Some request ->
            request.completion.TrySetCanceled request.cancellationToken |> ignore
            st.bus.PostToAgent(Ag_Log $"Memory request {requestId} was canceled.")

            { st with
                inFlight = st.inFlight |> Map.remove requestId }

    let private failRequest (st: State) requestId message =
        match st.inFlight |> Map.tryFind requestId with
        | None -> st
        | Some request ->
            request.completion.TrySetException(InvalidOperationException message) |> ignore
            st.bus.PostToAgent(Ag_Log $"Memory request {requestId} failed: {message}")
            st.bus.PostToAgent(Ag_ResponseReady(request.snapshot, None))

            { st with
                inFlight = st.inFlight |> Map.remove requestId }

    let private update (st: State) (msg: AgentMsg) =
        async {
            match msg with
            | Ag_SourcesUpdated(mode, sources, flags) ->
                let st =
                    { st with
                        logExpansions = flags.logExpansions
                        logChunks = flags.logChunks
                        useLexicalFilter = flags.useLexicalFilter }

                return! loadSources st mode sources
            | Ag_MemoryRequested request ->
                if request.cancellationToken.IsCancellationRequested then
                    request.completion.TrySetCanceled request.cancellationToken |> ignore
                    return st
                else
                    startMemoryResolution st request

                    return
                        { st with
                            inFlight = st.inFlight |> Map.add request.requestId request }
            | Ag_MemoryReady(request, context) -> return completeReadyRequest st request context
            | Ag_MemoryRequestCanceled requestId -> return cancelRequest st requestId
            | Ag_MemoryRequestFailed(requestId, message) -> return failRequest st requestId message
            | Ag_MemoryJobStarted jobId ->
                st.bus.PostToAgent(Ag_Log $"Memory job started: {jobId}")
                return st
            | Ag_MemoryJobFinished(jobId, result) ->
                match result with
                | Ok() -> st.bus.PostToAgent(Ag_Log $"Memory job finished: {jobId}")
                | Error err -> st.bus.PostToAgent(Ag_Log $"Memory job finished with fallback: {jobId}; {err}")

                return st
            | Ag_FlowDone _ ->
                KnowledgeSources.disposeIndex st.retrieval
                return st
            | _ -> return st
        }

    let start bus =
        let st0 =
            { bus = bus
              retrievalMode = FsColbertWithFallback
              retrieval = KnowledgeSources.emptyIndex
              logExpansions = false
              logChunks = false
              useLexicalFilter = true
              inFlight = Map.empty }

        bus.AgentBus.RunAsync("memory", st0, update) |> FlowUtils.catch bus.PostToFlow
