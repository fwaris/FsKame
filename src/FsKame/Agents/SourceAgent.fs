namespace FsKame.WorkFlow

open FSharp.Control
open FsKame
open RTFlow
open RTFlow.Functions

module SourceAgent =
    type State =
        { bus: WBus<FlowMsg, AgentMsg>
          apiKey: string option
          retrievalMode: RetrievalMode
          retrieval: KnowledgeSources.RetrievalIndex
          logExpansions: bool
          logChunks: bool }

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

    let private update (st: State) (msg: AgentMsg) =
        async {
            match msg with
            | Ag_SourcesUpdated(mode, sources, flags) -> 
                let st = { st with logExpansions = flags.logExpansions; logChunks = flags.logChunks }
                return! loadSources st mode sources
            | Ag_TranscriptUpdated snapshot when snapshot.isFinal ->
                let report msg = st.bus.PostToAgent(Ag_Log msg)
                let! context = KnowledgeSources.rank st.apiKey st.logExpansions st.logChunks report snapshot.text 6 st.retrieval
                st.bus.PostToAgent(Ag_ContextReady(snapshot, context, st.retrieval.sources))
                return st
            | Ag_FlowDone _ ->
                KnowledgeSources.disposeIndex st.retrieval
                return st
            | _ -> return st
        }

    let start apiKey bus =
        let st0 =
            { bus = bus
              apiKey = apiKey
              retrievalMode = FsColbertWithFallback
              retrieval = KnowledgeSources.emptyIndex
              logExpansions = false
              logChunks = false }

        bus.AgentBus.RunAsync("source", st0, update) |> FlowUtils.catch bus.PostToFlow
