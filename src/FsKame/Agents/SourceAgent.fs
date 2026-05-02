namespace FsKame.WorkFlow

open FSharp.Control
open RTFlow
open RTFlow.Functions

module SourceAgent =
    type State =
        { bus: WBus<FlowMsg, AgentMsg>
          sources: KnowledgeSource list
          chunks: SourceChunk list }

    let private loadSources (st: State) (sources: KnowledgeSource list) =
        async {
            st.bus.PostToAgent(Ag_Log $"Loading {sources.Length} knowledge source(s).")
            let! chunks, errors = KnowledgeSources.loadChunks sources

            for err in errors do
                st.bus.PostToAgent(Ag_Log err)

            st.bus.PostToAgent(Ag_Log $"Loaded {chunks.Length} source chunk(s).")

            return
                { st with
                    sources = sources
                    chunks = chunks }
        }

    let private update (st: State) (msg: AgentMsg) =
        async {
            match msg with
            | Ag_SourcesUpdated sources -> return! loadSources st sources
            | Ag_TranscriptUpdated snapshot when snapshot.isFinal ->
                let context = KnowledgeSources.rank snapshot.text 6 st.chunks
                st.bus.PostToAgent(Ag_ContextReady(snapshot, context, st.sources))
                return st
            | _ -> return st
        }

    let start bus =
        let st0 = { bus = bus; sources = []; chunks = [] }

        bus.AgentBus.RunAsync("source", st0, update) |> FlowUtils.catch bus.PostToFlow
