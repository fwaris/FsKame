namespace FsKame.WorkFlow

open System
open Microsoft.Extensions.AI
open FSharp.Control
open RTFlow
open RTFlow.Functions

module OracleAgent =
    type State =
        { bus: WBus<FlowMsg, AgentMsg>
          apiKey: string
          modelId: string }

    let private createClient (key: string) (modelId: string) : IChatClient =
        let client = OpenAI.OpenAIClient(key)
        client.GetResponsesClient().AsIChatClient(modelId)

    let private prompt (snapshot: TranscriptSnapshot) (context: SourceChunk list) (inventory: KnowledgeSource list) =
        let sourceContext = KnowledgeSources.renderContext context
        let inventory = KnowledgeSources.renderInventory inventory

        [ ChatMessage(
              ChatRole.System,
              "You are FsKame's backend oracle. Answer the user's question briefly, usually in one to three short sentences. Do not include preambles, long caveats, implementation details, or exhaustive lists unless the user asks for them. Prefer the supplied PDF context when it is relevant. If the user asks which documents or sources are available, answer from the supplied PDF inventory. If the selected PDF context does not answer the question, say so briefly and answer from general knowledge only when appropriate."
          )
          ChatMessage(
              ChatRole.User,
              $"User transcript:\n{snapshot.text}\n\nSelected source inventory:\n{inventory}\n\nMatched external context:\n{sourceContext}\n\nReturn the answer the realtime voice agent should say now."
          ) ]

    let private runOracle
        (st: State)
        (snapshot: TranscriptSnapshot)
        (context: SourceChunk list)
        (inventory: KnowledgeSource list)
        =
        async {
            if String.IsNullOrWhiteSpace st.apiKey then
                st.bus.PostToAgent(
                    Ag_Log "OpenAI key is missing; realtime will answer without backend oracle guidance."
                )

                st.bus.PostToAgent(Ag_ResponseReady(snapshot, None))
            else
                try
                    st.bus.PostToAgent(Ag_Log $"Oracle request for turn {snapshot.turnId}.")
                    use client = createClient st.apiKey st.modelId
                    let opts = ChatOptions()
                    opts.Temperature <- Nullable 0.2f
                    opts.MaxOutputTokens <- Nullable 300

                    let! resp =
                        client.GetResponseAsync(prompt snapshot context inventory, opts)
                        |> Async.AwaitTask

                    let answer = resp.Text |> FsKame.Text.normalizeWhitespace

                    let candidate =
                        { turnId = snapshot.turnId
                          revision = snapshot.revision
                          source = st.modelId
                          answer = answer
                          context = context
                          isFinal = true
                          createdAt = DateTimeOffset.UtcNow }

                    st.bus.PostToAgent(Ag_ResponseReady(snapshot, Some candidate))
                with ex ->
                    st.bus.PostToAgent(Ag_Log $"Oracle failed: {ex.Message}")
                    st.bus.PostToAgent(Ag_ResponseReady(snapshot, None))
        }

    let private update (st: State) (msg: AgentMsg) =
        async {
            match msg with
            | Ag_ContextReady(snapshot, context, inventory) ->
                do! runOracle st snapshot context inventory
                return st
            | _ -> return st
        }

    let start apiKey modelId bus =
        let st0 =
            { bus = bus
              apiKey = apiKey
              modelId = modelId }

        bus.AgentBus.RunAsync("oracle", st0, update) |> FlowUtils.catch bus.PostToFlow
