namespace FsKame.WorkFlow

open System
open System.Threading
open Microsoft.Extensions.AI
open FSharp.Control
open RTFlow
open RTFlow.Functions

module OracleAgent =
    type State =
        { bus: WBus<FlowMsg, AgentMsg>
          modelId: string
          client: IChatClient option }

    let private createClient (key: string) (modelId: string) : IChatClient =
        let client = OpenAI.OpenAIClient(key)
        client.GetResponsesClient().AsIChatClient(modelId)

    let private prompt (snapshot: TranscriptSnapshot) (context: SourceChunk list) (inventory: KnowledgeSource list) =
        let sourceContext = KnowledgeSources.renderContext context
        let inventory = KnowledgeSources.renderInventory inventory

        [ ChatMessage(
              ChatRole.System,
              "You are FsKame's backend oracle in strict document mode. Answer only from the selected PDF inventory and matched PDF context. Do not use general knowledge. If the answer is not present in the selected PDFs, say briefly that the selected documents do not contain that answer. If no selected PDFs are available, say no documents are currently selected. Keep answers conversational and brief, usually one to three short sentences."
          )
          ChatMessage(
              ChatRole.User,
              $"User transcript:\n{snapshot.text}\n\nSelected PDF inventory:\n{inventory}\n\nMatched PDF context:\n{sourceContext}\n\nReturn only what the realtime voice agent should say now."
          ) ]

    let private runOracle
        (st: State)
        (cancellationToken: CancellationToken)
        (snapshot: TranscriptSnapshot)
        (context: SourceChunk list)
        (inventory: KnowledgeSource list)
        =
        async {
            match st.client with
            | None ->
                st.bus.PostToAgent(
                    Ag_Log "OpenAI key is missing; realtime will answer without backend oracle guidance."
                )

                st.bus.PostToAgent(Ag_ResponseReady(snapshot, None))
            | Some client ->
                try
                    st.bus.PostToAgent(Ag_Log $"Oracle request for turn {snapshot.turnId}.")
                    let opts = ChatOptions()
                    opts.Temperature <- Nullable 0.2f
                    opts.MaxOutputTokens <- Nullable 180

                    let! resp =
                        client.GetResponseAsync(prompt snapshot context inventory, opts, cancellationToken)
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
            | Ag_OracleRequested(request, memoryContext) ->
                runOracle st request.cancellationToken request.snapshot memoryContext.context memoryContext.inventory
                |> Async.Start

                return st
            | Ag_FlowDone _ ->
                st.client |> Option.iter (fun client -> client.Dispose())
                return st
            | _ -> return st
        }

    let start apiKey modelId bus =
        let client =
            if String.IsNullOrWhiteSpace apiKey then
                None
            else
                Some(createClient apiKey modelId)

        let st0 =
            { bus = bus
              modelId = modelId
              client = client }

        bus.AgentBus.RunAsync("oracle", st0, update) |> FlowUtils.catch bus.PostToFlow
