namespace FsKame.WorkFlow

open System
open System.Threading
open FsKame
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

    let private renderCards (cards: MemoryCard list) =
        if List.isEmpty cards then
            "No tool observations were recorded."
        else
            cards
            |> List.truncate 12
            |> List.mapi (fun index card ->
                $"[{index + 1}] {card.kind}: {card.title}\n{Text.truncate 900 card.content}")
            |> String.concat "\n\n"

    let private renderTypedMemory (memoryContext: MemoryContext) =
        let memories =
            if List.isEmpty memoryContext.memories then
                "No durable typed memory records matched this turn."
            else
                memoryContext.memories
                |> List.truncate 12
                |> List.mapi (fun index hit ->
                    let record = hit.record

                    $"[{index + 1}] {DurableMemory.kindName record.kind}/{DurableMemory.statusName record.status} score={hit.score:F2}\n{record.title}\n{Text.truncate 700 record.text}")
                |> String.concat "\n\n"

        let conflicts =
            if List.isEmpty memoryContext.conflicts then
                "No conflict or supersession notes were detected."
            else
                memoryContext.conflicts |> String.concat "\n"

        $"Recall policy:\n{DurableMemory.renderRecallSpec memoryContext.supervisorDecision}\n\nTyped memory evidence:\n{memories}\n\nConflict and supersession notes:\n{conflicts}"

    let private prompt (memoryContext: MemoryContext) =
        let snapshot = memoryContext.snapshot
        let sourceContext = KnowledgeSources.renderContext memoryContext.context
        let inventory = KnowledgeSources.renderInventory memoryContext.inventory
        let observations = renderCards memoryContext.cards
        let typedMemory = renderTypedMemory memoryContext

        [ ChatMessage(
              ChatRole.System,
              "You are FsKame's backend oracle. Answer general questions directly when no tool, selected source context, or durable memory context is required. Use typed durable memory as long-horizon context: directives are standing instructions, decisions are prior choices, commitments are open loops, claims are factual context, and episodes are compressed prior conversation. Treat tool observations as authoritative for current/session-specific facts such as time, app state, and source lookup. Prefer current memory records over superseded or uncertain records unless the user asks for history. For volatile live information such as weather, breaking news, market prices, or stock quotes, only give current values when a tool observation provides them; otherwise say briefly that no live data tool is available for that value. Do not invent unavailable tool results, source details, citations, live values, or app state. Keep answers conversational and brief, usually one to three short sentences."
          )
          ChatMessage(
              ChatRole.User,
              $"User transcript:\n{snapshot.text}\n\nTyped durable memory envelope:\n{typedMemory}\n\nCurrent tool observations and task-board cards:\n{observations}\n\nSelected source inventory:\n{inventory}\n\nMatched source context:\n{sourceContext}\n\nReturn only what the realtime voice agent should say now."
          ) ]

    let private runOracle (st: State) (cancellationToken: CancellationToken) (memoryContext: MemoryContext) =
        async {
            let snapshot = memoryContext.snapshot

            match st.client with
            | None ->
                st.bus.PostToAgent(
                    Ag_Log "OpenAI key is missing; realtime will answer without backend oracle guidance."
                )

                st.bus.PostToAgent(Ag_ResponseReady(snapshot, None))
            | Some client ->
                try
                    st.bus.PostToAgent(
                        Ag_Log
                            $"Oracle request trace turn={snapshot.turnId} transcript_chars={snapshot.text.Length} source_chunks={memoryContext.context.Length} memories={memoryContext.memories.Length} cards={memoryContext.cards.Length} question='{Text.truncate 120 snapshot.text}'"
                    )

                    let opts = ChatOptions()

                    if ModelCapabilities.supportsTemperature st.modelId then
                        opts.Temperature <- Nullable 0.2f

                    opts.MaxOutputTokens <- Nullable 180

                    let! resp =
                        client.GetResponseAsync(prompt memoryContext, opts, cancellationToken)
                        |> Async.AwaitTask

                    let answer = resp.Text |> FsKame.Text.normalizeWhitespace

                    let candidate =
                        { turnId = snapshot.turnId
                          revision = snapshot.revision
                          source = st.modelId
                          answer = answer
                          context = memoryContext.context
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
                runOracle st request.cancellationToken memoryContext |> Async.Start

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
