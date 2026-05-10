namespace FsKame.WorkFlow

open System
open System.Text.Json
open System.Threading
open FsKame
open Microsoft.Extensions.AI
open Microsoft.SemanticKernel
open RTFlow
open RTFlow.Functions

module MemoryAgent =
    type private PlannedToolCall =
        { pluginName: string
          functionName: string
          query: string
          maxResults: int
          reason: string }

    type private MemoryTurnPlan =
        { decision: SupervisorDecision
          toolCalls: PlannedToolCall list
          selectedSourceQuery: string option
          selectedSourceMaxResults: int
          usedLlm: bool
          judgeNotes: string list }

    type State =
        { bus: WBus<FlowMsg, AgentMsg>
          controllerModelId: string
          largeControllerModelId: string
          retrievalMode: RetrievalMode
          retrieval: KnowledgeSources.RetrievalIndex
          memoryStore: DurableMemory.Store
          toolHostContext: ToolHostContext
          toolCatalog: ToolCatalog
          smallController: IChatClient option
          largeController: IChatClient option
          logExpansions: bool
          logChunks: bool
          useLexicalFilter: bool
          inFlight: Map<string, MemoryRequest>
          recentContexts: Map<string, MemoryContext> }

    let private createClient (key: string) (modelId: string) : IChatClient =
        let client = OpenAI.OpenAIClient(key)
        client.GetResponsesClient().AsIChatClient(modelId)

    let private card kind title content source =
        { kind = kind
          title = title
          content = content
          source = source
          createdAt = DateTimeOffset.UtcNow }

    let private rememberCards (st: State) cards =
        for card in cards do
            st.toolHostContext.Blackboard.Enqueue card

    let private clearRememberedCards (st: State) = st.toolHostContext.Blackboard.Clear()

    let private configureToolContext (st: State) =
        st.toolHostContext.Retrieval <- st.retrieval
        st.toolHostContext.MemoryStore <- st.memoryStore
        st.toolHostContext.LogExpansions <- st.logExpansions
        st.toolHostContext.LogChunks <- st.logChunks
        st.toolHostContext.UseLexicalFilter <- st.useLexicalFilter

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

            let! memoryStore, memoryIndexLogs =
                match retrieval.encoder, List.isEmpty st.memoryStore.records with
                | Some encoder, false -> DurableMemory.buildMainIndex report encoder st.memoryStore
                | _ -> async { return st.memoryStore, [] }

            for log in memoryIndexLogs do
                st.bus.PostToAgent(Ag_Log log)

            let st =
                { st with
                    retrievalMode = mode
                    retrieval = retrieval
                    memoryStore = memoryStore }

            configureToolContext st
            return st
        }

    let private remainingTimeout (request: MemoryRequest) =
        let remaining = request.deadline - DateTimeOffset.UtcNow

        if remaining <= TimeSpan.Zero then
            TimeSpan.Zero
        else
            remaining

    let private createContext
        (request: MemoryRequest)
        (decision: SupervisorDecision)
        (retrieval: KnowledgeSources.RetrievalIndex)
        (chunks: SourceChunk list)
        (memories: MemoryRecallHit list)
        (conflicts: string list)
        (cards: MemoryCard list)
        timedOut
        : MemoryContext =
        { requestId = request.requestId
          snapshot = request.snapshot
          supervisorDecision = decision
          context = chunks
          inventory = retrieval.sources
          memories = memories
          conflicts = conflicts
          cards = cards
          timedOut = timedOut
          createdAt = DateTimeOffset.UtcNow }

    let private looksLikeInventoryQuestion (text: string) =
        let text = text.ToLowerInvariant()

        [ "what documents"
          "which documents"
          "selected documents"
          "sources"
          "which sources"
          "inventory"
          "files" ]
        |> List.exists text.Contains

    let private looksLikeFollowUp (text: string) =
        let text = text.ToLowerInvariant()

        [ "that"; "those"; "it"; "previous"; "earlier"; "follow up"; "again" ]
        |> List.exists text.Contains

    let private looksLikeTimeQuestion (text: string) =
        let text = text.ToLowerInvariant()

        [ "what time"
          "what is the time"
          "what's the time"
          "tell me the time"
          "current time"
          "time is it"
          "today's date"
          "todays date"
          "current date"
          "what date" ]
        |> List.exists text.Contains

    let private looksLikeMemoryQuestion (text: string) =
        let text = text.ToLowerInvariant()

        [ "remember"
          "memory"
          "preference"
          "decided"
          "decision"
          "earlier"
          "previously"
          "follow up"
          "open loop"
          "commitment" ]
        |> List.exists text.Contains

    let private deterministicToolCalls (snapshot: TranscriptSnapshot) =
        let text = snapshot.text

        [ if looksLikeTimeQuestion text then
              yield
                  { pluginName = "FsKameTools"
                    functionName = "current_time"
                    query = text
                    maxResults = 1
                    reason = "Deterministic current-time cue." }

          if looksLikeMemoryQuestion text then
              yield
                  { pluginName = "FsKameTools"
                    functionName = "durable_memory_search"
                    query = text
                    maxResults = C.REALTIME_MEMORY_CANDIDATE_CHUNKS
                    reason = "Deterministic memory-reference cue." }

          if looksLikeInventoryQuestion text then
              yield
                  { pluginName = "FsKameTools"
                    functionName = "source_inventory"
                    query = text
                    maxResults = C.REALTIME_MEMORY_CANDIDATE_CHUNKS
                    reason = "Deterministic source-inventory cue." }

          if looksLikeFollowUp text then
              yield
                  { pluginName = "FsKameTools"
                    functionName = "blackboard_search"
                    query = text
                    maxResults = 8
                    reason = "Deterministic follow-up cue." }

          if not (String.IsNullOrWhiteSpace snapshot.text) then
              yield
                  { pluginName = "FsKameTools"
                    functionName = "selected_source_search"
                    query = text
                    maxResults = C.REALTIME_MEMORY_CANDIDATE_CHUNKS
                    reason = "Fallback selected-source grounding." } ]

    let private deterministicPlan (snapshot: TranscriptSnapshot) judgement =
        let decision = DurableMemory.createSupervisorDecision snapshot judgement
        let toolCalls = deterministicToolCalls snapshot

        { decision = decision
          toolCalls = toolCalls
          selectedSourceQuery =
            toolCalls
            |> List.tryFind (fun call -> call.functionName = "selected_source_search")
            |> Option.map _.query
          selectedSourceMaxResults = C.REALTIME_MEMORY_CANDIDATE_CHUNKS
          usedLlm = false
          judgeNotes = [ "deterministic fallback plan" ] }

    let private availableToolKeys (catalog: ToolCatalog) =
        catalog.plugins
        |> List.collect (fun plugin -> plugin |> Seq.map (fun fn -> $"{plugin.Name}.{fn.Name}") |> Seq.toList)
        |> Set.ofList

    let private toolInventory (catalog: ToolCatalog) =
        catalog.plugins
        |> List.collect (fun plugin -> plugin |> Seq.map (fun fn -> $"{plugin.Name}.{fn.Name}") |> Seq.toList)
        |> List.sort
        |> String.concat "\n"

    let private extractJsonObject (text: string) =
        let text =
            text |> Option.ofObj |> Option.defaultValue "" |> (fun value -> value.Trim())

        let text =
            if text.StartsWith("```") then
                text.Replace("```json", "").Replace("```", "").Trim()
            else
                text

        let first = text.IndexOf('{')
        let last = text.LastIndexOf('}')

        if first >= 0 && last >= first then
            text.Substring(first, last - first + 1)
        else
            text

    let private tryGetProperty (element: JsonElement) (name: string) =
        let mutable prop = Unchecked.defaultof<JsonElement>

        if element.ValueKind = JsonValueKind.Object && element.TryGetProperty(name, &prop) then
            Some prop
        else
            None

    let private stringProp name (element: JsonElement) =
        tryGetProperty element name
        |> Option.bind (fun prop ->
            match prop.ValueKind with
            | JsonValueKind.String -> prop.GetString() |> Option.ofObj |> Option.bind Text.notEmpty
            | JsonValueKind.Number -> Some(prop.GetRawText())
            | JsonValueKind.True -> Some "true"
            | JsonValueKind.False -> Some "false"
            | _ -> None)

    let private boolProp name (element: JsonElement) =
        stringProp name element
        |> Option.bind (fun value ->
            match value.Trim().ToLowerInvariant() with
            | "true"
            | "yes"
            | "1" -> Some true
            | "false"
            | "no"
            | "0" -> Some false
            | _ -> None)

    let private intProp name (element: JsonElement) =
        stringProp name element
        |> Option.bind (fun value ->
            match Int32.TryParse value with
            | true, parsed -> Some parsed
            | false, _ -> None)

    let private floatProp name (element: JsonElement) =
        stringProp name element
        |> Option.bind (fun value ->
            match Double.TryParse value with
            | true, parsed -> Some parsed
            | false, _ -> None)

    let private stringListProp name (element: JsonElement) =
        tryGetProperty element name
        |> Option.map (fun prop ->
            match prop.ValueKind with
            | JsonValueKind.Array ->
                prop.EnumerateArray()
                |> Seq.choose (fun value ->
                    match value.ValueKind with
                    | JsonValueKind.String -> value.GetString() |> Option.ofObj |> Option.bind Text.notEmpty
                    | _ -> None)
                |> Seq.toList
            | JsonValueKind.String ->
                prop.GetString()
                |> Option.ofObj
                |> Option.map Text.splitLines
                |> Option.defaultValue []
            | _ -> [])
        |> Option.defaultValue []

    let private parseMemoryKind value =
        match
            value
            |> Option.defaultValue ""
            |> fun value -> value.Trim().ToLowerInvariant()
        with
        | "directive" -> Some Directive
        | "claim" -> Some Claim
        | "decision" -> Some Decision
        | "commitment" -> Some Commitment
        | "episode" -> Some Episode
        | _ -> None

    let private parseMemoryScope value =
        match
            value
            |> Option.defaultValue ""
            |> fun value -> value.Trim().ToLowerInvariant()
        with
        | "session" -> Some Session
        | "user" -> Some User
        | "workspace" -> Some Workspace
        | "global" -> Some Global
        | _ -> None

    let private parseSensitivity value =
        match
            value
            |> Option.defaultValue ""
            |> fun value -> value.Trim().ToLowerInvariant()
        with
        | "normal" -> Some Normal
        | "personal" -> Some Personal
        | "sensitive" -> Some Sensitive
        | "secret" -> Some Secret
        | _ -> None

    let private parseTemporalMode value =
        match
            value
            |> Option.defaultValue ""
            |> fun value -> value.Trim().ToLowerInvariant()
        with
        | "current_only" -> Some CurrentOnly
        | "as_of_time" -> Some AsOfTime
        | "changed_since" -> Some ChangedSince
        | "include_history" -> Some IncludeHistory
        | _ -> None

    let private parseRecallBudget value =
        match
            value
            |> Option.defaultValue ""
            |> fun value -> value.Trim().ToLowerInvariant()
        with
        | "fast" -> Some Fast
        | "medium" -> Some Medium
        | "large" -> Some Large
        | "conflict_probe" -> Some ConflictProbe
        | _ -> None

    let private parseControllerPath value =
        match
            value
            |> Option.defaultValue ""
            |> fun value -> value.Trim().ToLowerInvariant()
        with
        | "zero_controller" -> Some ZeroController
        | "small_controller" -> Some SmallController
        | "large_controller" -> Some LargeController
        | "hybrid_small_then_large" -> Some HybridSmallThenLarge
        | _ -> None

    let private clampCandidates budget value =
        let upper =
            match budget with
            | Fast -> 20
            | Medium -> 80
            | Large -> 160
            | ConflictProbe -> 50

        max 1 (min upper value)

    let private budgetLatency =
        function
        | Fast -> TimeSpan.FromMilliseconds 90.
        | Medium -> TimeSpan.FromMilliseconds 180.
        | Large -> TimeSpan.FromMilliseconds 360.
        | ConflictProbe -> TimeSpan.FromMilliseconds 160.

    let private runJsonController
        (client: IChatClient)
        (modelId: string)
        (maxTokens: int)
        (cancellationToken: CancellationToken)
        (systemPrompt: string)
        (userPrompt: string)
        =
        async {
            let opts = ChatOptions()

            if ModelCapabilities.supportsTemperature modelId then
                opts.Temperature <- Nullable 0.1f

            opts.MaxOutputTokens <- Nullable maxTokens

            let messages =
                [ ChatMessage(ChatRole.System, systemPrompt)
                  ChatMessage(ChatRole.User, userPrompt) ]

            let! response = client.GetResponseAsync(messages, opts, cancellationToken) |> Async.AwaitTask
            return response.Text |> extractJsonObject
        }

    let private tryParseJsonObject (text: string) =
        try
            let doc = JsonDocument.Parse(text)
            let root = doc.RootElement.Clone()
            Some root
        with _ ->
            None

    let private judgePrompt
        (prefix: string)
        (snapshot: TranscriptSnapshot)
        (judgement: RealtimeJudgement option)
        (extra: string)
        =
        let hints =
            match judgement with
            | None -> "No realtime judgement hints were supplied."
            | Some hint ->
                let turnKind = hint.turnKind |> Option.defaultValue "unknown"
                let topicContinuity = hint.topicContinuity |> Option.defaultValue "unknown"
                let memoryAction = hint.memoryAction |> Option.defaultValue "unknown"

                let needsExternalContext =
                    hint.needsExternalContext |> Option.map string |> Option.defaultValue "unknown"

                [ $"turn_kind={turnKind}"
                  $"topic_continuity={topicContinuity}"
                  $"memory_action={memoryAction}"
                  $"needs_external_context={needsExternalContext}"
                  $"confidence={hint.confidence:F2}"
                  $"memory_mutation={hint.riskFlags.memoryMutation}"
                  $"sensitive={hint.riskFlags.sensitive}"
                  $"conflict_likely={hint.riskFlags.conflictLikely}" ]
                |> String.concat "\n"

        $"{prefix}\n\nUser turn:\n{snapshot.text}\n\nRealtime judgement hints:\n{hints}\n\n{extra}"

    let private parseRiskFlags fallback (root: JsonElement) =
        { memoryMutation = boolProp "memory_mutation" root |> Option.defaultValue fallback.memoryMutation
          sensitive = boolProp "sensitive" root |> Option.defaultValue fallback.sensitive
          conflictLikely = boolProp "conflict_likely" root |> Option.defaultValue fallback.conflictLikely }

    let private parseRecallSpec (fallback: RecallSpec) (root: JsonElement) =
        let budget =
            parseRecallBudget (stringProp "recall_budget" root)
            |> Option.defaultValue fallback.recallBudget

        let kinds =
            stringListProp "kinds" root
            |> List.choose (Some >> parseMemoryKind)
            |> fun kinds -> if List.isEmpty kinds then fallback.kinds else kinds

        let scopes =
            stringListProp "scopes" root
            |> List.choose (Some >> parseMemoryScope)
            |> fun scopes -> if List.isEmpty scopes then fallback.scopes else scopes

        { fallback with
            query =
                stringProp "query" root
                |> Option.defaultValue fallback.query
                |> Text.normalizeWhitespace
            kinds = kinds
            scopes = scopes
            temporalMode =
                parseTemporalMode (stringProp "temporal_mode" root)
                |> Option.defaultValue fallback.temporalMode
            includeSuperseded =
                boolProp "include_superseded" root
                |> Option.defaultValue fallback.includeSuperseded
            recallBudget = budget
            maxCandidates =
                intProp "max_candidates" root
                |> Option.defaultValue fallback.maxCandidates
                |> clampCandidates budget
            latencyBudget = budgetLatency budget }

    let private parseToolCalls availableTools (snapshot: TranscriptSnapshot) (root: JsonElement) =
        tryGetProperty root "tool_calls"
        |> Option.filter (fun prop -> prop.ValueKind = JsonValueKind.Array)
        |> Option.map (fun prop ->
            prop.EnumerateArray()
            |> Seq.choose (fun item ->
                let pluginName =
                    stringProp "plugin" item
                    |> Option.orElse (stringProp "plugin_name" item)
                    |> Option.defaultValue "FsKameTools"

                let functionName =
                    stringProp "function" item |> Option.orElse (stringProp "function_name" item)

                match functionName with
                | None -> None
                | Some functionName ->
                    let key = $"{pluginName}.{functionName}"

                    if Set.contains key availableTools then
                        Some
                            { pluginName = pluginName
                              functionName = functionName
                              query =
                                stringProp "query" item
                                |> Option.defaultValue snapshot.text
                                |> Text.normalizeWhitespace
                              maxResults =
                                intProp "max_results" item
                                |> Option.orElse (intProp "maxResults" item)
                                |> Option.defaultValue C.REALTIME_MEMORY_CANDIDATE_CHUNKS
                                |> fun value -> max 1 (min 30 value)
                              reason = stringProp "reason" item |> Option.defaultValue "LLM-planned tool call." }
                    else
                        None)
            |> Seq.distinctBy (fun call -> call.pluginName, call.functionName, call.query)
            |> Seq.truncate 8
            |> Seq.toList)
        |> Option.defaultValue []

    let private parseToolPlan availableTools (snapshot: TranscriptSnapshot) (root: JsonElement) =
        let toolCalls = parseToolCalls availableTools snapshot root

        let selectedSourceQuery =
            stringProp "selected_source_query" root
            |> Option.orElse (
                toolCalls
                |> List.tryFind (fun call -> call.functionName = "selected_source_search")
                |> Option.map _.query
            )

        let selectedSourceMaxResults =
            intProp "selected_source_max_results" root
            |> Option.defaultValue C.REALTIME_MEMORY_CANDIDATE_CHUNKS
            |> fun value -> max 1 (min C.REALTIME_MEMORY_CANDIDATE_CHUNKS value)

        toolCalls, selectedSourceQuery, selectedSourceMaxResults

    let private parseControllerPlan
        (fallback: MemoryTurnPlan)
        availableTools
        (snapshot: TranscriptSnapshot)
        (root: JsonElement)
        =
        let riskFlags = parseRiskFlags fallback.decision.riskFlags root

        let decision =
            { fallback.decision with
                controllerPath =
                    parseControllerPath (stringProp "controller_path" root)
                    |> Option.defaultValue fallback.decision.controllerPath
                recallSpec = parseRecallSpec fallback.decision.recallSpec root
                riskFlags = riskFlags
                acceptedRealtimeJudgement = false
                reason = stringProp "reason" root |> Option.defaultValue fallback.decision.reason }

        let toolCalls, selectedSourceQuery, selectedSourceMaxResults =
            parseToolPlan availableTools snapshot root

        { decision = decision
          toolCalls = toolCalls
          selectedSourceQuery = selectedSourceQuery
          selectedSourceMaxResults = selectedSourceMaxResults
          usedLlm = true
          judgeNotes = [ decision.reason ] }

    let private tryRunJudge name (work: Async<JsonElement option>) =
        async {
            try
                let! value = work
                return value |> Option.map (fun root -> name, root)
            with _ ->
                return None
        }

    let private runClassificationJudge (st: State) (request: MemoryRequest) cancellationToken =
        async {
            match st.smallController with
            | None -> return None
            | Some client ->
                let prompt =
                    judgePrompt
                        "Classify this realtime turn for FsKame's memory supervisor. Return JSON only."
                        request.snapshot
                        request.realtimeJudgement
                        """Schema:
{
  "controller_path": "zero_controller | small_controller | large_controller | hybrid_small_then_large",
  "memory_mutation": false,
  "sensitive": false,
  "conflict_likely": false,
  "reason": "brief reason"
}
Use large_controller for remember/forget/correction, sensitive data, ambiguity, or likely conflict."""

                let! json =
                    runJsonController
                        client
                        st.controllerModelId
                        220
                        cancellationToken
                        "You are a fast, conservative memory-turn classifier. Do not answer the user."
                        prompt

                return tryParseJsonObject json
        }

    let private runRecallJudge (st: State) (request: MemoryRequest) cancellationToken =
        async {
            match st.smallController with
            | None -> return None
            | Some client ->
                let prompt =
                    judgePrompt
                        "Create a compact RecallSpec for typed durable memory. Return JSON only."
                        request.snapshot
                        request.realtimeJudgement
                        """Schema:
{
  "query": "single standalone recall query",
  "kinds": ["directive","claim","decision","commitment","episode"],
  "scopes": ["session","user","workspace","global"],
  "temporal_mode": "current_only | as_of_time | changed_since | include_history",
  "include_superseded": false,
  "recall_budget": "fast | medium | large",
  "max_candidates": 60,
  "reason": "brief reason"
}
Prefer one broad query. Include history only for audit, old-state, correction, or contradiction turns."""

                let! json =
                    runJsonController
                        client
                        st.controllerModelId
                        340
                        cancellationToken
                        "You are a memory recall planner. Choose policy, not answers."
                        prompt

                return tryParseJsonObject json
        }

    let private runToolJudge (st: State) (request: MemoryRequest) cancellationToken =
        async {
            match st.smallController with
            | None -> return None
            | Some client ->
                let tools = toolInventory st.toolCatalog

                let prompt =
                    judgePrompt
                        "Choose memory-agent tool calls for this turn. Return JSON only."
                        request.snapshot
                        request.realtimeJudgement
                        $"""Available tools:
{tools}

Schema:
{{
  "tool_calls": [
    {{"plugin":"FsKameTools","function":"selected_source_search","query":"...","max_results":12,"reason":"..."}}
  ],
  "selected_source_query": "query for source chunk context or null",
  "selected_source_max_results": 12,
  "reason": "brief reason"
}}
Use current_time for time/date. Use source_inventory for document inventory questions. Use selected_source_search only when selected documents may matter. Use durable_memory_search for memory/preferences/decisions/history. Use blackboard_search for follow-ups."""

                let! json =
                    runJsonController
                        client
                        st.controllerModelId
                        420
                        cancellationToken
                        "You are a tool planner for FsKame's memory agent. Choose tool calls; do not answer."
                        prompt

                return tryParseJsonObject json
        }

    let private mergeSmallJudgeResults
        (fallback: MemoryTurnPlan)
        availableTools
        (snapshot: TranscriptSnapshot)
        results
        =
        ((fallback, []), results)
        ||> Array.fold (fun (plan, notes) result ->
            match result with
            | None -> plan, notes
            | Some(name, root) ->
                match name with
                | "classification" ->
                    let riskFlags = parseRiskFlags plan.decision.riskFlags root

                    let decision =
                        { plan.decision with
                            controllerPath =
                                parseControllerPath (stringProp "controller_path" root)
                                |> Option.defaultValue plan.decision.controllerPath
                            riskFlags = riskFlags
                            acceptedRealtimeJudgement = false
                            reason = stringProp "reason" root |> Option.defaultValue plan.decision.reason }

                    { plan with
                        decision = decision
                        usedLlm = true },
                    notes @ [ $"classification: {decision.reason}" ]
                | "recall" ->
                    let recallSpec = parseRecallSpec plan.decision.recallSpec root
                    let reason = stringProp "reason" root |> Option.defaultValue "LLM recall judge."

                    { plan with
                        decision =
                            { plan.decision with
                                recallSpec = recallSpec
                                reason = reason }
                        usedLlm = true },
                    notes @ [ $"recall: {reason}" ]
                | "tools" ->
                    let toolCalls, selectedSourceQuery, selectedSourceMaxResults =
                        parseToolPlan availableTools snapshot root

                    let reason = stringProp "reason" root |> Option.defaultValue "LLM tool judge."

                    { plan with
                        toolCalls = toolCalls
                        selectedSourceQuery = selectedSourceQuery
                        selectedSourceMaxResults = selectedSourceMaxResults
                        usedLlm = true },
                    notes @ [ $"tools: {reason}" ]
                | _ -> plan, notes)
        |> fun (plan, notes) ->
            if List.isEmpty notes then
                plan
            else
                { plan with judgeNotes = notes }

    let private largeControllerPrompt
        (plan: MemoryTurnPlan)
        (snapshot: TranscriptSnapshot)
        (judgement: RealtimeJudgement option)
        availableTools
        =
        let kinds =
            plan.decision.recallSpec.kinds
            |> List.map DurableMemory.kindName
            |> String.concat ","

        let tools =
            plan.toolCalls
            |> List.map (fun call -> $"{call.pluginName}.{call.functionName}:{call.query}")
            |> String.concat " | "

        let smallPlan =
            [ $"controller_path={DurableMemory.controllerPathName plan.decision.controllerPath}"
              $"reason={plan.decision.reason}"
              $"recall_query={plan.decision.recallSpec.query}"
              $"kinds={kinds}"
              $"temporal_mode={DurableMemory.temporalModeName plan.decision.recallSpec.temporalMode}"
              $"budget={DurableMemory.budgetName plan.decision.recallSpec.recallBudget}"
              $"tools={tools}" ]
            |> String.concat "\n"

        judgePrompt
            "Adjudicate the final memory-controller plan. Return JSON only."
            snapshot
            judgement
            $"""Available tools:
{availableTools}

Small-judge draft:
{smallPlan}

Return the final plan with this schema:
{{
  "controller_path": "large_controller | hybrid_small_then_large | small_controller",
  "memory_mutation": false,
  "sensitive": false,
  "conflict_likely": false,
  "query": "single standalone recall query",
  "kinds": ["directive","claim","decision","commitment","episode"],
  "scopes": ["session","user","workspace","global"],
  "temporal_mode": "current_only | as_of_time | changed_since | include_history",
  "include_superseded": false,
  "recall_budget": "medium | large",
  "max_candidates": 80,
  "tool_calls": [
    {{"plugin":"FsKameTools","function":"durable_memory_search","query":"...","max_results":20,"reason":"..."}}
  ],
  "selected_source_query": "query for selected source chunk context or null",
  "selected_source_max_results": 12,
  "reason": "brief reason"
}}
Use this only to adjudicate corrections, memory mutations, sensitive turns, ambiguous references, history/audit, or conflicting evidence."""

    let private shouldEscalateToLarge (plan: MemoryTurnPlan) =
        plan.decision.controllerPath = LargeController
        || plan.decision.controllerPath = HybridSmallThenLarge
        || plan.decision.riskFlags.memoryMutation
        || plan.decision.riskFlags.sensitive
        || plan.decision.riskFlags.conflictLikely
        || plan.decision.recallSpec.temporalMode = IncludeHistory

    let private runLargeController
        (st: State)
        (request: MemoryRequest)
        availableTools
        (plan: MemoryTurnPlan)
        cancellationToken
        =
        async {
            match st.largeController with
            | None -> return plan
            | Some client when not (shouldEscalateToLarge plan) -> return plan
            | Some client ->
                try
                    let prompt =
                        largeControllerPrompt
                            plan
                            request.snapshot
                            request.realtimeJudgement
                            (toolInventory st.toolCatalog)

                    let! json =
                        runJsonController
                            client
                            st.largeControllerModelId
                            620
                            cancellationToken
                            "You are FsKame's larger memory controller. Adjudicate hard turns; do not answer the user."
                            prompt

                    match tryParseJsonObject json with
                    | None -> return plan
                    | Some root ->
                        let largePlan = parseControllerPlan plan availableTools request.snapshot root

                        return
                            { largePlan with
                                decision =
                                    { largePlan.decision with
                                        controllerPath =
                                            if largePlan.decision.controllerPath = SmallController then
                                                HybridSmallThenLarge
                                            else
                                                largePlan.decision.controllerPath }
                                judgeNotes = plan.judgeNotes @ [ $"large: {largePlan.decision.reason}" ] }
                with _ ->
                    return plan
        }

    let private createTurnPlan (st: State) (request: MemoryRequest) cancellationToken =
        async {
            let fallback = deterministicPlan request.snapshot request.realtimeJudgement
            let availableTools = availableToolKeys st.toolCatalog

            match st.smallController with
            | None -> return fallback
            | Some _ when fallback.decision.controllerPath = ZeroController ->
                return
                    { fallback with
                        toolCalls = []
                        selectedSourceQuery = None
                        selectedSourceMaxResults = 0
                        judgeNotes = [ "accepted realtime judgement; zero-controller fast path" ] }
            | Some _ ->
                let judges =
                    [ tryRunJudge "classification" (runClassificationJudge st request cancellationToken)
                      tryRunJudge "recall" (runRecallJudge st request cancellationToken)
                      tryRunJudge "tools" (runToolJudge st request cancellationToken) ]

                let! results = judges |> Async.Parallel

                let smallPlan =
                    mergeSmallJudgeResults fallback availableTools request.snapshot results

                let! finalPlan = runLargeController st request availableTools smallPlan cancellationToken

                if List.isEmpty finalPlan.toolCalls && not finalPlan.usedLlm then
                    return fallback
                else
                    return finalPlan
        }

    let private kernelFromCatalog (catalog: ToolCatalog) =
        let builder = Kernel.CreateBuilder()

        for plugin in catalog.plugins do
            builder.Plugins.Add plugin |> ignore

        builder.Build()

    let private invokeFunction (kernel: Kernel) cancellationToken pluginName functionName question maxResults =
        task {
            let args = KernelArguments()

            args["question"] <- question
            args["query"] <- question
            args["maxResults"] <- maxResults

            let! result = kernel.InvokeAsync<string>(pluginName, functionName, args, cancellationToken)
            return result |> FsKame.Text.normalizeWhitespace
        }
        |> Async.AwaitTask

    let private invokePlannedTool (kernel: Kernel) cancellationToken (toolCall: PlannedToolCall) =
        async {
            try
                let! output =
                    invokeFunction
                        kernel
                        cancellationToken
                        toolCall.pluginName
                        toolCall.functionName
                        toolCall.query
                        toolCall.maxResults

                let title = $"{toolCall.pluginName}.{toolCall.functionName}"
                return card ToolObservation title output (Some title)
            with ex ->
                let title = $"{toolCall.pluginName}.{toolCall.functionName}"
                return card OpenQuestion title $"Tool invocation failed: {ex.Message}" (Some title)
        }

    let private runToolPlan (st: State) (request: MemoryRequest) (plan: MemoryTurnPlan) cancellationToken =
        async {
            let kernel = kernelFromCatalog st.toolCatalog

            let planCard =
                let content =
                    [ yield $"controller: {DurableMemory.controllerPathName plan.decision.controllerPath}"
                      yield $"used_llm: {plan.usedLlm}"
                      yield! plan.judgeNotes
                      yield "tools:"
                      yield!
                          plan.toolCalls
                          |> List.map (fun call ->
                              $"{call.pluginName}.{call.functionName} query='{call.query}' reason='{call.reason}'") ]
                    |> String.concat "\n"

                card ToolPlan "Memory-controller tool plan" content (Some "MemoryAgent")

            let! observations =
                plan.toolCalls
                |> List.map (invokePlannedTool kernel cancellationToken)
                |> Async.Parallel

            return
                [ card UserIntent "Final user turn" request.snapshot.text None; planCard ]
                @ List.ofArray observations
        }

    let private recallCards (decision: SupervisorDecision) (hits: MemoryRecallHit list) =
        let planCard =
            card RecallPlan "Supervisor recall decision" (DurableMemory.renderRecallSpec decision) (Some "MemoryAgent")

        let memoryCards =
            hits
            |> List.truncate 8
            |> List.map (fun hit ->
                let record = hit.record

                card
                    DurableMemoryRecord
                    $"{DurableMemory.kindName record.kind}: {record.title}"
                    (Text.truncate 700 record.text)
                    (Some record.memoryId))

        planCard :: memoryCards

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

    let private sourceCoverageTerms =
        [ "both"
          "each"
          "all"
          "documents"
          "document"
          "docs"
          "papers"
          "sources"
          "selected"
          "compare"
          "contrast"
          "summarize"
          "summarise"
          "summary"
          "them"
          "these" ]

    let private wantsSelectedSourceCoverage (query: string) (retrieval: KnowledgeSources.RetrievalIndex) =
        let sourceCount = retrieval.sources |> List.filter _.enabled |> List.length
        let queryTerms = Text.terms query |> Set.ofList

        sourceCount > 1
        && (sourceCoverageTerms
            |> List.exists (fun term -> queryTerms.Contains(term.ToLowerInvariant())))

    let private balanceContextBySource (retrieval: KnowledgeSources.RetrievalIndex) (chunks: SourceChunk list) =
        let coverage =
            retrieval.sources
            |> List.filter _.enabled
            |> List.choose (fun source -> chunks |> List.tryFind (fun chunk -> sourceEquals chunk.source source))
            |> List.truncate C.REALTIME_MEMORY_MAX_CONTEXT_CHUNKS

        coverage @ chunks
        |> List.fold
            (fun (seen, kept) chunk ->
                let key = chunkKey chunk

                if Set.contains key seen then
                    seen, kept
                else
                    Set.add key seen, chunk :: kept)
            (Set.empty, [])
        |> snd
        |> List.rev
        |> List.truncate C.REALTIME_MEMORY_MAX_CONTEXT_CHUNKS

    let private packContextCandidates retrieval forceSourceCoverage ranked =
        let candidates = contextCandidates retrieval ranked

        if forceSourceCoverage then
            balanceContextBySource retrieval candidates
        else
            candidates |> List.truncate C.REALTIME_MEMORY_MAX_CONTEXT_CHUNKS

    let private rankSelectedSourcesForPlan report (st: State) (request: MemoryRequest) (plan: MemoryTurnPlan) =
        async {
            match plan.selectedSourceQuery with
            | None -> return []
            | Some query when String.IsNullOrWhiteSpace query -> return []
            | Some query ->
                let maxResults =
                    if plan.selectedSourceMaxResults <= 0 then
                        C.REALTIME_MEMORY_CANDIDATE_CHUNKS
                    else
                        min C.REALTIME_MEMORY_CANDIDATE_CHUNKS plan.selectedSourceMaxResults

                let rank useLexicalFilter =
                    KnowledgeSources.rank
                        None
                        st.logExpansions
                        st.logChunks
                        useLexicalFilter
                        report
                        query
                        maxResults
                        st.retrieval

                let! ranked = rank st.useLexicalFilter

                let! ranked =
                    if List.isEmpty ranked && st.useLexicalFilter then
                        st.bus.PostToAgent(
                            Ag_Log
                                $"Selected-source search returned no chunks with lexical filtering; retrying dense search for turn {request.snapshot.turnId}."
                        )

                        rank false
                    else
                        async { return ranked }

                let forceSourceCoverage =
                    wantsSelectedSourceCoverage request.snapshot.text st.retrieval
                    || wantsSelectedSourceCoverage query st.retrieval

                return packContextCandidates st.retrieval forceSourceCoverage ranked
        }

    let private awaitFallbackSourceChunks
        (sourceTask: System.Threading.Tasks.Task<SourceChunk list>)
        (cancellationToken: CancellationToken)
        =
        async {
            try
                if sourceTask.IsCompleted then
                    return! sourceTask |> Async.AwaitTask
                else
                    return!
                        sourceTask.WaitAsync(TimeSpan.FromMilliseconds 160., cancellationToken)
                        |> Async.AwaitTask
            with _ ->
                return []
        }

    let private sourceEvidenceCards (chunks: SourceChunk list) =
        chunks
        |> List.truncate 4
        |> List.map (fun chunk ->
            card
                Evidence
                $"{chunk.source.DisplayName} chunk {chunk.index}"
                (Text.truncate 650 chunk.text)
                (Some chunk.source.location))

    let private logContextTrace (st: State) (request: MemoryRequest) (context: MemoryContext) =
        let snippet = request.snapshot.text |> Text.truncate 120

        st.bus.PostToAgent(
            Ag_Log
                $"Memory context trace turn={request.snapshot.turnId} transcript_chars={request.snapshot.text.Length} source_chunks={context.context.Length} memories={context.memories.Length} cards={context.cards.Length} timed_out={context.timedOut} question='{snippet}'"
        )

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
                    configureToolContext st

                    let fallbackPlan = deterministicPlan request.snapshot request.realtimeJudgement

                    let fallbackSourceTask =
                        rankSelectedSourcesForPlan report st request fallbackPlan |> Async.StartAsTask

                    let! plan = createTurnPlan st request linkedCts.Token

                    st.bus.PostToAgent(
                        Ag_Log
                            $"Memory controller selected {DurableMemory.controllerPathName plan.decision.controllerPath}; LLM={plan.usedLlm}."
                    )

                    let memoryTask =
                        DurableMemory.recall st.retrieval.encoder plan.decision.recallSpec st.memoryStore
                        |> Async.StartAsTask

                    let toolTask = runToolPlan st request plan linkedCts.Token |> Async.StartAsTask

                    let sourceTask =
                        rankSelectedSourcesForPlan report st request plan |> Async.StartAsTask

                    let! memoryHits = memoryTask.WaitAsync(timeout, linkedCts.Token) |> Async.AwaitTask
                    let! cards = toolTask.WaitAsync(timeout, linkedCts.Token) |> Async.AwaitTask
                    let! plannedChunks = sourceTask.WaitAsync(timeout, linkedCts.Token) |> Async.AwaitTask

                    let! fallbackChunks =
                        if List.isEmpty plannedChunks then
                            awaitFallbackSourceChunks fallbackSourceTask request.cancellationToken
                        else
                            async { return [] }

                    let chunks =
                        if List.isEmpty plannedChunks then
                            if not (List.isEmpty fallbackChunks) then
                                st.bus.PostToAgent(
                                    Ag_Log
                                        $"Memory controller produced no source chunks for turn {request.snapshot.turnId}; using deterministic selected-source fallback."
                                )

                            fallbackChunks
                        else
                            plannedChunks

                    let evidence = sourceEvidenceCards chunks

                    let cards = recallCards plan.decision memoryHits @ cards @ evidence
                    rememberCards st cards

                    let context =
                        createContext request plan.decision st.retrieval chunks memoryHits [] cards false

                    logContextTrace st request context
                    st.bus.PostToAgent(Ag_MemoryReady(request, context))
                    st.bus.PostToAgent(Ag_MemoryJobFinished(request.requestId, Ok()))
                with
                | :? OperationCanceledException when request.cancellationToken.IsCancellationRequested ->
                    st.bus.PostToAgent(Ag_MemoryRequestCanceled request.requestId)
                    st.bus.PostToAgent(Ag_MemoryJobFinished(request.requestId, Error "Canceled"))
                | :? TimeoutException ->
                    let cards =
                        [ card
                              OpenQuestion
                              "Memory timeout"
                              "Tool planning or retrieval timed out."
                              (Some "MemoryAgent") ]

                    let fallbackPlan = deterministicPlan request.snapshot request.realtimeJudgement
                    let! chunks = rankSelectedSourcesForPlan report st request fallbackPlan
                    let cards = cards @ sourceEvidenceCards chunks

                    rememberCards st cards

                    let decision =
                        DurableMemory.createSupervisorDecision request.snapshot request.realtimeJudgement

                    let context = createContext request decision st.retrieval chunks [] [] cards true
                    logContextTrace st request context
                    st.bus.PostToAgent(Ag_MemoryReady(request, context))
                    st.bus.PostToAgent(Ag_MemoryJobFinished(request.requestId, Error "Timed out"))
                | :? OperationCanceledException ->
                    let cards =
                        [ card
                              OpenQuestion
                              "Memory timeout"
                              "Tool planning or retrieval timed out."
                              (Some "MemoryAgent") ]

                    let fallbackPlan = deterministicPlan request.snapshot request.realtimeJudgement
                    let! chunks = rankSelectedSourcesForPlan report st request fallbackPlan
                    let cards = cards @ sourceEvidenceCards chunks

                    rememberCards st cards

                    let decision =
                        DurableMemory.createSupervisorDecision request.snapshot request.realtimeJudgement

                    let context = createContext request decision st.retrieval chunks [] [] cards true
                    logContextTrace st request context
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

            let recentContexts =
                if completed && not current.cancellationToken.IsCancellationRequested then
                    st.recentContexts |> Map.add current.snapshot.turnId context
                else
                    st.recentContexts

            { st with
                inFlight = st.inFlight |> Map.remove request.requestId
                recentContexts = recentContexts }

    let private cancelRequest (st: State) requestId =
        match st.inFlight |> Map.tryFind requestId with
        | None -> st
        | Some request ->
            request.completion.TrySetCanceled request.cancellationToken |> ignore
            st.bus.PostToAgent(Ag_Log $"Memory request {requestId} was canceled.")

            { st with
                inFlight = st.inFlight |> Map.remove requestId }

    let private cancelInFlightRequests (st: State) reason =
        for KeyValue(requestId, request) in st.inFlight do
            request.completion.TrySetCanceled request.cancellationToken |> ignore
            st.bus.PostToAgent(Ag_Log $"Memory request {requestId} was canceled: {reason}")

        { st with inFlight = Map.empty }

    let private failRequest (st: State) requestId message =
        match st.inFlight |> Map.tryFind requestId with
        | None -> st
        | Some request ->
            request.completion.TrySetException(InvalidOperationException message) |> ignore
            st.bus.PostToAgent(Ag_Log $"Memory request {requestId} failed: {message}")
            st.bus.PostToAgent(Ag_ResponseReady(request.snapshot, None))

            { st with
                inFlight = st.inFlight |> Map.remove requestId }

    let private logCommittedMemory (st: State) (updates: CommittedMemoryUpdate list) =
        for update in updates do
            st.bus.PostToAgent(Ag_Log update.message)

            match update.committedRecord with
            | Some record ->
                card
                    CommittedMemory
                    $"{DurableMemory.kindName record.kind}: {record.title}"
                    update.message
                    (Some record.memoryId)
                |> st.toolHostContext.Blackboard.Enqueue
            | None -> ()

    let private proposalFromJson (snapshot: TranscriptSnapshot) (item: JsonElement) =
        match parseMemoryKind (stringProp "kind" item), stringProp "text" item with
        | Some kind, Some text ->
            let scope =
                parseMemoryScope (stringProp "scope" item) |> Option.defaultValue Session

            let sensitivity =
                parseSensitivity (stringProp "sensitivity" item) |> Option.defaultValue Normal

            let title = stringProp "title" item |> Option.defaultValue (Text.truncate 72 text)

            let summary =
                stringProp "summary" item |> Option.defaultValue (Text.truncate 180 text)

            Some
                { kind = kind
                  title = title
                  text = Text.normalizeWhitespace text
                  summary = Text.normalizeWhitespace summary
                  scope = scope
                  namespaceId = DurableMemory.defaultNamespace
                  entities = stringListProp "entities" item
                  tags = stringListProp "tags" item
                  confidence = floatProp "confidence" item |> Option.defaultValue 0.75
                  importance = floatProp "importance" item |> Option.defaultValue 0.5
                  sensitivity = sensitivity
                  provenance =
                    { sourceType = "memory_controller"
                      sourceIds = [ snapshot.turnId ]
                      toolName = None
                      toolArgsHash = None
                      resultHash = None }
                  observedAt = snapshot.receivedAt
                  explicitCorrection = boolProp "explicit_correction" item |> Option.defaultValue false }
        | _ -> None

    let private parseWritebackProposals (snapshot: TranscriptSnapshot) (root: JsonElement) =
        tryGetProperty root "proposals"
        |> Option.filter (fun prop -> prop.ValueKind = JsonValueKind.Array)
        |> Option.map (fun prop ->
            prop.EnumerateArray()
            |> Seq.choose (proposalFromJson snapshot)
            |> Seq.truncate 6
            |> Seq.toList)
        |> Option.defaultValue []

    let private writebackPrompt (snapshot: TranscriptSnapshot) (answer: string) =
        $"""Decide whether this completed turn should write durable typed memory.

User turn:
{snapshot.text}

Oracle answer:
{answer}

Return JSON only:
{{
  "proposals": [
    {{
      "kind": "directive | claim | decision | commitment | episode",
      "title": "short title",
      "text": "self-contained durable memory text",
      "summary": "short summary",
      "scope": "session | user | workspace | global",
      "entities": ["optional"],
      "tags": ["optional"],
      "confidence": 0.8,
      "importance": 0.6,
      "sensitivity": "normal | personal | sensitive | secret",
      "explicit_correction": false
    }}
  ],
  "reason": "brief reason"
}}

Write only durable, future-useful facts. Use directive for preferences/instructions, decision for choices, commitment for open loops, claim for factual context, and episode for substantial conversation summaries. Return an empty proposals array for trivial turns."""

    let private llmWritebackProposals (st: State) (snapshot: TranscriptSnapshot) (answer: string) cancellationToken =
        async {
            let decision = DurableMemory.createSupervisorDecision snapshot None

            let shouldUseLarge =
                decision.riskFlags.memoryMutation
                || decision.riskFlags.conflictLikely
                || decision.riskFlags.sensitive

            let client, modelId =
                if shouldUseLarge && Option.isSome st.largeController then
                    st.largeController, st.largeControllerModelId
                else
                    st.smallController, st.controllerModelId

            match client with
            | None -> return None
            | Some client ->
                try
                    let! json =
                        runJsonController
                            client
                            modelId
                            620
                            cancellationToken
                            "You are FsKame's memory writeback controller. Propose typed durable memory writes; do not answer the user."
                            (writebackPrompt snapshot answer)

                    return tryParseJsonObject json |> Option.map (parseWritebackProposals snapshot)
                with _ ->
                    return None
        }

    let private applyWriteback (st: State) (snapshot: TranscriptSnapshot) (candidate: OracleCandidate) =
        async {
            let memoryStore, retractionLogs =
                DurableMemory.retractFromTurn st.memoryStore snapshot

            for log in retractionLogs do
                st.bus.PostToAgent(Ag_Log log)

            let! proposals =
                async {
                    if List.isEmpty retractionLogs then
                        use writebackCts = new CancellationTokenSource(TimeSpan.FromMilliseconds 700.)

                        let! llmProposals = llmWritebackProposals st snapshot candidate.answer writebackCts.Token

                        return
                            llmProposals
                            |> Option.defaultValue (DurableMemory.proposalsFromExchange snapshot candidate.answer)
                    else
                        return []
                }

            if List.isEmpty proposals && List.isEmpty retractionLogs then
                return st
            else
                let memoryStore, updates, saveLogs =
                    DurableMemory.commitProposals memoryStore proposals

                for log in saveLogs do
                    st.bus.PostToAgent(Ag_Log log)

                let st =
                    { st with
                        memoryStore = memoryStore
                        recentContexts = st.recentContexts |> Map.remove snapshot.turnId }

                configureToolContext st
                logCommittedMemory st updates
                return st
        }

    let private update (st: State) (msg: AgentMsg) =
        async {
            match msg with
            | Ag_SourcesUpdated(mode, sources, flags) ->
                let st = cancelInFlightRequests st "selected sources changed"
                clearRememberedCards st

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
            | Ag_ResponseReady(snapshot, Some candidate) -> return! applyWriteback st snapshot candidate
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
                st.smallController |> Option.iter (fun client -> client.Dispose())
                st.largeController |> Option.iter (fun client -> client.Dispose())
                return st
            | _ -> return st
        }

    let start apiKey oracleModel bus toolHostContext toolCatalog =
        let memoryStore, memoryLogs = DurableMemory.loadDefault ()

        let smallController =
            if String.IsNullOrWhiteSpace apiKey then
                None
            else
                Some(createClient apiKey C.NANO_MODEL)

        let largeController =
            if String.IsNullOrWhiteSpace apiKey then
                None
            else
                Some(createClient apiKey oracleModel)

        let st0 =
            { bus = bus
              controllerModelId = C.NANO_MODEL
              largeControllerModelId = oracleModel
              retrievalMode = FsColbertWithFallback
              retrieval = KnowledgeSources.emptyIndex
              memoryStore = memoryStore
              toolHostContext = toolHostContext
              toolCatalog = toolCatalog
              smallController = smallController
              largeController = largeController
              logExpansions = false
              logChunks = false
              useLexicalFilter = true
              inFlight = Map.empty
              recentContexts = Map.empty }

        configureToolContext st0

        for log in memoryLogs do
            bus.PostToAgent(Ag_Log log)

        bus.AgentBus.RunAsync("memory", st0, update) |> FlowUtils.catch bus.PostToFlow
