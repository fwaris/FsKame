namespace FsKame.QA

open System
open System.Collections.Generic
open System.Diagnostics
open System.Text.Json
open System.Threading
open System.Threading.Tasks
open Microsoft.Extensions.AI

type QaModelClients =
    { queryExpansion: IChatClient option
      toolPlanner: IChatClient option
      answerGenerator: IChatClient option }

module QaModelClients =
    let none =
        { queryExpansion = None
          toolPlanner = None
          answerGenerator = None }

type QaSessionOptions =
    { storageRoot: string
      memoryStorePath: string option
      toolProviderDirectory: string option
      retrievalMode: RetrievalMode
      clients: QaModelClients
      useCaseProfile: QaUseCaseProfile
      answerModelId: string
      keywordModelId: string
      elaborateIndexKeywords: bool
      memoryService: IMemoryService option
      contextProviders: IQaContextProvider list
      toolProviders: IQaToolProvider list
      enableToolPlanner: bool
      enableQueryExpansion: bool
      logTimings: bool
      logExpansions: bool
      logChunks: bool
      useLexicalFilter: bool
      autoWriteback: bool
      report: string -> unit }

module QaSessionOptions =
    let create storageRoot =
        { storageRoot = storageRoot
          memoryStorePath = None
          toolProviderDirectory = None
          retrievalMode = FsColbertWithFallback
          clients = QaModelClients.none
          useCaseProfile = QaUseCaseProfile.generic
          answerModelId = QaDefaults.answerModel
          keywordModelId = QaDefaults.nanoModel
          elaborateIndexKeywords = true
          memoryService = None
          contextProviders = []
          toolProviders = []
          enableToolPlanner = true
          enableQueryExpansion = false
          logTimings = false
          logExpansions = false
          logChunks = false
          useLexicalFilter = true
          autoWriteback = true
          report = ignore }

type private PlannedToolCall =
    { tool: IQaTool
      query: string
      maxResults: int
      arguments: IReadOnlyDictionary<string, string> }

type QaSession(options: QaSessionOptions) =
    let mutable contextProviders = options.contextProviders

    let memoryPath =
        options.memoryStorePath
        |> Option.defaultValue (DurableMemory.defaultPath options.storageRoot)

    let currentMemoryEncoder () =
        contextProviders
        |> List.tryPick (fun provider ->
            match provider with
            | :? FsColbertContextProvider as fsColbert -> fsColbert.Retrieval.encoder
            | _ -> None)

    let memoryService =
        options.memoryService
        |> Option.defaultValue (DurableMemoryService(memoryPath, currentMemoryEncoder) :> IMemoryService)

    let blackboard = ResizeArray<QaToolObservation>()

    let report message = options.report message

    let findTool pluginName toolName (catalog: QaToolCatalog) =
        catalog.tools
        |> List.tryFind (fun tool ->
            String.Equals(tool.PluginName, pluginName, StringComparison.OrdinalIgnoreCase)
            && String.Equals(tool.Name, toolName, StringComparison.OrdinalIgnoreCase))

    let clamp (maxValue: int) (value: int) = Math.Max(1, Math.Min(maxValue, value))

    let lower (value: string) =
        (defaultArg (Option.ofObj value) "").ToLowerInvariant()

    let containsAny (needles: string list) (haystack: string) =
        let haystack = lower haystack
        needles |> List.exists haystack.Contains

    let makeArgs (values: (string * string) list) =
        let dict = Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)

        for key, value in values do
            dict[key] <- value

        dict :> IReadOnlyDictionary<string, string>

    let renderToolInventory (catalog: QaToolCatalog) =
        catalog.tools
        |> List.sortBy (fun tool -> tool.PluginName, tool.Name)
        |> List.map (fun tool ->
            let parameters =
                tool.Parameters
                |> List.map (fun p -> if p.required then $"{p.name}*" else p.name)
                |> String.concat ", "

            $"{tool.PluginName}.{tool.Name}({parameters}): {tool.Description}")
        |> String.concat "\n"

    let renderObservations (observations: QaToolObservation list) =
        if List.isEmpty observations then
            "No tool observations were recorded."
        else
            observations
            |> List.truncate 12
            |> List.mapi (fun index observation ->
                $"[{index + 1}] {observation.pluginName}.{observation.toolName}\n{Text.truncate 900 observation.content}")
            |> String.concat "\n\n"

    let isBuiltInContextTool (tool: IQaTool) =
        String.Equals(tool.PluginName, "FsKameTools", StringComparison.OrdinalIgnoreCase)
        && ([ "selected_source_search"
              "source_inventory"
              "durable_memory_search"
              "blackboard_search" ]
            |> List.exists (fun name -> String.Equals(tool.Name, name, StringComparison.OrdinalIgnoreCase)))

    let hasPlannerCandidateTools (catalog: QaToolCatalog) =
        catalog.tools |> List.exists (isBuiltInContextTool >> not)

    let retrieveContext question maxResults cancellationToken =
        async {
            let request =
                { query = question
                  maxResults = clamp QaDefaults.memoryCandidateChunks maxResults }

            let! results =
                contextProviders
                |> List.map (fun provider ->
                    async {
                        try
                            let! chunks = provider.RetrieveAsync(request, cancellationToken) |> Async.AwaitTask
                            return chunks
                        with ex ->
                            report $"Context provider {provider.DisplayName} failed: {ex.Message}"
                            return []
                    })
                |> Async.Parallel

            return
                results
                |> Array.toList
                |> List.collect id
                |> List.sortByDescending _.score
                |> List.truncate request.maxResults
        }

    let contextInventory cancellationToken =
        task {
            if List.isEmpty contextProviders then
                return "No selected source context providers are loaded."
            else
                let! inventories =
                    contextProviders
                    |> List.map (fun provider ->
                        task {
                            try
                                return! provider.InventoryAsync cancellationToken
                            with ex ->
                                report $"Context provider {provider.DisplayName} inventory failed: {ex.Message}"
                                return $"No inventory was available for {provider.DisplayName}."
                        })
                    |> Task.WhenAll

                return
                    inventories
                    |> Array.toList
                    |> List.filter (String.IsNullOrWhiteSpace >> not)
                    |> String.concat "\n\n"
        }

    let contextSources () =
        contextProviders
        |> List.collect _.Sources
        |> List.distinctBy (fun source -> source.kind, source.location)

    let renderTypedMemory decision hits =
        let memories = DurableMemory.renderRecall hits
        let policy = DurableMemory.renderRecallSpec decision
        $"Recall policy:\n{policy}\n\nTyped memory evidence:\n{memories}"

    let answerPrompt
        (snapshot: TranscriptSnapshot)
        (decision: SupervisorDecision)
        (memoryHits: MemoryRecallHit list)
        (chunks: SourceChunk list)
        (observations: QaToolObservation list)
        =
        let sourceContext = KnowledgeSources.renderContext chunks
        let inventory = KnowledgeSources.renderInventory (contextSources ())
        let typedMemory = renderTypedMemory decision memoryHits
        let toolObservations = renderObservations observations

        [ ChatMessage(
              ChatRole.System,
              (options.useCaseProfile.answerSystemInstruction
               |> Option.defaultValue
                   "You are FsKame's reusable QA backend. Answer from selected knowledge sources, durable memory, and tool observations when they are relevant. Treat tool observations as authoritative for current facts. Do not invent citations, source details, tool results, or live values. If the selected sources and tools do not contain enough information, say that plainly. Keep answers concise and useful.")
          )
          ChatMessage(
              ChatRole.User,
              $"User question:\n{snapshot.text}\n\nTyped durable memory:\n{typedMemory}\n\nTool observations:\n{toolObservations}\n\nSelected source inventory:\n{inventory}\n\nMatched source context:\n{sourceContext}\n\nReturn only the answer."
          ) ]

    let createSnapshot (request: QaTurnRequest) =
        { turnId = request.turnId
          itemId = request.turnId
          revision = 1
          text = Text.normalizeWhitespace request.question
          isFinal = true
          receivedAt = DateTimeOffset.UtcNow }

    let deterministicPlan (catalog: QaToolCatalog) question =
        [ if
              containsAny
                  [ "what time"
                    "current time"
                    "time is it"
                    "today"
                    "current date"
                    "what date" ]
                  question
          then
              match catalog |> findTool "FsKameTools" "current_time" with
              | Some tool ->
                  yield
                      { tool = tool
                        query = question
                        maxResults = 1
                        arguments = makeArgs [ "query", question ] }
              | None -> ()

          if
              containsAny
                  [ "what documents"
                    "which documents"
                    "selected documents"
                    "sources"
                    "inventory"
                    "files" ]
                  question
          then
              match catalog |> findTool "FsKameTools" "source_inventory" with
              | Some tool ->
                  yield
                      { tool = tool
                        query = question
                        maxResults = QaDefaults.memoryCandidateChunks
                        arguments = makeArgs [] }
              | None -> ()

          if
              containsAny
                  [ "remember"
                    "memory"
                    "preference"
                    "decided"
                    "decision"
                    "earlier"
                    "previously" ]
                  question
          then
              match catalog |> findTool "FsKameTools" "durable_memory_search" with
              | Some tool ->
                  yield
                      { tool = tool
                        query = question
                        maxResults = QaDefaults.memoryCandidateChunks
                        arguments =
                          makeArgs [ "query", question; "max_results", string QaDefaults.memoryCandidateChunks ] }
              | None -> () ]

    let tryJsonString (name: string) (element: JsonElement) =
        let mutable prop = Unchecked.defaultof<JsonElement>

        if element.ValueKind = JsonValueKind.Object && element.TryGetProperty(name, &prop) then
            match prop.ValueKind with
            | JsonValueKind.String -> prop.GetString() |> Option.ofObj |> Option.bind Text.notEmpty
            | JsonValueKind.Number -> Some(prop.GetRawText())
            | JsonValueKind.True -> Some "true"
            | JsonValueKind.False -> Some "false"
            | _ -> None
        else
            None

    let tryJsonInt name fallback element =
        tryJsonString name element
        |> Option.bind (fun value ->
            match Int32.TryParse value with
            | true, parsed -> Some parsed
            | false, _ -> None)
        |> Option.defaultValue fallback

    let toolPlanPrompt question (catalog: QaToolCatalog) =
        [ ChatMessage(
              ChatRole.System,
              "You are a conservative tool planner. Return compact JSON only. Select a tool only when it is clearly useful for the user question."
          )
          ChatMessage(
              ChatRole.User,
              $"""Available tools:
{renderToolInventory catalog}

Question:
{question}

Return JSON:
{{"calls":[{{"plugin":"FsKameTools","tool":"source_inventory","query":"...","max_results":3,"arguments":{{"query":"..."}}}}]}}

Use an empty calls array when no tool is needed."""
          ) ]

    let parseToolPlan (catalog: QaToolCatalog) (text: string) =
        try
            let first = text.IndexOf('{')
            let last = text.LastIndexOf('}')

            let json =
                if first >= 0 && last >= first then
                    text.Substring(first, last - first + 1)
                else
                    text

            use doc = JsonDocument.Parse(json)
            let mutable calls = Unchecked.defaultof<JsonElement>

            if
                doc.RootElement.TryGetProperty("calls", &calls)
                && calls.ValueKind = JsonValueKind.Array
            then
                calls.EnumerateArray()
                |> Seq.choose (fun item ->
                    let plugin = tryJsonString "plugin" item

                    let toolName =
                        tryJsonString "tool" item |> Option.orElse (tryJsonString "function" item)

                    match plugin, toolName with
                    | Some plugin, Some toolName ->
                        match
                            catalog.tools
                            |> List.tryFind (fun t -> t.PluginName = plugin && t.Name = toolName)
                        with
                        | None -> None
                        | Some tool ->
                            let query = tryJsonString "query" item |> Option.defaultValue ""
                            let maxResults = tryJsonInt "max_results" 6 item |> clamp 30
                            let args = Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)

                            match item.TryGetProperty("arguments", &calls) with
                            | true when calls.ValueKind = JsonValueKind.Object ->
                                for prop in calls.EnumerateObject() do
                                    match prop.Value.ValueKind with
                                    | JsonValueKind.String ->
                                        args[prop.Name] <-
                                            prop.Value.GetString() |> Option.ofObj |> Option.defaultValue ""
                                    | JsonValueKind.Number
                                    | JsonValueKind.True
                                    | JsonValueKind.False -> args[prop.Name] <- prop.Value.GetRawText()
                                    | _ -> ()
                            | _ -> ()

                            if not (args.ContainsKey "query") && not (String.IsNullOrWhiteSpace query) then
                                args["query"] <- query

                            if not (args.ContainsKey "question") && tool.Name = "selected_source_search" then
                                args["question"] <- query

                            if not (args.ContainsKey "max_results") then
                                args["max_results"] <- string maxResults

                            Some
                                { tool = tool
                                  query = query
                                  maxResults = maxResults
                                  arguments = args :> IReadOnlyDictionary<string, string> }
                    | _ -> None)
                |> Seq.truncate 4
                |> Seq.toList
            else
                []
        with _ ->
            []

    let runToolPlanner (catalog: QaToolCatalog) question cancellationToken =
        async {
            match options.enableToolPlanner, hasPlannerCandidateTools catalog, options.clients.toolPlanner with
            | false, _, _
            | _, false, _
            | _, _, None -> return []
            | true, true, Some client ->
                try
                    let opts = ChatOptions()
                    opts.MaxOutputTokens <- Nullable 500

                    let! response =
                        client.GetResponseAsync(toolPlanPrompt question catalog, opts, cancellationToken)
                        |> Async.AwaitTask

                    return parseToolPlan catalog response.Text
                with ex ->
                    report $"Tool planning failed: {ex.Message}"
                    return []
        }

    let invokeTool (call: PlannedToolCall) cancellationToken =
        task {
            let! result = call.tool.InvokeAsync(call.arguments, cancellationToken)

            return
                { pluginName = call.tool.PluginName
                  toolName = call.tool.Name
                  query = call.query
                  content = result.content
                  createdAt = DateTimeOffset.UtcNow }
        }

    let invokeTools calls cancellationToken =
        async {
            let deduped =
                calls
                |> List.distinctBy (fun call -> call.tool.PluginName, call.tool.Name, call.query)
                |> List.truncate 6

            let! observations =
                deduped
                |> List.map (fun call ->
                    async {
                        try
                            let! observation = invokeTool call cancellationToken |> Async.AwaitTask
                            return Some observation
                        with ex ->
                            report $"Tool {call.tool.PluginName}.{call.tool.Name} failed: {ex.Message}"
                            return None
                    })
                |> Async.Parallel

            return observations |> Array.choose id |> Array.toList
        }

    let host =
        { new IQaToolHost with
            member _.Report message = report message

            member _.SearchKnowledgeAsync(question, maxResults, cancellationToken) =
                retrieveContext question maxResults cancellationToken
                |> Async.StartAsTask
                |> fun task ->
                    task.ContinueWith(
                        (fun (t: Task<SourceChunk list>) -> KnowledgeSources.renderContext t.Result),
                        cancellationToken
                    )

            member _.SourceInventoryAsync cancellationToken = contextInventory cancellationToken

            member _.SearchMemoryAsync(query, maxResults, cancellationToken) =
                memoryService.SearchAsync(query, maxResults, cancellationToken)

            member _.SearchBlackboardAsync(query, _) =
                let query = Text.normalizeWhitespace query

                let matches =
                    blackboard
                    |> Seq.rev
                    |> Seq.filter (fun observation ->
                        String.IsNullOrWhiteSpace query
                        || observation.content.Contains(query, StringComparison.OrdinalIgnoreCase)
                        || observation.query.Contains(query, StringComparison.OrdinalIgnoreCase))
                    |> Seq.truncate 8
                    |> Seq.toList

                let content =
                    if List.isEmpty matches then
                        "No matching blackboard observations were found."
                    else
                        matches
                        |> List.mapi (fun index obs -> $"[{index + 1}] {obs.pluginName}.{obs.toolName}\n{obs.content}")
                        |> String.concat "\n\n"

                Task.FromResult content }

    let mutable catalog =
        QaToolLoader.loadWithProviders host options.toolProviderDirectory options.toolProviders

    do
        for log in memoryService.StartupLogs @ catalog.logs do
            report log

    let answerWithModel
        (snapshot: TranscriptSnapshot)
        (decision: SupervisorDecision)
        (memoryHits: MemoryRecallHit list)
        (chunks: SourceChunk list)
        (observations: QaToolObservation list)
        cancellationToken
        =
        async {
            match options.clients.answerGenerator with
            | None -> return "No answer model is configured for this QA session."
            | Some client ->
                let opts = ChatOptions()

                if ModelCapabilities.supportsTemperature options.answerModelId then
                    opts.Temperature <- Nullable 0.2f

                opts.MaxOutputTokens <- Nullable 300

                let! response =
                    client.GetResponseAsync(
                        answerPrompt snapshot decision memoryHits chunks observations,
                        opts,
                        cancellationToken
                    )
                    |> Async.AwaitTask

                return response.Text |> Text.normalizeWhitespace
        }

    let applyWriteback (snapshot: TranscriptSnapshot) (answer: string) =
        if options.autoWriteback then
            let proposals = memoryService.ProposalsFromExchange(snapshot, answer)
            let updates, logs = memoryService.CommitProposals proposals

            for update in updates do
                report update.message

            for log in logs do
                report log

    member _.ToolCatalog = catalog

    member _.ConfigureAsync(providers: IQaContextProvider list, cancellationToken) =
        task {
            for provider in contextProviders do
                do! provider.DisposeAsync().AsTask()

            contextProviders <- providers

            let! results =
                contextProviders
                |> List.map (fun provider ->
                    task {
                        try
                            return! provider.LoadAsync cancellationToken
                        with ex ->
                            return [ $"Context provider {provider.DisplayName} failed to load: {ex.Message}" ]
                    })
                |> Task.WhenAll

            catalog <- QaToolLoader.loadWithProviders host options.toolProviderDirectory options.toolProviders

            for log in catalog.logs do
                report log

            return results |> Array.toList |> List.collect id
        }

    member this.LoadSourcesAsync(mode, sources, cancellationToken) =
        task {
            let providerOptions =
                { FsColbertContextProviderOptions.create options.storageRoot mode sources with
                    queryExpansionClient =
                        if options.enableQueryExpansion then
                            options.clients.queryExpansion
                        else
                            None
                    keywordGenerationClient = options.clients.queryExpansion
                    useCaseProfile = options.useCaseProfile
                    keywordModelId = options.keywordModelId
                    elaborateIndexKeywords = options.elaborateIndexKeywords
                    logExpansions = options.logExpansions
                    logChunks = options.logChunks
                    useLexicalFilter = options.useLexicalFilter
                    report = report }

            let provider = FsColbertContextProvider providerOptions :> IQaContextProvider
            return! this.ConfigureAsync([ provider ], cancellationToken)
        }

    member _.AnswerAsync(request: QaTurnRequest, cancellationToken) =
        task {
            let totalSw = Stopwatch.StartNew()
            let snapshot = createSnapshot request

            let decision =
                memoryService.CreateSupervisorDecision(snapshot, request.realtimeJudgement)

            let memoryTask =
                task {
                    let sw = Stopwatch.StartNew()
                    let! hits = memoryService.RecallAsync(decision, cancellationToken)
                    sw.Stop()
                    return hits, sw.Elapsed.TotalMilliseconds
                }

            let sourceTask =
                task {
                    let sw = Stopwatch.StartNew()

                    let! chunks =
                        retrieveContext snapshot.text QaDefaults.memoryCandidateChunks cancellationToken
                        |> Async.StartAsTask

                    sw.Stop()
                    return chunks, sw.Elapsed.TotalMilliseconds
                }

            let plannerSw = Stopwatch.StartNew()
            let plannedTools = deterministicPlan catalog snapshot.text
            let! llmTools = runToolPlanner catalog snapshot.text cancellationToken |> Async.StartAsTask
            plannerSw.Stop()

            let toolSw = Stopwatch.StartNew()
            let! observations = invokeTools (plannedTools @ llmTools) cancellationToken |> Async.StartAsTask
            toolSw.Stop()

            let! memoryHits, memoryElapsedMs = memoryTask
            let! chunks, sourceRetrievalElapsedMs = sourceTask

            for observation in observations do
                blackboard.Add observation

            let answerSw = Stopwatch.StartNew()

            let! answer =
                answerWithModel snapshot decision memoryHits chunks observations cancellationToken
                |> Async.StartAsTask

            answerSw.Stop()

            let writebackSw = Stopwatch.StartNew()
            applyWriteback snapshot answer
            writebackSw.Stop()
            totalSw.Stop()

            if options.logTimings then
                report
                    $"QA timing: total={totalSw.Elapsed.TotalMilliseconds:F0}ms; source={sourceRetrievalElapsedMs:F0}ms; memory={memoryElapsedMs:F0}ms; planner={plannerSw.Elapsed.TotalMilliseconds:F0}ms; tools={toolSw.Elapsed.TotalMilliseconds:F0}ms; answer={answerSw.Elapsed.TotalMilliseconds:F0}ms; writeback={writebackSw.Elapsed.TotalMilliseconds:F0}ms; toolObservations={observations.Length}."

            return
                { turnId = request.turnId
                  answer = answer
                  model = options.answerModelId
                  context = chunks
                  sourceRetrievalElapsedMs = sourceRetrievalElapsedMs
                  inventory = contextSources ()
                  toolObservations = observations
                  timedOut = false
                  createdAt = DateTimeOffset.UtcNow }
        }

    interface IAsyncDisposable with
        member _.DisposeAsync() =
            for provider in contextProviders do
                provider.DisposeAsync().AsTask().GetAwaiter().GetResult()

            for client in
                [ options.clients.queryExpansion
                  options.clients.toolPlanner
                  options.clients.answerGenerator ]
                |> List.choose id do
                client.Dispose()

            ValueTask()

    interface IQaSession with
        member this.LoadSourcesAsync(mode, sources, cancellationToken) =
            this.LoadSourcesAsync(mode, sources, cancellationToken)

        member this.AnswerAsync(request, cancellationToken) =
            this.AnswerAsync(request, cancellationToken)

    interface IQaOrchestrator with
        member this.ConfigureAsync(providers, cancellationToken) =
            this.ConfigureAsync(providers, cancellationToken)

        member this.AnswerAsync(request, cancellationToken) =
            this.AnswerAsync(request, cancellationToken)
