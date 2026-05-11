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
      answerModelId: string
      keywordModelId: string
      elaborateIndexKeywords: bool
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
          answerModelId = QaDefaults.answerModel
          keywordModelId = QaDefaults.nanoModel
          elaborateIndexKeywords = true
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
    let mutable retrieval = KnowledgeSources.emptyIndex

    let memoryPath =
        options.memoryStorePath
        |> Option.defaultValue (DurableMemory.defaultPath options.storageRoot)

    let mutable memoryStore, memoryLogs = DurableMemory.load memoryPath
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
        let inventory = KnowledgeSources.renderInventory retrieval.sources
        let typedMemory = renderTypedMemory decision memoryHits
        let toolObservations = renderObservations observations

        [ ChatMessage(
              ChatRole.System,
              "You are FsKame's reusable QA backend. Answer from selected knowledge sources, durable memory, and tool observations when they are relevant. Treat tool observations as authoritative for current facts. Do not invent citations, source details, tool results, or live values. If the selected sources and tools do not contain enough information, say that plainly. Keep answers concise and useful."
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
            match options.clients.toolPlanner with
            | None -> return []
            | Some client ->
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
                KnowledgeSources.rank
                    options.clients.queryExpansion
                    options.logExpansions
                    options.logChunks
                    options.useLexicalFilter
                    report
                    question
                    maxResults
                    retrieval
                |> Async.StartAsTask
                |> fun task ->
                    task.ContinueWith(
                        (fun (t: Task<SourceChunk list>) -> KnowledgeSources.renderContext t.Result),
                        cancellationToken
                    )

            member _.SourceInventoryAsync _ =
                Task.FromResult(KnowledgeSources.renderInventory retrieval.sources)

            member _.SearchMemoryAsync(query, maxResults, cancellationToken) =
                let budget = if maxResults <= 20 then Fast else Medium

                let spec =
                    { query = Text.normalizeWhitespace query
                      kinds = [ Directive; Decision; Claim; Commitment; Episode ]
                      scopes = [ Session; User; Workspace; Global ]
                      namespaceId = DurableMemory.defaultNamespace
                      temporalMode = CurrentOnly
                      temporalReference = None
                      includeSuperseded = false
                      recallBudget = budget
                      maxCandidates = clamp 60 maxResults
                      minScore = None
                      latencyBudget =
                        if budget = Fast then
                            TimeSpan.FromMilliseconds 90.
                        else
                            TimeSpan.FromMilliseconds 180. }

                DurableMemory.recall retrieval.encoder spec memoryStore
                |> Async.StartAsTask
                |> fun task ->
                    task.ContinueWith(
                        (fun (t: Task<MemoryRecallHit list>) -> DurableMemory.renderRecall t.Result),
                        cancellationToken
                    )

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

    let mutable catalog = QaToolLoader.load host options.toolProviderDirectory

    do
        for log in memoryLogs @ catalog.logs do
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
            let proposals = DurableMemory.proposalsFromExchange snapshot answer
            let updated, updates, logs = DurableMemory.commitProposals memoryStore proposals
            memoryStore <- updated

            for update in updates do
                report update.message

            for log in logs do
                report log

    member _.ToolCatalog = catalog

    member _.LoadSourcesAsync(mode, sources, cancellationToken) =
        task {
            let sources = sources |> List.filter _.enabled
            report $"Loading {sources.Length} knowledge source(s) with {RetrievalModes.displayName mode}."

            let loadWork =
                match mode with
                | InternalDocumentIndex -> KnowledgeSources.loadInternalIndex sources
                | FsColbertWithFallback ->
                    let keywordOptions =
                        { KnowledgeSources.KeywordGenerationOptions.defaults with
                            enabled = options.elaborateIndexKeywords
                            client = options.clients.queryExpansion
                            modelId = options.keywordModelId }

                    KnowledgeSources.loadIndex options.storageRoot report keywordOptions sources

            let! loaded, errors = loadWork |> Async.StartAsTask

            KnowledgeSources.disposeIndex retrieval
            retrieval <- loaded
            report $"Loaded {retrieval.chunks.Length} source chunk(s)."

            catalog <- QaToolLoader.load host options.toolProviderDirectory

            for log in catalog.logs do
                report log

            return errors
        }

    member _.AnswerAsync(request: QaTurnRequest, cancellationToken) =
        task {
            let snapshot = createSnapshot request

            let decision =
                DurableMemory.createSupervisorDecision snapshot request.realtimeJudgement

            let memoryTask =
                DurableMemory.recall retrieval.encoder decision.recallSpec memoryStore
                |> Async.StartAsTask

            let sourceTask =
                task {
                    let sw = Stopwatch.StartNew()

                    let! chunks =
                        KnowledgeSources.rank
                            options.clients.queryExpansion
                            options.logExpansions
                            options.logChunks
                            options.useLexicalFilter
                            report
                            snapshot.text
                            QaDefaults.memoryCandidateChunks
                            retrieval
                        |> Async.StartAsTask

                    sw.Stop()
                    return chunks, sw.Elapsed.TotalMilliseconds
                }

            let plannedTools = deterministicPlan catalog snapshot.text
            let! llmTools = runToolPlanner catalog snapshot.text cancellationToken |> Async.StartAsTask
            let! observations = invokeTools (plannedTools @ llmTools) cancellationToken |> Async.StartAsTask
            let! memoryHits = memoryTask
            let! chunks, sourceRetrievalElapsedMs = sourceTask

            for observation in observations do
                blackboard.Add observation

            let! answer =
                answerWithModel snapshot decision memoryHits chunks observations cancellationToken
                |> Async.StartAsTask

            applyWriteback snapshot answer

            return
                { turnId = request.turnId
                  answer = answer
                  model = options.answerModelId
                  context = chunks
                  sourceRetrievalElapsedMs = sourceRetrievalElapsedMs
                  inventory = retrieval.sources
                  toolObservations = observations
                  timedOut = false
                  createdAt = DateTimeOffset.UtcNow }
        }

    interface IQaSession with
        member this.LoadSourcesAsync(mode, sources, cancellationToken) =
            this.LoadSourcesAsync(mode, sources, cancellationToken)

        member this.AnswerAsync(request, cancellationToken) =
            this.AnswerAsync(request, cancellationToken)

        member _.DisposeAsync() =
            KnowledgeSources.disposeIndex retrieval

            for client in
                [ options.clients.queryExpansion
                  options.clients.toolPlanner
                  options.clients.answerGenerator ]
                |> List.choose id do
                client.Dispose()

            ValueTask()
