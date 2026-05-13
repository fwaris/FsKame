module FsKame.Tests

open Xunit
open System
open System.IO
open System.Security.Cryptography
open System.Text
open System.Text.Json
open System.Threading
open System.Threading.Tasks
open System.Collections.Generic
open Microsoft.Extensions.AI
open FSharp.Control
open FsKame.QA

type MockChatClient() =
    interface IChatClient with
        member this.Dispose() = ()
        member this.GetService(serviceType: Type, serviceKey: obj) = null

        member this.GetResponseAsync(chatMessages, options, cancellationToken) =
            let response =
                ChatResponse(
                    ChatMessage(
                        ChatRole.Assistant,
                        """{"terms":["synonym1","technicalTerm1"],"rewrittenQueries":["technical query"],"sectionName":null,"queryType":"Question"}"""
                    )
                )

            Task.FromResult(response)

        member this.GetStreamingResponseAsync(chatMessages, options, cancellationToken) =
            asyncSeq { yield ChatResponseUpdate() }

type InvalidJsonSchemaChatClient() =
    interface IChatClient with
        member this.Dispose() = ()
        member this.GetService(serviceType: Type, serviceKey: obj) = null

        member this.GetResponseAsync(chatMessages, options, cancellationToken) =
            InvalidOperationException("HTTP 400 invalid_request_error invalid_json_schema: schema type error")
            |> Task.FromException<ChatResponse>

        member this.GetStreamingResponseAsync(chatMessages, options, cancellationToken) =
            asyncSeq { yield ChatResponseUpdate() }

type CountingChatClient(responseText: string) =
    let mutable count = 0

    member _.Count = count

    interface IChatClient with
        member _.Dispose() = ()
        member _.GetService(serviceType: Type, serviceKey: obj) = null

        member _.GetResponseAsync(chatMessages, options, cancellationToken) =
            count <- count + 1
            ChatResponse(ChatMessage(ChatRole.Assistant, responseText)) |> Task.FromResult

        member _.GetStreamingResponseAsync(chatMessages, options, cancellationToken) =
            asyncSeq { yield ChatResponseUpdate() }

type RecordingChatClient(responseText: string) =
    let mutable messages: ChatMessage list = []
    let mutable maxOutputTokens = Nullable<int>()
    let mutable temperature = Nullable<float32>()

    member _.Messages = messages
    member _.MaxOutputTokens = maxOutputTokens
    member _.Temperature = temperature

    interface IChatClient with
        member _.Dispose() = ()
        member _.GetService(serviceType: Type, serviceKey: obj) = null

        member _.GetResponseAsync(chatMessages, options, cancellationToken) =
            messages <- chatMessages |> Seq.toList

            if not (isNull options) then
                maxOutputTokens <- options.MaxOutputTokens
                temperature <- options.Temperature

            ChatResponse(ChatMessage(ChatRole.Assistant, responseText)) |> Task.FromResult

        member _.GetStreamingResponseAsync(chatMessages, options, cancellationToken) =
            asyncSeq { yield ChatResponseUpdate() }

type FakeQaToolHost() =
    interface IQaToolHost with
        member _.Report _ = ()

        member _.SearchKnowledgeAsync(_, _, _) = Task.FromResult("No source context.")

        member _.SourceInventoryAsync _ = Task.FromResult("No selected sources.")

        member _.SearchMemoryAsync(_, _, _) = Task.FromResult("No memory context.")

        member _.SearchBlackboardAsync(_, _) =
            Task.FromResult("No blackboard context.")

type FakeContextProvider(source: KnowledgeSource) =
    interface IQaContextProvider with
        member _.ProviderId = "fake.context"
        member _.DisplayName = "Fake Context"
        member _.Sources = [ source ]

        member _.LoadAsync _ = Task.FromResult([])

        member _.RetrieveAsync(request, _) =
            [ { source = source
                index = 0
                text = $"Fake context for {request.query}."
                score = 1.0f } ]
            |> Task.FromResult

        member _.InventoryAsync _ =
            Task.FromResult("Fake Context inventory.")

        member _.DisposeAsync() = ValueTask()

let private keywordPassage (source: KnowledgeSource) index text : FsColbert.PassageRef =
    { sourceId = source.location
      sourceDisplayName = source.DisplayName
      sourceLocation = source.location
      index = index
      text = text
      keywords = [] }

let private keywordOptions schemaVersion client =
    { KnowledgeSources.KeywordGenerationOptions.defaults with
        client = Some client
        schemaVersion = schemaVersion
        batchSize = 1
        parallelism = 2 }

let private tempStorageRoot () =
    Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"))

let private hashText (value: string) =
    use sha = SHA256.Create()
    let bytes = Encoding.UTF8.GetBytes value

    sha.ComputeHash bytes
    |> Convert.ToHexString
    |> fun hash -> hash.ToLowerInvariant()

let private testSourceKindId sourceKind =
    match sourceKind with
    | Pdf -> "pdf"
    | Markdown -> "markdown"
    | Json -> "json"

let private testSourceFingerprint (source: KnowledgeSource) =
    let filePart =
        let info = FileInfo source.location

        if info.Exists then
            $"{info.FullName}:{info.Length}:{info.LastWriteTimeUtc.Ticks}"
        else
            source.location

    $"{testSourceKindId source.kind}:{filePart}"

let private testKeywordCachePath storageRoot sourceFingerprint (options: KnowledgeSources.KeywordGenerationOptions) =
    let folder = Path.Combine(storageRoot, "FsKame", "FsColbert", "KeywordCache")
    Directory.CreateDirectory folder |> ignore

    let fileName =
        [ yield sourceFingerprint
          yield options.modelId
          yield options.schemaVersion
          yield QaUseCaseProfile.fingerprint options.useCaseProfile

          if not (String.IsNullOrWhiteSpace options.useCaseFingerprint) then
              yield options.useCaseFingerprint ]
        |> String.concat "\n"
        |> hashText

    Path.Combine(folder, $"{fileName}.jsonl")

let private seedKeywordCache
    storageRoot
    (source: KnowledgeSource)
    (options: KnowledgeSources.KeywordGenerationOptions)
    (passage: FsColbert.PassageRef)
    keywords
    =
    let sourceFingerprint = testSourceFingerprint source
    let textHash = hashText passage.text

    let key =
        [ yield sourceFingerprint
          yield string passage.index
          yield textHash
          yield options.modelId
          yield options.schemaVersion
          yield QaUseCaseProfile.fingerprint options.useCaseProfile

          if not (String.IsNullOrWhiteSpace options.useCaseFingerprint) then
              yield options.useCaseFingerprint ]
        |> String.concat "\n"
        |> hashText

    let cachePath = testKeywordCachePath storageRoot sourceFingerprint options

    let line =
        JsonSerializer.Serialize(
            {| key = key
               sourceFingerprint = sourceFingerprint
               passageIndex = passage.index
               textHash = textHash
               modelId = options.modelId
               schemaVersion = options.schemaVersion
               profileFingerprint = QaUseCaseProfile.fingerprint options.useCaseProfile
               keywords = keywords |}
        )

    File.AppendAllText(cachePath, line + Environment.NewLine, Encoding.UTF8)

[<Theory>]
[<InlineData("gpt-5.5")>]
[<InlineData("gpt-5.5-mini")>]
[<InlineData(" GPT-5.5 ")>]
let ``model capability omits temperature for models that reject it`` modelId =
    Assert.False(ModelCapabilities.supportsTemperature modelId)

[<Theory>]
[<InlineData("gpt-5.1")>]
[<InlineData("gpt-4.1-mini")>]
let ``model capability keeps temperature for supported models`` modelId =
    Assert.True(ModelCapabilities.supportsTemperature modelId)

[<Fact>]
let ``generic use case supplies model roles and runtime defaults`` () =
    let plugin = new GenericQaUseCasePlugin() :> IUseCasePlugin
    let definition = plugin.Definition |> UseCaseDefinition.sanitize

    Assert.Equal(UseCaseDefinition.currentContractVersion, plugin.ContractVersion)
    Assert.Equal("generic", definition.id)
    Assert.Equal("gpt-realtime-2", (UseCaseDefinition.model Realtime definition).modelId)
    Assert.Equal("gpt-5.5", (UseCaseDefinition.model Answer definition).modelId)
    Assert.Equal("gpt-5-nano", (UseCaseDefinition.model Keyword definition).modelId)
    Assert.True(definition.runtime.enableToolPlanner)
    Assert.False(definition.runtime.enableQueryExpansion)

[<Fact>]
let ``qa session applies custom prompts and answer role options`` () =
    task {
        let source =
            { kind = Json
              location = "memory://fake"
              enabled = true }

        let recorder = new RecordingChatClient("custom response")

        let answerConfig =
            { ModelRoleConfig.create "gpt-4.1-mini" with
                maxOutputTokens = Some 123
                temperature = Some 0.1f }

        let options =
            { QaSessionOptions.create (tempStorageRoot ()) with
                autoWriteback = false
                contextProviders = [ FakeContextProvider(source) ]
                clients =
                    { QaModelClients.none with
                        answerGenerator = Some recorder }
                answerModelId = answerConfig.modelId
                modelRoles = UseCaseDefinition.defaultModels |> Map.add Answer answerConfig
                prompts =
                    { PromptSet.empty with
                        answerSystem = Some "CUSTOM SYSTEM"
                        answerUserTemplate = Some "Q={{question}}\nCTX={{sourceContext}}\nINV={{sourceInventory}}" } }

        use session = new QaSession(options)

        let request =
            { turnId = Guid.NewGuid().ToString("N")
              question = "What does the fake context say?"
              realtimeJudgement = None
              deadline = None }

        let! answer = session.AnswerAsync(request, CancellationToken.None)

        Assert.Equal("custom response", answer.answer)
        Assert.Equal(123, recorder.MaxOutputTokens.Value)
        Assert.Equal(0.1f, recorder.Temperature.Value)
        Assert.Equal("CUSTOM SYSTEM", recorder.Messages[0].Text)
        Assert.Contains("Q=What does the fake context say?", recorder.Messages[1].Text)
        Assert.Contains("Fake context for What does the fake context say?", recorder.Messages[1].Text)
    }

[<Fact>]
let ``getSynonyms parses comma-separated keywords correctly`` () =
    async {
        let client = new MockChatClient()
        let mutable reportedMsg = ""
        let report msg = reportedMsg <- msg
        let! expansion = KnowledgeSources.getSynonyms client (Some report) "query"

        match expansion with
        | Some expansion ->
            Assert.Equal<int>(2, expansion.terms.Length)
            Assert.Contains("synonym1", expansion.terms)
            Assert.Contains("technicalTerm1", expansion.terms)
            Assert.Equal<KnowledgeSources.QueryType>(KnowledgeSources.QueryType.Question, expansion.queryType)
            Assert.Contains("Retrieval query", reportedMsg)
        | None -> failwith "Expected query expansion."
    }
    |> Async.RunSynchronously

[<Fact>]
let ``getSynonyms handles empty query`` () =
    async {
        let client = new MockChatClient()
        let! expansion = KnowledgeSources.getSynonyms client None ""
        Assert.True(Option.isNone expansion)
    }
    |> Async.RunSynchronously

[<Fact>]
let ``query post-processing keeps domain expansion in supplied use-case profile`` () =
    let profile =
        { QaUseCaseProfile.generic with
            id = "test-domain"
            displayName = "Test Domain"
            voiceReplacements =
                [ { pattern = @"\bmedicare\s+gap\b"
                    replacement = "Medigap" } ]
            queryExpansionRules =
                [ { triggers = [ "medigap" ]
                    terms = [ "medicare"; "supplement"; "plan" ] } ] }

    let generic = Text.QueryPostProcessing.forVoiceLikeRetrieval "medicare gap"

    let profiled =
        Text.QueryPostProcessing.forVoiceLikeRetrievalWithProfile profile "medicare gap"

    Assert.DoesNotContain("supplement", generic.searchTerms)
    Assert.Contains("supplement", profiled.searchTerms)
    Assert.Contains("Medigap", profiled.normalizedQuery)

[<Fact>]
let ``json knowledge source can be loaded as generic QA content`` () =
    async {
        let path =
            Path.Combine(Path.GetTempPath(), $"fskame-json-source-{Guid.NewGuid():N}.json")

        File.WriteAllText(
            path,
            JsonSerializer.Serialize(
                {| id = "test-source"
                   title = "Test Source"
                   documents =
                    [ {| id = "1"
                         title = "Answer label: 1"
                         text = "Use the scheduling guide for emergency dental appointments."
                         keywords = [ "appointment lookup" ] |} ] |}
            )
        )

        let source =
            { kind = Json
              location = path
              enabled = true }

        let! retrieval, errors = KnowledgeSources.loadInternalIndex [ source ]

        Assert.Empty errors
        let chunk = Assert.Single retrieval.chunks
        Assert.Contains("Answer label: 1", chunk.text)
        Assert.Contains("emergency dental appointments", chunk.text)
    }
    |> Async.RunSynchronously

[<Fact>]
let ``qa session composes injected context providers`` () =
    task {
        let source =
            { kind = Json
              location = "fake://source"
              enabled = true }

        let provider = FakeContextProvider source :> IQaContextProvider
        let storageRoot = tempStorageRoot ()

        let options =
            { QaSessionOptions.create storageRoot with
                autoWriteback = false }

        let session = new QaSession(options)
        let! errors = (session :> IQaOrchestrator).ConfigureAsync([ provider ], CancellationToken.None)

        let request =
            { turnId = Guid.NewGuid().ToString("N")
              question = "composition"
              realtimeJudgement = None
              deadline = None }

        let! answer = session.AnswerAsync(request, CancellationToken.None)

        Assert.Empty errors
        Assert.Single answer.context |> ignore
        Assert.Contains("composition", answer.context.Head.text)
        Assert.Equal<KnowledgeSource list>([ source ], answer.inventory)

        do! (session :> IAsyncDisposable).DisposeAsync().AsTask()
    }

[<Fact>]
let ``qa session skips llm tool planner when only built-in context tools are available`` () =
    task {
        let source =
            { kind = Json
              location = "fake://source"
              enabled = true }

        let planner = new CountingChatClient("""{"calls":[]}""")
        let provider = FakeContextProvider source :> IQaContextProvider
        let storageRoot = tempStorageRoot ()

        let options =
            { QaSessionOptions.create storageRoot with
                autoWriteback = false
                clients =
                    { QaModelClients.none with
                        toolPlanner = Some planner } }

        let session = new QaSession(options)
        let! _ = (session :> IQaOrchestrator).ConfigureAsync([ provider ], CancellationToken.None)

        let request =
            { turnId = Guid.NewGuid().ToString("N")
              question = "composition"
              realtimeJudgement = None
              deadline = None }

        let! _ = session.AnswerAsync(request, CancellationToken.None)

        Assert.Equal(0, planner.Count)

        do! (session :> IAsyncDisposable).DisposeAsync().AsTask()
    }

[<Fact>]
let ``qa session does not call llm query expansion by default`` () =
    task {
        let path =
            Path.Combine(Path.GetTempPath(), $"fskame-no-query-expansion-{Guid.NewGuid():N}.json")

        File.WriteAllText(
            path,
            JsonSerializer.Serialize(
                {| documents =
                    [ {| id = "1"
                         title = "Policy"
                         text = "The debug answer is BLUE-42."
                         keywords = [] |} ] |}
            )
        )

        let expansion =
            new CountingChatClient(
                """{"terms":["BLUE-42"],"rewrittenQueries":["BLUE-42"],"sectionName":null,"queryType":"Question"}"""
            )

        let options =
            { QaSessionOptions.create (tempStorageRoot ()) with
                autoWriteback = false
                clients =
                    { QaModelClients.none with
                        queryExpansion = Some expansion } }

        use session = new QaSession(options)

        let source =
            { kind = Json
              location = path
              enabled = true }

        let! _ = session.LoadSourcesAsync(InternalDocumentIndex, [ source ], CancellationToken.None)

        let request =
            { turnId = Guid.NewGuid().ToString("N")
              question = "What is the debug answer?"
              realtimeJudgement = None
              deadline = None }

        let! answer = session.AnswerAsync(request, CancellationToken.None)

        Assert.Equal(0, expansion.Count)
        Assert.Contains("BLUE-42", answer.context.Head.text)
    }

[<Fact>]
let ``keyword schema rejection does not block keyword attachment`` () =
    async {
        let source =
            { kind = Markdown
              location = $"/tmp/schema-reject-{Guid.NewGuid():N}.md"
              enabled = true }

        let passages =
            [ keywordPassage source 0 "The guide covers emergency dental scheduling."
              keywordPassage source 1 "The guide excludes cosmetic dental scheduling." ]

        let logs = ResizeArray<string>()
        let storageRoot = tempStorageRoot ()

        let! enriched, _ =
            KnowledgeSources.attachKeywords
                storageRoot
                logs.Add
                (keywordOptions $"test-schema-{Guid.NewGuid():N}" (new InvalidJsonSchemaChatClient() :> IChatClient))
                source
                passages

        Assert.Equal<int>(2, enriched.Length)
        Assert.All(enriched, fun passage -> Assert.Empty passage.keywords)

        let schemaRejectionLogs =
            logs
            |> Seq.filter (fun log -> log.Contains("Keyword schema was rejected by OpenAI"))
            |> Seq.length

        Assert.Equal(1, schemaRejectionLogs)
    }
    |> Async.RunSynchronously

[<Fact>]
let ``keyword schema rejection keeps cached keyword records`` () =
    async {
        let schemaVersion = $"test-cache-schema-{Guid.NewGuid():N}"
        let storageRoot = tempStorageRoot ()

        let source =
            { kind = Markdown
              location = $"/tmp/schema-cache-{Guid.NewGuid():N}.md"
              enabled = true }

        let passages =
            [ keywordPassage source 0 "The guide covers emergency dental scheduling."
              keywordPassage source 1 "The guide excludes cosmetic dental scheduling." ]

        let options =
            keywordOptions schemaVersion (new InvalidJsonSchemaChatClient() :> IChatClient)

        seedKeywordCache storageRoot source options passages.Head [ "cached scheduling term" ]
        let logs = ResizeArray<string>()

        let! enriched, _ = KnowledgeSources.attachKeywords storageRoot logs.Add options source passages

        Assert.Equal<string list>([ "cached scheduling term" ], enriched.Head.keywords)
        Assert.Empty(enriched.Tail.Head.keywords)

        let schemaRejectionLogs =
            logs
            |> Seq.filter (fun log -> log.Contains("Keyword schema was rejected by OpenAI"))
            |> Seq.length

        Assert.Equal(1, schemaRejectionLogs)
    }
    |> Async.RunSynchronously

[<Fact>]
let ``tool loader includes current time tool from tools project`` () =
    let providerFolder =
        System.IO.Path.GetDirectoryName(typeof<FsKame.Tools.CurrentTimeToolProvider>.Assembly.Location)

    let catalog = QaToolLoader.load (FakeQaToolHost()) (Some providerFolder)

    let hasCurrentTimeTool =
        catalog.tools
        |> List.exists (fun tool -> tool.PluginName = "FsKameTools" && tool.Name = "current_time")

    Assert.True(hasCurrentTimeTool)

[<Fact>]
let ``rank promotes exact section target over lexical distractors`` () =
    async {
        let source =
            { kind = Pdf
              location = "/tmp/paper.pdf"
              enabled = true }

        let retrieval =
            { KnowledgeSources.emptyIndex with
                sources = [ source ]
                chunks =
                    [ { source = source
                        index = 1
                        text = "Section: Notes\nThis appendix mentions abstract abstract abstract."
                        score = 0.0f }
                      { source = source
                        index = 2
                        text = "Section: ABSTRACT\nThis paper introduces a retrieval method."
                        score = 0.0f } ] }

        let! chunks = KnowledgeSources.rank None false false true ignore "Can you summarize the abstract?" 1 retrieval

        Assert.Equal(2, chunks.Head.index)
    }
    |> Async.RunSynchronously

[<Fact>]
let ``rank corrects misspelled section target before searching`` () =
    async {
        let source =
            { kind = Pdf
              location = "/tmp/paper.pdf"
              enabled = true }

        let retrieval =
            { KnowledgeSources.emptyIndex with
                sources = [ source ]
                chunks =
                    [ { source = source
                        index = 1
                        text = "Section: Notes\nThis appendix mentions abstract abstract abstract."
                        score = 0.0f }
                      { source = source
                        index = 2
                        text = "Section: ABSTRACT\nThis paper introduces a retrieval method."
                        score = 0.0f } ] }

        let! chunks = KnowledgeSources.rank None false false true ignore "Can you summarize the abtract?" 1 retrieval

        Assert.Equal(2, chunks.Head.index)
    }
    |> Async.RunSynchronously

[<Fact>]
let ``rank keeps coverage from each selected source for multi-document questions`` () =
    async {
        let sourceA =
            { kind = Pdf
              location = "/tmp/source-a.pdf"
              enabled = true }

        let sourceB =
            { kind = Pdf
              location = "/tmp/source-b.pdf"
              enabled = true }

        let retrieval =
            { KnowledgeSources.emptyIndex with
                sources = [ sourceA; sourceB ]
                chunks =
                    [ { source = sourceA
                        index = 1
                        text = "Latency latency latency for source A."
                        score = 0.0f }
                      { source = sourceA
                        index = 2
                        text = "Latency latency latency architecture in source A."
                        score = 0.0f }
                      { source = sourceB
                        index = 1
                        text = "Latency tradeoffs for source B."
                        score = 0.0f } ] }

        let! chunks = KnowledgeSources.rank None false false true ignore "Compare both documents on latency" 2 retrieval

        let sourceLocations =
            chunks |> List.map (fun chunk -> chunk.source.location) |> Set.ofList

        Assert.Equal<int>(2, chunks.Length)
        Assert.Contains(sourceA.location, sourceLocations)
        Assert.Contains(sourceB.location, sourceLocations)
    }
    |> Async.RunSynchronously

[<Fact>]
let ``rank returns representative content for broad selected-document summaries`` () =
    async {
        let sourceA =
            { kind = Pdf
              location = "/tmp/broad-a.pdf"
              enabled = true }

        let sourceB =
            { kind = Pdf
              location = "/tmp/broad-b.pdf"
              enabled = true }

        let retrieval =
            { KnowledgeSources.emptyIndex with
                sources = [ sourceA; sourceB ]
                chunks =
                    [ { source = sourceA
                        index = 1
                        text = "Quantum scheduling methods are introduced here."
                        score = 0.0f }
                      { source = sourceB
                        index = 1
                        text = "Energy market simulations are introduced here."
                        score = 0.0f } ] }

        let! chunks = KnowledgeSources.rank None false false true ignore "Summarize these documents" 4 retrieval

        let sourceLocations =
            chunks |> List.map (fun chunk -> chunk.source.location) |> Set.ofList

        Assert.Contains(sourceA.location, sourceLocations)
        Assert.Contains(sourceB.location, sourceLocations)
    }
    |> Async.RunSynchronously

[<Fact>]
let ``durable memory types explicit user preference as directive`` () =
    let snapshot =
        { turnId = "turn_preference"
          itemId = "item_preference"
          revision = 1
          text = "Remember that I prefer concise technical answers."
          isFinal = true
          receivedAt = DateTimeOffset.UtcNow }

    let proposals =
        DurableMemory.proposalsFromExchange snapshot "Okay, I will remember that."

    let _ = Assert.Single proposals

    Assert.Equal(Directive, proposals.Head.kind)
    Assert.Contains("concise", proposals.Head.text, StringComparison.OrdinalIgnoreCase)

[<Fact>]
let ``durable memory hot overlay makes committed memory immediately recallable`` () =
    let snapshot =
        { turnId = "turn_decision"
          itemId = "item_decision"
          revision = 1
          text = "We decided to use typed memory records for realtime recall."
          isFinal = true
          receivedAt = DateTimeOffset.UtcNow }

    let proposals =
        DurableMemory.proposalsFromExchange snapshot "That decision is recorded."

    let store, updates, _ =
        DurableMemory.commitProposals (DurableMemory.inMemory []) proposals

    let spec =
        { query = "typed memory realtime recall decision"
          kinds = [ Decision ]
          scopes = [ Session ]
          namespaceId = DurableMemory.defaultNamespace
          temporalMode = CurrentOnly
          temporalReference = None
          includeSuperseded = false
          recallBudget = Fast
          maxCandidates = 10
          minScore = None
          latencyBudget = TimeSpan.FromMilliseconds 90. }

    let hits = DurableMemory.recall None spec store |> Async.RunSynchronously

    let _ = Assert.Single updates

    Assert.NotEmpty hits
    Assert.Equal(Decision, hits.Head.record.kind)

[<Fact>]
let ``durable memory correction supersedes related directive`` () =
    let original =
        { turnId = "turn_original"
          itemId = "item_original"
          revision = 1
          text = "I prefer verbose technical answers."
          isFinal = true
          receivedAt = DateTimeOffset.UtcNow.AddMinutes(-1.) }

    let correction =
        { turnId = "turn_correction"
          itemId = "item_correction"
          revision = 2
          text = "Actually, I prefer concise technical answers."
          isFinal = true
          receivedAt = DateTimeOffset.UtcNow }

    let store, _, _ =
        DurableMemory.commitProposals
            (DurableMemory.inMemory [])
            (DurableMemory.proposalsFromExchange original "Noted.")

    let store, updates, _ =
        DurableMemory.commitProposals store (DurableMemory.proposalsFromExchange correction "Updated.")

    let currentDirectives =
        store.records
        |> List.filter (fun record -> record.kind = Directive && record.status = Current)

    let supersededDirectives =
        store.records
        |> List.filter (fun record -> record.kind = Directive && record.status = Superseded)

    Assert.True(updates |> List.exists (fun update -> update.outcome = Contradiction))
    let _ = Assert.Single currentDirectives
    let _ = Assert.Single supersededDirectives

    Assert.Contains("concise", currentDirectives.Head.text, StringComparison.OrdinalIgnoreCase)

[<Fact>]
let ``durable memory forget request retracts related current record`` () =
    let original =
        { turnId = "turn_original_forget"
          itemId = "item_original_forget"
          revision = 1
          text = "I prefer verbose implementation notes."
          isFinal = true
          receivedAt = DateTimeOffset.UtcNow.AddMinutes(-1.) }

    let forget =
        { turnId = "turn_forget"
          itemId = "item_forget"
          revision = 2
          text = "Forget my preference for verbose implementation notes."
          isFinal = true
          receivedAt = DateTimeOffset.UtcNow }

    let store, _, _ =
        DurableMemory.commitProposals
            (DurableMemory.inMemory [])
            (DurableMemory.proposalsFromExchange original "Noted.")

    let store, logs = DurableMemory.retractFromTurn store forget

    Assert.True(
        logs
        |> List.exists (fun log -> log.Contains("Retracted", StringComparison.OrdinalIgnoreCase))
    )

    Assert.True(
        store.records
        |> List.exists (fun record -> record.kind = Directive && record.status = Retracted)
    )
