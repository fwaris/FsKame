module FsKame.Tests

open Xunit
open System
open System.Collections.Concurrent
open System.Threading
open System.Threading.Tasks
open System.Collections.Generic
open Microsoft.Extensions.AI
open System.Text.Json.Serialization
open FSharp.Control
open RTOpenAI.Events
open FsKame
open FsKame.WorkFlow

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

let private included name value =
    match value with
    | Include(Some x) -> x
    | Include None -> failwith $"{name} was explicitly null."
    | Skip -> failwith $"{name} was skipped."

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
let ``realtime session enables automatic VAD tool responses and interruptions`` () =
    let input = VoiceAgent.sessionAudio.input |> included "audio.input"
    let turnDetection = input.turn_detection |> included "audio.input.turn_detection"

    match turnDetection with
    | VAD.Server_Vad config ->
        Assert.True(config.create_response)
        Assert.True(config.interrupt_response)
        Assert.Equal(300, config.prefix_padding_ms)
        Assert.Equal(200, config.silence_duration_ms)
        Assert.Equal(0.5, config.threshold)

        match config.idle_timeout_ms with
        | Skip -> ()
        | Include _ -> failwith "Expected idle_timeout_ms to be skipped."
    | VAD.Semantic_Vad _ -> failwith "Expected server_vad turn detection."

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
let ``tool loader includes current time tool from tools project`` () =
    let context = ToolHostContext(ignore, ConcurrentQueue<MemoryCard>())

    let missingProviderFolder =
        System.IO.Path.Combine(System.IO.Path.GetTempPath(), Guid.NewGuid().ToString("N"))

    let catalog = ToolLoader.load context missingProviderFolder

    let hasCurrentTimeTool =
        catalog.plugins
        |> List.exists (fun plugin ->
            plugin.Name = "FsKameTools"
            && (plugin |> Seq.exists (fun fn -> fn.Name = "current_time")))

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
