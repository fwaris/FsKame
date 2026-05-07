module FsKame.Tests

open Xunit
open System
open System.Threading
open System.Threading.Tasks
open System.Collections.Generic
open Microsoft.Extensions.AI
open System.Text.Json.Serialization
open FSharp.Control
open RTOpenAI.Events
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
            asyncSeq { yield ChatResponseUpdate() } :> IAsyncEnumerable<ChatResponseUpdate>

let private included name value =
    match value with
    | Include(Some x) -> x
    | Include None -> failwith $"{name} was explicitly null."
    | Skip -> failwith $"{name} was skipped."



[<Fact>]
let ``realtime session disables automatic VAD responses and interruptions`` () =
    let input = VoiceAgent.sessionAudio.input |> included "audio.input"
    let turnDetection = input.turn_detection |> included "audio.input.turn_detection"

    match turnDetection with
    | VAD.Server_Vad config ->
        Assert.False(config.create_response)
        Assert.False(config.interrupt_response)
        Assert.Equal(300, config.prefix_padding_ms)
        Assert.Equal(500, config.silence_duration_ms)
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
