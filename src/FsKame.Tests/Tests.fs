module FsKame.Tests

open Xunit
open System
open System.Threading
open System.Threading.Tasks
open System.Collections.Generic
open Microsoft.Extensions.AI
open FSharp.Control
open FsKame.WorkFlow

type MockChatClient() =
    interface IChatClient with
        member this.Dispose() = ()
        member this.GetService(serviceType: Type, serviceKey: obj) = null
        member this.GetResponseAsync(chatMessages, options, cancellationToken) =
            let response = ChatResponse(ChatMessage(ChatRole.Assistant, "synonym1 antonym1 technicalTerm1"))
            Task.FromResult(response)
        member this.GetStreamingResponseAsync(chatMessages, options, cancellationToken) =
            asyncSeq { yield ChatResponseUpdate() } :> IAsyncEnumerable<ChatResponseUpdate>




[<Fact>]
let ``getSynonyms parses comma-separated keywords correctly`` () =
    async {
        let client = new MockChatClient()
        let mutable reportedMsg = ""
        let report msg = reportedMsg <- msg
        let! terms = KnowledgeSources.getSynonyms client (Some report) "query"
        
        Assert.Equal<int>(3, terms.Length)
        Assert.Contains("synonym1", terms)
        Assert.Contains("antonym1", terms)
        Assert.Contains("technicalTerm1", terms)
        Assert.Contains("Expanded query keywords", reportedMsg)
    } |> Async.RunSynchronously

[<Fact>]
let ``getSynonyms handles empty query`` () =
    async {
        let client = new MockChatClient()
        let! terms = KnowledgeSources.getSynonyms client None ""
        Assert.Empty(terms)
    } |> Async.RunSynchronously
