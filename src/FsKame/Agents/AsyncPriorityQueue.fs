namespace FsKame.WorkFlow

open System
open System.Collections.Generic
open System.Threading.Tasks
open FSharp.Control

[<StructuralEquality; StructuralComparison>]
type private QueueKey = { Priority: int; Sequence: int64 }

type private AsyncPriorityQueueMsg<'T> =
    | Enqueue of 'T * int
    | Dequeue of AsyncReplyChannel<Result<'T, exn>>
    | Complete

/// Thread-safe priority queue backed by a MailboxProcessor.
/// Lower priority values are dequeued first; equal priorities are FIFO.
type internal AsyncPriorityQueue<'T>() =
    let completedExn = InvalidOperationException("Queue has been completed.")

    let tryDequeue (queue: PriorityQueue<'T, QueueKey>) =
        let mutable item = Unchecked.defaultof<'T>
        let mutable key = Unchecked.defaultof<QueueKey>

        if queue.TryDequeue(&item, &key) then Some item else None

    let agent =
        MailboxProcessor.Start(fun inbox ->
            let queue = PriorityQueue<'T, QueueKey>()

            let rec running (waiters: AsyncReplyChannel<Result<'T, exn>> list) nextSeq isCompleted =
                async {
                    let! msg = inbox.Receive()

                    match msg with
                    | Enqueue(item, priority) ->
                        if isCompleted then
                            return! running waiters nextSeq isCompleted
                        else
                            match waiters with
                            | waiter :: rest ->
                                waiter.Reply(Ok item)
                                return! running rest nextSeq isCompleted
                            | [] ->
                                queue.Enqueue(
                                    item,
                                    { Priority = priority
                                      Sequence = nextSeq }
                                )

                                return! running waiters (nextSeq + 1L) isCompleted

                    | Dequeue reply ->
                        match tryDequeue queue with
                        | Some item ->
                            reply.Reply(Ok item)
                            return! running waiters nextSeq isCompleted
                        | None when isCompleted ->
                            reply.Reply(Error completedExn)
                            return! running waiters nextSeq isCompleted
                        | None -> return! running (waiters @ [ reply ]) nextSeq isCompleted

                    | Complete ->
                        for waiter in waiters do
                            waiter.Reply(Error completedExn)

                        return! running [] nextSeq true
                }

            running [] 0L false)

    member _.Enqueue(item: 'T, priority: int) = agent.Post(Enqueue(item, priority))

    member _.DequeueAsync() =
        async {
            let! result = agent.PostAndAsyncReply Dequeue

            match result with
            | Ok item -> return item
            | Error ex -> return raise ex
        }

    member this.DequeueTask() : Task<'T> =
        this.DequeueAsync() |> Async.StartAsTask

    member _.Complete() = agent.Post Complete

    member _.ToAsyncSeq() : AsyncSeq<'T> =
        asyncSeq {
            let mutable isDone = false

            while not isDone do
                let! result = agent.PostAndAsyncReply Dequeue

                match result with
                | Ok item -> yield item
                | Error _ -> isDone <- true
        }
