namespace FsKame.QA

open System
open System.Threading
open System.Threading.Tasks

type IMemoryService =
    abstract StartupLogs: string list
    abstract DefaultNamespace: string
    abstract CreateSupervisorDecision: TranscriptSnapshot * RealtimeJudgement option -> SupervisorDecision
    abstract RecallAsync: SupervisorDecision * CancellationToken -> Task<MemoryRecallHit list>
    abstract SearchAsync: string * int * CancellationToken -> Task<string>
    abstract ProposalsFromExchange: TranscriptSnapshot * string -> MemoryWriteProposal list
    abstract CommitProposals: MemoryWriteProposal list -> CommittedMemoryUpdate list * string list

type DurableMemoryService(path: string, encoder: unit -> FsColbert.OnnxColbertEncoder option) =
    let mutable store, startupLogs = DurableMemory.load path

    let clamp (maxValue: int) (value: int) = Math.Max(1, Math.Min(maxValue, value))

    let recallSpec query maxResults =
        let budget = if maxResults <= 20 then Fast else Medium

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

    new(path: string) = DurableMemoryService(path, fun () -> None)

    interface IMemoryService with
        member _.StartupLogs = startupLogs

        member _.DefaultNamespace = DurableMemory.defaultNamespace

        member _.CreateSupervisorDecision(snapshot, judgement) =
            DurableMemory.createSupervisorDecision snapshot judgement

        member _.RecallAsync(decision, cancellationToken) =
            DurableMemory.recall (encoder ()) decision.recallSpec store
            |> fun work -> Async.StartAsTask(work, cancellationToken = cancellationToken)

        member _.SearchAsync(query, maxResults, cancellationToken) =
            task {
                let! hits =
                    DurableMemory.recall (encoder ()) (recallSpec query maxResults) store
                    |> fun work -> Async.StartAsTask(work, cancellationToken = cancellationToken)

                return DurableMemory.renderRecall hits
            }

        member _.ProposalsFromExchange(snapshot, answer) =
            DurableMemory.proposalsFromExchange snapshot answer

        member _.CommitProposals proposals =
            let updated, updates, logs = DurableMemory.commitProposals store proposals
            store <- updated
            updates, logs
