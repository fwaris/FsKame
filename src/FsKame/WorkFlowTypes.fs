namespace FsKame.WorkFlow

open System
open System.Threading
open System.Threading.Tasks
open FsKame
open RTOpenAI.Events
open RTFlow

type SourceKind =
    | Pdf
    | Markdown
    | Json

type KnowledgeSource =
    { kind: SourceKind
      location: string
      enabled: bool }

    member this.DisplayName =
        match this.kind with
        | Pdf -> $"PDF: {this.location}"
        | Markdown -> $"Markdown: {this.location}"
        | Json -> $"JSON: {this.location}"

type SourceChunk =
    { source: KnowledgeSource
      index: int
      text: string
      score: float32 }

type TranscriptSnapshot =
    { turnId: string
      itemId: string
      revision: int
      text: string
      isFinal: bool
      receivedAt: DateTimeOffset }

type OracleCandidate =
    { turnId: string
      revision: int
      source: string
      answer: string
      context: SourceChunk list
      isFinal: bool
      createdAt: DateTimeOffset }

type MemoryCardKind =
    | UserIntent
    | ToolPlan
    | ToolObservation
    | RecallPlan
    | DurableMemoryRecord
    | ConflictNote
    | CommittedMemory
    | Evidence
    | OpenQuestion
    | ResolvedAnswer
    | CurrentFocus

type MemoryCard =
    { kind: MemoryCardKind
      title: string
      content: string
      source: string option
      createdAt: DateTimeOffset }

type MemoryKind =
    | Directive
    | Claim
    | Decision
    | Commitment
    | Episode

type MemoryScope =
    | Session
    | User
    | Workspace
    | Global

type MemoryStatus =
    | Current
    | Superseded
    | Retracted
    | Expired
    | Uncertain

type MemorySensitivity =
    | Normal
    | Personal
    | Sensitive
    | Secret

type TemporalMode =
    | CurrentOnly
    | AsOfTime
    | ChangedSince
    | IncludeHistory

type RecallBudget =
    | Fast
    | Medium
    | Large
    | ConflictProbe

type ControllerPath =
    | ZeroController
    | SmallController
    | LargeController
    | HybridSmallThenLarge

type MemoryTemporal =
    { observedAt: DateTimeOffset
      validFrom: DateTimeOffset
      validTo: DateTimeOffset option
      lastConfirmedAt: DateTimeOffset }

type MemoryProvenance =
    { sourceType: string
      sourceIds: string list
      toolName: string option
      toolArgsHash: string option
      resultHash: string option }

type MemoryRelations =
    { supersedes: string list
      supersededBy: string option
      conflictsWith: string list
      derivedFrom: string list }

type MemoryRetrieval =
    { indexText: string
      colbertDocIds: string list
      indexedAt: DateTimeOffset option
      embeddingModel: string option }

type MemoryRecord =
    { memoryId: string
      kind: MemoryKind
      title: string
      text: string
      summary: string
      scope: MemoryScope
      namespaceId: string
      entities: string list
      tags: string list
      status: MemoryStatus
      confidence: float
      importance: float
      sensitivity: MemorySensitivity
      temporal: MemoryTemporal
      provenance: MemoryProvenance
      relations: MemoryRelations
      retrieval: MemoryRetrieval
      version: int }

type MemoryRecallHit =
    { record: MemoryRecord
      score: float32
      reasons: string list }

type RiskFlags =
    { memoryMutation: bool
      sensitive: bool
      conflictLikely: bool }

type RealtimeJudgement =
    { turnKind: string option
      topicContinuity: string option
      memoryAction: string option
      needsExternalContext: bool option
      confidence: float
      riskFlags: RiskFlags }

type RecallSpec =
    { query: string
      kinds: MemoryKind list
      scopes: MemoryScope list
      namespaceId: string
      temporalMode: TemporalMode
      temporalReference: DateTimeOffset option
      includeSuperseded: bool
      recallBudget: RecallBudget
      maxCandidates: int
      minScore: float32 option
      latencyBudget: TimeSpan }

type SupervisorDecision =
    { controllerPath: ControllerPath
      recallSpec: RecallSpec
      acceptedRealtimeJudgement: bool
      riskFlags: RiskFlags
      reason: string }

type MemoryWriteProposal =
    { kind: MemoryKind
      title: string
      text: string
      summary: string
      scope: MemoryScope
      namespaceId: string
      entities: string list
      tags: string list
      confidence: float
      importance: float
      sensitivity: MemorySensitivity
      provenance: MemoryProvenance
      observedAt: DateTimeOffset
      explicitCorrection: bool }

type MemoryConflictKind =
    | Duplicate
    | Refinement
    | Contradiction
    | Unrelated
    | Retraction

type CommittedMemoryUpdate =
    { proposal: MemoryWriteProposal
      outcome: MemoryConflictKind
      committedRecord: MemoryRecord option
      affectedMemoryIds: string list
      message: string }

type MemoryContext =
    { requestId: string
      snapshot: TranscriptSnapshot
      supervisorDecision: SupervisorDecision
      context: SourceChunk list
      inventory: KnowledgeSource list
      memories: MemoryRecallHit list
      conflicts: string list
      cards: MemoryCard list
      timedOut: bool
      createdAt: DateTimeOffset }

type MemoryRequest =
    { requestId: string
      snapshot: TranscriptSnapshot
      realtimeJudgement: RealtimeJudgement option
      deadline: DateTimeOffset
      cancellationToken: CancellationToken
      completion: TaskCompletionSource<MemoryContext> }

type VoiceToolCall =
    { name: string
      callId: string
      content: string
      snapshot: TranscriptSnapshot
      cancellation: CancellationTokenSource
      timeout: TimeSpan
      task: TaskCompletionSource<ContentFunctionCallOutput> }

type FlowMsg =
    | Fl_Start
    | Fl_Terminate of {| abnormal: bool |}

type AgentMsg =
    | Ag_FlowError of WErrorType
    | Ag_FlowDone of {| abnormal: bool |}
    | Ag_SourcesUpdated of
        RetrievalMode *
        KnowledgeSource list *
        {| logExpansions: bool
           logChunks: bool
           useLexicalFilter: bool
           elaborateIndexKeywords: bool
           useHybridPdfParsing: bool |}
    | Ag_TranscriptUpdated of TranscriptSnapshot
    | Ag_MemoryRequested of MemoryRequest
    | Ag_MemoryReady of MemoryRequest * MemoryContext
    | Ag_MemoryRequestCanceled of string
    | Ag_MemoryRequestFailed of string * string
    | Ag_MemoryJobStarted of string
    | Ag_MemoryJobFinished of string * Result<unit, string>
    | Ag_ContextReady of TranscriptSnapshot * SourceChunk list * KnowledgeSource list
    | Ag_OracleRequested of MemoryRequest * MemoryContext
    | Ag_ResponseReady of TranscriptSnapshot * OracleCandidate option
    | Ag_VoiceServerEvent of ServerEvent
    | Ag_ToolCallOutputReady of string * string
    | Ag_Log of string

    override this.ToString() =
        match this with
        | Ag_FlowError _ -> "Ag_FlowError"
        | Ag_FlowDone _ -> "Ag_FlowDone"
        | Ag_SourcesUpdated _ -> "Ag_SourcesUpdated"
        | Ag_TranscriptUpdated _ -> "Ag_TranscriptUpdated"
        | Ag_MemoryRequested _ -> "Ag_MemoryRequested"
        | Ag_MemoryReady _ -> "Ag_MemoryReady"
        | Ag_MemoryRequestCanceled _ -> "Ag_MemoryRequestCanceled"
        | Ag_MemoryRequestFailed _ -> "Ag_MemoryRequestFailed"
        | Ag_MemoryJobStarted _ -> "Ag_MemoryJobStarted"
        | Ag_MemoryJobFinished _ -> "Ag_MemoryJobFinished"
        | Ag_ContextReady _ -> "Ag_ContextReady"
        | Ag_OracleRequested _ -> "Ag_OracleRequested"
        | Ag_ResponseReady _ -> "Ag_ResponseReady"
        | Ag_VoiceServerEvent _ -> "Ag_VoiceServerEvent"
        | Ag_ToolCallOutputReady _ -> "Ag_ToolCallOutputReady"
        | Ag_Log _ -> "Ag_Log"
