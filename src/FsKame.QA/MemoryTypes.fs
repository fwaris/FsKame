namespace FsKame.QA

open System

type TranscriptSnapshot =
    { turnId: string
      itemId: string
      revision: int
      text: string
      isFinal: bool
      receivedAt: DateTimeOffset }

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
