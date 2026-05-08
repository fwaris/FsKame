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

type KnowledgeSource =
    { kind: SourceKind
      location: string
      enabled: bool }

    member this.DisplayName =
        match this.kind with
        | Pdf -> $"PDF: {this.location}"
        | Markdown -> $"Markdown: {this.location}"

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

type MemoryContext =
    { requestId: string
      snapshot: TranscriptSnapshot
      context: SourceChunk list
      inventory: KnowledgeSource list
      cards: MemoryCard list
      timedOut: bool
      createdAt: DateTimeOffset }

type MemoryRequest =
    { requestId: string
      snapshot: TranscriptSnapshot
      deadline: DateTimeOffset
      cancellationToken: CancellationToken
      completion: TaskCompletionSource<MemoryContext> }

type VoiceToolCall =
    { name: string
      callId: string
      content: string
      snapshot: TranscriptSnapshot
      cancellation: CancellationTokenSource
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
           useLexicalFilter: bool |}
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
