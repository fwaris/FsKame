namespace FsKame.QA

open System
open System.Collections.Generic
open System.Threading
open System.Threading.Tasks

type KnowledgeSourceKind =
    | Pdf
    | Markdown
    | Json

type KnowledgeSource =
    { kind: KnowledgeSourceKind
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

type QaContextRequest = { query: string; maxResults: int }

type RetrievalMode =
    | InternalDocumentIndex
    | FsColbertWithFallback

module RetrievalModes =
    let toStorageValue mode =
        match mode with
        | InternalDocumentIndex -> "internal"
        | FsColbertWithFallback -> "fscolbert-with-fallback"

    let ofStorageValue value =
        match (defaultArg (Option.ofObj value) "").Trim().ToLowerInvariant() with
        | "internal"
        | "internal-document-index" -> InternalDocumentIndex
        | _ -> FsColbertWithFallback

    let displayName mode =
        match mode with
        | InternalDocumentIndex -> "Internal document index"
        | FsColbertWithFallback -> "FsColbert index with fallback"

type RiskFlags =
    { memoryMutation: bool
      sensitive: bool
      conflictLikely: bool }

module RiskFlags =
    let none =
        { memoryMutation = false
          sensitive = false
          conflictLikely = false }

type RealtimeJudgement =
    { turnKind: string option
      topicContinuity: string option
      memoryAction: string option
      needsExternalContext: bool option
      confidence: float
      riskFlags: RiskFlags }

type QaToolParameter =
    { name: string
      description: string
      required: bool }

type QaToolResult =
    { content: string
      metadata: IReadOnlyDictionary<string, string> }

module QaToolResult =
    let text content =
        { content = content
          metadata = Dictionary<string, string>() :> IReadOnlyDictionary<string, string> }

type IQaTool =
    abstract PluginName: string
    abstract Name: string
    abstract Description: string
    abstract Parameters: QaToolParameter list
    abstract InvokeAsync: IReadOnlyDictionary<string, string> * CancellationToken -> Task<QaToolResult>

type IQaToolHost =
    abstract Report: string -> unit

    abstract SearchKnowledgeAsync:
        question: string * maxResults: int * cancellationToken: CancellationToken -> Task<string>

    abstract SourceInventoryAsync: cancellationToken: CancellationToken -> Task<string>

    abstract SearchMemoryAsync: query: string * maxResults: int * cancellationToken: CancellationToken -> Task<string>

    abstract SearchBlackboardAsync: query: string * cancellationToken: CancellationToken -> Task<string>

type IQaToolProvider =
    abstract ContractVersion: int
    abstract GetTools: IQaToolHost -> IQaTool list

type IQaContextProvider =
    inherit IAsyncDisposable

    abstract ProviderId: string
    abstract DisplayName: string
    abstract Sources: KnowledgeSource list
    abstract LoadAsync: CancellationToken -> Task<string list>
    abstract RetrieveAsync: QaContextRequest * CancellationToken -> Task<SourceChunk list>
    abstract InventoryAsync: CancellationToken -> Task<string>

type QaTurnRequest =
    { turnId: string
      question: string
      realtimeJudgement: RealtimeJudgement option
      deadline: DateTimeOffset option }

type QaToolObservation =
    { pluginName: string
      toolName: string
      query: string
      content: string
      createdAt: DateTimeOffset }

type QaAnswer =
    { turnId: string
      answer: string
      model: string
      context: SourceChunk list
      sourceRetrievalElapsedMs: float
      inventory: KnowledgeSource list
      toolObservations: QaToolObservation list
      timedOut: bool
      createdAt: DateTimeOffset }

type IQaSession =
    inherit IAsyncDisposable

    abstract LoadSourcesAsync: RetrievalMode * KnowledgeSource list * CancellationToken -> Task<string list>

    abstract AnswerAsync: QaTurnRequest * CancellationToken -> Task<QaAnswer>

type IQaOrchestrator =
    inherit IAsyncDisposable

    abstract ConfigureAsync: IQaContextProvider list * CancellationToken -> Task<string list>
    abstract AnswerAsync: QaTurnRequest * CancellationToken -> Task<QaAnswer>
