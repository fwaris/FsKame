namespace FsKame.WorkFlow

open System
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
    | Ag_ContextReady of TranscriptSnapshot * SourceChunk list * KnowledgeSource list
    | Ag_ResponseReady of TranscriptSnapshot * OracleCandidate option
    | Ag_VoiceServerEvent of ServerEvent
    | Ag_Log of string

    override this.ToString() =
        match this with
        | Ag_FlowError _ -> "Ag_FlowError"
        | Ag_FlowDone _ -> "Ag_FlowDone"
        | Ag_SourcesUpdated _ -> "Ag_SourcesUpdated"
        | Ag_TranscriptUpdated _ -> "Ag_TranscriptUpdated"
        | Ag_ContextReady _ -> "Ag_ContextReady"
        | Ag_ResponseReady _ -> "Ag_ResponseReady"
        | Ag_VoiceServerEvent _ -> "Ag_VoiceServerEvent"
        | Ag_Log _ -> "Ag_Log"
