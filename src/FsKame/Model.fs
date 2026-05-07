namespace FsKame

open System.Threading.Channels
open FsKame.WorkFlow
open RTFlow
open RTOpenAI.Api

type AppPage =
    | Main
    | Settings

type ConnectionBundle =
    { flow: IFlow<FlowMsg, AgentMsg>
      connection: Connection }

type StartParams =
    { apiKey: string
      oracleModel: string
      retrievalMode: RetrievalMode
      sources: KnowledgeSource list
      mailbox: Channel<Msg>
      logExpansions: bool
      logChunks: bool
      useLexicalFilter: bool }

and Model =
    { currentPage: AppPage
      mailbox: Channel<Msg>
      bundle: ConnectionBundle option
      sessionState: RTOpenAI.WebRTC.State
      openAiKey: string
      oracleModel: string
      retrievalMode: RetrievalMode
      pdfDocuments: PdfDocumentSource list
      log: string list
      logFontSize: float
      hideSecrets: bool
      isBusy: bool
      logExpansions: bool
      logChunks: bool
      useLexicalFilter: bool }

and Msg =
    | OpenAiKeyChanged of string
    | OracleModelChanged of string
    | RetrievalModeChanged of RetrievalMode
    | Settings_Show
    | Settings_Close
    | ToggleSecretVisibility
    | PickPdfs
    | PickPdfsCompleted of Result<PdfDocumentSource list, exn>
    | PdfProcessingCompleted of Result<PdfProcessResult list, exn>
    | PdfSelectionChanged of string * bool
    | RetryPdfProcessing of string
    | DeletePdf of string
    | DeletePdfCompleted of Result<PdfDeleteResult, exn>
    | ApplySources
    | StartStop
    | StartCompleted of Result<ConnectionBundle, exn>
    | StopCompleted of Result<unit, exn>
    | WebRTC_StateChanged of RTOpenAI.WebRTC.State
    | Log_Append of string
    | Log_Clear
    | LogFont_Increase
    | LogFont_Decrease
    | EventError of exn
    | LogExpansionsToggled of bool
    | LogChunksToggled of bool
    | UseLexicalFilterToggled of bool
