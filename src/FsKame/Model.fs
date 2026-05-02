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
      sources: KnowledgeSource list
      mailbox: Channel<Msg> }

and Model =
    { currentPage: AppPage
      mailbox: Channel<Msg>
      bundle: ConnectionBundle option
      sessionState: RTOpenAI.WebRTC.State
      openAiKey: string
      oracleModel: string
      pdfDocuments: PdfDocumentSource list
      log: string list
      hideSecrets: bool
      isBusy: bool }

and Msg =
    | OpenAiKeyChanged of string
    | OracleModelChanged of string
    | Settings_Show
    | Settings_Close
    | ToggleSecretVisibility
    | PickPdfs
    | PickPdfsCompleted of Result<PdfDocumentSource list, exn>
    | PdfProcessingCompleted of Result<PdfProcessResult list, exn>
    | PdfSelectionChanged of string * bool
    | RetryPdfProcessing of string
    | ApplySources
    | StartStop
    | StartCompleted of Result<ConnectionBundle, exn>
    | StopCompleted of Result<unit, exn>
    | WebRTC_StateChanged of RTOpenAI.WebRTC.State
    | Log_Append of string
    | Log_Clear
    | EventError of exn
