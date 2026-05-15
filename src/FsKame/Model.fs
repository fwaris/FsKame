namespace FsKame

open System.Threading
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
      useCase: FsKame.QA.UseCaseDefinition
      useCasePlugin: FsKame.QA.IUseCasePlugin
      useCaseSettings: Map<string, string>
      retrievalMode: RetrievalMode
      sources: KnowledgeSource list
      mailbox: Channel<Msg>
      logExpansions: bool
      logChunks: bool
      useLexicalFilter: bool
      elaborateIndexKeywords: bool
      useHybridPdfParsing: bool }

and Model =
    { currentPage: AppPage
      mailbox: Channel<Msg>
      bundle: ConnectionBundle option
      sessionState: RTOpenAI.WebRTC.State
      openAiKey: string
      activeUseCase: FsKame.QA.UseCaseDefinition
      useCasePlugin: FsKame.QA.IUseCasePlugin
      useCaseSettings: Map<string, string>
      modelRoleOverrides: Map<FsKame.QA.ModelRole, string>
      retrievalMode: RetrievalMode
      pdfDocuments: PdfDocumentSource list
      log: string list
      logFontSize: float
      hideSecrets: bool
      isBusy: bool
      documentProcessingCancellation: CancellationTokenSource option
      logExpansions: bool
      logChunks: bool
      useLexicalFilter: bool
      elaborateIndexKeywords: bool
      useHybridPdfParsing: bool }

and Msg =
    | OpenAiKeyChanged of string
    | ModelRoleModelChanged of FsKame.QA.ModelRole * string
    | UseCaseSettingChanged of string * string
    | RetrievalModeChanged of RetrievalMode
    | Settings_Show
    | Settings_Close
    | ToggleSecretVisibility
    | PickPdfs
    | PickPdfsCompleted of Result<PdfDocumentSource list, exn>
    | PdfProcessingCompleted of Result<PdfProcessResult list, exn>
    | CancelPdfProcessing
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
    | ElaborateIndexKeywordsToggled of bool
    | UseHybridPdfParsingToggled of bool
    | PrebuiltDocumentsInstalled of Result<PdfDocumentSource list * string list, exn>
