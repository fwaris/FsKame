namespace FsKame.WorkFlow

open System.Threading
open System.Threading.Channels
open FsKame
open RTFlow

module StateMachine =
    type SubState =
        { mailbox: Channel<Msg>
          bus: WBus<FlowMsg, AgentMsg>
          apiKey: string
          useCase: FsKame.QA.UseCaseDefinition
          useCasePlugin: FsKame.QA.IUseCasePlugin
          useCaseSettings: Map<string, string>
          oracleModel: string
          retrievalMode: RetrievalMode
          conn: RTOpenAI.Api.Connection
          sources: KnowledgeSource list
          toolHostContext: ToolHostContext
          toolCatalog: ToolCatalog
          logExpansions: bool
          logChunks: bool
          useLexicalFilter: bool
          elaborateIndexKeywords: bool }

    let private startAgents ss =
        async {
            AppAgent.start ss.mailbox ss.bus

            let flags: QaAgent.SourceFlags =
                { logExpansions = ss.logExpansions
                  logChunks = ss.logChunks
                  useLexicalFilter = ss.useLexicalFilter
                  elaborateIndexKeywords = ss.elaborateIndexKeywords }

            QaAgent.start
                ss.apiKey
                ss.useCase
                ss.useCasePlugin
                ss.useCaseSettings
                ss.retrievalMode
                ss.sources
                flags
                ss.bus

            VoiceAgent.start ss.apiKey ss.useCase ss.conn ss.bus
        }

    let rec private terminate isAbnormal ss =
        async {
            Log.info "Terminating FsKame flow."
            RTOpenAI.Api.Connection.close ss.conn
            do! Async.Sleep 250
            ss.bus.Close()
        }
        |> Async.Start

        F(s_terminate ss, [ Ag_FlowDone {| abnormal = isAbnormal |} ])

    and private s_start ss msg =
        async {
            match msg with
            | W_Err err -> return F(s_terminate ss, [ Ag_FlowError err; Ag_FlowDone {| abnormal = true |} ])
            | W_Msg Fl_Start ->
                do! startAgents ss

                let flags =
                    {| logExpansions = ss.logExpansions
                       logChunks = ss.logChunks
                       useLexicalFilter = ss.useLexicalFilter
                       elaborateIndexKeywords = ss.elaborateIndexKeywords |}

                return F(s_run ss, [ Ag_SourcesUpdated(ss.retrievalMode, ss.sources, flags) ])
            | W_Msg(Fl_Terminate x) -> return terminate x.abnormal ss
        }

    and private s_run ss msg =
        async {
            match msg with
            | W_Err err -> return F(s_terminate ss, [ Ag_FlowError err; Ag_FlowDone {| abnormal = true |} ])
            | W_Msg(Fl_Terminate x) -> return terminate x.abnormal ss
            | W_Msg Fl_Start -> return F(s_run ss, [])
        }

    and private s_terminate ss _ = async { return F(s_terminate ss, []) }

    let create
        mailbox
        apiKey
        useCase
        useCasePlugin
        useCaseSettings
        oracleModel
        retrievalMode
        conn
        sources
        toolHostContext
        toolCatalog
        logExpansions
        logChunks
        useLexicalFilter
        elaborateIndexKeywords
        =
        let bus = WBus<FlowMsg, AgentMsg>.Create()

        let ss =
            { mailbox = mailbox
              bus = bus
              apiKey = apiKey
              useCase = useCase
              useCasePlugin = useCasePlugin
              useCaseSettings = useCaseSettings
              oracleModel = oracleModel
              retrievalMode = retrievalMode
              conn = conn
              sources = sources
              toolHostContext = toolHostContext
              toolCatalog = toolCatalog
              logExpansions = logExpansions
              logChunks = logChunks
              useLexicalFilter = useLexicalFilter
              elaborateIndexKeywords = elaborateIndexKeywords }

        RTFlow.Workflow.run CancellationToken.None bus (s_start ss)

        { new IFlow<FlowMsg, AgentMsg> with
            member _.PostToFlow msg = bus.PostToFlow(W_Msg msg)
            member _.PostToAgent msg = bus.PostToAgent msg

            member _.Terminate() =
                bus.PostToFlow(W_Msg(Fl_Terminate {| abnormal = false |})) }
