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
          oracleModel: string
          retrievalMode: RetrievalMode
          conn: RTOpenAI.Api.Connection
          sources: KnowledgeSource list
          logExpansions: bool
          logChunks: bool }

    let private startAgents ss =
        async {
            AppAgent.start ss.mailbox ss.bus
            SourceAgent.start (Some ss.apiKey) ss.bus
            OracleAgent.start ss.apiKey ss.oracleModel ss.bus
            VoiceAgent.start ss.apiKey ss.conn ss.bus
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
                let flags = {| logExpansions = ss.logExpansions; logChunks = ss.logChunks |}
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

    let create mailbox apiKey oracleModel retrievalMode conn sources logExpansions logChunks =
        let bus = WBus<FlowMsg, AgentMsg>.Create()

        let ss =
            { mailbox = mailbox
              bus = bus
              apiKey = apiKey
              oracleModel = oracleModel
              retrievalMode = retrievalMode
              conn = conn
              sources = sources
              logExpansions = logExpansions
              logChunks = logChunks }

        RTFlow.Workflow.run CancellationToken.None bus (s_start ss)

        { new IFlow<FlowMsg, AgentMsg> with
            member _.PostToFlow msg = bus.PostToFlow(W_Msg msg)
            member _.PostToAgent msg = bus.PostToAgent msg

            member _.Terminate() =
                bus.PostToFlow(W_Msg(Fl_Terminate {| abnormal = false |})) }
