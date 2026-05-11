namespace FsKame

open System
open System.Collections.Concurrent
open FsKame.WorkFlow
open RTOpenAI.Api

module Connect =
    let start (parms: StartParams) : Async<Result<ConnectionBundle, exn>> =
        async {
            try
                match Text.notEmpty parms.apiKey with
                | None -> return Error(InvalidOperationException "OpenAI API key is required." :> exn)
                | Some apiKey ->
                    let! hasPermission = Audio.haveRecordPermission ()

                    if not hasPermission then
                        return Error(UnauthorizedAccessException "Microphone permission was not granted." :> exn)
                    else
                        let conn = Connection.create ()
                        let blackboard = ConcurrentQueue<MemoryCard>()

                        let toolHostContext =
                            ToolHostContext(
                                (fun msg -> parms.mailbox.Writer.TryWrite(Log_Append msg) |> ignore),
                                blackboard
                            )

                        let loadedCatalog =
                            ToolLoader.load toolHostContext (ToolLoader.defaultProviderFolder ())

                        let toolCatalog =
                            { plugins = parms.toolCatalog.plugins @ loadedCatalog.plugins
                              logs = parms.toolCatalog.logs @ loadedCatalog.logs }

                        for log in toolCatalog.logs do
                            parms.mailbox.Writer.TryWrite(Log_Append log) |> ignore

                        let stateSubscription =
                            conn.WebRtcClient.StateChanged.Subscribe(fun state ->
                                parms.mailbox.Writer.TryWrite(WebRTC_StateChanged state) |> ignore)

                        let conn =
                            { conn with
                                Disposables = stateSubscription :: conn.Disposables }

                        let flow =
                            StateMachine.create
                                parms.mailbox
                                apiKey
                                parms.oracleModel
                                parms.retrievalMode
                                conn
                                parms.sources
                                toolHostContext
                                toolCatalog
                                parms.logExpansions
                                parms.logChunks
                                parms.useLexicalFilter
                                parms.elaborateIndexKeywords

                        flow.PostToFlow Fl_Start

                        return Ok { flow = flow; connection = conn }
            with ex ->
                Log.exn (ex, "Connect.start")
                return Error ex
        }

    let stop (bundle: ConnectionBundle) : Async<Result<unit, exn>> =
        async {
            try
                bundle.flow.Terminate()
                return Ok()
            with ex ->
                Log.exn (ex, "Connect.stop")
                return Error ex
        }
