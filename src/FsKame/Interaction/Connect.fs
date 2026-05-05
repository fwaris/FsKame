namespace FsKame

open System
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
                                parms.logExpansions
                                parms.logChunks

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
