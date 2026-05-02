namespace FsKame.WorkFlow

open System
open System.Text.Json.Serialization
open FSharp.Control
open RTOpenAI.Api
open RTOpenAI.Events
open RTFlow
open RTFlow.Functions

module VoiceAgent =
    type VoiceState =
        { initialized: bool
          transcriptByItem: Map<string, string>
          revision: int
          respondedTurns: Set<string>
          bus: WBus<FlowMsg, AgentMsg> }

    let sessionAudio =
        { Audio.Default with
            input =
                Include(
                    Some
                        { AudioInput.Default with
                            transcription =
                                { language = "en"
                                  model = "gpt-4o-mini-transcribe"
                                  prompt = Some "Expect natural question-answering conversation." }
                                |> Some
                                |> Include }
                ) }

    let private instructions =
        "You are FsKame, a low-latency voice question-answering assistant. Keep answers conversational and brief: usually one to three short spoken sentences. Avoid preambles, long caveats, and exhaustive lists unless the user asks. When backend oracle guidance is supplied, use it as the best available answer. If PDFs are mentioned, cite them naturally."

    let private updateSession (session: Session) =
        { session with
            id = Skip
            ``object`` = Skip
            model = Some FsKame.C.DEFAULT_REALTIME_MODEL
            audio = Include(Some sessionAudio)
            instructions = Some instructions
            tool_choice = Include(Some "none")
            tools = Include(Some [])
            expires_at = Skip }

    let private sendSessionUpdate conn session =
        { SessionUpdate.Default with
            event_id = Utils.newId ()
            session = updateSession session }
        |> ClientEvent.SessionUpdate
        |> Connection.sendClientEvent conn

    let private responseCreate conn (responseInstructions: string) =
        let response =
            { Response.Default with
                instructions = Include(Some responseInstructions)
                output_modalities = Include(Some [ "audio" ]) }

        { ResponseCreate.Default with
            event_id = Utils.newId ()
            response = Include(Some response) }
        |> ClientEvent.ResponseCreate
        |> Connection.sendClientEvent conn

    let private fallbackInstructions (snapshot: TranscriptSnapshot) =
        $"{instructions}\n\nThe user just said:\n{snapshot.text}\n\nAnswer now."

    let private oracleInstructions (snapshot: TranscriptSnapshot) (candidate: OracleCandidate) =
        let contextNote =
            if List.isEmpty candidate.context then
                "No PDF source matched this question."
            else
                candidate.context
                |> List.map (fun c -> c.source.DisplayName)
                |> List.distinct
                |> String.concat "; "
                |> sprintf "Relevant external source(s): %s"

        $"{instructions}\n\nThe user just said:\n{snapshot.text}\n\nBackend oracle guidance:\n{candidate.answer}\n\n{contextNote}\n\nSay the guided answer naturally and avoid exposing implementation details."

    let private publishTranscript st itemId text isFinal =
        let revision = st.revision + 1

        let snapshot =
            { turnId = itemId
              itemId = itemId
              revision = revision
              text = text
              isFinal = isFinal
              receivedAt = DateTimeOffset.UtcNow }

        st.bus.PostToAgent(Ag_TranscriptUpdated snapshot)
        { st with revision = revision }

    let private updateVoice conn (st: VoiceState) (ev: ServerEvent) =
        async {
            match ev with
            | ServerEvent.SessionCreated s when not st.initialized ->
                sendSessionUpdate conn s.session
                st.bus.PostToAgent(Ag_Log "Realtime session created.")
                return { st with initialized = true }
            | ServerEvent.SessionUpdated _ ->
                st.bus.PostToAgent(Ag_Log "Realtime session updated.")
                return st
            | ServerEvent.ConversationItemInputAudioTranscriptionDelta ev ->
                let previous =
                    st.transcriptByItem |> Map.tryFind ev.item_id |> Option.defaultValue ""

                let text = previous + ev.delta
                let st = publishTranscript st ev.item_id text false

                return
                    { st with
                        transcriptByItem = st.transcriptByItem |> Map.add ev.item_id text }
            | ServerEvent.ConversationItemInputAudioTranscriptionCompleted ev ->
                let text = FsKame.Text.normalizeWhitespace ev.transcript
                st.bus.PostToAgent(Ag_Log $"User: {text}")
                let st = publishTranscript st ev.item_id text true

                return
                    { st with
                        transcriptByItem = st.transcriptByItem |> Map.remove ev.item_id }
            | ServerEvent.Error e ->
                st.bus.PostToAgent(Ag_Log $"Realtime API error: {e.error.message}")
                return st
            | ServerEvent.EventHandlingError(t, msg, _) ->
                st.bus.PostToAgent(Ag_Log $"Realtime event handling error for {t}: {msg}")
                return st
            | _ -> return st
        }

    let private startVoice apiKey conn bus =
        async {
            let keyReq =
                { KeyReq.Default with
                    session = updateSession KeyReq.Default.session }

            let! ephemKey = Connection.getEphemeralKey apiKey keyReq |> Async.AwaitTask
            do! Connection.connect ephemKey conn |> Async.AwaitTask

            let initState =
                { initialized = false
                  transcriptByItem = Map.empty
                  revision = 0
                  respondedTurns = Set.empty
                  bus = bus }

            do!
                conn.WebRtcClient.OutputChannel.Reader.ReadAllAsync()
                |> AsyncSeq.map SerDe.toEvent
                |> AsyncSeq.scanAsync (updateVoice conn) initState
                |> AsyncSeq.iter (fun _ -> ())
        }

    let private updateResponder conn (st: VoiceState) (msg: AgentMsg) =
        async {
            match msg with
            | Ag_ResponseReady(snapshot, candidate) when not (st.respondedTurns.Contains snapshot.turnId) ->
                let responseInstructions =
                    match candidate with
                    | Some candidate -> oracleInstructions snapshot candidate
                    | None -> fallbackInstructions snapshot

                responseCreate conn responseInstructions
                st.bus.PostToAgent(Ag_Log "Realtime response requested.")

                return
                    { st with
                        respondedTurns = st.respondedTurns.Add snapshot.turnId }
            | _ -> return st
        }

    let private startResponder conn bus =
        let st =
            { initialized = true
              transcriptByItem = Map.empty
              revision = 0
              respondedTurns = Set.empty
              bus = bus }

        bus.AgentBus.RunAsync("voice-responder", st, updateResponder conn)

    let start apiKey conn bus =
        async {
            let! voice = Async.StartChild(startVoice apiKey conn bus)
            let! responder = Async.StartChild(startResponder conn bus)
            do! voice
            do! responder
        }
        |> FlowUtils.catch bus.PostToFlow
