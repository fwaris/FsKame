namespace FsKame.WorkFlow

open System
open System.Text.Json.Serialization
open FSharp.Control
open RTOpenAI.Api
open RTOpenAI.Events
open RTFlow
open RTFlow.Functions

module VoiceAgent =
    type private Q = SendClientEvent of ClientEvent

    type private UserSpeechState =
        | Silent
        | Speaking

    type private ResponseCreatedState =
        | NoActiveResponse
        | ActiveResponse of {| id: string option |}

    type private PendingResponse =
        { turnId: string; instructions: string }

    type private VoiceState =
        { initialized: bool
          realtimeSession: Session
          transcriptByItem: Map<string, string>
          revision: int
          respondedTurns: Set<string>
          userSpeechState: UserSpeechState
          responseCreatedState: ResponseCreatedState
          assistantSpeechStarted: bool
          pendingResponses: PendingResponse list
          outputQueue: AsyncPriorityQueue<Q>
          bus: WBus<FlowMsg, AgentMsg> }

    let private CONTROL_EVENT_PRIORITY = 10
    let private SPEAK_PRIORITY = 20

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
        "You are an assistant for answering user questions over documents. Your conversation with the user is relayed to a backend oracle. The oracle generates responses to user questions from the pdf documents as soon as the user is done speaking. Your job is convey these answers back to the user. Only answer from the information provided by the oracle. If you don't have the answer you may ask the user to wait allow the oracle to catch up. Don't make things up. Stay grounded. Keep answers conversational and brief: usually one to three short spoken sentences."

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

    let private enqueueOutbound (outputQueue: AsyncPriorityQueue<Q>) priority work = outputQueue.Enqueue(work, priority)

    let private enqueueClientEvent state event =
        enqueueOutbound state.outputQueue CONTROL_EVENT_PRIORITY (SendClientEvent event)

    let private sendClientEvent conn event = Connection.sendClientEvent conn event

    let private sessionUpdateEvent (session: Session) =
        { SessionUpdate.Default with
            event_id = Utils.newId ()
            session = updateSession session }
        |> ClientEvent.SessionUpdate

    let private responseCreateEvent (responseInstructions: string) =
        let response =
            { Response.Default with
                instructions = Include(Some responseInstructions)
                output_modalities = Include(Some [ "audio" ]) }

        { ResponseCreate.Default with
            event_id = Utils.newId ()
            response = Include(Some response) }
        |> ClientEvent.ResponseCreate

    let private responseCancelEvent () =
        { ResponseCancel.Default with
            event_id = Utils.newId () }
        |> ClientEvent.ResponseCancel

    let private fallbackInstructions (snapshot: TranscriptSnapshot) =
        $"{instructions}\n\nThe user just said:\n{snapshot.text}\n\nNo backend document guidance is available. Say briefly that you cannot answer from the selected documents right now."

    let private oracleInstructions (snapshot: TranscriptSnapshot) (candidate: OracleCandidate) =
        let contextNote =
            if List.isEmpty candidate.context then
                "No selected PDF chunk matched this question. Do not add outside knowledge."
            else
                candidate.context
                |> List.map (fun c -> c.source.DisplayName)
                |> List.distinct
                |> String.concat "; "
                |> sprintf "Relevant selected PDF source(s): %s"

        $"{instructions}\n\nThe user just said:\n{snapshot.text}\n\nBackend oracle guidance:\n{candidate.answer}\n\n{contextNote}\n\nSay the guided answer naturally. Do not add facts that are not in the selected PDFs."

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

    let private isResponseActive =
        function
        | ActiveResponse _ -> true
        | NoActiveResponse -> false

    let private tryScheduleResponse (st: VoiceState) =
        match st.userSpeechState, st.responseCreatedState, st.pendingResponses with
        | Silent, NoActiveResponse, pending :: remaining ->
            responseCreateEvent pending.instructions
            |> SendClientEvent
            |> enqueueOutbound st.outputQueue SPEAK_PRIORITY

            st.bus.PostToAgent(Ag_Log $"Realtime response requested for turn {pending.turnId}.")

            { st with
                responseCreatedState = ActiveResponse {| id = None |}
                pendingResponses = remaining }
        | _ -> st

    let private queueResponse (st: VoiceState) (snapshot: TranscriptSnapshot) (candidate: OracleCandidate option) =
        if st.respondedTurns.Contains snapshot.turnId then
            st
        else
            let responseInstructions =
                match candidate with
                | Some candidate -> oracleInstructions snapshot candidate
                | None -> fallbackInstructions snapshot

            st.bus.PostToAgent(Ag_Log $"Realtime response queued for turn {snapshot.turnId}.")

            { st with
                respondedTurns = st.respondedTurns.Add snapshot.turnId
                pendingResponses =
                    st.pendingResponses
                    @ [ { turnId = snapshot.turnId
                          instructions = responseInstructions } ] }
            |> tryScheduleResponse

    let private updateVoice (st: VoiceState) (ev: ServerEvent) =
        async {
            match ev with
            | ServerEvent.SessionCreated s when not st.initialized ->
                sessionUpdateEvent s.session |> enqueueClientEvent st
                st.bus.PostToAgent(Ag_Log "Realtime session created.")

                return
                    { st with
                        initialized = true
                        realtimeSession = s.session }
            | ServerEvent.SessionUpdated ev ->
                st.bus.PostToAgent(Ag_Log "Realtime session updated.")
                return { st with realtimeSession = ev.session }
            | ServerEvent.ResponseCreated ev ->
                let responseId =
                    match ev.response.id with
                    | Include(Some value) when not (String.IsNullOrWhiteSpace value) -> Some value
                    | _ -> None

                return
                    { st with
                        responseCreatedState = ActiveResponse {| id = responseId |} }
            | ServerEvent.OutputAudioBufferStarted _ ->
                return
                    { st with
                        assistantSpeechStarted = true }
            | ServerEvent.ResponseDone _ ->
                return
                    { st with
                        responseCreatedState = NoActiveResponse
                        assistantSpeechStarted = false }
                    |> tryScheduleResponse
            | ServerEvent.ResponseOutputAudioDone _ ->
                return
                    { st with
                        assistantSpeechStarted = false }
            | ServerEvent.InputAudioBufferSpeechStarted _ ->
                if st.assistantSpeechStarted || isResponseActive st.responseCreatedState then
                    responseCancelEvent () |> enqueueClientEvent st
                    st.bus.PostToAgent(Ag_Log "User interrupted; cancelling response.")

                return { st with userSpeechState = Speaking }
            | ServerEvent.InputAudioBufferSpeechStopped _ ->
                return { st with userSpeechState = Silent } |> tryScheduleResponse
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

    let private update (st: VoiceState) (msg: AgentMsg) =
        async {
            match msg with
            | Ag_VoiceServerEvent ev -> return! updateVoice st ev
            | Ag_ResponseReady(snapshot, candidate) -> return queueResponse st snapshot candidate
            | Ag_FlowDone _ ->
                st.outputQueue.Complete()
                return st
            | _ -> return st
        }

    let private startRealtime apiKey conn (bus: WBus<FlowMsg, AgentMsg>) =
        async {
            let keyReq =
                { KeyReq.Default with
                    session = updateSession KeyReq.Default.session }

            let! ephemKey = Connection.getEphemeralKey apiKey keyReq |> Async.AwaitTask
            do! Connection.connect ephemKey conn |> Async.AwaitTask

            do!
                conn.WebRtcClient.OutputChannel.Reader.ReadAllAsync()
                |> AsyncSeq.map SerDe.toEvent
                |> AsyncSeq.iter (Ag_VoiceServerEvent >> bus.PostToAgent)
        }

    let private startOutputPump conn (bus: WBus<FlowMsg, AgentMsg>) (outputQueue: AsyncPriorityQueue<Q>) =
        outputQueue.ToAsyncSeq()
        |> AsyncSeq.iter (fun work ->
            match work with
            | SendClientEvent event ->
                try
                    sendClientEvent conn event
                with ex ->
                    bus.PostToAgent(Ag_Log $"Failed to send realtime client event: {ex.Message}"))

    let private startAgent outputQueue (bus: WBus<FlowMsg, AgentMsg>) =
        let st =
            { initialized = false
              realtimeSession = Session.Default
              transcriptByItem = Map.empty
              revision = 0
              respondedTurns = Set.empty
              userSpeechState = Silent
              responseCreatedState = NoActiveResponse
              assistantSpeechStarted = false
              pendingResponses = []
              outputQueue = outputQueue
              bus = bus }

        bus.AgentBus.RunAsync("voice", st, update)

    let start apiKey conn bus =
        let outputQueue = AsyncPriorityQueue<Q>()

        async {
            let! outputPump = Async.StartChild(startOutputPump conn bus outputQueue)
            let! voiceAgent = Async.StartChild(startAgent outputQueue bus)

            try
                do! startRealtime apiKey conn bus
            finally
                outputQueue.Complete()

            do! voiceAgent
            do! outputPump
        }
        |> FlowUtils.catch bus.PostToFlow
