namespace FsKame.WorkFlow

open System
open System.Text.Json
open System.Text.Json.Serialization
open System.Threading.Tasks
open FSharp.Control
open RTOpenAI.Api
open RTOpenAI.Events
open RTFlow
open RTFlow.Functions

module VoiceAgent =
    module private ToolNames =
        [<Literal>]
        let QUERY_DOCUMENT_ORACLE = "QUERY_DOCUMENT_ORACLE"

    type private Q =
        | SendClientEvent of ClientEvent
        | AwaitToolCall of VoiceToolCall

    type private UserSpeechState =
        | Silent
        | Speaking

    type private ResponseCreatedState =
        | NoActiveResponse
        | ActiveResponse of {| id: string option |}

    type private VoiceState =
        { initialized: bool
          realtimeSession: Session
          transcriptByItem: Map<string, string>
          revision: int
          pendingToolCalls: Map<string, VoiceToolCall>
          completedToolTurns: Set<string>
          userSpeechState: UserSpeechState
          responseCreatedState: ResponseCreatedState
          assistantSpeechStarted: bool
          pendingSpeakTexts: string list
          outputQueue: AsyncPriorityQueue<Q>
          bus: WBus<FlowMsg, AgentMsg> }

    let private TOOL_CALL_PRIORITY = 0
    let private CONTROL_EVENT_PRIORITY = 10
    let private SPEAK_PRIORITY = 20
    let private FUNCTION_CALL_TIMEOUT = TimeSpan.FromSeconds 45.

    let sessionAudio =
        { Audio.Default with
            input =
                Include(
                    Some
                        { AudioInput.Default with
                            transcription =
                                { language = "en"
                                  model = "gpt-4o-mini-transcribe"
                                  prompt =
                                    Some
                                        "Expect natural question-answering conversation over selected PDF and Markdown documents." }
                                |> Some
                                |> Include
                            turn_detection =
                                VAD.Server_Vad
                                    {| create_response = true
                                       idle_timeout_ms = Skip
                                       interrupt_response = true
                                       prefix_padding_ms = 300
                                       silence_duration_ms = 200
                                       threshold = 0.5 |}
                                |> Some
                                |> Include }
                ) }

    let private instructions =
        """You are FsKame's low-latency spoken front-end for document question answering.

Allowed direct actions:
- greet the user
- handle short rapport, repetition, or simple clarification
- ask a brief follow-up when the request is too vague to search safely
- say a short preamble before a tool call, such as "Let me check the documents."

Tool use:
- For every substantive question, summary, comparison, or follow-up that needs information from selected PDFs or Markdown documents, call QUERY_DOCUMENT_ORACLE.
- Pass the user's request as the `question` argument.
- Do not answer document questions from your own knowledge.
- Do not invent source details, page contents, citations, or facts.

After QUERY_DOCUMENT_ORACLE returns:
- Treat the tool result as the authoritative answer.
- Speak the tool result naturally and briefly, without adding new facts.
- If the tool says the documents do not contain the answer, say that plainly."""

    let private documentOracleTool =
        { Tool.Default with
            name = ToolNames.QUERY_DOCUMENT_ORACLE
            description =
                "Use this to answer any user question that requires selected PDF or Markdown document content. The tool returns the exact grounded wording to speak."
            parameters =
                { Parameters.Default with
                    properties =
                        Map.ofList
                            [ "question",
                              JsProperty.String
                                  { description =
                                      Some
                                          "The user's document question or follow-up, rewritten as a concise standalone request."
                                    enum = None } ]
                    required = [ "question" ] } }

    let private updateSession (session: Session) =
        { session with
            id = Skip
            ``object`` = Skip
            model = Some FsKame.C.DEFAULT_REALTIME_MODEL
            audio = Include(Some sessionAudio)
            instructions = Some instructions
            tool_choice = Include(Some "auto")
            tools = Include(Some [ documentOracleTool ])
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

    let private createFunctionOutputEvent (result: ContentFunctionCallOutput) =
        { ConversationItemCreate.Default with
            event_id = Utils.newId ()
            item = ConversationItem.Function_call_output result }
        |> ClientEvent.ConversationItemCreate

    let private fallbackToolOutput (snapshot: TranscriptSnapshot) =
        $"I cannot answer that from the selected documents right now. The user asked: {snapshot.text}"

    let private oracleToolOutput (_snapshot: TranscriptSnapshot) (candidate: OracleCandidate) =
        candidate.answer |> FsKame.Text.normalizeWhitespace

    let private makeTranscriptSnapshot st itemId text isFinal =
        let revision = st.revision + 1

        revision,
        { turnId = itemId
          itemId = itemId
          revision = revision
          text = text
          isFinal = isFinal
          receivedAt = DateTimeOffset.UtcNow }

    let private isResponseActive =
        function
        | ActiveResponse _ -> true
        | NoActiveResponse -> false

    let private speakToolResultInstructions text =
        $"The document oracle tool returned this grounded answer. Speak it to the user naturally and briefly. Do not add facts or reinterpret it.\n\n{text}"

    let private tryScheduleSpeak (st: VoiceState) =
        match st.userSpeechState, st.responseCreatedState, st.pendingSpeakTexts with
        | Silent, NoActiveResponse, text :: remaining ->
            responseCreateEvent (speakToolResultInstructions text)
            |> SendClientEvent
            |> enqueueOutbound st.outputQueue SPEAK_PRIORITY

            st.bus.PostToAgent(Ag_Log "Realtime response requested from document oracle tool output.")

            { st with
                responseCreatedState = ActiveResponse {| id = None |}
                pendingSpeakTexts = remaining }
        | _ -> st

    let private toolQuestion (content: string) =
        let content =
            content
            |> Option.ofObj
            |> Option.map (fun value -> value.Trim())
            |> Option.defaultValue ""

        if String.IsNullOrWhiteSpace content then
            ""
        else
            try
                use doc = JsonDocument.Parse(content)
                let mutable prop = Unchecked.defaultof<JsonElement>

                if
                    doc.RootElement.TryGetProperty("question", &prop)
                    && prop.ValueKind = JsonValueKind.String
                then
                    prop.GetString()
                    |> Option.ofObj
                    |> Option.defaultValue ""
                    |> FsKame.Text.normalizeWhitespace
                else
                    content
            with _ ->
                content

    let private dispatchToolCall (st: VoiceState) (fc: ContentFunctionCall) =
        let question = toolQuestion fc.arguments

        let question =
            if String.IsNullOrWhiteSpace question then
                "The user asked a document question, but the tool call did not include the question text."
            else
                question

        match fc.name with
        | ToolNames.QUERY_DOCUMENT_ORACLE ->
            let revision, snapshot = makeTranscriptSnapshot st fc.call_id question true

            let call =
                { name = fc.name
                  callId = fc.call_id
                  content = fc.arguments
                  snapshot = snapshot
                  task =
                    TaskCompletionSource<ContentFunctionCallOutput>(TaskCreationOptions.RunContinuationsAsynchronously) }

            enqueueOutbound st.outputQueue TOOL_CALL_PRIORITY (AwaitToolCall call)
            st.bus.PostToAgent(Ag_Log $"Document oracle tool call started: {question}")
            st.bus.PostToAgent(Ag_TranscriptUpdated snapshot)

            { st with
                revision = revision
                pendingToolCalls = st.pendingToolCalls |> Map.add snapshot.turnId call }
        | _ ->
            let reply = $"The tool '{fc.name}' is not available in this FsKame session."
            let result = ContentFunctionCallOutput.Create fc.call_id reply

            createFunctionOutputEvent result
            |> SendClientEvent
            |> enqueueOutbound st.outputQueue TOOL_CALL_PRIORITY

            { st with
                pendingSpeakTexts = st.pendingSpeakTexts @ [ reply ] }
            |> tryScheduleSpeak

    let private completeToolCall (st: VoiceState) (snapshot: TranscriptSnapshot) (candidate: OracleCandidate option) =
        if st.completedToolTurns.Contains snapshot.turnId then
            st
        else
            match st.pendingToolCalls |> Map.tryFind snapshot.turnId with
            | None ->
                st.bus.PostToAgent(
                    Ag_Log $"Ignoring oracle response for turn without pending tool call: {snapshot.turnId}."
                )

                st
            | Some call ->
                let output =
                    match candidate with
                    | Some candidate -> oracleToolOutput snapshot candidate
                    | None -> fallbackToolOutput snapshot

                ContentFunctionCallOutput.Create call.callId output
                |> call.task.TrySetResult
                |> ignore

                st.bus.PostToAgent(Ag_Log $"Document oracle tool call completed for turn {snapshot.turnId}.")

                { st with
                    pendingToolCalls = st.pendingToolCalls |> Map.remove snapshot.turnId
                    completedToolTurns = st.completedToolTurns.Add snapshot.turnId }

    let private removePendingToolCall callId (st: VoiceState) =
        match
            st.pendingToolCalls
            |> Map.tryPick (fun turnId call -> if call.callId = callId then Some turnId else None)
        with
        | Some turnId ->
            { st with
                pendingToolCalls = st.pendingToolCalls |> Map.remove turnId
                completedToolTurns = st.completedToolTurns.Add turnId }
        | None -> st

    let private handleToolOutputReady (st: VoiceState) callId output =
        let st = removePendingToolCall callId st

        if String.IsNullOrWhiteSpace output then
            st
        else
            st.bus.PostToAgent(Ag_Log $"Document oracle tool output ready for call {callId}.")

            { st with
                pendingSpeakTexts = st.pendingSpeakTexts @ [ output ] }
            |> tryScheduleSpeak

    let private runAwaitedToolCall (bus: WBus<FlowMsg, AgentMsg>) conn (toolCall: VoiceToolCall) =
        async {
            try
                let! result = toolCall.task.Task.WaitAsync(FUNCTION_CALL_TIMEOUT) |> Async.AwaitTask
                createFunctionOutputEvent result |> sendClientEvent conn
                bus.PostToAgent(Ag_ToolCallOutputReady(toolCall.callId, result.output))
            with ex ->
                let output = "The document oracle took too long to answer. Please try again."
                let result = ContentFunctionCallOutput.Create toolCall.callId output
                createFunctionOutputEvent result |> sendClientEvent conn
                bus.PostToAgent(Ag_Log $"Document oracle tool call timed out for {toolCall.callId}: {ex.Message}")
                bus.PostToAgent(Ag_ToolCallOutputReady(toolCall.callId, output))
        }

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
            | ServerEvent.ResponseOutputItemDone responseItem ->
                match responseItem.item with
                | Function_call fc -> return dispatchToolCall st fc
                | _ -> return st
            | ServerEvent.ResponseDone _ ->
                return
                    { st with
                        responseCreatedState = NoActiveResponse
                        assistantSpeechStarted = false }
                    |> tryScheduleSpeak
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
                return { st with userSpeechState = Silent } |> tryScheduleSpeak
            | ServerEvent.ConversationItemInputAudioTranscriptionDelta ev ->
                let previous =
                    st.transcriptByItem |> Map.tryFind ev.item_id |> Option.defaultValue ""

                let text = previous + ev.delta
                let revision, _ = makeTranscriptSnapshot st ev.item_id text false

                return
                    { st with
                        revision = revision
                        transcriptByItem = st.transcriptByItem |> Map.add ev.item_id text }
            | ServerEvent.ConversationItemInputAudioTranscriptionCompleted ev ->
                let text = FsKame.Text.normalizeWhitespace ev.transcript
                st.bus.PostToAgent(Ag_Log $"User: {text}")
                let revision, _ = makeTranscriptSnapshot st ev.item_id text true

                return
                    { st with
                        revision = revision
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
            | Ag_ResponseReady(snapshot, candidate) -> return completeToolCall st snapshot candidate
            | Ag_ToolCallOutputReady(callId, output) -> return handleToolOutputReady st callId output
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
        |> AsyncSeq.iterAsync (fun work ->
            async {
                match work with
                | SendClientEvent event ->
                    try
                        sendClientEvent conn event
                    with ex ->
                        bus.PostToAgent(Ag_Log $"Failed to send realtime client event: {ex.Message}")
                | AwaitToolCall toolCall -> do! runAwaitedToolCall bus conn toolCall
            })

    let private startAgent outputQueue (bus: WBus<FlowMsg, AgentMsg>) =
        let st =
            { initialized = false
              realtimeSession = Session.Default
              transcriptByItem = Map.empty
              revision = 0
              pendingToolCalls = Map.empty
              completedToolTurns = Set.empty
              userSpeechState = Silent
              responseCreatedState = NoActiveResponse
              assistantSpeechStarted = false
              pendingSpeakTexts = []
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
