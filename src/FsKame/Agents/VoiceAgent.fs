namespace FsKame.WorkFlow

open System
open System.Text.RegularExpressions
open System.Text.Json
open System.Text.Json.Serialization
open System.Threading
open System.Threading.Tasks
open FSharp.Control
open RTOpenAI.Api
open RTOpenAI.Events
open RTFlow
open RTFlow.Functions

module VoiceAgent =
    module private ToolNames =
        [<Literal>]
        let QUERY_ORACLE = "QUERY_ORACLE"

    type private Q =
        | SendClientEvent of ClientEvent
        | AwaitToolCall of VoiceToolCall

    type private UserSpeechState =
        | Silent
        | Speaking

    type private ResponseCreatedState =
        | NoActiveResponse
        | ActiveResponse of {| id: string option |}

    type private VoiceUseCaseConfig =
        { realtimeModel: string
          transcriberModel: string
          transcriberPrompt: string
          instructions: string
          speechResultInstructions: string
          memoryRequestTimeout: TimeSpan
          functionCallTimeout: TimeSpan }

    type private VoiceState =
        { initialized: bool
          config: VoiceUseCaseConfig
          realtimeSession: Session
          transcriptByItem: Map<string, string>
          revision: int
          pendingToolCalls: Map<string, VoiceToolCall>
          completedToolTurns: Set<string>
          pendingSpeakByCallId: Map<string, string>
          userSpeechState: UserSpeechState
          responseCreatedState: ResponseCreatedState
          assistantSpeechStarted: bool
          pendingSpeakTexts: string list
          outputQueue: AsyncPriorityQueue<Q>
          bus: WBus<FlowMsg, AgentMsg> }

    let private TOOL_CALL_PRIORITY = 0
    let private CONTROL_EVENT_PRIORITY = 10
    let private SPEAK_PRIORITY = 20

    let private useCaseConfig (useCase: FsKame.QA.UseCaseDefinition) =
        let useCase = FsKame.QA.UseCaseDefinition.sanitize useCase
        let realtime = FsKame.QA.UseCaseDefinition.model FsKame.QA.Realtime useCase
        let transcriber = FsKame.QA.UseCaseDefinition.model FsKame.QA.Transcriber useCase

        { realtimeModel = realtime.modelId
          transcriberModel = transcriber.modelId
          transcriberPrompt =
            useCase.prompts.transcriberPrompt
            |> Option.defaultValue FsKame.QA.DefaultUseCasePrompts.transcriberPrompt
          instructions =
            useCase.prompts.realtimeInstructions
            |> Option.defaultValue FsKame.QA.DefaultUseCasePrompts.realtimeInstructions
          speechResultInstructions =
            useCase.prompts.speechResultInstruction
            |> Option.defaultValue FsKame.QA.DefaultUseCasePrompts.speechResultInstruction
          memoryRequestTimeout = TimeSpan.FromMilliseconds(float useCase.runtime.realtimeMemoryTimeoutMs)
          functionCallTimeout = TimeSpan.FromMilliseconds(float useCase.runtime.functionCallTimeoutMs) }

    let private sessionAudio config =
        { Audio.Default with
            input =
                Include(
                    Some
                        { AudioInput.Default with
                            transcription =
                                { language = "en"
                                  model = config.transcriberModel
                                  prompt = Some config.transcriberPrompt }
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
        """You are FsKame's low-latency spoken front-end for question answering.

Allowed direct actions:
- greet the user
- handle short rapport, repetition, or simple clarification
- ask a brief follow-up when the request is too vague to answer safely
- answer simple conversational turns directly

Tool use:
- For every question (even simple ones about time, weather, etc.), request, summary, comparison, current-info question, or follow-up - which can't be answered trivially from existing context - call QUERY_ORACLE.
- Pass the user's request as the `question` argument.
- When you call QUERY_ORACLE, you may pass compact advisory hints only when they are clear from the user's words and recent conversation: turn_kind, topic_continuity, memory_action, needs_external_context, sensitive, and confidence. These are hints only; the backend validates them and decides memory/tool/oracle handling.
- Call QUERY_ORACLE silently. Do not say filler or status phrases before the call, such as "let me check", "one moment", "I'll look that up", or similar.
- Do not refuse a request just because it is not about documents or selected sources.
- Do not invent tool results, source details, page contents, citations, live values, or app state.

After QUERY_ORACLE returns:
- Treat the tool result as the authoritative answer.
- Speak the tool result naturally and briefly, without adding new facts or a status preface.
- If the tool says there is not enough information, say that plainly."""

    let private oracleTool =
        { Tool.Default with
            name = ToolNames.QUERY_ORACLE
            description =
                "Use this for every substantive user question. It can answer with the backend oracle using available tools, app context, selected sources, and general reasoning. The tool returns the exact wording to speak."
            parameters =
                { Parameters.Default with
                    properties =
                        [ "question",
                          JsProperty.String
                              { description =
                                  Some "The user's question or follow-up, rewritten as a concise standalone request."
                                enum = None }
                          "turn_kind",
                          JsProperty.String
                              { description =
                                  Some
                                      "Advisory turn kind from the utterance: acknowledgement, question, followup, topic_shift, correction, summary, comparison, or memory_request."
                                enum = None }
                          "topic_continuity",
                          JsProperty.String
                              { description =
                                  Some
                                      "Advisory topic continuity from recent conversation only: same_topic, topic_shift, older_topic, or unknown."
                                enum = None }
                          "memory_action",
                          JsProperty.String
                              { description =
                                  Some
                                      "Advisory explicit memory action requested by the user: none, remember, forget, correct, or unknown."
                                enum = None }
                          "needs_external_context",
                          JsProperty.Boolean
                              { description =
                                  Some
                                      "True when the user appears to ask for documents, tools, current facts, app state, memory, comparison, or summary beyond casual chat." }
                          "confidence",
                          JsProperty.String
                              { description = Some "Advisory confidence from 0.0 to 1.0."
                                enum = None }
                          "sensitive",
                          JsProperty.Boolean
                              { description =
                                  Some
                                      "True when the utterance includes secrets, credentials, private identifiers, or sensitive personal data." } ]
                        |> Map.ofList
                    required = [ "question" ] } }

    let private updateSession config (session: Session) =
        { session with
            id = Skip
            ``object`` = Skip
            model = Some config.realtimeModel
            audio = Include(Some(sessionAudio config))
            instructions = Some config.instructions
            tool_choice = Include(Some "auto")
            tools = Include(Some [ oracleTool ])
            expires_at = Skip }

    let private enqueueOutbound (outputQueue: AsyncPriorityQueue<Q>) priority work = outputQueue.Enqueue(work, priority)

    let private enqueueClientEvent state event =
        enqueueOutbound state.outputQueue CONTROL_EVENT_PRIORITY (SendClientEvent event)

    let private sendClientEvent conn event = Connection.sendClientEvent conn event

    let private sessionUpdateEvent config (session: Session) =
        { SessionUpdate.Default with
            event_id = Utils.newId ()
            session = updateSession config session }
        |> ClientEvent.SessionUpdate

    let private oracleAnswerInput answer =
        { ContentMessage.Default with
            role = "user"
            content =
                [ MessageContent.Input_text
                      {| text = $"Speak this oracle answer exactly, without adding facts:\n\n{answer}" |} ] }
        |> ConversationItem.Message

    let private responseCreateEvent (responseInstructions: string) (input: ConversationItem list option) =
        let response =
            { Response.Default with
                instructions = Include(Some responseInstructions)
                input =
                    match input with
                    | Some items -> Include(Some items)
                    | None -> Skip
                output_modalities = Include(Some [ "audio" ]) }

        { ResponseCreate.Default with
            event_id = Utils.newId ()
            response = Include(Some response) }
        |> ClientEvent.ResponseCreate

    let private responseInstructionsForOracleAnswer baseInstructions answer =
        $"{baseInstructions}\n\nOracle answer to speak exactly, without adding facts:\n\n{answer}"

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
        $"I cannot answer that from the available tools and context right now. The user asked: {snapshot.text}"

    let private speechFriendlyMarkdown (text: string) =
        let replace (pattern: string) (replacement: string) (value: string) =
            Regex.Replace(value, pattern, replacement)

        text
        |> replace @"(?m)^\s{0,3}#{1,6}\s*" ""
        |> replace @"(?m)^\s{0,3}>\s?" ""
        |> replace @"(?m)^\s*[-*+]\s+" ""
        |> replace @"(?m)^\s*\d+[.)]\s+" ""
        |> replace @"(?m)^\s*(-{3,}|\*{3,}|_{3,})\s*$" ""
        |> replace @"!\[([^\]]*)\]\([^)]+\)" "$1"
        |> replace @"\[([^\]]+)\]\([^)]+\)" "$1"
        |> replace @"`([^`]+)`" "$1"
        |> replace @"(\*\*|__)(.*?)\1" "$2"
        |> replace @"(\*|_)(.*?)\1" "$2"
        |> replace @"(?<!\S)#{1,6}(?!\S)" ""
        |> FsKame.Text.normalizeWhitespace

    let private oracleToolOutput (_snapshot: TranscriptSnapshot) (candidate: OracleCandidate) =
        candidate.answer |> speechFriendlyMarkdown

    let private makeTranscriptSnapshot st itemId text isFinal =
        let revision = st.revision + 1

        revision,
        { turnId = itemId
          itemId = itemId
          revision = revision
          text = text
          isFinal = isFinal
          receivedAt = DateTimeOffset.UtcNow }

    let private createMemoryRequest config snapshot realtimeJudgement cancellationToken =
        { requestId = Utils.newId ()
          snapshot = snapshot
          realtimeJudgement = realtimeJudgement
          deadline = DateTimeOffset.UtcNow + config.memoryRequestTimeout
          cancellationToken = cancellationToken
          completion = TaskCompletionSource<MemoryContext>(TaskCreationOptions.RunContinuationsAsynchronously) }

    let private isResponseActive =
        function
        | ActiveResponse _ -> true
        | NoActiveResponse -> false

    let private tryScheduleSpeak (st: VoiceState) =
        match st.userSpeechState, st.responseCreatedState, st.pendingSpeakTexts with
        | Silent, NoActiveResponse, text :: remaining ->
            responseCreateEvent
                (responseInstructionsForOracleAnswer st.config.speechResultInstructions text)
                (Some [ oracleAnswerInput text ])
            |> SendClientEvent
            |> enqueueOutbound st.outputQueue SPEAK_PRIORITY

            st.bus.PostToAgent(
                Ag_Log $"Realtime response requested from oracle tool output; output_chars={text.Length}."
            )

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

    let private tryJsonString (root: JsonElement) (name: string) =
        let mutable prop = Unchecked.defaultof<JsonElement>

        if root.TryGetProperty(name, &prop) then
            match prop.ValueKind with
            | JsonValueKind.String -> prop.GetString() |> Option.ofObj |> Option.bind FsKame.Text.notEmpty
            | JsonValueKind.Number -> Some(prop.GetRawText())
            | JsonValueKind.True -> Some "true"
            | JsonValueKind.False -> Some "false"
            | _ -> None
        else
            None

    let private tryJsonBool (root: JsonElement) name =
        tryJsonString root name
        |> Option.exists (fun value ->
            String.Equals(value, "true", StringComparison.OrdinalIgnoreCase)
            || String.Equals(value, "yes", StringComparison.OrdinalIgnoreCase)
            || value = "1")

    let private tryJsonBoolOption (root: JsonElement) name =
        tryJsonString root name
        |> Option.bind (fun value ->
            if
                String.Equals(value, "true", StringComparison.OrdinalIgnoreCase)
                || String.Equals(value, "yes", StringComparison.OrdinalIgnoreCase)
                || value = "1"
            then
                Some true
            elif
                String.Equals(value, "false", StringComparison.OrdinalIgnoreCase)
                || String.Equals(value, "no", StringComparison.OrdinalIgnoreCase)
                || value = "0"
            then
                Some false
            else
                None)

    let private tryJsonFloat (root: JsonElement) name =
        tryJsonString root name
        |> Option.bind (fun value ->
            match Double.TryParse value with
            | true, parsed -> Some parsed
            | false, _ -> None)

    let private stringIsAny values value =
        values
        |> List.exists (fun candidate -> String.Equals(candidate, value, StringComparison.OrdinalIgnoreCase))

    let private memoryActionFromLegacy (root: JsonElement) =
        match tryJsonString root "memory_need" with
        | Some value when stringIsAny [ "writeback"; "remember"; "typed_recall" ] value -> Some "remember"
        | Some value when stringIsAny [ "forget"; "delete" ] value -> Some "forget"
        | Some value when stringIsAny [ "correct"; "correction" ] value -> Some "correct"
        | _ -> None

    let private needsExternalContextFromLegacy (root: JsonElement) =
        match tryJsonString root "tool_need" with
        | Some value when stringIsAny [ "none"; "unknown" ] value -> Some false
        | Some _ -> Some true
        | None -> None

    let private isMemoryMutationAction =
        function
        | Some value when stringIsAny [ "remember"; "forget"; "correct" ] value -> true
        | _ -> false

    let private isCorrectionTurn turnKind memoryAction =
        match turnKind, memoryAction with
        | Some value, _ when stringIsAny [ "correction"; "correct" ] value -> true
        | _, Some value when stringIsAny [ "correct"; "forget" ] value -> true
        | _ -> false

    let private tryRealtimeJudgement (content: string) =
        try
            use doc = JsonDocument.Parse(content)

            let root =
                let mutable nested = Unchecked.defaultof<JsonElement>

                if doc.RootElement.TryGetProperty("realtime_judgement", &nested) then
                    nested
                else
                    doc.RootElement

            let hasAnyHint =
                [ "turn_kind"
                  "topic_continuity"
                  "memory_action"
                  "needs_external_context"
                  "confidence"
                  "sensitive"
                  // Backward-compatible parse for old realtime sessions.
                  "memory_need"
                  "tool_need"
                  "memory_mutation"
                  "conflict_likely" ]
                |> List.exists (fun name -> tryJsonString root name |> Option.isSome)

            if not hasAnyHint then
                None
            else
                let turnKind = tryJsonString root "turn_kind"

                let memoryAction =
                    tryJsonString root "memory_action"
                    |> Option.orElse (memoryActionFromLegacy root)

                let needsExternalContext =
                    tryJsonBoolOption root "needs_external_context"
                    |> Option.orElse (needsExternalContextFromLegacy root)

                let sensitive = tryJsonBool root "sensitive"

                let memoryMutation =
                    tryJsonBool root "memory_mutation" || isMemoryMutationAction memoryAction

                let conflictLikely =
                    tryJsonBool root "conflict_likely" || isCorrectionTurn turnKind memoryAction

                Some
                    { turnKind = turnKind
                      topicContinuity = tryJsonString root "topic_continuity"
                      memoryAction = memoryAction
                      needsExternalContext = needsExternalContext
                      confidence = tryJsonFloat root "confidence" |> Option.defaultValue 0.5
                      riskFlags =
                        { memoryMutation = memoryMutation
                          sensitive = sensitive
                          conflictLikely = conflictLikely } }
        with _ ->
            None

    let private dispatchToolCall (st: VoiceState) (fc: ContentFunctionCall) =
        let question = toolQuestion fc.arguments
        let realtimeJudgement = tryRealtimeJudgement fc.arguments

        let question =
            if String.IsNullOrWhiteSpace question then
                "The user asked a question, but the tool call did not include the question text."
            else
                question

        match fc.name with
        | ToolNames.QUERY_ORACLE ->
            let revision, snapshot = makeTranscriptSnapshot st fc.call_id question true
            let cancellation = new CancellationTokenSource()
            cancellation.CancelAfter st.config.functionCallTimeout

            let call =
                { name = fc.name
                  callId = fc.call_id
                  content = fc.arguments
                  snapshot = snapshot
                  cancellation = cancellation
                  timeout = st.config.functionCallTimeout
                  task =
                    TaskCompletionSource<ContentFunctionCallOutput>(TaskCreationOptions.RunContinuationsAsynchronously) }

            let memoryRequest =
                createMemoryRequest st.config snapshot realtimeJudgement cancellation.Token

            enqueueOutbound st.outputQueue TOOL_CALL_PRIORITY (AwaitToolCall call)

            st.bus.PostToAgent(
                Ag_Log
                    $"Oracle tool call started: question_chars={question.Length} args_chars={fc.arguments.Length} question='{FsKame.Text.truncate 120 question}'"
            )

            st.bus.PostToAgent(Ag_TranscriptUpdated snapshot)
            st.bus.PostToAgent(Ag_MemoryRequested memoryRequest)

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

                st.bus.PostToAgent(Ag_Log $"Oracle tool call completed for turn {snapshot.turnId}.")

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
            st.bus.PostToAgent(
                Ag_Log
                    $"Oracle tool output ready for call {callId}; output_chars={output.Length}; waiting for realtime item acknowledgement."
            )

            { st with
                pendingSpeakByCallId = st.pendingSpeakByCallId |> Map.add callId output }

    let private acknowledgeFunctionOutput (st: VoiceState) (eventName: string) (item: ConversationItem) =
        match item with
        | Function_call_output output ->
            match st.pendingSpeakByCallId |> Map.tryFind output.call_id with
            | None -> st
            | Some text ->
                st.bus.PostToAgent(
                    Ag_Log
                        $"Realtime acknowledged oracle tool output for call {output.call_id} via {eventName}; scheduling speech."
                )

                { st with
                    pendingSpeakByCallId = st.pendingSpeakByCallId |> Map.remove output.call_id
                    pendingSpeakTexts = st.pendingSpeakTexts @ [ text ] }
                |> tryScheduleSpeak
        | _ -> st

    let private runAwaitedToolCall (bus: WBus<FlowMsg, AgentMsg>) conn (toolCall: VoiceToolCall) =
        async {
            try
                let! result =
                    toolCall.task.Task.WaitAsync(toolCall.timeout, toolCall.cancellation.Token)
                    |> Async.AwaitTask

                createFunctionOutputEvent result |> sendClientEvent conn
                bus.PostToAgent(Ag_ToolCallOutputReady(toolCall.callId, result.output))
            with ex ->
                toolCall.cancellation.Cancel()
                let output = "The oracle took too long to answer. Please try again."
                let result = ContentFunctionCallOutput.Create toolCall.callId output
                createFunctionOutputEvent result |> sendClientEvent conn
                bus.PostToAgent(Ag_Log $"Oracle tool call timed out for {toolCall.callId}: {ex.Message}")
                bus.PostToAgent(Ag_ToolCallOutputReady(toolCall.callId, output))
        }

    let private updateVoice (st: VoiceState) (ev: ServerEvent) =
        async {
            match ev with
            | ServerEvent.SessionCreated s when not st.initialized ->
                sessionUpdateEvent st.config s.session |> enqueueClientEvent st
                st.bus.PostToAgent(Ag_Log "Realtime session created.")

                return
                    { st with
                        initialized = true
                        realtimeSession = s.session }
            | ServerEvent.SessionUpdated ev ->
                st.bus.PostToAgent(Ag_Log "Realtime session updated.")
                return { st with realtimeSession = ev.session }
            | ServerEvent.ConversationItemAdded ev ->
                return acknowledgeFunctionOutput st "conversation.item.added" ev.item
            | ServerEvent.ConversationItemDone ev ->
                return acknowledgeFunctionOutput st "conversation.item.done" ev.item
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

    let private startRealtime config apiKey conn (bus: WBus<FlowMsg, AgentMsg>) =
        async {
            let keyReq =
                { KeyReq.Default with
                    session = updateSession config KeyReq.Default.session }

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
                | AwaitToolCall toolCall -> runAwaitedToolCall bus conn toolCall |> Async.Start
            })

    let private startAgent config outputQueue (bus: WBus<FlowMsg, AgentMsg>) =
        let st =
            { initialized = false
              config = config
              realtimeSession = Session.Default
              transcriptByItem = Map.empty
              revision = 0
              pendingToolCalls = Map.empty
              completedToolTurns = Set.empty
              pendingSpeakByCallId = Map.empty
              userSpeechState = Silent
              responseCreatedState = NoActiveResponse
              assistantSpeechStarted = false
              pendingSpeakTexts = []
              outputQueue = outputQueue
              bus = bus }

        bus.AgentBus.RunAsync("voice", st, update)

    let start apiKey useCase conn bus =
        let outputQueue = AsyncPriorityQueue<Q>()
        let config = useCaseConfig useCase

        async {
            let! outputPump = Async.StartChild(startOutputPump conn bus outputQueue)
            let! voiceAgent = Async.StartChild(startAgent config outputQueue bus)

            try
                do! startRealtime config apiKey conn bus
            finally
                outputQueue.Complete()

            do! voiceAgent
            do! outputPump
        }
        |> FlowUtils.catch bus.PostToFlow
