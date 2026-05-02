namespace FsKame.WorkFlow

open System.Threading.Channels
open FSharp.Control
open FsKame
open RTFlow
open RTFlow.Functions

module AppAgent =
    type State =
        { mailbox: Channel<Msg>
          bus: WBus<FlowMsg, AgentMsg> }

        member this.Send msg =
            this.mailbox.Writer.TryWrite msg |> ignore

    let private sourceNames (chunks: SourceChunk list) =
        chunks
        |> List.map (fun c -> c.source.DisplayName)
        |> List.distinct
        |> String.concat "; "

    let private update (st: State) (msg: AgentMsg) =
        async {
            match msg with
            | Ag_Log msg -> st.Send(Log_Append msg)
            | Ag_TranscriptUpdated snapshot when snapshot.isFinal ->
                st.Send(Log_Append $"Transcript finalized: {snapshot.text}")
            | Ag_ContextReady(snapshot, chunks, inventory) ->
                let msg =
                    if List.isEmpty chunks then
                        let sourceCount = inventory |> List.filter _.enabled |> List.length
                        $"No selected PDF match for: {snapshot.text}; {sourceCount} PDF source(s) available."
                    else
                        $"Matched {chunks.Length} source chunk(s): {sourceNames chunks}"

                st.Send(Log_Append msg)
            | Ag_ResponseReady(_, Some candidate) ->
                st.Send(Log_Append $"Oracle: {FsKame.Text.truncate 500 candidate.answer}")
            | Ag_ResponseReady(_, None) -> st.Send(Log_Append "No oracle guidance; document-only fallback.")
            | Ag_FlowError err -> st.Send(Log_Append err.ErrorText)
            | Ag_FlowDone e -> st.Send(Log_Append $"Flow done abnormal={e.abnormal}")
            | _ -> ()

            return st
        }

    let start mailbox bus =
        let st0 = { mailbox = mailbox; bus = bus }

        bus.AgentBus.RunAsync("app", st0, update) |> FlowUtils.catch bus.PostToFlow
