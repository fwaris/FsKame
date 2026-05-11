namespace FsKame.Tools

open System
open System.Collections.Generic
open System.Threading
open System.Threading.Tasks
open FsKame.QA

type CurrentTimeQaTool() =
    interface IQaTool with
        member _.PluginName = "FsKameTools"
        member _.Name = "current_time"
        member _.Description = "Returns the current local date and time for the running host."
        member _.Parameters = []

        member _.InvokeAsync(_: IReadOnlyDictionary<string, string>, _: CancellationToken) =
            DateTimeOffset.Now.ToString("O") |> QaToolResult.text |> Task.FromResult

type CurrentTimeToolProvider() =
    interface IQaToolProvider with
        member _.ContractVersion = 1
        member _.GetTools(_host: IQaToolHost) = [ CurrentTimeQaTool() :> IQaTool ]
