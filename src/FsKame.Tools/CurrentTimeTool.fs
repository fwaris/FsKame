namespace FsKame.Tools

open System
open System.ComponentModel
open Microsoft.SemanticKernel

type CurrentTimeTool() =
    [<KernelFunction("current_time")>]
    [<Description("Returns the current local date and time for the running FsKame app.")>]
    member _.GetCurrentTime() =
        DateTimeOffset.Now.ToString("O")
