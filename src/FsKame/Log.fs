namespace FsKame

open System
open FSharp.DI

type FsKameLog() = class end

module Log =
    let mutable debug_logging = false
    let log = DI.loggerLazy<FsKameLog> ()
