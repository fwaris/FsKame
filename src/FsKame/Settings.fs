namespace FsKame

open System
open System.Text.Json
open Microsoft.Maui.Storage

module Settings =
    [<CLIMutable>]
    type private PdfDocumentDto =
        { id: string
          kind: string
          displayName: string
          storedPath: string
          originalPath: string
          selected: bool
          status: string
          chunkCount: int
          error: string }

    let private kindToString kind =
        match kind with
        | PdfFile -> "pdf"
        | MarkdownFile -> "markdown"

    let private kindFromString value =
        match (defaultArg (Option.ofObj value) "").Trim().ToLowerInvariant() with
        | "markdown"
        | "md" -> MarkdownFile
        | _ -> PdfFile

    let private statusToString (status: PdfProcessingStatus) =
        match status with
        | Queued -> "queued"
        | Processing -> "processing"
        | Ready -> "ready"
        | Failed -> "failed"

    let private statusFromString value : PdfProcessingStatus =
        match (defaultArg (Option.ofObj value) "").Trim().ToLowerInvariant() with
        | "processing" -> Processing
        | "ready" -> Ready
        | "failed" -> Failed
        | _ -> Queued

    let private toDto (doc: PdfDocumentSource) : PdfDocumentDto =
        { id = doc.id
          kind = kindToString doc.kind
          displayName = doc.displayName
          storedPath = doc.storedPath
          originalPath = doc.originalPath
          selected = doc.selected
          status = statusToString doc.status
          chunkCount = doc.chunkCount
          error = defaultArg doc.error "" }

    let private ofDto (dto: PdfDocumentDto) : PdfDocumentSource =
        let status = statusFromString dto.status

        let status, selected, error =
            match status with
            | Processing
            | Queued -> Failed, false, Some "Processing was interrupted. Tap retry."
            | Ready -> Ready, dto.selected, Text.notEmpty dto.error
            | Failed -> Failed, false, Text.notEmpty dto.error

        { id = dto.id
          kind = kindFromString dto.kind
          displayName = dto.displayName
          storedPath = dto.storedPath
          originalPath = dto.originalPath
          selected = selected
          status = status
          chunkCount = dto.chunkCount
          error = error }

    let openAiKey () =
        Preferences.Default.Get(C.SETTINGS_OPENAI_KEY, "").Trim()

    let setOpenAiKey (value: string) =
        Preferences.Default.Set(C.SETTINGS_OPENAI_KEY, value.Trim())

    let pdfLibrary () : PdfDocumentSource list =
        let json = Preferences.Default.Get(C.SETTINGS_PDF_LIBRARY, "")

        if String.IsNullOrWhiteSpace json then
            []
        else
            try
                JsonSerializer.Deserialize<PdfDocumentDto array>(json)
                |> Option.ofObj
                |> Option.map (Array.toList >> List.map ofDto)
                |> Option.defaultValue []
            with _ ->
                []

    let setPdfLibrary (docs: PdfDocumentSource list) =
        let json = docs |> List.map toDto |> List.toArray |> JsonSerializer.Serialize

        Preferences.Default.Set(C.SETTINGS_PDF_LIBRARY, json)

    let oracleModel () =
        Preferences.Default.Get(C.SETTINGS_ORACLE_MODEL, C.DEFAULT_ORACLE_MODEL).Trim()

    let setOracleModel (value: string) =
        let value =
            match Text.notEmpty value with
            | Some v -> v
            | None -> C.DEFAULT_ORACLE_MODEL

        Preferences.Default.Set(C.SETTINGS_ORACLE_MODEL, value)

    let retrievalMode () =
        Preferences.Default.Get(C.SETTINGS_RETRIEVAL_MODE, RetrievalModes.toStorageValue FsColbertWithFallback)
        |> RetrievalModes.ofStorageValue

    let setRetrievalMode mode =
        Preferences.Default.Set(C.SETTINGS_RETRIEVAL_MODE, RetrievalModes.toStorageValue mode)

    let logExpansions () =
        Preferences.Default.Get(C.SETTINGS_LOG_EXPANSIONS, false)

    let setLogExpansions value =
        Preferences.Default.Set(C.SETTINGS_LOG_EXPANSIONS, value)

    let logChunks () =
        Preferences.Default.Get(C.SETTINGS_LOG_CHUNKS, false)

    let setLogChunks value =
        Preferences.Default.Set(C.SETTINGS_LOG_CHUNKS, value)

    let useLexicalFilter () =
        Preferences.Default.Get(C.SETTINGS_USE_LEXICAL_FILTER, true)

    let setUseLexicalFilter value =
        Preferences.Default.Set(C.SETTINGS_USE_LEXICAL_FILTER, value)
