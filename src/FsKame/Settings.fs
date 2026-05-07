namespace FsKame

open System
open System.IO
open System.Text.Json
open Microsoft.Maui.Storage

module Settings =
    [<CLIMutable>]
    type PdfDocumentDto =
        { id: string
          kind: string
          displayName: string
          storedPath: string
          originalPath: string
          selected: bool
          status: string
          chunkCount: int
          error: string }

    let private fsKameFolder () =
        let path = Path.Combine(FileSystem.AppDataDirectory, "FsKame")
        Directory.CreateDirectory(path) |> ignore
        path

    let private pdfLibraryPath () =
        Path.Combine(fsKameFolder (), "pdf-library.json")

    let pdfLibraryStoragePath () = pdfLibraryPath ()

    let private documentsFolder () =
        Path.Combine(fsKameFolder (), "Documents")

    let private kindToString kind =
        match kind with
        | PdfFile -> "pdf"
        | MarkdownFile -> "markdown"

    let private kindFromString value =
        match (defaultArg (Option.ofObj value) "").Trim().ToLowerInvariant() with
        | "markdown"
        | "md" -> MarkdownFile
        | _ -> PdfFile

    let private kindFromFileName (fileName: string) =
        match Path.GetExtension(fileName).TrimStart('.').ToLowerInvariant() with
        | "pdf" -> PdfFile
        | _ -> MarkdownFile

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

    let private isValidDto (dto: PdfDocumentDto) =
        Text.notEmpty dto.id |> Option.isSome
        && Text.notEmpty dto.displayName |> Option.isSome
        && Text.notEmpty dto.storedPath |> Option.isSome

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

    let private deserializePdfLibrary json =
        if String.IsNullOrWhiteSpace json then
            None
        else
            try
                JsonSerializer.Deserialize<PdfDocumentDto array>(json)
                |> Option.ofObj
                |> Option.bind (fun dtos ->
                    if dtos |> Array.forall isValidDto then
                        Some(dtos |> Array.toList |> List.map ofDto)
                    else
                        None)
            with _ ->
                None

    let private tryReadPdfLibraryFile () =
        try
            let path = pdfLibraryPath ()

            if File.Exists path then
                File.ReadAllText path |> deserializePdfLibrary
            else
                None
        with _ ->
            None

    let private tryWritePdfLibraryFile (json: string) =
        try
            let path = pdfLibraryPath ()
            let tempPath = $"{path}.{Guid.NewGuid():N}.tmp"

            File.WriteAllText(tempPath, json)

            if File.Exists path then
                File.Replace(tempPath, path, null)
            else
                File.Move(tempPath, path)

            let savedJson = File.ReadAllText path

            if savedJson = json then
                Ok $"{path} ({json.Length} byte(s))"
            else
                Error
                    $"Read-back verification failed for {path}: expected {json.Length} byte(s), found {savedJson.Length} byte(s)."
        with ex ->
            try
                for tempPath in Directory.EnumerateFiles(fsKameFolder (), "pdf-library.json.*.tmp") do
                    File.Delete tempPath
            with _ ->
                ()

            Error ex.Message

    let private recoverStoredDocuments () =
        try
            let folder = documentsFolder ()

            if Directory.Exists folder then
                Directory.EnumerateFiles folder
                |> Seq.map (fun path ->
                    let storedName = Path.GetFileName path

                    let id, displayName =
                        if storedName.Length > 33 && storedName.[32] = '-' then
                            storedName.Substring(0, 32), storedName.Substring(33)
                        else
                            storedName, storedName

                    ({ id = id
                       kind = kindFromFileName displayName
                       displayName = displayName
                       storedPath = path
                       originalPath = path
                       selected = true
                       status = Ready
                       chunkCount = 0
                       error = None }
                    : PdfDocumentSource))
                |> Seq.toList
            else
                []
        with _ ->
            []

    let pdfLibrary () : PdfDocumentSource list =
        match tryReadPdfLibraryFile () with
        | Some docs -> docs
        | None ->
            let json = Preferences.Default.Get(C.SETTINGS_PDF_LIBRARY, "")

            match deserializePdfLibrary json with
            | Some docs ->
                tryWritePdfLibraryFile json |> ignore
                docs
            | None ->
                let docs = recoverStoredDocuments ()

                if not (List.isEmpty docs) then
                    let json = docs |> List.map toDto |> List.toArray |> JsonSerializer.Serialize
                    tryWritePdfLibraryFile json |> ignore

                docs

    let setPdfLibrary (docs: PdfDocumentSource list) : Result<string, string> =
        let json = docs |> List.map toDto |> List.toArray |> JsonSerializer.Serialize

        Preferences.Default.Set(C.SETTINGS_PDF_LIBRARY, json)
        tryWritePdfLibraryFile json

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
