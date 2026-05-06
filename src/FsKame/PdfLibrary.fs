namespace FsKame

open System
open System.IO
open FsKame.WorkFlow
open Microsoft.Maui.Storage

module PdfLibrary =
    let private folder () =
        let path = Path.Combine(FileSystem.AppDataDirectory, "FsKame", "Documents")
        Directory.CreateDirectory(path) |> ignore
        path

    let private sanitizeFileName (name: string) =
        let invalid = Path.GetInvalidFileNameChars() |> Set.ofArray

        name.ToCharArray()
        |> Array.map (fun c -> if invalid.Contains c then '_' else c)
        |> String

    let private kindFromFileName (fileName: string) =
        match Path.GetExtension(fileName).TrimStart('.').ToLowerInvariant() with
        | "pdf" -> PdfFile
        | "md"
        | "markdown"
        | "mdown"
        | "mkdn" -> MarkdownFile
        | _ -> MarkdownFile

    let private sourceKind kind =
        match kind with
        | PdfFile -> Pdf
        | MarkdownFile -> Markdown

    let private readPassages (doc: PdfDocumentSource) =
        let source =
            FsColbert.PassageSource.create
                doc.storedPath
                $"{PdfDocuments.kindLabel doc}: {doc.displayName}"
                doc.storedPath

        match doc.kind with
        | PdfFile -> FsColbert.PdfDocuments.readPassages FsColbert.ChunkOptions.fsKameDefaults source doc.storedPath
        | MarkdownFile ->
            FsColbert.MarkdownDocuments.readPassages FsColbert.ChunkOptions.fsKameDefaults source doc.storedPath

    let private hasExisting (docs: PdfDocumentSource list) originalPath fileName =
        docs
        |> List.exists (fun doc ->
            String.Equals(doc.originalPath, originalPath, StringComparison.OrdinalIgnoreCase)
            || String.Equals(doc.displayName, fileName, StringComparison.OrdinalIgnoreCase))

    let private copyPickedFile (result: FileResult) =
        async {
            let id = Guid.NewGuid().ToString("N")
            let fileName = sanitizeFileName result.FileName
            let storedName = $"{id}-{fileName}"
            let storedPath = Path.Combine(folder (), storedName)

            use! source = result.OpenReadAsync() |> Async.AwaitTask
            use target = File.Create(storedPath)
            do! source.CopyToAsync(target) |> Async.AwaitTask

            return
                { id = id
                  kind = kindFromFileName fileName
                  displayName = fileName
                  storedPath = storedPath
                  originalPath = defaultArg (Text.notEmpty result.FullPath) result.FileName
                  selected = false
                  status = Processing
                  chunkCount = 0
                  error = None }
        }

    let copyNewDocuments (existing: PdfDocumentSource list) (results: FileResult seq) =
        async {
            let candidates =
                results
                |> Seq.filter (fun result ->
                    let originalPath = defaultArg (Text.notEmpty result.FullPath) result.FileName
                    not (hasExisting existing originalPath result.FileName))
                |> Seq.toList

            let! copied = candidates |> List.map copyPickedFile |> Async.Parallel

            return copied |> Array.toList
        }

    let processDocument report (doc: PdfDocumentSource) =
        async {
            let! result = readPassages doc

            match result with
            | Ok passages ->
                let source =
                    { kind = sourceKind doc.kind
                      location = doc.storedPath
                      enabled = true }

                do! KnowledgeSources.InindexSource report source

                return
                    { id = doc.id
                      chunkCount = passages.Length
                      error = None }
            | Error err ->
                return
                    { id = doc.id
                      chunkCount = 0
                      error = Some err }
        }

    let processDocuments report docs =
        async {
            let mutable results = []

            for doc in docs do
                let! result = processDocument report doc
                results <- result :: results

            return List.rev results
        }

    let deleteStoredDocument (doc: PdfDocumentSource) =
        async {
            if String.IsNullOrWhiteSpace doc.storedPath || not (File.Exists doc.storedPath) then
                return false
            else
                File.Delete doc.storedPath
                return true
        }
