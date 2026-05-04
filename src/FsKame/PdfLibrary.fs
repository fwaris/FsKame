namespace FsKame

open System
open System.IO
open FsKame.WorkFlow
open Microsoft.Maui.Storage

module PdfLibrary =
    let private folder () =
        let path = Path.Combine(FileSystem.AppDataDirectory, "FsKame", "Pdfs")
        Directory.CreateDirectory(path) |> ignore
        path

    let private sanitizeFileName (name: string) =
        let invalid = Path.GetInvalidFileNameChars() |> Set.ofArray

        name.ToCharArray()
        |> Array.map (fun c -> if invalid.Contains c then '_' else c)
        |> String

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
                  displayName = fileName
                  storedPath = storedPath
                  originalPath = defaultArg (Text.notEmpty result.FullPath) result.FileName
                  selected = false
                  status = Processing
                  chunkCount = 0
                  error = None }
        }

    let copyNewPdfs (existing: PdfDocumentSource list) (results: FileResult seq) =
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

    let processPdf (doc: PdfDocumentSource) =
        async {
            let! result = KnowledgeSources.readPdfText doc.storedPath

            match result with
            | Ok text ->
                let chunkCount = KnowledgeSources.chunkText 1800 250 text |> List.length

                return
                    { id = doc.id
                      chunkCount = chunkCount
                      error = None }
            | Error err ->
                return
                    { id = doc.id
                      chunkCount = 0
                      error = Some err }
        }

    let processPdfs docs =
        async {
            let! results = docs |> List.map processPdf |> Async.Parallel

            return results |> Array.toList
        }

    let deleteStoredPdf (doc: PdfDocumentSource) =
        async {
            if String.IsNullOrWhiteSpace doc.storedPath || not (File.Exists doc.storedPath) then
                return false
            else
                File.Delete doc.storedPath
                return true
        }
