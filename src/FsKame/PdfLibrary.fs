namespace FsKame

open System
open System.IO
open System.Text.Json
open FsKame.WorkFlow
open Microsoft.Maui.Storage

module PdfLibrary =
    [<CLIMutable>]
    type PrebuiltKnowledgeAsset =
        { id: string
          kind: string
          displayName: string
          documentAsset: string
          indexAsset: string
          selected: bool }

    [<CLIMutable>]
    type InstalledPrebuiltIndex =
        { id: string
          kind: string
          displayName: string
          storedPath: string
          indexPath: string }

    let private folder () =
        let path = Path.Combine(FileSystem.AppDataDirectory, "FsKame", "Documents")
        Directory.CreateDirectory(path) |> ignore
        path

    let private prebuiltManifestAsset = "FsColbertIndexes/prebuilt-indexes.json"

    let private sanitizeFileName (name: string) =
        let invalid = Path.GetInvalidFileNameChars() |> Set.ofArray

        name.ToCharArray()
        |> Array.map (fun c -> if invalid.Contains c then '_' else c)
        |> String

    let private safeId (value: string) =
        let value = defaultArg (Option.ofObj value) ""

        let cleaned =
            value.ToCharArray()
            |> Array.map (fun c ->
                if Char.IsLetterOrDigit c || c = '-' || c = '_' then
                    c
                else
                    '-')
            |> String

        match Text.notEmpty cleaned with
        | Some id -> id
        | None -> Guid.NewGuid().ToString("N")

    let private kindFromFileName (fileName: string) =
        match Path.GetExtension(fileName).TrimStart('.').ToLowerInvariant() with
        | "pdf" -> PdfFile
        | "md"
        | "markdown"
        | "mdown"
        | "mkdn" -> MarkdownFile
        | _ -> MarkdownFile

    let private qaSourceKind kind =
        match kind with
        | PdfFile -> FsKame.QA.KnowledgeSourceKind.Pdf
        | MarkdownFile -> FsKame.QA.KnowledgeSourceKind.Markdown

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

    let private tryOpenPackageFile logicalName =
        async {
            try
                let! stream = FileSystem.OpenAppPackageFileAsync(logicalName) |> Async.AwaitTask
                return Some stream
            with _ ->
                return None
        }

    let private copyPackageFile logicalName path =
        async {
            match! tryOpenPackageFile logicalName with
            | None -> return false
            | Some source ->
                use source = source

                Directory.CreateDirectory(Path.GetDirectoryName(Path.GetFullPath path))
                |> ignore

                use target = File.Create path
                do! source.CopyToAsync(target) |> Async.AwaitTask
                return true
        }

    let private readPrebuiltManifest () =
        async {
            match! tryOpenPackageFile prebuiltManifestAsset with
            | None -> return []
            | Some stream ->
                use stream = stream
                use reader = new StreamReader(stream)
                let! json = reader.ReadToEndAsync() |> Async.AwaitTask

                if String.IsNullOrWhiteSpace json then
                    return []
                else
                    try
                        return
                            JsonSerializer.Deserialize<PrebuiltKnowledgeAsset array>(json)
                            |> Option.ofObj
                            |> Option.map Array.toList
                            |> Option.defaultValue []
                    with _ ->
                        return []
        }

    let private prebuiltIndexFolder () =
        let path = KnowledgeSources.prebuiltFolder ()
        Directory.CreateDirectory path |> ignore
        path

    let private writeInstalledPrebuiltManifest entries =
        let path = KnowledgeSources.prebuiltManifestPath ()

        Directory.CreateDirectory(Path.GetDirectoryName(Path.GetFullPath path))
        |> ignore

        let json = entries |> List.toArray |> JsonSerializer.Serialize
        File.WriteAllText(path, json)

    let private existingPrebuiltDoc (asset: PrebuiltKnowledgeAsset) docs =
        let originalPath = $"app://{asset.documentAsset}"

        docs
        |> List.tryFind (fun doc ->
            String.Equals(doc.originalPath, originalPath, StringComparison.OrdinalIgnoreCase)
            || String.Equals(doc.id, $"prebuilt-{asset.id}", StringComparison.OrdinalIgnoreCase))

    let private prebuiltKind value =
        match (defaultArg (Option.ofObj value) "").Trim().ToLowerInvariant() with
        | "pdf" -> PdfFile
        | _ -> MarkdownFile

    let private prebuiltChunkCount indexPath =
        try
            if File.Exists indexPath then
                (FsColbert.IndexPersistence.load indexPath).passages.Length
            else
                0
        with _ ->
            0

    let installPrebuiltDocuments (existing: PdfDocumentSource list) =
        async {
            let! assets = readPrebuiltManifest ()

            if List.isEmpty assets then
                return existing, []
            else
                let docs = ResizeArray<PdfDocumentSource>(existing)
                let installed = ResizeArray<InstalledPrebuiltIndex>()
                let logs = ResizeArray<string>()

                for asset in assets do
                    let id = $"prebuilt-{safeId asset.id}"
                    let documentName = Path.GetFileName asset.documentAsset |> sanitizeFileName
                    let storedPath = Path.Combine(folder (), $"{id}-{documentName}")
                    let indexPath = Path.Combine(prebuiltIndexFolder (), $"{id}.fsci")

                    let! documentCopied =
                        if File.Exists storedPath then
                            async.Return false
                        else
                            copyPackageFile asset.documentAsset storedPath

                    let! indexCopied =
                        if File.Exists indexPath then
                            async.Return false
                        else
                            copyPackageFile asset.indexAsset indexPath

                    let chunkCount = prebuiltChunkCount indexPath

                    let doc =
                        match existingPrebuiltDoc asset (List.ofSeq docs) with
                        | Some current ->
                            { current with
                                storedPath = storedPath
                                status = Ready
                                chunkCount = chunkCount
                                error = None }
                        | None ->
                            { id = id
                              kind = prebuiltKind asset.kind
                              displayName = asset.displayName |> Text.notEmpty |> Option.defaultValue documentName
                              storedPath = storedPath
                              originalPath = $"app://{asset.documentAsset}"
                              selected = asset.selected
                              status = Ready
                              chunkCount = chunkCount
                              error = None }

                    docs.RemoveAll(fun item -> item.id = doc.id) |> ignore
                    docs.Add doc

                    installed.Add
                        { id = id
                          kind =
                            match doc.kind with
                            | PdfFile -> "pdf"
                            | MarkdownFile -> "markdown"
                          displayName = doc.displayName
                          storedPath = storedPath
                          indexPath = indexPath }

                    if documentCopied || indexCopied then
                        logs.Add $"Installed prebuilt knowledge index: {doc.displayName}."

                writeInstalledPrebuiltManifest (List.ofSeq installed)

                return List.ofSeq docs, List.ofSeq logs
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

    let processDocument report keywordOptions (doc: PdfDocumentSource) =
        async {
            let! result = readPassages doc

            match result with
            | Ok passages ->
                let source: FsKame.QA.KnowledgeSource =
                    { FsKame.QA.KnowledgeSource.kind = qaSourceKind doc.kind
                      location = doc.storedPath
                      enabled = true }

                match!
                    FsKame.QA.KnowledgeSources.InindexSource FileSystem.AppDataDirectory report keywordOptions source
                with
                | Ok() ->
                    return
                        { id = doc.id
                          chunkCount = passages.Length
                          error = None }
                | Error err ->
                    return
                        { id = doc.id
                          chunkCount = 0
                          error = Some err }
            | Error err ->
                return
                    { id = doc.id
                      chunkCount = 0
                      error = Some err }
        }

    let processDocuments report keywordOptions docs =
        async {
            let mutable results = []

            for doc in docs do
                let! result = processDocument report keywordOptions doc
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
