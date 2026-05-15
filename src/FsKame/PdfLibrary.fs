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

    let private prebuiltBundleManifestAsset = "FsColbertIndexes/index-bundle.json"

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
        | "json" -> JsonFile
        | _ -> MarkdownFile

    let private qaSourceKind kind =
        match kind with
        | PdfFile -> FsKame.QA.KnowledgeSourceKind.Pdf
        | MarkdownFile -> FsKame.QA.KnowledgeSourceKind.Markdown
        | JsonFile -> FsKame.QA.KnowledgeSourceKind.Json

    let private readJsonPassages source path =
        async {
            try
                match FsColbert.DoclingJson.tryDeserialize (File.ReadAllText path) with
                | Ok document ->
                    return
                        Ok(FsColbert.DoclingPassages.toPassages FsColbert.ChunkOptions.fsKameDefaults source document)
                | Error err -> return Error $"Unable to read JSON knowledge source '{path}': {err}"
            with ex ->
                return Error $"Unable to read JSON knowledge source '{path}': {ex.Message}"
        }

    let private pdfParsingMode useHybridPdfParsing =
        if useHybridPdfParsing then
            FsKame.QA.KnowledgeSources.PdfParsingMode.Hybrid
        else
            FsKame.QA.KnowledgeSources.PdfParsingMode.Legacy

    let private readPassages report useHybridPdfParsing (doc: PdfDocumentSource) =
        let source: FsKame.QA.KnowledgeSource =
            { FsKame.QA.KnowledgeSource.kind = qaSourceKind doc.kind
              location = doc.storedPath
              enabled = true }

        FsKame.QA.KnowledgeSources.loadPassagesForIndexing
            FileSystem.AppDataDirectory
            report
            (pdfParsingMode useHybridPdfParsing)
            source

    let private throwIfKeywordCancellationRequested (options: FsKame.QA.KnowledgeSources.KeywordGenerationOptions) =
        options.cancellationToken |> Option.iter _.ThrowIfCancellationRequested()

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

    let private readPrebuiltBundleManifest () =
        async {
            match! tryOpenPackageFile prebuiltBundleManifestAsset with
            | None -> return None
            | Some stream ->
                use stream = stream
                use reader = new StreamReader(stream)
                let! json = reader.ReadToEndAsync() |> Async.AwaitTask

                if String.IsNullOrWhiteSpace json then
                    return None
                else
                    return
                        match FsColbert.IndexBundle.tryDeserialize json with
                        | Ok manifest -> Some manifest
                        | Error _ -> None
        }

    let private packageIndexAssetPath (path: string) =
        let value = (defaultArg (Option.ofObj path) "").Replace('\\', '/')

        if String.IsNullOrWhiteSpace value then
            value
        elif value.StartsWith("FsColbertIndexes/", StringComparison.OrdinalIgnoreCase) then
            value
        else
            $"FsColbertIndexes/{value}"

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

    let private writeInstalledBundleManifest manifest =
        let path = Path.Combine(prebuiltIndexFolder (), "index-bundle.json")
        FsColbert.IndexBundle.writeManifest path manifest

    let private existingPrebuiltDoc (asset: PrebuiltKnowledgeAsset) docs =
        let originalPath = $"app://{asset.documentAsset}"

        docs
        |> List.tryFind (fun doc ->
            String.Equals(doc.originalPath, originalPath, StringComparison.OrdinalIgnoreCase)
            || String.Equals(doc.id, $"prebuilt-{asset.id}", StringComparison.OrdinalIgnoreCase))

    let private prebuiltKind value =
        match (defaultArg (Option.ofObj value) "").Trim().ToLowerInvariant() with
        | "pdf" -> PdfFile
        | "json"
        | "docling-json" -> JsonFile
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
            let! bundleManifest = readPrebuiltBundleManifest ()

            if List.isEmpty assets && Option.isNone bundleManifest then
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
                            | JsonFile -> "json"
                          displayName = doc.displayName
                          storedPath = storedPath
                          indexPath = indexPath }

                    if documentCopied || indexCopied then
                        logs.Add $"Installed prebuilt knowledge index: {doc.displayName}."

                match bundleManifest with
                | None -> ()
                | Some manifest ->
                    let installedBundleSources = ResizeArray<FsColbert.IndexBundleSource>()

                    for source in manifest.sources do
                        match source.sourceLocation |> Option.bind Text.notEmpty with
                        | None -> ()
                        | Some sourceLocation ->
                            let documentAsset = packageIndexAssetPath sourceLocation
                            let indexAsset = packageIndexAssetPath source.indexFile
                            let sourceKind = source.sourceKind |> Option.defaultValue ""

                            let asset =
                                { id = source.sourceId
                                  kind = sourceKind
                                  displayName = source.sourceDisplayName
                                  documentAsset = documentAsset
                                  indexAsset = indexAsset
                                  selected = false }

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
                                        kind = prebuiltKind asset.kind
                                        storedPath = storedPath
                                        status = Ready
                                        chunkCount = chunkCount
                                        error = None }
                                | None ->
                                    { id = id
                                      kind = prebuiltKind asset.kind
                                      displayName =
                                        asset.displayName |> Text.notEmpty |> Option.defaultValue documentName
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
                                    | JsonFile -> "json"
                                  displayName = doc.displayName
                                  storedPath = storedPath
                                  indexPath = indexPath }

                            installedBundleSources.Add
                                { source with
                                    sourceLocation = Some storedPath
                                    sourceKind = Some asset.kind
                                    indexFile = Path.GetFileName indexPath }

                            if documentCopied || indexCopied then
                                logs.Add $"Installed bundled FsColbert index: {doc.displayName}."

                    if installedBundleSources.Count > 0 then
                        writeInstalledBundleManifest
                            { manifest with
                                sources = List.ofSeq installedBundleSources }

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

    let processDocument report keywordOptions useHybridPdfParsing (doc: PdfDocumentSource) =
        async {
            let parserName = if useHybridPdfParsing then "Hybrid" else "Legacy"

            report $"Reading {doc.displayName} with {parserName} parser."

            throwIfKeywordCancellationRequested keywordOptions

            let! result = readPassages report useHybridPdfParsing doc

            match result with
            | Ok passages ->
                report $"Read {passages.Length} passage(s) from {doc.displayName}; indexing content."
                throwIfKeywordCancellationRequested keywordOptions

                let source: FsKame.QA.KnowledgeSource =
                    { FsKame.QA.KnowledgeSource.kind = qaSourceKind doc.kind
                      location = doc.storedPath
                      enabled = true }

                match!
                    FsKame.QA.KnowledgeSources.InindexPassages
                        FileSystem.AppDataDirectory
                        report
                        keywordOptions
                        (pdfParsingMode useHybridPdfParsing)
                        source
                        passages
                with
                | Ok() ->
                    report $"Indexed {doc.displayName} with {passages.Length} passage(s)."

                    return
                        { id = doc.id
                          chunkCount = passages.Length
                          error = None }
                | Error err ->
                    report $"Indexing {doc.displayName} failed: {err}"

                    return
                        { id = doc.id
                          chunkCount = 0
                          error = Some err }
            | Error err ->
                report $"Reading {doc.displayName} failed: {err}"

                return
                    { id = doc.id
                      chunkCount = 0
                      error = Some err }
        }

    let processDocuments report keywordOptions useHybridPdfParsing (docs: PdfDocumentSource list) =
        async {
            let mutable results = []

            for doc in docs do
                throwIfKeywordCancellationRequested keywordOptions

                report $"Processing document {doc.displayName}."
                let! result = processDocument report keywordOptions useHybridPdfParsing doc
                report $"Finished processing document {doc.displayName}."
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
