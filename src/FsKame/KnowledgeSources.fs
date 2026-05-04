namespace FsKame.WorkFlow

open System
open System.IO
open System.Net.Http
open System.Security.Cryptography
open System.Text
open FsKame
open Microsoft.Maui.Storage
open UglyToad.PdfPig.Content
open UglyToad.PdfPig
open UglyToad.PdfPig.DocumentLayoutAnalysis.TextExtractor

module KnowledgeSources =
    type RetrievalIndex =
        { sources: KnowledgeSource list
          chunks: SourceChunk list
          colbertIndex: FsColbert.ColbertIndex option
          encoder: FsColbert.OnnxColbertEncoder option }

    let emptyIndex =
        { sources = []
          chunks = []
          colbertIndex = None
          encoder = None }

    let disposeIndex (retrieval: RetrievalIndex) =
        retrieval.encoder
        |> Option.iter (fun encoder -> (encoder :> IDisposable).Dispose())

    let fromPdfDocuments (docs: PdfDocumentSource list) =
        docs
        |> PdfDocuments.selectedReady
        |> List.map (fun doc ->
            { kind = Pdf
              location = doc.storedPath
              enabled = true })

    let selectedSources (docs: PdfDocumentSource list) = fromPdfDocuments docs

    let private pageImageNotes (page: Page) =
        let images = page.GetImages() |> Seq.toList

        if List.isEmpty images then
            ""
        else
            images
            |> List.mapi (fun index image ->
                $"[PDF image note: page {page.Number}, image {index + 1}, {image.WidthInSamples}x{image.HeightInSamples} samples. Embedded image detected during PDF processing.]")
            |> String.concat "\n"

    let readPdfText path =
        async {
            if not (File.Exists path) then
                return Error $"PDF not found: {path}"
            else
                try
                    use document = PdfDocument.Open(path)

                    let text =
                        document.GetPages()
                        |> Seq.map (fun page ->
                            [ ContentOrderTextExtractor.GetText page; pageImageNotes page ]
                            |> List.filter (String.IsNullOrWhiteSpace >> not)
                            |> String.concat "\n")
                        |> String.concat "\n\n"
                        |> Text.normalizeWhitespace

                    return Ok text
                with ex ->
                    return Error $"Unable to read PDF '{path}': {ex.Message}"
        }

    let chunkText size overlap (text: string) =
        let options =
            { FsColbert.ChunkOptions.fsKameDefaults with
                maxChars = size
                overlapChars = overlap }

        FsColbert.Text.chunkText options text

    let loadChunks (sources: KnowledgeSource list) : Async<SourceChunk list * string list> =
        async {
            let! loaded =
                sources
                |> List.filter _.enabled
                |> List.map (fun source ->
                    async {
                        let! result =
                            match source.kind with
                            | Pdf -> readPdfText source.location

                        return source, result
                    })
                |> Async.Parallel

            let chunks = ResizeArray<SourceChunk>()
            let errors = ResizeArray<string>()

            for source, result in loaded do
                match result with
                | Ok text ->
                    for index, snip in chunkText 1800 250 text do
                        chunks.Add(
                            { source = source
                              index = index
                              text = snip
                              score = 0.0f }
                        )
                | Error err -> errors.Add err

            return List.ofSeq chunks, List.ofSeq errors
        }

    let private sourceKindId sourceKind =
        match sourceKind with
        | Pdf -> "pdf"

    let private sourceFromLocation (sources: KnowledgeSource list) (location: string) : KnowledgeSource =
        sources
        |> List.tryFind (fun source -> String.Equals(source.location, location, StringComparison.OrdinalIgnoreCase))
        |> Option.defaultValue
            { kind = Pdf
              location = location
              enabled = true }

    let private hitToChunk sources (hit: FsColbert.SearchHit) =
        { source = sourceFromLocation sources hit.reference.sourceLocation
          index = hit.reference.index
          text = hit.reference.text
          score = hit.score }

    let private chunksFromIndex sources (index: FsColbert.ColbertIndex) =
        index.passages
        |> List.map (fun passage ->
            { source = sourceFromLocation sources passage.reference.sourceLocation
              index = passage.reference.index
              text = passage.reference.text
              score = 0.0f })

    let private rankLexically (query: string) (maxResults: int) (chunks: SourceChunk list) : SourceChunk list =
        let queryTerms = Text.terms query |> Set.ofList

        if Set.isEmpty queryTerms then
            []
        else
            chunks
            |> List.choose (fun (chunk: SourceChunk) ->
                let haystack = chunk.text.ToLowerInvariant()

                let score =
                    queryTerms |> Seq.sumBy (fun term -> if haystack.Contains term then 1 else 0)

                if score = 0 then
                    None
                else
                    Some { chunk with score = float32 score })
            |> List.sortByDescending (fun (c: SourceChunk) -> c.score, c.text.Length * -1)
            |> List.truncate maxResults

    let rank (query: string) (maxResults: int) (retrieval: RetrievalIndex) : Async<SourceChunk list> =
        async {
            match retrieval.encoder, retrieval.colbertIndex with
            | Some encoder, Some index ->
                try
                    let options =
                        { FsColbert.SearchOptions.defaults with
                            maxResults = maxResults }

                    let! hits = FsColbert.Search.query encoder options index query

                    return hits |> List.map (hitToChunk retrieval.sources)
                with _ ->
                    return rankLexically query maxResults retrieval.chunks
            | _ -> return rankLexically query maxResults retrieval.chunks
        }

    let private enabledSources sources = sources |> List.filter _.enabled

    let loadInternalIndex (sources: KnowledgeSource list) : Async<RetrievalIndex * string list> =
        async {
            let sources = enabledSources sources
            let! chunks, errors = loadChunks sources

            return
                { sources = sources
                  chunks = chunks
                  colbertIndex = None
                  encoder = None },
                errors
        }

    let private fsColbertRoot () =
        let path = Path.Combine(FileSystem.AppDataDirectory, "FsKame", "FsColbert")
        Directory.CreateDirectory path |> ignore
        path

    let private modelFolder () =
        Path.Combine(fsColbertRoot (), "Models", "mxbai-edge-colbert")

    let private indexFolder () =
        let path = Path.Combine(fsColbertRoot (), "Indexes")
        Directory.CreateDirectory path |> ignore
        path

    let clearPersistedIndexes () =
        async {
            let files = Directory.EnumerateFiles(indexFolder (), "*.fsci") |> Seq.toList
            let errors = ResizeArray<string>()

            let deleted =
                files
                |> List.sumBy (fun path ->
                    try
                        File.Delete path
                        1
                    with ex ->
                        errors.Add $"Unable to delete FsColbert index '{path}': {ex.Message}"
                        0)

            return deleted, List.ofSeq errors
        }

    let private sourceFingerprint (source: KnowledgeSource) =
        let filePart =
            let info = FileInfo source.location

            if info.Exists then
                $"{info.FullName}:{info.Length}:{info.LastWriteTimeUtc.Ticks}"
            else
                source.location

        $"{sourceKindId source.kind}:{filePart}"

    let private hashText (value: string) =
        use sha = SHA256.Create()
        let bytes = Encoding.UTF8.GetBytes value

        sha.ComputeHash bytes
        |> Convert.ToHexString
        |> fun hash -> hash.ToLowerInvariant()

    let private indexPath sources =
        let options = FsColbert.ChunkOptions.fsKameDefaults

        let fingerprint =
            [ yield $"model={FsColbert.ModelCatalog.mxbaiEdgeColbertInt8.id}"
              yield $"chunk={options.maxChars}:{options.overlapChars}:{options.minChars}"
              yield! (sources |> List.map sourceFingerprint |> List.sort) ]
            |> String.concat "\n"

        Path.Combine(indexFolder (), $"{hashText fingerprint}.fsci")

    let private tryLoadPersistedIndex path =
        if File.Exists path then
            try
                Ok(Some(FsColbert.IndexPersistence.load path))
            with ex ->
                Error $"Unable to load FsColbert index '{path}': {ex.Message}"
        else
            Ok None

    let private loadDocuments sources =
        async {
            let! loaded =
                sources
                |> List.map (fun source ->
                    async {
                        let! result =
                            match source.kind with
                            | Pdf -> readPdfText source.location

                        return source, result
                    })
                |> Async.Parallel

            let documents = ResizeArray<FsColbert.SourceDocument>()
            let chunks = ResizeArray<SourceChunk>()
            let errors = ResizeArray<string>()

            for source, result in loaded do
                match result with
                | Ok text ->
                    documents.Add(
                        FsColbert.SourceDocuments.create
                            source.location
                            source.DisplayName
                            source.location
                            text
                            source.enabled
                    )

                    for index, snip in chunkText 1800 250 text do
                        chunks.Add(
                            { source = source
                              index = index
                              text = snip
                              score = 0.0f }
                        )
                | Error err -> errors.Add err

            return List.ofSeq documents, List.ofSeq chunks, List.ofSeq errors
        }

    let private loadEncoder () =
        async {
            use client = new HttpClient()

            let! files =
                FsColbert.ModelCatalog.ensureDownloadedAsync
                    client
                    (modelFolder ())
                    FsColbert.ModelCatalog.mxbaiEdgeColbertInt8

            return FsColbert.OnnxColbertEncoder.Load files
        }

    let private buildIndex report encoder documents path =
        async {
            let mutable lastReported = -1

            let progress (update: FsColbert.IndexProgress) =
                if update.totalPassages > 0 then
                    let completed = update.completedPassages

                    if
                        completed = 0
                        || completed = update.totalPassages
                        || completed - lastReported >= 10
                    then
                        lastReported <- completed
                        report $"FsColbert indexed {completed}/{update.totalPassages} passage(s)."

            let! index = FsColbert.IndexBuilder.createWithDefaults encoder documents (Some progress)
            FsColbert.IndexPersistence.save path index
            return index
        }

    let loadIndex report (sources: KnowledgeSource list) : Async<RetrievalIndex * string list> =
        async {
            let sources = enabledSources sources

            if List.isEmpty sources then
                return { emptyIndex with sources = sources }, []
            else
                let path = indexPath sources

                let persisted, loadErrors =
                    match tryLoadPersistedIndex path with
                    | Ok index -> index, []
                    | Error err -> None, [ err ]

                match persisted with
                | Some index ->
                    let chunks = chunksFromIndex sources index

                    if List.isEmpty chunks then
                        return
                            { sources = sources
                              chunks = chunks
                              colbertIndex = None
                              encoder = None },
                            loadErrors
                    else
                        try
                            report $"Preparing FsColbert model for {chunks.Length} cached passage(s)."
                            let! encoder = loadEncoder ()
                            report $"Loaded FsColbert index with {chunks.Length} passage(s)."

                            return
                                { sources = sources
                                  chunks = chunks
                                  colbertIndex = Some index
                                  encoder = Some encoder },
                                loadErrors
                        with ex ->
                            Log.log.Value.exn(ex,"loadIndex")
                            return
                                { sources = sources
                                  chunks = chunks
                                  colbertIndex = None
                                  encoder = None },
                                loadErrors
                                @ [ $"FsColbert model unavailable; using lexical retrieval: {ex.Message}" ]
                | None ->
                    let! documents, chunks, readErrors = loadDocuments sources

                    if List.isEmpty chunks then
                        return
                            { sources = sources
                              chunks = chunks
                              colbertIndex = None
                              encoder = None },
                            loadErrors @ readErrors
                    else
                        try
                            report $"Preparing FsColbert model for {documents.Length} PDF source(s)."
                            let! encoder = loadEncoder ()

                            try
                                report $"Building FsColbert index for {chunks.Length} passage(s)."
                                let! index = buildIndex report encoder documents path
                                let chunks = chunksFromIndex sources index
                                report $"Saved FsColbert index with {chunks.Length} passage(s)."

                                return
                                    { sources = sources
                                      chunks = chunks
                                      colbertIndex = Some index
                                      encoder = Some encoder },
                                    loadErrors @ readErrors
                            with ex ->
                                (encoder :> IDisposable).Dispose()

                                return
                                    { sources = sources
                                      chunks = chunks
                                      colbertIndex = None
                                      encoder = None },
                                    loadErrors
                                    @ readErrors
                                    @ [ $"FsColbert indexing unavailable; using lexical retrieval: {ex.Message}" ]
                        with ex ->
                            Log.log.Value.exn(ex,"LoadIndex")
                            return
                                { sources = sources
                                  chunks = chunks
                                  colbertIndex = None
                                  encoder = None },
                                loadErrors
                                @ readErrors
                                @ [ $"FsColbert model unavailable; using lexical retrieval: {ex.Message}" ]
        }

    let renderContext (chunks: SourceChunk list) =
        if List.isEmpty chunks then
            "No selected PDF context was available."
        else
            chunks
            |> List.mapi (fun index (chunk: SourceChunk) ->
                let body = Text.truncate 900 chunk.text
                $"[{index + 1}] {chunk.source.DisplayName} chunk {chunk.index}\n{body}")
            |> String.concat "\n\n"

    let renderInventory (sources: KnowledgeSource list) =
        let enabled = sources |> List.filter _.enabled

        if List.isEmpty enabled then
            "No PDF sources are currently selected and ready."
        else
            enabled
            |> List.mapi (fun index source -> $"[{index + 1}] {source.DisplayName}")
            |> String.concat "\n"
