namespace FsKame.WorkFlow

open System
open System.IO
open System.Net.Http
open System.Security.Cryptography
open System.Text.Json.Serialization
open Microsoft.Extensions.AI
open System.Text
open FsKame
open Microsoft.Maui.Storage
open OpenAI.Chat
open UglyToad.PdfPig.Content
open UglyToad.PdfPig
open UglyToad.PdfPig.DocumentLayoutAnalysis.TextExtractor

module KnowledgeSources =
    type RetrievalIndex =
        { sources: KnowledgeSource list
          chunks: SourceChunk list
          colbertIndices: (KnowledgeSource * FsColbert.ColbertIndex) list
          encoder: FsColbert.OnnxColbertEncoder option }

    let emptyIndex =
        { sources = []
          chunks = []
          colbertIndices = []
          encoder = None }
            
    [<JsonConverter(typeof<JsonStringEnumConverter>)>]
    [<RequireQualifiedAccess>]
    type QueryType =        
        | Question = 1
        | SectionRetrieval = 2           
    type Expansion = {terms: string list; queryType:QueryType}

    let mutable private cachedEncoder: FsColbert.OnnxColbertEncoder option = None

    let disposeIndex (retrieval: RetrievalIndex) =
        retrieval.encoder
        |> Option.iter (fun encoder ->
            (encoder :> IDisposable).Dispose()
            if Some encoder = cachedEncoder then
                cachedEncoder <- None)

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

    let readPdfBlocks path =
        async {
            if not (File.Exists path) then
                return Error $"PDF not found: {path}"
            else
                try
                    use document = PdfDocument.Open(path)
                    let wordExtractor = UglyToad.PdfPig.DocumentLayoutAnalysis.WordExtractor.NearestNeighbourWordExtractor.Instance
                    let pageSegmenter = UglyToad.PdfPig.DocumentLayoutAnalysis.PageSegmenter.DocstrumBoundingBoxes.Instance

                    let blocks =
                        document.GetPages()
                        |> Seq.collect (fun page ->
                            let words = wordExtractor.GetWords(page.Letters)
                            let pageBlocks = pageSegmenter.GetBlocks(words)
                            let imageNote = pageImageNotes page
                            let blockTexts = pageBlocks |> Seq.map (fun b -> b.Text)
                            if String.IsNullOrWhiteSpace imageNote then
                                blockTexts
                            else
                                Seq.append blockTexts [imageNote]
                        )
                        |> Seq.map Text.normalizeWhitespace
                        |> Seq.filter (String.IsNullOrWhiteSpace >> not)
                        |> Seq.toList

                    return Ok blocks
                with ex ->
                    return Error $"Unable to read PDF '{path}': {ex.Message}"
        }

    let readPdfText path =
        async {
            let! result = readPdfBlocks path
            return result |> Result.map (String.concat "\n\n")
        }

    let chunkBlocks size overlap (blocks: string list) =
        let rec loop remaining currentChunk currentLen chunks =
            match remaining with
            | [] -> 
                if List.isEmpty currentChunk then chunks
                else (List.rev currentChunk |> String.concat "\n\n") :: chunks
            | (block: string) :: rest ->
                let blockLen = block.Length
                
                if blockLen > size && List.isEmpty currentChunk then
                    // Block is too big, let FsColbert string chunker handle it later or just emit it
                    // Emitting as is, we will apply string chunker on oversized blocks in loadChunks
                    loop rest [] 0 (block :: chunks)
                elif currentLen + blockLen + 2 > size && not (List.isEmpty currentChunk) then
                    let chunkText = List.rev currentChunk |> String.concat "\n\n"
                    
                    let rec getOverlap acc accLen revChunk =
                        match revChunk with
                        | [] -> acc
                        | (b: string) :: bs ->
                            if List.length acc = List.length currentChunk - 1 then acc
                            elif accLen + b.Length + 2 > overlap && not (List.isEmpty acc) then acc
                            else getOverlap (b :: acc) (accLen + b.Length + 2) bs
                            
                    let overlapBlocks = getOverlap [] 0 currentChunk
                    let overlapLen = overlapBlocks |> List.sumBy (fun (x:string) -> x.Length) |> (+) (if overlapBlocks.Length > 0 then (overlapBlocks.Length - 1) * 2 else 0)
                    
                    loop remaining overlapBlocks overlapLen (chunkText :: chunks)
                else
                    loop rest (block :: currentChunk) (currentLen + blockLen + 2) chunks

        loop blocks [] 0 []
        |> List.rev

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
                            | Pdf -> readPdfBlocks source.location

                        return source, result
                    })
                |> Async.Parallel

            let chunks = ResizeArray<SourceChunk>()
            let errors = ResizeArray<string>()

            for source, result in loaded do
                match result with
                | Ok blocks ->
                    let structuralChunks = chunkBlocks 1800 250 blocks
                    let mutable chunkIndex = 0
                    for sChunk in structuralChunks do
                        // If a single structural chunk is still > 1800 (giant block), fall back to char slicing
                        if sChunk.Length > 1800 then
                            for _, snip in chunkText 1800 250 sChunk do
                                chunks.Add(
                                    { source = source
                                      index = chunkIndex
                                      text = snip
                                      score = 0.0f }
                                )
                                chunkIndex <- chunkIndex + 1
                        else
                            chunks.Add(
                                { source = source
                                  index = chunkIndex
                                  text = sChunk
                                  score = 0.0f }
                            )
                            chunkIndex <- chunkIndex + 1
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

    let private createChatClient (key: string) (modelId: string) : IChatClient =
        let client = OpenAI.OpenAIClient(key)
        client.GetResponsesClient().AsIChatClient(modelId)

    let getSynonyms (client: IChatClient) (report: (string -> unit) option) query : Async<Expansion option> =
        async {
            if String.IsNullOrWhiteSpace query then
                return None
            else
                try
                    let prompt =
                        $"""
List 2-3 synonyms, strongly related technical terms, and optionally antonyms for the primary concepts in the following query to improve search recall (especially for negated queries). Return only a space-separated list of terms.
Also classify the query as type of 'Question' or SectionRetrieval.
Note that operational commands like 'Can you summarize the <section>', etc. are not questions for which answer needs to be generated but are equivalent to 'summarize the <section>'. Distinguish between true questions and request to perform an operation.

Query: {query}
"""
                    let opts = ChatOptions()
                    opts.ResponseFormat <- ChatResponseFormat.ForJsonSchema<Expansion>()
                    let! response = client.GetResponseAsync<Expansion>(prompt, opts) |> Async.AwaitTask
                    let expansion = response.Result
                    report |> Option.iter (fun r -> 
                        let keywords = String.concat ", " expansion.terms
                        r $"Query type: {expansion.queryType}. Expanded keywords: {keywords}")

                    return Some expansion
                with ex ->
                    report |> Option.iter (fun r -> r $"Query expansion failed: {ex.ToString()}")
                    return None
        }

    let private getSearchWeights = function
        | QueryType.Question -> 1.0f, 0.1f
        | QueryType.SectionRetrieval -> 0.1f, 1.0f
        | _ -> 1.0f, 1.0f

    let rank (apiKey: string option) (logExpansions: bool) (logChunks: bool) (useLexicalFilter: bool) (report: string -> unit) (query: string) (maxResults: int) (retrieval: RetrievalIndex) : Async<SourceChunk list> =
        async {
            let! expansion =
                match apiKey, useLexicalFilter with
                | Some key, true when not (String.IsNullOrWhiteSpace key) -> 
                    use client = createChatClient key C.NANO_MODEL
                    let r = if logExpansions then Some report else None
                    getSynonyms client r query
                | _ -> async { return None }

            let searchTerms = expansion |> Option.map (fun e -> e.terms) |> Option.defaultValue []
            let queryType = expansion |> Option.map (fun e -> e.queryType) |> Option.defaultValue QueryType.Question

            match retrieval.encoder, retrieval.colbertIndices with
            | Some encoder, indices when not (List.isEmpty indices) ->
                try
                    let denseWeight, lexicalWeight = getSearchWeights queryType
                    let options =
                        { FsColbert.SearchOptions.defaults with
                            maxResults = maxResults
                            useLexicalFilter = useLexicalFilter
                            useRRF = true
                            denseWeight = denseWeight
                            lexicalWeight = lexicalWeight }

                    let! allHits =
                        indices
                        |> List.map (fun (_, index) -> FsColbert.Search.queryWithSearchTerms encoder options index query searchTerms)
                        |> Async.Parallel

                    let context =
                        allHits
                        |> Array.collect (List.toArray >> Array.map (hitToChunk retrieval.sources))
                        |> Array.sortByDescending (fun c -> c.score)
                        |> Array.truncate maxResults
                        |> Array.toList
                    
                    if logChunks then
                        for chunk in context do
                            report $"Retrieved chunk: [{chunk.source.DisplayName}] {chunk.text}"

                    return context
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
                  colbertIndices = []
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

    let private sourceIndexPath (source: KnowledgeSource) =
        let options = FsColbert.ChunkOptions.fsKameDefaults

        let fingerprint =
            [ yield $"model={FsColbert.ModelCatalog.mxbaiEdgeColbertInt8.id}"
              yield $"chunk={options.maxChars}:{options.overlapChars}:{options.minChars}"
              yield sourceFingerprint source ]
            |> String.concat "\n"

        Path.Combine(indexFolder (), $"{hashText fingerprint}.fsci")

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
            match cachedEncoder with
            | Some e -> return e
            | None ->
                use client = new HttpClient()

                let! files =
                    FsColbert.ModelCatalog.ensureDownloadedAsync
                        client
                        (modelFolder ())
                        FsColbert.ModelCatalog.mxbaiEdgeColbertInt8

                let encoder = FsColbert.OnnxColbertEncoder.Load files
                cachedEncoder <- Some encoder
                return encoder
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

    let private buildIndexFromChunks report encoder (passages: FsColbert.PassageRef list) path =
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

            let! index = FsColbert.IndexBuilder.createFromPassages encoder FsColbert.ChunkOptions.fsKameDefaults passages (Some progress)
            FsColbert.IndexPersistence.save path index
            return index
        }

    let private chunksToPassages (source: KnowledgeSource) (blocks: string list) : FsColbert.PassageRef list =
        let structuralChunks = chunkBlocks 1800 250 blocks
        structuralChunks
        |> List.mapi (fun index text ->
            { FsColbert.PassageRef.sourceId = source.location
              sourceDisplayName = source.DisplayName
              sourceLocation = source.location
              index = index
              text = text })

    let InindexSource report (source: KnowledgeSource) =
        async {
            let path = sourceIndexPath source

            match tryLoadPersistedIndex path with
            | Ok(Some _) -> () // already indexed
            | _ ->
                let! result = readPdfBlocks source.location

                match result with
                | Error _ -> ()
                | Ok blocks ->
                    let passages = chunksToPassages source blocks

                    report $"Preparing FsColbert model for {source.DisplayName}."
                    let! encoder = loadEncoder ()
                    report $"Building FsColbert index for {source.DisplayName}."
                    let! _ = buildIndexFromChunks report encoder passages path
                    ()
        }

    let loadIndex report (sources: KnowledgeSource list) : Async<RetrievalIndex * string list> =
        async {
            let sources = enabledSources sources

            if List.isEmpty sources then
                return { emptyIndex with sources = sources }, []
            else
                let! encoder = loadEncoder ()
                let indices = ResizeArray<KnowledgeSource * FsColbert.ColbertIndex>()
                let errors = ResizeArray<string>()

                for source in sources do
                    let path = sourceIndexPath source

                    match tryLoadPersistedIndex path with
                    | Ok(Some index) -> indices.Add(source, index)
                    | Ok None ->
                        // If not found, we should build it, but normally it should have been built during processing
                        report $"Building missing FsColbert index for {source.DisplayName}."
                        let! result = readPdfBlocks source.location

                        match result with
                        | Error err -> errors.Add err
                        | Ok blocks ->
                            let passages = chunksToPassages source blocks
                            let! index = buildIndexFromChunks report encoder passages path
                            indices.Add(source, index)
                    | Error err -> errors.Add err

                let chunks =
                    indices
                    |> Seq.collect (fun (s, idx) -> chunksFromIndex [ s ] idx)
                    |> Seq.toList

                report $"Loaded FsColbert indices for {indices.Count} source(s)."

                return
                    { sources = sources
                      chunks = chunks
                      colbertIndices = List.ofSeq indices
                      encoder = Some encoder },
                    List.ofSeq errors
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
