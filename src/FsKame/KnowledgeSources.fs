namespace FsKame.WorkFlow

open System
open System.IO
open System.Net.Http
open System.Security.Cryptography
open System.Text.Json.Serialization
open Microsoft.Extensions.AI
open System.Text
open System.Text.RegularExpressions
open FsKame
open Microsoft.Maui.Storage
open OpenAI.Chat

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

    type Expansion =
        { terms: string list
          rewrittenQueries: string list
          sectionName: string option
          queryType: QueryType }

    let mutable private cachedEncoder: FsColbert.OnnxColbertEncoder option = None

    let private pdfIndexVersion = FsColbert.DocumentChunking.representationVersion

    let private sourceKindFromDocument kind =
        match kind with
        | PdfFile -> Pdf
        | MarkdownFile -> Markdown

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
            { kind = sourceKindFromDocument doc.kind
              location = doc.storedPath
              enabled = true })

    let selectedSources (docs: PdfDocumentSource list) = fromPdfDocuments docs

    let private fsKameChunkOptions = FsColbert.ChunkOptions.fsKameDefaults

    let private sourcePassages (source: KnowledgeSource) =
        let passageSource =
            FsColbert.PassageSource.create source.location source.DisplayName source.location

        match source.kind with
        | Pdf -> FsColbert.PdfDocuments.readPassages fsKameChunkOptions passageSource source.location
        | Markdown -> FsColbert.MarkdownDocuments.readPassages fsKameChunkOptions passageSource source.location

    let loadChunks (sources: KnowledgeSource list) : Async<SourceChunk list * string list> =
        async {
            let! loaded =
                sources
                |> List.filter _.enabled
                |> List.map (fun source ->
                    async {
                        let! result = sourcePassages source
                        return source, result
                    })
                |> Async.Parallel

            let chunks = ResizeArray<SourceChunk>()
            let errors = ResizeArray<string>()

            for source, result in loaded do
                match result with
                | Ok passages ->
                    for passage in passages do
                        chunks.Add(
                            { source = source
                              index = passage.index
                              text = passage.text
                              score = 0.0f }
                        )
                | Error err -> errors.Add err

            return List.ofSeq chunks, List.ofSeq errors
        }

    let private sourceKindId sourceKind =
        match sourceKind with
        | Pdf -> "pdf"
        | Markdown -> "markdown"

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

    let private applySectionBoost sectionName (chunks: SourceChunk list) =
        match sectionName with
        | None -> chunks
        | Some requested ->
            chunks
            |> List.map (fun chunk ->
                match FsColbert.DocumentSections.tryGetHeading chunk.text with
                | Some heading when FsColbert.DocumentSections.matches requested heading ->
                    { chunk with
                        score = chunk.score + 1.0f }
                | _ -> chunk)
            |> List.sortByDescending (fun chunk -> chunk.score)

    let private sectionHeadings (retrieval: RetrievalIndex) =
        seq {
            for chunk in retrieval.chunks do
                match FsColbert.DocumentSections.tryGetHeading chunk.text with
                | Some heading -> heading
                | None -> ()

            for _, index in retrieval.colbertIndices do
                for passage in index.passages do
                    match FsColbert.DocumentSections.tryGetHeading passage.reference.text with
                    | Some heading -> heading
                    | None -> ()
        }
        |> Seq.distinctBy FsColbert.DocumentSections.normalizedName
        |> Seq.toList

    let private tryResolveSectionName retrieval requested =
        sectionHeadings retrieval
        |> List.tryFind (fun heading -> FsColbert.DocumentSections.matches requested heading)

    let private createChatClient (key: string) (modelId: string) : IChatClient =
        let client = OpenAI.OpenAIClient(key)
        client.GetResponsesClient().AsIChatClient(modelId)

    let private retrievalActionTerms =
        [ "about"
          "can"
          "could"
          "describe"
          "explain"
          "extract"
          "find"
          "give"
          "list"
          "please"
          "provide"
          "section"
          "show"
          "summarise"
          "summarize"
          "summary"
          "tell"
          "the"
          "what"
          "would"
          "you" ]
        |> Set.ofList

    let private distinctNonEmpty (values: string seq) =
        values
        |> Seq.choose Text.notEmpty
        |> Seq.distinctBy _.ToLowerInvariant()
        |> Seq.toList

    let private compactTerms (values: string seq) =
        values
        |> Seq.collect Text.terms
        |> Seq.filter (fun term -> not (retrievalActionTerms.Contains term))
        |> Seq.distinct
        |> Seq.truncate 12
        |> Seq.toList

    let private cleanupRetrievalTarget (target: string) =
        let withoutSourceTail =
            Regex.Replace(target, @"(?i)\b(?:from|in|of)\s+(?:the\s+)?(?:paper|document|pdf|article|source)\b.*$", "")

        Regex.Replace(withoutSourceTail, @"[?.!,;:\s]+$", "").Trim()

    let private tryExtractRetrievalTarget (query: string) =
        let patterns =
            [ @"(?i)^\s*(?:can|could|would)\s+you\s+(?:please\s+)?(?:summari[sz]e|explain|describe|outline|extract|find|show|give|provide|tell\s+me\s+about)\s+(?:the\s+|a\s+|an\s+)?(?<target>[\p{L}\p{N}\s_/\-.'&()]{3,100})\s*[?.!]*\s*$"
              @"(?i)^\s*(?:please\s+)?(?:summari[sz]e|explain|describe|outline|extract|find|show|give|provide|tell\s+me\s+about)\s+(?:the\s+|a\s+|an\s+)?(?<target>[\p{L}\p{N}\s_/\-.'&()]{3,100})\s*[?.!]*\s*$" ]

        patterns
        |> List.tryPick (fun pattern ->
            let m = Regex.Match(query, pattern)

            if m.Success then
                m.Groups["target"].Value |> cleanupRetrievalTarget |> Text.notEmpty
            else
                None)

    let private createLocalExpansion query =
        if String.IsNullOrWhiteSpace query then
            None
        else
            let target = tryExtractRetrievalTarget query

            let queryType =
                if Option.isSome target then
                    QueryType.SectionRetrieval
                else
                    QueryType.Question

            let rewrittenQueries =
                match target with
                | Some value -> [ value; $"section {value}" ]
                | None -> [ query ]

            let terms =
                match target with
                | Some value -> compactTerms [ value; $"section {value}" ]
                | None -> compactTerms [ query ]

            Some
                { terms = terms
                  rewrittenQueries = distinctNonEmpty rewrittenQueries
                  sectionName = target
                  queryType = queryType }

    let private mergeExpansions localExpansion remoteExpansion =
        match localExpansion, remoteExpansion with
        | None, None -> None
        | Some local, None -> Some local
        | None, Some remote -> Some remote
        | Some local, Some remote ->
            let queryType =
                if local.queryType = QueryType.SectionRetrieval then
                    QueryType.SectionRetrieval
                else
                    remote.queryType

            Some
                { terms = distinctNonEmpty (Seq.append local.terms remote.terms)
                  rewrittenQueries = distinctNonEmpty (Seq.append local.rewrittenQueries remote.rewrittenQueries)
                  sectionName = remote.sectionName |> Option.orElse local.sectionName
                  queryType = queryType }

    let private sanitizeExpansion (expansion: Expansion) =
        { terms = distinctNonEmpty expansion.terms |> List.truncate 12
          rewrittenQueries = distinctNonEmpty expansion.rewrittenQueries |> List.truncate 3
          sectionName = expansion.sectionName |> Option.bind Text.notEmpty
          queryType = expansion.queryType }

    let private canonicalizeSectionTarget (retrieval: RetrievalIndex) (expansion: Expansion) =
        match expansion.sectionName |> Option.bind (tryResolveSectionName retrieval) with
        | None -> expansion
        | Some canonicalSection ->
            { expansion with
                terms =
                    seq {
                        canonicalSection
                        yield! expansion.terms
                    }
                    |> distinctNonEmpty
                    |> List.truncate 12
                rewrittenQueries =
                    seq {
                        canonicalSection
                        $"section {canonicalSection}"
                        yield! expansion.rewrittenQueries
                    }
                    |> distinctNonEmpty
                    |> List.truncate 3
                sectionName = Some canonicalSection }

    let getSynonyms (client: IChatClient) (report: (string -> unit) option) query : Async<Expansion option> =
        async {
            if String.IsNullOrWhiteSpace query then
                return None
            else
                try
                    let prompt =
                        $"""
Create compact retrieval signals for searching PDF passages.

Return JSON matching the schema:
- terms: at most 8 content keywords, aliases, or technical terms likely to appear in relevant passages. Avoid generic action words such as summarize, explain, describe, question, answer, paper, document, and PDF.
- rewrittenQueries: 1-2 short retrieval queries that describe the information to find, not the operation to perform. For a named section request, include the canonical target, e.g. "abstract" and "section abstract".
- sectionName: the named section, table, figure, appendix, or other document part being requested, or null when there is no such target.
- queryType: "Question" for factual questions; "SectionRetrieval" for requests to retrieve, summarize, explain, list, or show a named or broad document section.

Do not answer the user. Only provide retrieval signals.

Query: {query}
"""

                    let opts = ChatOptions()
                    opts.ResponseFormat <- ChatResponseFormat.ForJsonSchema<Expansion>()
                    let! response = client.GetResponseAsync<Expansion>(prompt, opts) |> Async.AwaitTask
                    let expansion = sanitizeExpansion response.Result

                    report
                    |> Option.iter (fun r ->
                        let keywords = String.concat ", " expansion.terms
                        let rewrites = String.concat " | " expansion.rewrittenQueries

                        r
                            $"Query type: {expansion.queryType}. Retrieval query: {rewrites}. Expanded keywords: {keywords}")

                    return Some expansion
                with ex ->
                    report |> Option.iter (fun r -> r $"Query expansion failed: {ex.ToString()}")
                    return None
        }

    let private getSearchWeights =
        function
        | QueryType.Question -> 1.0f, 0.1f
        | QueryType.SectionRetrieval -> 0.65f, 1.0f
        | _ -> 1.0f, 1.0f

    let rank
        (apiKey: string option)
        (logExpansions: bool)
        (logChunks: bool)
        (useLexicalFilter: bool)
        (report: string -> unit)
        (query: string)
        (maxResults: int)
        (retrieval: RetrievalIndex)
        : Async<SourceChunk list> =
        async {
            let localExpansion = createLocalExpansion query

            let! remoteExpansion =
                match apiKey, useLexicalFilter with
                | Some key, true when not (String.IsNullOrWhiteSpace key) ->
                    use client = createChatClient key C.NANO_MODEL
                    let r = if logExpansions then Some report else None
                    getSynonyms client r query
                | _ -> async { return None }

            let expansion =
                mergeExpansions localExpansion remoteExpansion
                |> Option.map (canonicalizeSectionTarget retrieval)

            let retrievalQuery =
                expansion
                |> Option.bind (fun e -> e.rewrittenQueries |> List.tryHead)
                |> Option.defaultValue query

            let searchTerms =
                expansion
                |> Option.map (fun e ->
                    [ yield! e.terms
                      match e.sectionName with
                      | Some sectionName -> yield sectionName
                      | None -> () ])
                |> Option.defaultValue []
                |> distinctNonEmpty

            let queryType =
                expansion
                |> Option.map (fun e -> e.queryType)
                |> Option.defaultValue QueryType.Question

            let lexicalQuery =
                if List.isEmpty searchTerms then
                    retrievalQuery
                else
                    String.concat " " searchTerms

            let sectionName = expansion |> Option.bind (fun e -> e.sectionName)

            let lexicalFallback () =
                let lexicalMaxResults =
                    match queryType with
                    | QueryType.SectionRetrieval -> max maxResults 20
                    | _ -> maxResults

                retrieval.chunks
                |> rankLexically lexicalQuery lexicalMaxResults
                |> applySectionBoost sectionName
                |> List.truncate maxResults

            match retrieval.encoder, retrieval.colbertIndices with
            | Some encoder, indices when not (List.isEmpty indices) ->
                try
                    let denseWeight, lexicalWeight = getSearchWeights queryType

                    let rawMaxResults =
                        match queryType with
                        | QueryType.SectionRetrieval -> max maxResults 20
                        | _ -> maxResults

                    let options =
                        { FsColbert.SearchOptions.defaults with
                            maxResults = rawMaxResults
                            candidateLimit =
                                match queryType with
                                | QueryType.SectionRetrieval -> max FsColbert.SearchOptions.defaults.candidateLimit 256
                                | _ -> FsColbert.SearchOptions.defaults.candidateLimit
                            useLexicalFilter = useLexicalFilter
                            useRRF = true
                            denseWeight = denseWeight
                            lexicalWeight = lexicalWeight }

                    let! allHits =
                        indices
                        |> List.map (fun (_, index) ->
                            FsColbert.Search.queryWithSearchTerms encoder options index retrievalQuery searchTerms)
                        |> Async.Parallel

                    let context =
                        allHits
                        |> Array.collect (List.toArray >> Array.map (hitToChunk retrieval.sources))
                        |> Array.sortByDescending (fun c -> c.score)
                        |> Array.truncate rawMaxResults
                        |> Array.toList
                        |> applySectionBoost sectionName
                        |> List.truncate maxResults

                    if logChunks then
                        for chunk in context do
                            report $"Retrieved chunk: [{chunk.source.DisplayName}] {chunk.text}"

                    return context
                with _ ->
                    return lexicalFallback ()
            | _ -> return lexicalFallback ()
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
              yield $"pdfIndexVersion={pdfIndexVersion}"
              yield $"chunk={options.maxChars}:{options.overlapChars}:{options.minChars}"
              yield sourceFingerprint source ]
            |> String.concat "\n"

        Path.Combine(indexFolder (), $"{hashText fingerprint}.fsci")

    let private indexPath sources =
        let options = FsColbert.ChunkOptions.fsKameDefaults

        let fingerprint =
            [ yield $"model={FsColbert.ModelCatalog.mxbaiEdgeColbertInt8.id}"
              yield $"pdfIndexVersion={pdfIndexVersion}"
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

            let! index =
                FsColbert.IndexBuilder.createFromPassages
                    encoder
                    FsColbert.ChunkOptions.fsKameDefaults
                    passages
                    (Some progress)

            FsColbert.IndexPersistence.save path index
            return index
        }

    let InindexSource report (source: KnowledgeSource) =
        async {
            let path = sourceIndexPath source

            match tryLoadPersistedIndex path with
            | Ok(Some _) -> () // already indexed
            | _ ->
                let! result = sourcePassages source

                match result with
                | Error _ -> ()
                | Ok passages ->
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
                        let! result = sourcePassages source

                        match result with
                        | Error err -> errors.Add err
                        | Ok passages ->
                            let! index = buildIndexFromChunks report encoder passages path
                            indices.Add(source, index)
                    | Error err -> errors.Add err

                let chunks =
                    indices |> Seq.collect (fun (s, idx) -> chunksFromIndex [ s ] idx) |> Seq.toList

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
            "No selected document context was available."
        else
            chunks
            |> List.mapi (fun index (chunk: SourceChunk) ->
                let body = Text.truncate 900 chunk.text
                $"[{index + 1}] {chunk.source.DisplayName} chunk {chunk.index}\n{body}")
            |> String.concat "\n\n"

    let renderInventory (sources: KnowledgeSource list) =
        let enabled = sources |> List.filter _.enabled

        if List.isEmpty enabled then
            "No document sources are currently selected and ready."
        else
            enabled
            |> List.mapi (fun index source -> $"[{index + 1}] {source.DisplayName}")
            |> String.concat "\n"
