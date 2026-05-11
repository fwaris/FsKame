namespace FsKame.QA

open System
open System.IO
open System.Net.Http
open System.Security.Cryptography
open System.Text.Json
open System.Text.Json.Serialization
open System.Threading
open Microsoft.Extensions.AI
open System.Text
open System.Text.RegularExpressions

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

    type KeywordGenerationOptions =
        { enabled: bool
          client: IChatClient option
          modelId: string
          schemaVersion: string
          batchSize: int
          parallelism: int
          maxOutputTokens: int }

    module KeywordGenerationOptions =
        let defaults =
            { enabled = true
              client = None
              modelId = "gpt-5-nano"
              schemaVersion = "passage-keywords-v1"
              batchSize = 4
              parallelism = 2
              maxOutputTokens = 25000 }

        let disabled = { defaults with enabled = false }

    [<CLIMutable>]
    type InstalledPrebuiltIndex =
        { id: string
          kind: string
          displayName: string
          storedPath: string
          indexPath: string }

    let mutable private cachedEncoder: FsColbert.OnnxColbertEncoder option = None

    let private pdfIndexVersion = FsColbert.DocumentChunking.representationVersion

    let disposeIndex (retrieval: RetrievalIndex) =
        retrieval.encoder
        |> Option.iter (fun encoder ->
            (encoder :> IDisposable).Dispose()

            if Some encoder = cachedEncoder then
                cachedEncoder <- None)

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

    let private sourceKey (source: KnowledgeSource) = source.location.ToLowerInvariant()

    let private chunkKey (chunk: SourceChunk) = sourceKey chunk.source, chunk.index

    let private sourceEquals (left: KnowledgeSource) (right: KnowledgeSource) =
        String.Equals(left.location, right.location, StringComparison.OrdinalIgnoreCase)

    let private distinctChunks chunks =
        chunks
        |> List.fold
            (fun (seen, kept) chunk ->
                let key = chunkKey chunk

                if Set.contains key seen then
                    seen, kept
                else
                    Set.add key seen, chunk :: kept)
            (Set.empty, [])
        |> snd
        |> List.rev

    let private sourceCoverageTerms =
        [ "both"
          "each"
          "all"
          "documents"
          "document"
          "docs"
          "papers"
          "sources"
          "selected"
          "compare"
          "contrast"
          "summarize"
          "summarise"
          "summary"
          "them"
          "these" ]

    let private representativeTerms =
        [ "this"
          "it"
          "overview"
          "about"
          "gist"
          "abstract"
          "introduction"
          "main"
          "points" ]

    let private representativePhrases =
        [ "main point"
          "main points"
          "high level"
          "high-level"
          "what is this"
          "what are these"
          "what does this"
          "what do these" ]

    let private queryHasAnyTerm (terms: string list) (query: string) =
        let queryTerms = Text.terms query |> Set.ofList

        terms |> List.exists (fun term -> queryTerms.Contains(term.ToLowerInvariant()))

    let private queryHasAnyPhrase (phrases: string list) (query: string) =
        let lower = query.ToLowerInvariant()

        phrases |> List.exists (fun phrase -> lower.Contains(phrase))

    let private needsSourceCoverage (query: string) (queryType: QueryType) (sourceCount: int) =
        sourceCount > 1
        && (queryType = QueryType.SectionRetrieval
            || queryHasAnyTerm sourceCoverageTerms query
            || queryHasAnyPhrase representativePhrases query)

    let private needsRepresentativeContext (query: string) (queryType: QueryType) (sourceCount: int) =
        sourceCount > 0
        && (queryType = QueryType.SectionRetrieval
            || queryHasAnyTerm (sourceCoverageTerms @ representativeTerms) query
            || queryHasAnyPhrase representativePhrases query)

    let private firstChunkForSource (source: KnowledgeSource) (chunks: SourceChunk list) =
        chunks
        |> List.filter (fun chunk -> sourceEquals chunk.source source)
        |> List.sortBy (fun chunk -> chunk.index)
        |> List.tryHead

    let private representativeChunks maxResults (retrieval: RetrievalIndex) =
        let sources = retrieval.sources |> List.filter _.enabled

        let chunksBySource =
            sources
            |> List.map (fun source ->
                retrieval.chunks
                |> List.filter (fun chunk -> sourceEquals chunk.source source)
                |> List.sortBy (fun chunk -> chunk.index))

        let maxDepth = chunksBySource |> List.map List.length |> List.fold max 0

        [ for depth in 0 .. maxDepth - 1 do
              for chunks in chunksBySource do
                  match chunks |> List.tryItem depth with
                  | Some chunk ->
                      yield
                          { chunk with
                              score = max chunk.score 0.01f }
                  | None -> () ]
        |> List.truncate maxResults

    let private balanceBySource
        (query: string)
        (queryType: QueryType)
        (maxResults: int)
        (retrieval: RetrievalIndex)
        (ranked: SourceChunk list)
        =
        let sources = retrieval.sources |> List.filter _.enabled
        let ranked = distinctChunks ranked

        let representatives () =
            representativeChunks maxResults retrieval

        if maxResults <= 0 then
            []
        elif List.isEmpty ranked && needsRepresentativeContext query queryType sources.Length then
            representatives ()
        elif not (needsSourceCoverage query queryType sources.Length) then
            ranked |> List.truncate maxResults
        else
            let coverage =
                sources
                |> List.choose (fun source ->
                    ranked
                    |> List.tryFind (fun chunk -> sourceEquals chunk.source source)
                    |> Option.orElse (firstChunkForSource source retrieval.chunks))
                |> List.truncate maxResults

            coverage @ ranked @ representatives ()
            |> distinctChunks
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
            let processed = Text.QueryPostProcessing.forVoiceLikeRetrieval query
            let target = tryExtractRetrievalTarget query

            let queryType =
                if Option.isSome target then
                    QueryType.SectionRetrieval
                else
                    QueryType.Question

            let rewrittenQueries =
                match target with
                | Some value -> [ yield value; yield $"section {value}"; yield! processed.rewrittenQueries ]
                | None -> processed.rewrittenQueries

            let baseTerms =
                match target with
                | Some value -> compactTerms [ value; $"section {value}" ]
                | None -> compactTerms [ query ]

            Some
                { terms =
                    Seq.append processed.searchTerms baseTerms
                    |> distinctNonEmpty
                    |> List.truncate 32
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
        (queryExpansionClient: IChatClient option)
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
                match queryExpansionClient, useLexicalFilter with
                | Some client, true ->
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
                |> balanceBySource query queryType maxResults retrieval

            match retrieval.encoder, retrieval.colbertIndices with
            | Some encoder, indices when not (List.isEmpty indices) ->
                try
                    let denseWeight, lexicalWeight = getSearchWeights queryType

                    let rawMaxResults =
                        match queryType with
                        | QueryType.SectionRetrieval -> max maxResults 20
                        | _ -> maxResults

                    let options =
                        let tunedCandidateLimit = max FsColbert.SearchOptions.defaults.candidateLimit 256

                        { FsColbert.SearchOptions.defaults with
                            maxResults = rawMaxResults
                            candidateLimit = tunedCandidateLimit
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
                        |> Array.toList
                        |> applySectionBoost sectionName
                        |> balanceBySource query queryType maxResults retrieval

                    if logChunks then
                        for chunk in context do
                            report $"Retrieved chunk: [{chunk.source.DisplayName}] {chunk.text}"

                    return context
                with _ ->
                    return lexicalFallback ()
            | _ -> return lexicalFallback ()
        }

    let private enabledSources (sources: KnowledgeSource list) =
        sources |> List.filter (fun source -> source.enabled)

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

    let private fsColbertRoot storageRoot =
        let path = Path.Combine(storageRoot, "FsKame", "FsColbert")
        Directory.CreateDirectory path |> ignore
        path

    let private modelFolder storageRoot =
        Path.Combine(fsColbertRoot storageRoot, "Models", "mxbai-edge-colbert")

    let private indexFolder storageRoot =
        let path = Path.Combine(fsColbertRoot storageRoot, "Indexes")
        Directory.CreateDirectory path |> ignore
        path

    let private prebuiltFolder storageRoot =
        let path = Path.Combine(fsColbertRoot storageRoot, "Prebuilt")
        Directory.CreateDirectory path |> ignore
        path

    let private prebuiltManifestPath storageRoot =
        Path.Combine(prebuiltFolder storageRoot, "prebuilt-indexes.installed.json")

    let clearPersistedIndexes storageRoot =
        async {
            let files =
                Directory.EnumerateFiles(indexFolder storageRoot, "*.fsci") |> Seq.toList

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

    type private PassageKeywordItem =
        { passageIndex: int
          keywords: string list }

    type private PassageKeywordBatch = { items: PassageKeywordItem list }

    type private KeywordBatchGeneration =
        | GeneratedKeywords of PassageKeywordItem list
        | RejectedKeywordSchema of string

    type private KeywordCacheRecord =
        { key: string
          sourceFingerprint: string
          passageIndex: int
          textHash: string
          modelId: string
          schemaVersion: string
          keywords: string list }

    let private keywordCacheFolder storageRoot =
        let path = Path.Combine(fsColbertRoot storageRoot, "KeywordCache")
        Directory.CreateDirectory path |> ignore
        path

    let private keywordCachePath storageRoot sourceFingerprint modelId schemaVersion =
        let fileName =
            [ sourceFingerprint; modelId; schemaVersion ] |> String.concat "\n" |> hashText

        Path.Combine(keywordCacheFolder storageRoot, $"{fileName}.jsonl")

    let private sanitizeKeywordOptions (options: KeywordGenerationOptions) : KeywordGenerationOptions =
        { options with
            batchSize = max 1 options.batchSize
            parallelism = max 1 options.parallelism
            maxOutputTokens = max 1024 options.maxOutputTokens
            modelId =
                options.modelId
                |> Text.notEmpty
                |> Option.defaultValue KeywordGenerationOptions.defaults.modelId
            schemaVersion =
                options.schemaVersion
                |> Text.notEmpty
                |> Option.defaultValue KeywordGenerationOptions.defaults.schemaVersion }

    let private keywordCacheKey sourceFingerprint (options: KeywordGenerationOptions) passageIndex textHash =
        [ sourceFingerprint
          string passageIndex
          textHash
          options.modelId
          options.schemaVersion ]
        |> String.concat "\n"
        |> hashText

    let private cleanKeywords maxValues (values: string list) =
        values
        |> List.choose (Text.normalizeWhitespace >> Text.notEmpty)
        |> List.distinctBy _.ToLowerInvariant()
        |> List.truncate maxValues

    let private strictStructuredOutputKey = "strict"

    let private strictJsonSchemaResponseFormat (schema: string) (name: string) (description: string) =
        use document = JsonDocument.Parse schema

        ChatResponseFormat.ForJsonSchema(document.RootElement.Clone(), name, description)

    let private applyStrictStructuredOutput (options: ChatOptions) =
        if isNull options.AdditionalProperties then
            options.AdditionalProperties <- AdditionalPropertiesDictionary()

        options.AdditionalProperties[strictStructuredOutputKey] <- box true

    let private exceptionDetails (ex: exn) =
        let rec collect (current: exn) =
            seq {
                if not (isNull current) then
                    yield current.GetType().FullName
                    yield current.Message
                    yield! collect current.InnerException
            }

        collect ex |> Seq.choose Text.notEmpty |> String.concat "\n"

    let private isInvalidJsonSchemaError (ex: exn) =
        let details = exceptionDetails ex |> fun value -> value.ToLowerInvariant()

        details.Contains("invalid_json_schema")
        || ((details.Contains("http 400")
             || details.Contains("status 400")
             || details.Contains("bad request")
             || details.Contains("invalid request"))
            && (details.Contains("json_schema")
                || details.Contains("response_format")
                || details.Contains("structured output")
                || details.Contains("schema")))

    let private tryReadStringProperty (name: string) (root: JsonElement) =
        let mutable property = Unchecked.defaultof<JsonElement>

        if
            root.TryGetProperty(name, &property)
            && property.ValueKind = JsonValueKind.String
        then
            property.GetString() |> Text.notEmpty
        else
            None

    let private tryReadIntProperty (name: string) (root: JsonElement) =
        let mutable property = Unchecked.defaultof<JsonElement>

        if
            root.TryGetProperty(name, &property)
            && property.ValueKind = JsonValueKind.Number
        then
            match property.TryGetInt32() with
            | true, value -> Some value
            | _ -> None
        else
            None

    let private tryReadStringListProperty (name: string) (root: JsonElement) =
        let mutable property = Unchecked.defaultof<JsonElement>

        if root.TryGetProperty(name, &property) && property.ValueKind = JsonValueKind.Array then
            property.EnumerateArray()
            |> Seq.choose (fun item ->
                if item.ValueKind = JsonValueKind.String then
                    item.GetString() |> Text.notEmpty
                else
                    None)
            |> Seq.toList
            |> Some
        else
            None

    let private tryReadKeywordCacheRecord (line: string) =
        try
            use document = JsonDocument.Parse line
            let root = document.RootElement

            match
                tryReadStringProperty "key" root,
                tryReadStringProperty "sourceFingerprint" root,
                tryReadIntProperty "passageIndex" root,
                tryReadStringProperty "textHash" root,
                tryReadStringProperty "modelId" root,
                tryReadStringProperty "schemaVersion" root,
                tryReadStringListProperty "keywords" root
            with
            | Some key,
              Some sourceFingerprint,
              Some passageIndex,
              Some textHash,
              Some modelId,
              Some schemaVersion,
              Some keywords ->
                Some
                    { key = key
                      sourceFingerprint = sourceFingerprint
                      passageIndex = passageIndex
                      textHash = textHash
                      modelId = modelId
                      schemaVersion = schemaVersion
                      keywords = cleanKeywords 16 keywords }
            | _ -> None
        with _ ->
            None

    let private loadKeywordCache path sourceFingerprint (options: KeywordGenerationOptions) =
        if File.Exists path then
            File.ReadLines path
            |> Seq.choose tryReadKeywordCacheRecord
            |> Seq.filter (fun record ->
                record.sourceFingerprint = sourceFingerprint
                && record.modelId = options.modelId
                && record.schemaVersion = options.schemaVersion)
            |> Seq.map (fun record -> record.key, record)
            |> Map.ofSeq
        else
            Map.empty

    let private appendKeywordCache path (records: KeywordCacheRecord list) =
        if not (List.isEmpty records) then
            Directory.CreateDirectory(Path.GetDirectoryName(Path.GetFullPath path))
            |> ignore

            use writer = new StreamWriter(path, true, Encoding.UTF8)

            for record in records do
                let line =
                    JsonSerializer.Serialize(
                        {| key = record.key
                           sourceFingerprint = record.sourceFingerprint
                           passageIndex = record.passageIndex
                           textHash = record.textHash
                           modelId = record.modelId
                           schemaVersion = record.schemaVersion
                           keywords = record.keywords |}
                    )

                writer.WriteLine line

    let private promptPassageText (text: string) =
        let normalized = Text.normalizeWhitespace text

        if normalized.Length <= 1000 then
            normalized
        else
            normalized.Substring(0, 1000)

    let private keywordPrompt (batch: FsColbert.PassageRef list) =
        let payload =
            batch
            |> List.map (fun passage ->
                {| passageIndex = passage.index
                   text = promptPassageText passage.text |})
            |> fun items -> JsonSerializer.Serialize items

        $"""
Create compact lexical search keywords for customer-support retrieval passages.

Return only a JSON object with one property, "items", whose value is an array.
Each item must have:
- passageIndex: the same integer passageIndex.
- keywords: 6-14 short phrases, synonyms, abbreviations, product names, coverage terms, entities, and likely customer wording.

Rules:
- Use only facts supported by the passage text.
- Prefer terms that may be useful for exact lexical search and may not appear literally in the passage.
- Do not include markdown, explanations, answer prose, or fields other than passageIndex and keywords.

Passages:
{payload}
"""

    let private keywordResponseFormat () =
        let schema =
            """
{
  "type": "object",
  "additionalProperties": false,
  "properties": {
    "items": {
      "type": "array",
      "items": {
        "type": "object",
        "additionalProperties": false,
        "properties": {
          "passageIndex": { "type": "integer" },
          "keywords": {
            "type": "array",
            "items": { "type": "string" }
          }
        },
        "required": [ "passageIndex", "keywords" ]
      }
    }
  },
  "required": [ "items" ]
}
"""

        strictJsonSchemaResponseFormat schema "passage_keyword_batch" "Keyword metadata for a batch of passages."

    let private generateKeywordBatch
        (client: IChatClient)
        (options: KeywordGenerationOptions)
        (batch: FsColbert.PassageRef list)
        =
        async {
            let opts = ChatOptions()
            opts.MaxOutputTokens <- Nullable options.maxOutputTokens
            opts.ResponseFormat <- keywordResponseFormat ()
            applyStrictStructuredOutput opts

            let reasoning = ReasoningOptions()
            reasoning.Effort <- Nullable ReasoningEffort.Low
            reasoning.Output <- Nullable ReasoningOutput.None
            opts.Reasoning <- reasoning

            let! response =
                client.GetResponseAsync<PassageKeywordBatch>(keywordPrompt batch, opts)
                |> Async.AwaitTask

            let requested = batch |> List.map _.index |> Set.ofList

            let items = response.Result.items |> Option.ofObj |> Option.defaultValue []

            return
                items
                |> List.filter (fun item -> Set.contains item.passageIndex requested)
                |> List.map (fun item ->
                    { item with
                        keywords = cleanKeywords 16 item.keywords })
                |> List.filter (fun item -> not (List.isEmpty item.keywords))
                |> List.distinctBy _.passageIndex
        }

    let rec private generateKeywordBatchRecover report client options batch =
        async {
            try
                let! generated = generateKeywordBatch client options batch
                let generatedIndexes = generated |> List.map _.passageIndex |> Set.ofList

                let missing =
                    batch
                    |> List.filter (fun passage -> not (Set.contains passage.index generatedIndexes))

                if List.isEmpty missing || batch.Length <= 1 then
                    return GeneratedKeywords generated
                else
                    let! recovered =
                        missing
                        |> List.map (fun passage -> generateKeywordBatchRecover report client options [ passage ])
                        |> Async.Parallel

                    match
                        recovered
                        |> Array.tryPick (function
                            | RejectedKeywordSchema reason -> Some reason
                            | _ -> None)
                    with
                    | Some reason -> return RejectedKeywordSchema reason
                    | None ->
                        let recoveredKeywords =
                            recovered
                            |> Array.collect (function
                                | GeneratedKeywords items -> List.toArray items
                                | RejectedKeywordSchema _ -> [||])
                            |> Array.toList

                        return GeneratedKeywords(generated @ recoveredKeywords)
            with ex ->
                if isInvalidJsonSchemaError ex then
                    return RejectedKeywordSchema(ex.Message)
                elif batch.Length <= 1 then
                    match batch with
                    | passage :: _ ->
                        report
                            $"Keyword generation failed for {passage.sourceDisplayName} chunk {passage.index}: {ex.Message}"
                    | [] -> ()

                    return GeneratedKeywords []
                else
                    let half = max 1 (batch.Length / 2)
                    let left, right = batch |> List.splitAt half

                    let! recovered =
                        [ generateKeywordBatchRecover report client options left
                          generateKeywordBatchRecover report client options right ]
                        |> Async.Parallel

                    match
                        recovered
                        |> Array.tryPick (function
                            | RejectedKeywordSchema reason -> Some reason
                            | _ -> None)
                    with
                    | Some reason -> return RejectedKeywordSchema reason
                    | None ->
                        return
                            recovered
                            |> Array.collect (function
                                | GeneratedKeywords items -> List.toArray items
                                | RejectedKeywordSchema _ -> [||])
                            |> Array.toList
                            |> GeneratedKeywords
        }

    let private mapAsyncThrottled degree work items =
        async {
            use semaphore = new SemaphoreSlim(max 1 degree)

            let run item =
                async {
                    do! semaphore.WaitAsync() |> Async.AwaitTask

                    try
                        return! work item
                    finally
                        semaphore.Release() |> ignore
                }

            let! results = items |> List.map run |> Async.Parallel
            return results |> Array.toList
        }

    let private keywordMetadataFingerprint (options: KeywordGenerationOptions) (passages: FsColbert.PassageRef list) =
        let metadata =
            passages
            |> List.map (fun passage ->
                {| passageIndex = passage.index
                   textHash = hashText passage.text
                   keywordsHash = passage.keywords |> String.concat "\n" |> hashText |})
            |> fun items -> JsonSerializer.Serialize items

        [ $"keywords=enabled"
          $"keywordModel={options.modelId}"
          $"keywordSchema={options.schemaVersion}"
          $"keywordMetadataHash={hashText metadata}"
          $"tfidfTextWeight={FsColbert.TfidfOptions.defaults.textWeight}"
          $"tfidfKeywordWeight={FsColbert.TfidfOptions.defaults.keywordWeight}" ]
        |> String.concat "\n"

    let private disabledKeywordFingerprint =
        [ "keywords=disabled"
          $"tfidfTextWeight={FsColbert.TfidfOptions.defaults.textWeight}"
          $"tfidfKeywordWeight={FsColbert.TfidfOptions.defaults.keywordWeight}" ]
        |> String.concat "\n"

    let internal attachKeywords
        (storageRoot: string)
        (report: string -> unit)
        (keywordOptions: KeywordGenerationOptions)
        (source: KnowledgeSource)
        (passages: FsColbert.PassageRef list)
        =
        async {
            let options = sanitizeKeywordOptions keywordOptions

            if not options.enabled then
                return passages, disabledKeywordFingerprint
            else
                let sourceFingerprintValue = sourceFingerprint source

                let cachePath =
                    keywordCachePath storageRoot sourceFingerprintValue options.modelId options.schemaVersion

                let cached = loadKeywordCache cachePath sourceFingerprintValue options

                let passageKeys =
                    passages
                    |> List.map (fun passage ->
                        let textHash = hashText passage.text

                        passage, textHash, keywordCacheKey sourceFingerprintValue options passage.index textHash)

                let missing =
                    passageKeys
                    |> List.choose (fun (passage, textHash, key) ->
                        if Map.containsKey key cached then
                            None
                        else
                            Some(passage, textHash, key))

                let! generatedRecords =
                    match missing, options.client with
                    | [], _ -> async.Return []
                    | _, None ->
                        report
                            $"Keyword cache is missing {missing.Length}/{passages.Length} passage(s) for {source.DisplayName}; using cached keyword metadata only."

                        async.Return []
                    | _, Some client ->
                        async {
                            report
                                $"Generating keyword metadata for {missing.Length}/{passages.Length} passage(s) in {source.DisplayName}."

                            let batches =
                                missing
                                |> List.map (fun (passage, _, _) -> passage)
                                |> List.chunkBySize options.batchSize

                            let! generated =
                                mapAsyncThrottled
                                    options.parallelism
                                    (generateKeywordBatchRecover report client options)
                                    batches

                            let schemaRejected =
                                generated
                                |> List.exists (function
                                    | RejectedKeywordSchema _ -> true
                                    | GeneratedKeywords _ -> false)

                            if schemaRejected then
                                report
                                    "Keyword schema was rejected by OpenAI; indexing will continue without generated keywords."

                            let generatedByIndex =
                                generated
                                |> List.collect (function
                                    | GeneratedKeywords items -> items
                                    | RejectedKeywordSchema _ -> [])
                                |> List.map (fun item -> item.passageIndex, item.keywords)
                                |> Map.ofList

                            return
                                missing
                                |> List.choose (fun (passage, textHash, key) ->
                                    generatedByIndex
                                    |> Map.tryFind passage.index
                                    |> Option.map (fun keywords ->
                                        { key = key
                                          sourceFingerprint = sourceFingerprintValue
                                          passageIndex = passage.index
                                          textHash = textHash
                                          modelId = options.modelId
                                          schemaVersion = options.schemaVersion
                                          keywords = keywords }))
                        }

                appendKeywordCache cachePath generatedRecords

                let allRecords =
                    generatedRecords
                    |> List.fold (fun records record -> Map.add record.key record records) cached

                let enriched =
                    passageKeys
                    |> List.map (fun (passage, _, key) ->
                        let keywords =
                            allRecords |> Map.tryFind key |> Option.map _.keywords |> Option.defaultValue []

                        { passage with keywords = keywords })

                return enriched, keywordMetadataFingerprint options enriched
        }

    let private sourceIndexPath storageRoot (source: KnowledgeSource) keywordFingerprint =
        let options = FsColbert.ChunkOptions.fsKameDefaults

        let fingerprint =
            [ yield $"model={FsColbert.ModelCatalog.mxbaiEdgeColbertInt8.id}"
              yield $"pdfIndexVersion={pdfIndexVersion}"
              yield $"chunk={options.maxChars}:{options.overlapChars}:{options.minChars}"
              yield keywordFingerprint
              yield sourceFingerprint source ]
            |> String.concat "\n"

        Path.Combine(indexFolder storageRoot, $"{hashText fingerprint}.fsci")

    let private indexPath storageRoot sources =
        let options = FsColbert.ChunkOptions.fsKameDefaults

        let fingerprint =
            [ yield $"model={FsColbert.ModelCatalog.mxbaiEdgeColbertInt8.id}"
              yield $"pdfIndexVersion={pdfIndexVersion}"
              yield $"chunk={options.maxChars}:{options.overlapChars}:{options.minChars}"
              yield! (sources |> List.map sourceFingerprint |> List.sort) ]
            |> String.concat "\n"

        Path.Combine(indexFolder storageRoot, $"{hashText fingerprint}.fsci")

    let private tryLoadPersistedIndex path = FsColbert.IndexPersistence.tryLoad path

    let private tryLoadPrebuiltManifest storageRoot =
        try
            let path = prebuiltManifestPath storageRoot

            if File.Exists path then
                JsonSerializer.Deserialize<InstalledPrebuiltIndex array>(File.ReadAllText path)
                |> Option.ofObj
                |> Option.map Array.toList
                |> Option.defaultValue []
            else
                []
        with _ ->
            []

    let private bindIndexToSource (source: KnowledgeSource) (index: FsColbert.ColbertIndex) =
        let passages =
            index.passages
            |> List.map (fun passage ->
                { passage with
                    reference =
                        { passage.reference with
                            sourceId = source.location
                            sourceDisplayName = source.DisplayName
                            sourceLocation = source.location } })

        { index with passages = passages }

    let private tryLoadPrebuiltIndex storageRoot (source: KnowledgeSource) =
        tryLoadPrebuiltManifest storageRoot
        |> List.tryFind (fun item ->
            String.Equals(item.storedPath, source.location, StringComparison.OrdinalIgnoreCase)
            && File.Exists item.indexPath)
        |> function
            | None -> Ok None
            | Some item ->
                match tryLoadPersistedIndex item.indexPath with
                | Ok(Some index) -> Ok(Some(bindIndexToSource source index))
                | Ok None -> Ok None
                | Error err -> Error err

    let private loadEncoder storageRoot =
        async {
            match cachedEncoder with
            | Some e -> return e
            | None ->
                use client = new HttpClient()

                let! files =
                    FsColbert.ModelCatalog.ensureDownloadedAsync
                        client
                        (modelFolder storageRoot)
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

    let InindexSource storageRoot report keywordOptions (source: KnowledgeSource) =
        async {
            match tryLoadPrebuiltIndex storageRoot source with
            | Ok(Some _) -> report $"Prebuilt FsColbert index is available for {source.DisplayName}."
            | _ ->
                let! result = sourcePassages source

                match result with
                | Error _ -> ()
                | Ok passages ->
                    let! passages, keywordFingerprint = attachKeywords storageRoot report keywordOptions source passages

                    let path = sourceIndexPath storageRoot source keywordFingerprint

                    match tryLoadPersistedIndex path with
                    | Ok(Some _) -> () // already indexed
                    | _ ->
                        report $"Preparing FsColbert model for {source.DisplayName}."
                        let! encoder = loadEncoder storageRoot
                        report $"Building FsColbert index for {source.DisplayName}."
                        let! _ = buildIndexFromChunks report encoder passages path
                        ()
        }

    let loadIndex
        storageRoot
        report
        (keywordOptions: KeywordGenerationOptions)
        (sources: KnowledgeSource list)
        : Async<RetrievalIndex * string list> =
        async {
            let sources = enabledSources sources

            if List.isEmpty sources then
                return { emptyIndex with sources = sources }, []
            else
                let! encoder = loadEncoder storageRoot
                let indices = ResizeArray<KnowledgeSource * FsColbert.ColbertIndex>()
                let errors = ResizeArray<string>()

                for source in sources do
                    match tryLoadPrebuiltIndex storageRoot source with
                    | Ok(Some index) ->
                        report $"Loaded prebuilt FsColbert index for {source.DisplayName}."
                        indices.Add(source, index)
                    | Error err -> errors.Add err
                    | Ok None ->
                        let! result = sourcePassages source

                        match result with
                        | Error err -> errors.Add err
                        | Ok passages ->
                            let! passages, keywordFingerprint =
                                attachKeywords storageRoot report keywordOptions source passages

                            let path = sourceIndexPath storageRoot source keywordFingerprint

                            match tryLoadPersistedIndex path with
                            | Ok(Some index) -> indices.Add(source, index)
                            | Ok None ->
                                // If not found, we should build it, but normally it should have been built during processing
                                report $"Building missing FsColbert index for {source.DisplayName}."
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

    let private renderedChunkMaxChars = 650

    let renderContext (chunks: SourceChunk list) =
        if List.isEmpty chunks then
            "No selected document context was available."
        else
            chunks
            |> List.truncate QaDefaults.maxContextChunks
            |> List.mapi (fun index (chunk: SourceChunk) ->
                let body = Text.truncate renderedChunkMaxChars chunk.text
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
