namespace FsKame.Cli

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.IO.Compression
open System.Net.Http
open System.Text.Json
open System.Text.Json.Serialization
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks
open FsKame.QA
open Microsoft.Extensions.AI

module Program =
    type ParsedArgs =
        { command: string
          options: Map<string, string list>
          flags: Set<string> }

    type InsuranceQuestion =
        { category: string
          question: string
          answerLabels: int list
          candidateLabels: int list }

    type SearchMetadata =
        { keywords: string list
          questions: string list }

    type private JudgeVerdict =
        { verdict: string option
          score: float option
          reason: string option }

    let jsonOptions =
        let opts = JsonSerializerOptions(WriteIndented = true)
        opts.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase
        opts.NumberHandling <- JsonNumberHandling.AllowReadingFromString
        opts.Converters.Add(JsonFSharpConverter())
        opts

    let jsonLineOptions =
        let opts = JsonSerializerOptions()
        opts.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase
        opts.NumberHandling <- JsonNumberHandling.AllowReadingFromString
        opts.Converters.Add(JsonFSharpConverter())
        opts

    let private tryDeserialize<'T> (text: string) =
        try
            JsonSerializer.Deserialize<'T>(text, jsonOptions) |> Some
        with _ ->
            None

    let usage =
        """
FsKame.Cli

Commands:
  ask --question "..." --source path [--source path] [--use-case-profile path] [--json]
  insuranceqa-eval [--sample 30] [--data-dir temp/insuranceqa] [--retrieval internal|fscolbert] [--no-judge]
  insuranceqa-search-eval [--sample 30] [--data-dir temp/insuranceqa] [--retrieval internal|fscolbert]
  insuranceqa-elaborate-index [--data-dir temp/insuranceqa] [--small-model gpt-5-nano]

Common options:
  --api-key value          Defaults to OPENAI_API_KEY.
  --answer-model value     Defaults to gpt-5.5.
  --small-model value      Defaults to gpt-5-nano.
  --storage-root path      Defaults to temp/fskame-cli.
  --index-path path        Optional persisted FsColbert .fsci file for search eval.
  --answers-source path    Optional JSON/Markdown InsuranceQA answer source to index/evaluate.
  --answers-markdown path  Back-compat alias for --answers-source.
  --use-case-profile path  Optional QA use-case profile JSON for domain terms and prompts.
  --label-list-path path   Optional newline-delimited InsuranceQA answer labels for index elaboration.
  --llm-query-expansion    Use the configured small model to add retrieval terms in search eval.
  --expansion-replay-path path  Replay saved LLM expansion JSONL for fusion tests.
  --fusion-mode value      Search eval fusion mode: standard|direct-local|direct-llm|rrf|tail.
  --candidate-limit value  Search eval candidate pool size for direct/fusion modes. Defaults to 256.
  --no-index-elaboration  Do not generate missing index-time keyword metadata with the small model.
"""

    let parseArgs (argv: string array) =
        let command =
            argv
            |> Array.tryFind (fun value -> not (value.StartsWith("--")))
            |> Option.defaultValue "help"

        let rec loop index options flags =
            if index >= argv.Length then
                { command = command
                  options = options
                  flags = flags }
            else
                let token = argv[index]

                if not (token.StartsWith("--")) then
                    loop (index + 1) options flags
                else
                    let key = token.Substring(2)

                    if index + 1 < argv.Length && not (argv[index + 1].StartsWith("--")) then
                        let existing = options |> Map.tryFind key |> Option.defaultValue []
                        loop (index + 2) (options |> Map.add key (existing @ [ argv[index + 1] ])) flags
                    else
                        loop (index + 1) options (flags |> Set.add key)

        loop 0 Map.empty Set.empty

    let optionValues name parsed =
        parsed.options |> Map.tryFind name |> Option.defaultValue []

    let optionValue name fallback parsed =
        optionValues name parsed |> List.tryLast |> Option.defaultValue fallback

    let optionValueAny names fallback parsed =
        names
        |> List.tryPick (fun name -> optionValues name parsed |> List.tryLast)
        |> Option.defaultValue fallback

    let hasFlag name parsed = parsed.flags.Contains name

    let apiKey parsed =
        let fromArg = optionValues "api-key" parsed |> List.tryLast

        match fromArg with
        | Some value when not (String.IsNullOrWhiteSpace value) -> Some value
        | _ ->
            Environment.GetEnvironmentVariable("OPENAI_API_KEY")
            |> Option.ofObj
            |> Option.bind Text.notEmpty

    let createClient (key: string) (modelId: string) : IChatClient =
        let client = OpenAI.OpenAIClient(key)
        client.GetResponsesClient().AsIChatClient(modelId)

    let clientsFromArgs parsed =
        match apiKey parsed with
        | None -> QaModelClients.none
        | Some key ->
            let smallModel = optionValue "small-model" QaDefaults.nanoModel parsed
            let answerModel = optionValue "answer-model" QaDefaults.answerModel parsed
            let small = createClient key smallModel
            let answer = createClient key answerModel

            { queryExpansion = Some small
              toolPlanner = Some small
              answerGenerator = Some answer }

    let retrievalMode parsed =
        match
            optionValue "retrieval" "fscolbert" parsed
            |> fun v -> v.Trim().ToLowerInvariant()
        with
        | "internal"
        | "lexical" -> InternalDocumentIndex
        | _ -> FsColbertWithFallback

    let sourceKind (path: string) =
        match Path.GetExtension(path).TrimStart('.').ToLowerInvariant() with
        | "pdf" -> KnowledgeSourceKind.Pdf
        | "json" -> KnowledgeSourceKind.Json
        | _ -> KnowledgeSourceKind.Markdown

    let sourceFromPath path =
        { kind = sourceKind path
          location = Path.GetFullPath path
          enabled = true }

    let storageRoot parsed =
        optionValue "storage-root" (Path.Combine("temp", "fskame-cli")) parsed
        |> Path.GetFullPath

    let private insuranceQaUseCaseProfilePath () =
        Path.Combine("data", "qa-use-cases", "insuranceqa.json") |> Path.GetFullPath

    let useCaseProfile parsed =
        let defaultProfile =
            if parsed.command.StartsWith("insuranceqa", StringComparison.OrdinalIgnoreCase) then
                Some(insuranceQaUseCaseProfilePath ())
            else
                None

        optionValues "use-case-profile" parsed
        |> List.tryLast
        |> Option.orElse defaultProfile
        |> Option.bind QaUseCaseProfile.tryLoad
        |> Option.defaultValue QaUseCaseProfile.generic

    let createSession parsed autoWriteback =
        let storageRoot = storageRoot parsed

        Directory.CreateDirectory storageRoot |> ignore

        let clients =
            let clients = clientsFromArgs parsed

            if hasFlag "no-tool-planner" parsed then
                { clients with toolPlanner = None }
            else
                clients

        let options =
            { QaSessionOptions.create storageRoot with
                retrievalMode = retrievalMode parsed
                clients = clients
                useCaseProfile = useCaseProfile parsed
                answerModelId = optionValue "answer-model" QaDefaults.answerModel parsed
                keywordModelId = optionValue "small-model" QaDefaults.nanoModel parsed
                elaborateIndexKeywords = not (hasFlag "no-index-elaboration" parsed)
                enableQueryExpansion = hasFlag "llm-query-expansion" parsed
                toolProviderDirectory = optionValues "tool-dir" parsed |> List.tryLast
                autoWriteback = autoWriteback
                report = fun msg -> Console.Error.WriteLine msg }

        new QaSession(options)

    let printJson value =
        JsonSerializer.Serialize(value, jsonOptions) |> printfn "%s"

    let sourceKindName =
        function
        | KnowledgeSourceKind.Pdf -> "pdf"
        | KnowledgeSourceKind.Markdown -> "markdown"
        | KnowledgeSourceKind.Json -> "json"

    let answerJson (answer: QaAnswer) =
        {| turnId = answer.turnId
           answer = answer.answer
           model = answer.model
           timedOut = answer.timedOut
           sourceRetrievalElapsedMs = answer.sourceRetrievalElapsedMs
           createdAt = answer.createdAt
           inventory =
            answer.inventory
            |> List.map (fun source ->
                {| kind = sourceKindName source.kind
                   location = source.location
                   enabled = source.enabled
                   displayName = source.DisplayName |})
           context =
            answer.context
            |> List.map (fun chunk ->
                {| source =
                    {| kind = sourceKindName chunk.source.kind
                       location = chunk.source.location
                       enabled = chunk.source.enabled
                       displayName = chunk.source.DisplayName |}
                   index = chunk.index
                   score = chunk.score
                   text = chunk.text |})
           toolObservations =
            answer.toolObservations
            |> List.map (fun observation ->
                {| pluginName = observation.pluginName
                   toolName = observation.toolName
                   query = observation.query
                   content = observation.content
                   createdAt = observation.createdAt |}) |}

    let runAsk parsed =
        task {
            let question = optionValue "question" "" parsed |> Text.normalizeWhitespace
            let sources = optionValues "source" parsed |> List.map sourceFromPath

            if String.IsNullOrWhiteSpace question then
                Console.Error.WriteLine "Missing --question."
                return 2
            else
                use session = createSession parsed true
                let! _ = session.LoadSourcesAsync(retrievalMode parsed, sources, CancellationToken.None)

                let request =
                    { turnId = Guid.NewGuid().ToString("N")
                      question = question
                      realtimeJudgement = None
                      deadline = None }

                let! answer = session.AnswerAsync(request, CancellationToken.None)

                if hasFlag "json" parsed then
                    printJson (answerJson answer)
                else
                    printfn "%s" answer.answer

                    if not (List.isEmpty answer.context) then
                        printfn "\nContext:"

                        answer.context
                        |> List.truncate 5
                        |> List.iter (fun chunk ->
                            printfn "- %s chunk %d score %.3f" chunk.source.DisplayName chunk.index chunk.score)

                return 0
        }

    let ensureDownloaded (client: HttpClient) url path =
        task {
            if not (File.Exists path) then
                Directory.CreateDirectory(Path.GetDirectoryName path) |> ignore
                use! stream = client.GetStreamAsync(Uri url)
                use target = File.Create path
                do! stream.CopyToAsync target
        }

    let readGzipLines path =
        seq {
            use file = File.OpenRead path
            use gzip = new GZipStream(file, CompressionMode.Decompress)
            use reader = new StreamReader(gzip)

            while not reader.EndOfStream do
                yield reader.ReadLine()
        }
        |> Seq.toList

    let readLines path = File.ReadLines path |> Seq.toList

    let parseVocab path =
        readLines path
        |> List.choose (fun line ->
            match line.Split('\t') with
            | parts when parts.Length >= 2 -> Some(parts[0], parts[1])
            | _ -> None)
        |> Map.ofList

    let decodeTokens (vocab: Map<string, string>) (encoded: string) =
        encoded.Split(' ', StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun token -> vocab |> Map.tryFind token |> Option.defaultValue token)
        |> String.concat " "
        |> Text.normalizeWhitespace

    let parseInts (value: string) =
        value.Split(' ', StringSplitOptions.RemoveEmptyEntries)
        |> Array.choose (fun token ->
            match Int32.TryParse token with
            | true, parsed -> Some parsed
            | false, _ -> None)
        |> Array.toList

    let private emptySearchMetadata = { keywords = []; questions = [] }

    let private writeInsuranceQaJsonSource (answers: Map<int, string>) (metadata: Map<int, SearchMetadata>) path =
        Directory.CreateDirectory(Path.GetDirectoryName(Path.GetFullPath path))
        |> ignore

        let documents =
            answers
            |> Map.toList
            |> List.map (fun (label, answer) ->
                let itemMetadata =
                    metadata |> Map.tryFind label |> Option.defaultValue emptySearchMetadata

                {| id = string label
                   title = $"Answer label: {label}"
                   text = answer
                   keywords =
                    seq {
                        yield! itemMetadata.keywords
                        yield! itemMetadata.questions
                    }
                    |> Seq.distinctBy _.ToLowerInvariant()
                    |> Seq.toList
                   questions = itemMetadata.questions |})

        let source =
            {| id = "insuranceqa-answers"
               title = "InsuranceQA Answers"
               documents = documents |}

        File.WriteAllText(path, JsonSerializer.Serialize(source, jsonOptions))

    let loadInsuranceQa dataDir =
        task {
            let dataDir = Path.GetFullPath dataDir
            Directory.CreateDirectory dataDir |> ignore

            let baseUrl = "https://raw.githubusercontent.com/shuzi/insuranceQA/master/V2"
            let vocabPath = Path.Combine(dataDir, "vocabulary")
            let answersPath = Path.Combine(dataDir, "InsuranceQA.label2answer.raw.encoded.gz")

            let testPath =
                Path.Combine(dataDir, "InsuranceQA.question.anslabel.raw.100.pool.solr.test.encoded.gz")

            use client = new HttpClient()
            do! ensureDownloaded client $"{baseUrl}/vocabulary" vocabPath
            do! ensureDownloaded client $"{baseUrl}/InsuranceQA.label2answer.raw.encoded.gz" answersPath

            do!
                ensureDownloaded
                    client
                    $"{baseUrl}/InsuranceQA.question.anslabel.raw.100.pool.solr.test.encoded.gz"
                    testPath

            let vocab = parseVocab vocabPath

            let answers =
                readGzipLines answersPath
                |> List.choose (fun line ->
                    match line.Split('\t') with
                    | parts when parts.Length >= 2 ->
                        match Int32.TryParse parts[0] with
                        | true, label -> Some(label, decodeTokens vocab parts[1])
                        | false, _ -> None
                    | _ -> None)
                |> Map.ofList

            let questions =
                readGzipLines testPath
                |> List.choose (fun line ->
                    match line.Split('\t') with
                    | parts when parts.Length >= 4 ->
                        Some
                            { category = parts[0]
                              question = decodeTokens vocab parts[1]
                              answerLabels = parseInts parts[2]
                              candidateLabels = parseInts parts[3] }
                    | _ -> None)

            let jsonPath = Path.Combine(dataDir, "insuranceqa.answers.json")

            if not (File.Exists jsonPath) then
                writeInsuranceQaJsonSource answers Map.empty jsonPath

            return answers, questions, jsonPath
        }

    let termSet value = Text.terms value |> Set.ofList

    let lexicalF1 generated references =
        let generatedTerms = termSet generated
        let referenceTerms = references |> String.concat " " |> termSet

        if Set.isEmpty generatedTerms || Set.isEmpty referenceTerms then
            0.0
        else
            let overlap = Set.intersect generatedTerms referenceTerms |> Set.count |> float
            let precision = overlap / float generatedTerms.Count
            let recall = overlap / float referenceTerms.Count

            if precision + recall = 0.0 then
                0.0
            else
                2.0 * precision * recall / (precision + recall)

    let contextHit labels (chunks: SourceChunk list) =
        labels
        |> List.exists (fun label ->
            chunks
            |> List.exists (fun chunk ->
                chunk.text.Contains($"Answer label: {label}", StringComparison.OrdinalIgnoreCase)))

    let tryAnswerLabel (text: string) =
        let marker = "Answer label:"
        let index = text.IndexOf(marker, StringComparison.OrdinalIgnoreCase)

        if index < 0 then
            None
        else
            let after = text.Substring(index + marker.Length).TrimStart()

            let digits = after |> Seq.takeWhile Char.IsDigit |> Seq.toArray |> String

            match Int32.TryParse digits with
            | true, label -> Some label
            | false, _ -> None

    let percentile p (values: ResizeArray<float>) =
        if values.Count = 0 then
            0.0
        else
            let sorted = values |> Seq.sort |> Seq.toArray
            let rank = int (Math.Ceiling(p * float sorted.Length)) - 1
            let index = Math.Clamp(rank, 0, sorted.Length - 1)
            sorted[index]

    let judgeAnswer (client: IChatClient option) generated references =
        task {
            match client with
            | None -> return None
            | Some client ->
                let referenceText = String.concat "\n\n" references

                let prompt =
                    [ ChatMessage(
                          ChatRole.System,
                          "You judge whether a generated answer is supported by reference answers. Return JSON only."
                      )
                      ChatMessage(
                          ChatRole.User,
                          $"Reference answer(s):\n{referenceText}\n\nGenerated answer:\n{generated}\n\nReturn JSON {{\"verdict\":\"correct|partial|incorrect\",\"score\":0.0,\"reason\":\"short\"}}"
                      ) ]

                let opts = ChatOptions()
                opts.MaxOutputTokens <- Nullable 220

                try
                    let! response = client.GetResponseAsync(prompt, opts, CancellationToken.None)
                    let text = response.Text
                    let first = text.IndexOf('{')
                    let last = text.LastIndexOf('}')

                    let json =
                        if first >= 0 && last >= first then
                            text.Substring(first, last - first + 1)
                        else
                            text

                    let parsed = tryDeserialize<JudgeVerdict> json

                    let verdict = parsed |> Option.bind _.verdict |> Option.defaultValue "unknown"

                    let score = parsed |> Option.bind _.score |> Option.defaultValue 0.0

                    return Some(verdict, score)
                with _ ->
                    return None
        }

    let loadFsColbertEncoder parsed =
        async {
            use client = new HttpClient()

            let modelFolder = Path.Combine(storageRoot parsed, "FsKame", "FsColbert", "Models")

            let! files =
                FsColbert.ModelCatalog.ensureDownloadedAsync
                    client
                    modelFolder
                    FsColbert.ModelCatalog.mxbaiEdgeColbertInt8

            return FsColbert.OnnxColbertEncoder.Load files
        }

    let loadRetrievalFromIndexPath parsed (source: KnowledgeSource) path =
        async {
            let index = FsColbert.IndexPersistence.load path
            let! encoder = loadFsColbertEncoder parsed

            let chunks =
                index.passages
                |> List.map (fun passage ->
                    { source = source
                      index = passage.reference.index
                      text = passage.reference.text
                      score = 0.0f })

            return
                { KnowledgeSources.emptyIndex with
                    sources = [ source ]
                    chunks = chunks
                    colbertIndices = [ source, index ]
                    encoder = Some encoder },
                []
        }

    let loadInsuranceQaRetrieval parsed profile (source: KnowledgeSource) =
        async {
            let report (msg: string) = Console.Error.WriteLine msg

            match optionValues "index-path" parsed |> List.tryLast with
            | Some path when not (String.IsNullOrWhiteSpace path) ->
                return! loadRetrievalFromIndexPath parsed source (Path.GetFullPath path)
            | _ ->
                match retrievalMode parsed with
                | InternalDocumentIndex ->
                    let! chunks, errors = KnowledgeSources.loadChunks [ source ]

                    return
                        { KnowledgeSources.emptyIndex with
                            sources = [ source ]
                            chunks = chunks },
                        errors
                | FsColbertWithFallback ->
                    Directory.CreateDirectory(storageRoot parsed) |> ignore
                    let clients = clientsFromArgs parsed

                    let keywordOptions =
                        { KnowledgeSources.KeywordGenerationOptions.defaults with
                            enabled = not (hasFlag "no-index-elaboration" parsed)
                            client = clients.queryExpansion
                            modelId = optionValue "small-model" QaDefaults.nanoModel parsed
                            useCaseProfile = profile }

                    return!
                        KnowledgeSources.loadIndex
                            (storageRoot parsed)
                            report
                            keywordOptions
                            KnowledgeSources.PdfParsingMode.Hybrid
                            true
                            [ source ]
        }

    type ReplayExpansion =
        { queries: string list
          terms: string list
          logs: string list }

    type FusionBranch =
        { weight: float
          chunks: SourceChunk list }

    let private emptyReplayExpansion = { queries = []; terms = []; logs = [] }

    let private splitValues (separators: char array) (value: string) =
        value.Split(separators, StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
        |> Array.choose Text.notEmpty
        |> Array.distinctBy _.ToLowerInvariant()
        |> Array.toList

    let private substringBetween (startMarker: string) (endMarker: string) (value: string) =
        let startIndex = value.IndexOf(startMarker, StringComparison.OrdinalIgnoreCase)

        if startIndex < 0 then
            None
        else
            let contentStart = startIndex + startMarker.Length

            let endIndex =
                value.IndexOf(endMarker, contentStart, StringComparison.OrdinalIgnoreCase)

            if endIndex < 0 then
                value.Substring(contentStart) |> Text.notEmpty
            else
                value.Substring(contentStart, endIndex - contentStart) |> Text.notEmpty

    let private substringAfter (marker: string) (value: string) =
        let index = value.IndexOf(marker, StringComparison.OrdinalIgnoreCase)

        if index < 0 then
            None
        else
            value.Substring(index + marker.Length) |> Text.notEmpty

    let private parseExpansionLog (logs: string list) =
        let queries =
            logs
            |> List.collect (fun log ->
                substringBetween "Retrieval query:" ". Expanded keywords:" log
                |> Option.map (splitValues [| '|' |])
                |> Option.defaultValue [])
            |> List.distinctBy _.ToLowerInvariant()

        let terms =
            logs
            |> List.collect (fun log ->
                substringAfter "Expanded keywords:" log
                |> Option.map (splitValues [| ','; '|' |])
                |> Option.defaultValue [])
            |> List.distinctBy _.ToLowerInvariant()

        { queries = queries
          terms = terms
          logs = logs }

    type private ReplayExpansionRecord =
        { index: int option
          expansionLogs: string list option }

    let private loadReplayExpansions (path: string) =
        if String.IsNullOrWhiteSpace path || not (File.Exists path) then
            Map.empty
        else
            File.ReadLines path
            |> Seq.choose (fun line ->
                if String.IsNullOrWhiteSpace line then
                    None
                else
                    tryDeserialize<ReplayExpansionRecord> line
                    |> Option.bind (fun record ->
                        match record.index, record.expansionLogs with
                        | Some index, Some logs -> Some(index, parseExpansionLog logs)
                        | _ -> None))
            |> Map.ofSeq

    type IndexElaboration =
        { label: int
          keywords: string list
          questions: string list }

    type IndexElaborationBatch = { items: IndexElaboration list }

    type IndexElaborationGeneration =
        | GeneratedIndexElaborations of IndexElaboration list
        | RejectedIndexElaborationSchema of string

    let private safeFilePart (value: string) =
        Regex.Replace(value.Trim().ToLowerInvariant(), @"[^a-z0-9._-]+", "-").Trim('-')

    let private cleanIndexValues maxValues values =
        values
        |> List.choose (Text.normalizeWhitespace >> Text.notEmpty)
        |> List.distinctBy _.ToLowerInvariant()
        |> List.truncate maxValues

    let private sanitizeIndexElaboration (elaboration: IndexElaboration) : IndexElaboration =
        { elaboration with
            keywords = cleanIndexValues 16 elaboration.keywords
            questions = cleanIndexValues 4 elaboration.questions }

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

    let private parseIndexElaborationJson (text: string) =
        let tryParseJson (value: string) =
            tryDeserialize<IndexElaboration list> value
            |> Option.orElseWith (fun () -> tryDeserialize<IndexElaborationBatch> value |> Option.map _.items)
            |> Option.orElseWith (fun () -> tryDeserialize<IndexElaboration> value |> Option.map List.singleton)
            |> Option.defaultValue []
            |> List.map sanitizeIndexElaboration

        let trimmed = text.Trim()
        let parsed = tryParseJson trimmed

        if not (List.isEmpty parsed) then
            parsed
        else
            let firstArray = trimmed.IndexOf('[')
            let lastArray = trimmed.LastIndexOf(']')

            if firstArray >= 0 && lastArray > firstArray then
                trimmed.Substring(firstArray, lastArray - firstArray + 1) |> tryParseJson
            else
                []

    let private loadIndexElaborationCache (path: string) =
        if String.IsNullOrWhiteSpace path || not (File.Exists path) then
            Map.empty
        else
            File.ReadLines path
            |> Seq.choose (fun line ->
                if String.IsNullOrWhiteSpace line then
                    None
                else
                    tryDeserialize<IndexElaboration> line
                    |> Option.map sanitizeIndexElaboration
                    |> Option.map (fun item -> item.label, item))
            |> Map.ofSeq

    let private promptAnswerText (answer: string) =
        let normalized = Text.normalizeWhitespace answer

        if normalized.Length <= 900 then
            normalized
        else
            normalized.Substring(0, 900)

    let private indexElaborationPrompt (profile: QaUseCaseProfile) (batch: (int * string) list) =
        let payload =
            batch
            |> List.map (fun (label, answer) ->
                {| label = label
                   answer = promptAnswerText answer |})
            |> fun items -> JsonSerializer.Serialize(items, jsonLineOptions)

        let profile = QaUseCaseProfile.sanitize profile

        let profileHints =
            let hints = QaUseCaseProfile.renderHints profile

            seq {
                yield $"Use case: {profile.displayName} ({profile.id})."

                match profile.description with
                | Some description -> yield $"Description: {description}"
                | None -> ()

                if not (String.IsNullOrWhiteSpace hints) then
                    yield $"Domain hints: {hints}"
            }
            |> String.concat "\n"

        $"""
Create compact search-index metadata for reusable QA answers.
{profileHints}

Return only a JSON object with one property, "items", whose value is an array.
Each item must have:
- label: the same integer label.
- keywords: 8-16 short phrases, synonyms, abbreviations, product or feature names, entities, and likely search wording.
- questions: 2-4 short user questions that this answer would directly answer.

Rules:
- Use only facts supported by the answer text.
- Prefer user wording that may not appear literally in the answer.
- Do not include markdown, explanations, answer prose, or fields other than label, keywords, questions.

Answers:
{payload}
"""

    let private indexElaborationResponseFormat () =
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
          "label": { "type": "integer" },
          "keywords": {
            "type": "array",
            "items": { "type": "string" }
          },
          "questions": {
            "type": "array",
            "items": { "type": "string" }
          }
        },
        "required": [ "label", "keywords", "questions" ]
      }
    }
  },
  "required": [ "items" ]
}
"""

        strictJsonSchemaResponseFormat
            schema
            "insuranceqa_index_elaboration_batch"
            "Search-index keyword and question metadata for QA answers."

    let private generateIndexElaborationBatch
        (profile: QaUseCaseProfile)
        (client: IChatClient)
        (maxOutputTokens: int)
        (batch: (int * string) list)
        =
        async {
            let opts = ChatOptions()
            opts.MaxOutputTokens <- Nullable(Math.Max(maxOutputTokens, batch.Length * 220))
            opts.ResponseFormat <- indexElaborationResponseFormat ()
            applyStrictStructuredOutput opts

            let reasoning = ReasoningOptions()
            reasoning.Effort <- Nullable ReasoningEffort.Low
            reasoning.Output <- Nullable ReasoningOutput.None
            opts.Reasoning <- reasoning

            let! response =
                client.GetResponseAsync<IndexElaborationBatch>(indexElaborationPrompt profile batch, opts)
                |> Async.AwaitTask

            let parsed = response.Result.items |> List.map sanitizeIndexElaboration

            if List.isEmpty parsed then
                let preview =
                    if String.IsNullOrWhiteSpace response.Text then "<empty>"
                    elif response.Text.Length <= 800 then response.Text
                    else response.Text.Substring(0, 800)

                Console.Error.WriteLine $"Index elaboration returned no parseable items. Response preview: {preview}"

            let requested = batch |> List.map fst |> Set.ofList

            return
                parsed
                |> List.filter (fun item -> Set.contains item.label requested)
                |> List.distinctBy _.label
        }

    let rec private generateIndexElaborations profile (client: IChatClient) maxOutputTokens batch =
        async {
            try
                let! generated = generateIndexElaborationBatch profile client maxOutputTokens batch
                let generatedLabels = generated |> List.map _.label |> Set.ofList

                let missing =
                    batch
                    |> List.filter (fun (label, _) -> not (Set.contains label generatedLabels))

                if List.isEmpty missing || batch.Length <= 1 then
                    return GeneratedIndexElaborations generated
                else
                    let! recovered =
                        missing
                        |> List.map (fun item -> generateIndexElaborations profile client maxOutputTokens [ item ])
                        |> Async.Parallel

                    match
                        recovered
                        |> Array.tryPick (function
                            | RejectedIndexElaborationSchema reason -> Some reason
                            | _ -> None)
                    with
                    | Some reason -> return RejectedIndexElaborationSchema reason
                    | None ->
                        let recoveredElaborations =
                            recovered
                            |> Array.collect (function
                                | GeneratedIndexElaborations items -> List.toArray items
                                | RejectedIndexElaborationSchema _ -> [||])
                            |> Array.toList

                        return GeneratedIndexElaborations(generated @ recoveredElaborations)
            with ex ->
                if isInvalidJsonSchemaError ex then
                    return RejectedIndexElaborationSchema(ex.Message)
                elif batch.Length <= 1 then
                    match batch with
                    | (label, _) :: _ ->
                        Console.Error.WriteLine $"Index elaboration failed for label {label}: {ex.Message}"
                    | [] -> ()

                    return GeneratedIndexElaborations []
                else
                    let firstHalf, secondHalf =
                        let half = max 1 (batch.Length / 2)
                        batch |> List.splitAt half

                    let! left = generateIndexElaborations profile client maxOutputTokens firstHalf
                    let! right = generateIndexElaborations profile client maxOutputTokens secondHalf

                    match left, right with
                    | RejectedIndexElaborationSchema reason, _
                    | _, RejectedIndexElaborationSchema reason -> return RejectedIndexElaborationSchema reason
                    | GeneratedIndexElaborations left, GeneratedIndexElaborations right ->
                        return GeneratedIndexElaborations(left @ right)
        }

    let private writeIndexElaborationJsonSource (answers: Map<int, string>) expansions path =
        let metadata =
            expansions
            |> Map.map (fun _ expansion ->
                { keywords = expansion.keywords
                  questions = expansion.questions })

        writeInsuranceQaJsonSource answers metadata path

    let runInsuranceQaElaborateIndex parsed =
        task {
            let dataDir = optionValue "data-dir" (Path.Combine("temp", "insuranceqa")) parsed
            let model = optionValue "small-model" "gpt-5-nano" parsed
            let profile = useCaseProfile parsed
            let batchSize = optionValue "batch-size" "8" parsed |> Int32.Parse
            let parallelism = optionValue "parallelism" "2" parsed |> Int32.Parse

            let maxOutputTokens =
                optionValue "elaboration-max-output-tokens" "25000" parsed |> Int32.Parse

            let limit = optionValues "limit" parsed |> List.tryLast |> Option.map Int32.Parse
            let! answers, _, _ = loadInsuranceQa dataDir

            let labelFilter =
                optionValues "label-list-path" parsed
                |> List.tryLast
                |> Option.map (fun path ->
                    File.ReadLines path
                    |> Seq.choose (fun line ->
                        match Int32.TryParse(line.Trim()) with
                        | true, label -> Some label
                        | false, _ -> None)
                    |> Set.ofSeq)

            let cachePath =
                optionValue
                    "elaboration-cache"
                    (Path.Combine(Path.GetFullPath dataDir, $"insuranceqa.index-elaboration.{safeFilePart model}.jsonl"))
                    parsed

            let answersSourcePath =
                optionValueAny
                    [ "answers-source"; "answers-markdown" ]
                    (Path.Combine(
                        Path.GetFullPath dataDir,
                        $"insuranceqa.answers.index-elaborated.{safeFilePart model}.json"
                    ))
                    parsed

            let existing = loadIndexElaborationCache cachePath

            let targetAnswers =
                answers
                |> Map.toList
                |> List.filter (fun (label, _) ->
                    labelFilter
                    |> Option.map (fun labels -> Set.contains label labels)
                    |> Option.defaultValue true)
                |> fun values ->
                    match limit with
                    | Some count -> values |> List.truncate count
                    | None -> values

            let targetLabels = targetAnswers |> List.map fst |> Set.ofList

            let missing =
                targetAnswers
                |> List.filter (fun (label, _) -> not (Map.containsKey label existing))

            match apiKey parsed with
            | None when not (List.isEmpty missing) ->
                Console.Error.WriteLine "Missing --api-key or OPENAI_API_KEY for index elaboration."
                return 2
            | _ ->
                let generated = ResizeArray<IndexElaboration>()

                if not (List.isEmpty missing) then
                    let client = createClient (apiKey parsed |> Option.get) model

                    Directory.CreateDirectory(Path.GetDirectoryName(Path.GetFullPath cachePath))
                    |> ignore

                    use writer = new StreamWriter(cachePath, true)
                    let batches = missing |> List.chunkBySize batchSize

                    let mutable completed =
                        existing
                        |> Map.toSeq
                        |> Seq.filter (fun (label, _) -> Set.contains label targetLabels)
                        |> Seq.length

                    let mutable schemaRejectedLogged = false

                    for wave in batches |> List.chunkBySize parallelism do
                        let! waveResults =
                            wave
                            |> List.map (generateIndexElaborations profile client maxOutputTokens)
                            |> Async.Parallel
                            |> Async.StartAsTask

                        for result in waveResults do
                            match result with
                            | RejectedIndexElaborationSchema _ ->
                                if not schemaRejectedLogged then
                                    schemaRejectedLogged <- true

                                    Console.Error.WriteLine
                                        "Index elaboration schema was rejected by OpenAI; continuing with cached metadata only."
                            | GeneratedIndexElaborations items ->
                                for item in items do
                                    generated.Add item
                                    writer.WriteLine(JsonSerializer.Serialize(item, jsonLineOptions))
                                    completed <- completed + 1

                        writer.Flush()
                        printfn "Index elaboration cached %d/%d item(s)." completed targetAnswers.Length

                let expansions =
                    seq {
                        yield! existing |> Map.toSeq |> Seq.map snd
                        yield! generated
                    }
                    |> Seq.map (fun item -> item.label, item)
                    |> Map.ofSeq

                writeIndexElaborationJsonSource answers expansions answersSourcePath

                printJson
                    {| model = model
                       useCaseProfile = profile.id
                       answers = answers.Count
                       targetedAnswers = targetAnswers.Length
                       targetedElaborations =
                        expansions
                        |> Map.toSeq
                        |> Seq.filter (fun (label, _) -> Set.contains label targetLabels)
                        |> Seq.length
                       cachedElaborations = expansions.Count
                       cachePath = Path.GetFullPath cachePath
                       answersSourcePath = Path.GetFullPath answersSourcePath |}

                return 0
        }

    let private replayPath parsed (dataDir: string) (sample: int) =
        optionValues "expansion-replay-path" parsed
        |> List.tryLast
        |> Option.defaultValue (
            Path.Combine(Path.GetFullPath dataDir, $"insuranceqa-search-eval-{sample}-llm-expansion.jsonl")
        )

    let private directFsColbertSearch
        (retrieval: KnowledgeSources.RetrievalIndex)
        (maxResults: int)
        (candidateLimit: int)
        (query: string)
        (searchTerms: string list)
        =
        async {
            match retrieval.encoder, retrieval.colbertIndices with
            | Some encoder, indices when not (List.isEmpty indices) ->
                let options =
                    { FsColbert.SearchOptions.defaults with
                        maxResults = maxResults
                        candidateLimit = candidateLimit
                        useLexicalFilter = true
                        useRRF = true
                        denseWeight = 1.0f
                        lexicalWeight = 0.1f }

                let! perIndex =
                    indices
                    |> List.map (fun (source, index) ->
                        async {
                            let! hits = FsColbert.Search.queryWithSearchTerms encoder options index query searchTerms

                            return
                                hits
                                |> List.map (fun hit ->
                                    { source = source
                                      index = hit.reference.index
                                      text = hit.reference.text
                                      score = hit.score })
                        })
                    |> Async.Parallel

                return
                    perIndex
                    |> Array.collect List.toArray
                    |> Array.sortByDescending (fun chunk -> chunk.score)
                    |> Array.truncate maxResults
                    |> Array.toList
            | _ -> return []
        }

    let private chunkKey (chunk: SourceChunk) =
        $"{chunk.source.location.ToLowerInvariant()}:{chunk.index}"

    let private distinctSourceChunks chunks =
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

    let private fuseRrf maxResults fusionK branches =
        let scores = Dictionary<string, float>()
        let chunks = Dictionary<string, SourceChunk>()

        for branch in branches do
            branch.chunks
            |> List.iteri (fun index chunk ->
                let key = chunkKey chunk
                chunks[key] <- chunk

                let contribution = branch.weight / (fusionK + float (index + 1))
                let existing = scores.TryGetValue key

                scores[key] <-
                    match existing with
                    | true, value -> value + contribution
                    | false, _ -> contribution)

        scores
        |> Seq.map (fun entry ->
            { chunks[entry.Key] with
                score = float32 entry.Value })
        |> Seq.sortByDescending (fun chunk -> chunk.score)
        |> Seq.truncate maxResults
        |> Seq.toList

    let private fuseTail maxResults localHead localChunks llmChunks =
        let head = localChunks |> List.truncate localHead
        let tail = llmChunks @ localChunks

        head @ tail |> distinctSourceChunks |> List.truncate maxResults

    let private fusedSearch
        (parsed: ParsedArgs)
        (retrieval: KnowledgeSources.RetrievalIndex)
        (maxResults: int)
        (processed: Text.ProcessedQuery)
        (replay: ReplayExpansion)
        : Async<SourceChunk list * string list> =
        async {
            let mode =
                optionValue "fusion-mode" "standard" parsed
                |> fun value -> value.Trim().ToLowerInvariant()

            let candidateLimit = optionValue "candidate-limit" "256" parsed |> Int32.Parse

            let branchResults =
                optionValue "fusion-branch-results" (string maxResults) parsed |> Int32.Parse

            let localWeight = optionValue "local-fusion-weight" "1.0" parsed |> Double.Parse
            let llmWeight = optionValue "llm-fusion-weight" "0.5" parsed |> Double.Parse
            let fusionK = optionValue "fusion-k" "60" parsed |> Double.Parse
            let localHead = optionValue "fusion-local-head" "10" parsed |> Int32.Parse

            let localQuery =
                processed.rewrittenQueries
                |> List.tryHead
                |> Option.defaultValue processed.normalizedQuery

            let llmQuery = replay.queries |> List.tryHead |> Option.defaultValue localQuery

            let llmTerms = replay.terms |> List.collect Text.terms |> List.distinct

            match mode with
            | "direct-local" ->
                let! local = directFsColbertSearch retrieval maxResults candidateLimit localQuery processed.searchTerms

                return local, replay.logs
            | "direct-llm" ->
                let! llm = directFsColbertSearch retrieval maxResults candidateLimit llmQuery llmTerms
                return llm, replay.logs
            | "rrf" ->
                let! local =
                    directFsColbertSearch retrieval branchResults candidateLimit localQuery processed.searchTerms

                let! llm = directFsColbertSearch retrieval branchResults candidateLimit llmQuery llmTerms

                return
                    fuseRrf
                        maxResults
                        fusionK
                        [ { weight = localWeight; chunks = local }
                          { weight = llmWeight; chunks = llm } ],
                    replay.logs
            | "tail" ->
                let! local = directFsColbertSearch retrieval maxResults candidateLimit localQuery processed.searchTerms

                let! llm = directFsColbertSearch retrieval branchResults candidateLimit llmQuery llmTerms
                return fuseTail maxResults localHead local llm, replay.logs
            | _ -> return [], replay.logs
        }

    let runInsuranceQaSearchEval parsed =
        task {
            let sample = optionValue "sample" "30" parsed |> fun value -> Int32.Parse value
            let dataDir = optionValue "data-dir" (Path.Combine("temp", "insuranceqa")) parsed

            let maxResults =
                optionValue "max-results" (string QaDefaults.memoryCandidateChunks) parsed
                |> Int32.Parse

            let profile = useCaseProfile parsed
            let! answers, questions, defaultAnswersSourcePath = loadInsuranceQa dataDir

            let answersSourcePath =
                optionValueAny [ "answers-source"; "answers-markdown" ] defaultAnswersSourcePath parsed
                |> Path.GetFullPath

            let selected = questions |> List.truncate sample

            let source =
                { kind = sourceKind answersSourcePath
                  location = answersSourcePath
                  enabled = true }

            let! retrieval, errors = loadInsuranceQaRetrieval parsed profile source

            for error in errors do
                Console.Error.WriteLine error

            let resultsPath =
                Path.Combine(Path.GetFullPath dataDir, $"insuranceqa-search-eval-{sample}.jsonl")

            let replayExpansions = loadReplayExpansions (replayPath parsed dataDir sample)

            let fusionMode =
                optionValue "fusion-mode" "standard" parsed
                |> fun value -> value.Trim().ToLowerInvariant()

            let queryExpansionClient =
                if
                    fusionMode = "standard"
                    && (hasFlag "llm-query-expansion" parsed
                        || hasFlag "realtime-query-expansion" parsed)
                then
                    (clientsFromArgs parsed).queryExpansion
                else
                    None

            use writer = new StreamWriter(resultsPath, false)
            let searchTimes = ResizeArray<float>()
            let mutable hits = 0
            let mutable top1 = 0
            let mutable top3 = 0
            let mutable top5 = 0
            let mutable top10 = 0

            for index, question in selected |> List.indexed do
                let processed =
                    Text.QueryPostProcessing.forVoiceLikeRetrievalWithProfile profile question.question

                let expansionLogs = ResizeArray<string>()
                let sw = Stopwatch.StartNew()

                let! chunks =
                    task {
                        if fusionMode = "standard" then
                            let! chunks =
                                KnowledgeSources.rankWithProfile
                                    profile
                                    queryExpansionClient
                                    (Option.isSome queryExpansionClient)
                                    false
                                    true
                                    (fun msg -> expansionLogs.Add msg)
                                    question.question
                                    maxResults
                                    retrieval

                            return chunks
                        else
                            let replay =
                                replayExpansions
                                |> Map.tryFind (index + 1)
                                |> Option.defaultValue emptyReplayExpansion

                            let! chunks, logs = fusedSearch parsed retrieval maxResults processed replay

                            for log in logs do
                                expansionLogs.Add log

                            return chunks
                    }

                sw.Stop()
                let elapsedMs = sw.Elapsed.TotalMilliseconds
                searchTimes.Add elapsedMs

                let expected = question.answerLabels |> Set.ofList

                let rank =
                    chunks
                    |> List.tryFindIndex (fun chunk ->
                        tryAnswerLabel chunk.text
                        |> Option.exists (fun label -> Set.contains label expected))
                    |> Option.map ((+) 1)

                let hit = Option.isSome rank

                if hit then
                    hits <- hits + 1

                if rank |> Option.exists (fun value -> value <= 1) then
                    top1 <- top1 + 1

                if rank |> Option.exists (fun value -> value <= 3) then
                    top3 <- top3 + 1

                if rank |> Option.exists (fun value -> value <= 5) then
                    top5 <- top5 + 1

                if rank |> Option.exists (fun value -> value <= 10) then
                    top10 <- top10 + 1

                let retrievedAnswerLabels =
                    chunks |> List.choose (fun chunk -> tryAnswerLabel chunk.text) |> List.distinct

                let matchedLabels =
                    retrievedAnswerLabels |> List.filter (fun label -> Set.contains label expected)

                let record =
                    {| index = index + 1
                       category = question.category
                       question = question.question
                       normalizedQuestion = processed.normalizedQuery
                       searchTerms = processed.searchTerms
                       rewrittenQueries = processed.rewrittenQueries
                       expansionLogs = List.ofSeq expansionLogs
                       answerLabels = question.answerLabels
                       retrievedAnswerLabels = retrievedAnswerLabels
                       retrievedChunks =
                        chunks
                        |> List.map (fun chunk ->
                            {| answerLabel = tryAnswerLabel chunk.text |> Option.defaultValue -1
                               score = chunk.score
                               index = chunk.index |})
                       matchedAnswerLabels = matchedLabels
                       hit = hit
                       rank = rank |> Option.defaultValue 0
                       sourceRetrievalElapsedMs = elapsedMs |}

                writer.WriteLine(JsonSerializer.Serialize(record, jsonLineOptions))
                writer.Flush()

                printfn
                    "InsuranceQA search %d/%d hit=%b rank=%d searchMs=%.0f"
                    (index + 1)
                    selected.Length
                    hit
                    (rank |> Option.defaultValue 0)
                    elapsedMs

            let rate count =
                if selected.IsEmpty then
                    0.0
                else
                    float count / float selected.Length

            let summary =
                {| sample = selected.Length
                   indexedAnswers = answers.Count
                   useCaseProfile = profile.id
                   retrievalMode = RetrievalModes.displayName (retrievalMode parsed)
                   maxResults = maxResults
                   hits = hits
                   hitRate = rate hits
                   top1Rate = rate top1
                   top3Rate = rate top3
                   top5Rate = rate top5
                   top10Rate = rate top10
                   averageSourceRetrievalMs =
                    if searchTimes.Count = 0 then
                        0.0
                    else
                        searchTimes |> Seq.average
                   medianSourceRetrievalMs = percentile 0.50 searchTimes
                   p95SourceRetrievalMs = percentile 0.95 searchTimes
                   maxSourceRetrievalMs =
                    if searchTimes.Count = 0 then
                        0.0
                    else
                        searchTimes |> Seq.max
                   resultsPath = resultsPath |}

            printJson summary
            KnowledgeSources.disposeIndex retrieval
            return 0
        }

    let runInsuranceQaEval parsed =
        task {
            let sample = optionValue "sample" "30" parsed |> fun value -> Int32.Parse value
            let dataDir = optionValue "data-dir" (Path.Combine("temp", "insuranceqa")) parsed
            let profile = useCaseProfile parsed
            let! answers, questions, defaultAnswersSourcePath = loadInsuranceQa dataDir

            let answersSourcePath =
                optionValueAny [ "answers-source"; "answers-markdown" ] defaultAnswersSourcePath parsed
                |> Path.GetFullPath

            let selected = questions |> List.truncate sample
            let useJudge = not (hasFlag "no-judge" parsed)

            let judgeClient =
                if useJudge then
                    apiKey parsed
                    |> Option.map (fun key -> createClient key (optionValue "judge-model" QaDefaults.nanoModel parsed))
                else
                    None

            let resultsPath =
                Path.Combine(Path.GetFullPath dataDir, $"insuranceqa-eval-{sample}.jsonl")

            use session = createSession parsed false

            let source =
                { kind = sourceKind answersSourcePath
                  location = answersSourcePath
                  enabled = true }

            let! _ = session.LoadSourcesAsync(retrievalMode parsed, [ source ], CancellationToken.None)

            use writer = new StreamWriter(resultsPath, false)
            let mutable totalF1 = 0.0
            let mutable hitCount = 0
            let mutable judged = 0
            let mutable judgeScore = 0.0
            let mutable correct = 0
            let sourceRetrievalTimes = ResizeArray<float>()
            let answerTimes = ResizeArray<float>()

            for index, question in selected |> List.indexed do
                let references =
                    question.answerLabels |> List.choose (fun label -> answers |> Map.tryFind label)

                let request =
                    { turnId = $"insuranceqa-{index + 1}"
                      question = question.question
                      realtimeJudgement = None
                      deadline = None }

                let answerSw = Stopwatch.StartNew()
                let! answer = session.AnswerAsync(request, CancellationToken.None)
                answerSw.Stop()

                let answerElapsedMs = answerSw.Elapsed.TotalMilliseconds
                let sourceRetrievalElapsedMs = answer.sourceRetrievalElapsedMs
                let f1 = lexicalF1 answer.answer references
                let hit = contextHit question.answerLabels answer.context
                let! judgement = judgeAnswer judgeClient answer.answer references

                totalF1 <- totalF1 + f1
                sourceRetrievalTimes.Add sourceRetrievalElapsedMs
                answerTimes.Add answerElapsedMs

                if hit then
                    hitCount <- hitCount + 1

                match judgement with
                | Some(verdict, score) ->
                    judged <- judged + 1
                    judgeScore <- judgeScore + score

                    if String.Equals(verdict, "correct", StringComparison.OrdinalIgnoreCase) then
                        correct <- correct + 1
                | None -> ()

                let record =
                    {| index = index + 1
                       category = question.category
                       question = question.question
                       answerLabels = question.answerLabels
                       retrievedAnswerLabels =
                        answer.context
                        |> List.choose (fun chunk -> tryAnswerLabel chunk.text)
                        |> List.distinct
                       retrievedChunks =
                        answer.context
                        |> List.map (fun chunk ->
                            {| answerLabel = tryAnswerLabel chunk.text |> Option.defaultValue -1
                               score = chunk.score
                               index = chunk.index |})
                       generatedAnswer = answer.answer
                       contextHit = hit
                       lexicalF1 = f1
                       sourceRetrievalElapsedMs = sourceRetrievalElapsedMs
                       answerElapsedMs = answerElapsedMs
                       judge = judgement |}

                writer.WriteLine(JsonSerializer.Serialize(record, jsonLineOptions))
                writer.Flush()

                printfn
                    "InsuranceQA %d/%d contextHit=%b lexicalF1=%.3f searchMs=%.0f answerMs=%.0f"
                    (index + 1)
                    selected.Length
                    hit
                    f1
                    sourceRetrievalElapsedMs
                    answerElapsedMs

            let summary =
                {| sample = selected.Length
                   indexedAnswers = answers.Count
                   useCaseProfile = profile.id
                   retrievalMode = RetrievalModes.displayName (retrievalMode parsed)
                   contextHitRate =
                    if selected.IsEmpty then
                        0.0
                    else
                        float hitCount / float selected.Length
                   averageLexicalF1 =
                    if selected.IsEmpty then
                        0.0
                    else
                        totalF1 / float selected.Length
                   averageSourceRetrievalMs =
                    if sourceRetrievalTimes.Count = 0 then
                        0.0
                    else
                        sourceRetrievalTimes |> Seq.average
                   medianSourceRetrievalMs = percentile 0.50 sourceRetrievalTimes
                   p95SourceRetrievalMs = percentile 0.95 sourceRetrievalTimes
                   maxSourceRetrievalMs =
                    if sourceRetrievalTimes.Count = 0 then
                        0.0
                    else
                        sourceRetrievalTimes |> Seq.max
                   averageAnswerMs =
                    if answerTimes.Count = 0 then
                        0.0
                    else
                        answerTimes |> Seq.average
                   medianAnswerMs = percentile 0.50 answerTimes
                   judged = judged
                   judgeAverageScore = if judged = 0 then 0.0 else judgeScore / float judged
                   judgeCorrectRate = if judged = 0 then 0.0 else float correct / float judged
                   resultsPath = resultsPath |}

            printJson summary
            return 0
        }

    [<EntryPoint>]
    let main argv =
        let parsed = parseArgs argv

        let task =
            match parsed.command with
            | "ask" -> runAsk parsed
            | "insuranceqa-elaborate-index" -> runInsuranceQaElaborateIndex parsed
            | "insuranceqa-eval" -> runInsuranceQaEval parsed
            | "insuranceqa-search-eval" -> runInsuranceQaSearchEval parsed
            | _ ->
                task {
                    printf "%s" usage
                    return 0
                }

        task.GetAwaiter().GetResult()
