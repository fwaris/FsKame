namespace FsKame.WorkFlow

open System
open System.IO
open System.Security.Cryptography
open System.Text
open System.Text.Json
open System.Text.RegularExpressions
open FsKame
open Microsoft.Maui.Storage

module DurableMemory =
    type Store =
        { path: string option
          records: MemoryRecord list
          hotOverlayIds: Set<string>
          colbertIndex: FsColbert.ColbertIndex option
          indexedAt: DateTimeOffset option
          epochs: Map<MemoryKind, int> }

    [<CLIMutable>]
    type private MemoryRecordDto =
        { memoryId: string
          kind: string
          title: string
          text: string
          summary: string
          scope: string
          namespaceId: string
          entities: string list
          tags: string list
          status: string
          confidence: float
          importance: float
          sensitivity: string
          observedAt: string
          validFrom: string
          validTo: string
          lastConfirmedAt: string
          sourceType: string
          sourceIds: string list
          toolName: string
          toolArgsHash: string
          resultHash: string
          supersedes: string list
          supersededBy: string
          conflictsWith: string list
          derivedFrom: string list
          indexText: string
          colbertDocIds: string list
          indexedAt: string
          embeddingModel: string
          version: int }

    [<CLIMutable>]
    type private StoreDto =
        { schemaVersion: int
          records: MemoryRecordDto list }

    let defaultNamespace = "default"

    let private jsonOptions =
        let opts = JsonSerializerOptions(WriteIndented = true)
        opts.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase
        opts

    let empty (path: string option) : Store =
        { path = path
          records = []
          hotOverlayIds = Set.empty
          colbertIndex = None
          indexedAt = None
          epochs = Map.empty }

    let inMemory (records: MemoryRecord list) : Store =
        { empty None with
            records = records
            hotOverlayIds = records |> List.map _.memoryId |> Set.ofList
            epochs =
                records
                |> List.groupBy _.kind
                |> List.map (fun (kind, values) -> kind, values.Length)
                |> Map.ofList }

    let private nonNullList values =
        if isNull (box values) then [] else values

    let private nonEmptyOption value =
        value |> Option.ofObj |> Option.bind Text.notEmpty

    let private optionToString value = value |> Option.defaultValue ""

    let private parseDate fallback value =
        match value |> Option.ofObj |> Option.bind Text.notEmpty with
        | None -> fallback
        | Some value ->
            match DateTimeOffset.TryParse value with
            | true, parsed -> parsed
            | false, _ -> fallback

    let private parseOptionDate value =
        value
        |> Option.ofObj
        |> Option.bind Text.notEmpty
        |> Option.bind (fun value ->
            match DateTimeOffset.TryParse value with
            | true, parsed -> Some parsed
            | false, _ -> None)

    let private normalizeKey value =
        Text.normalizeWhitespace value |> fun value -> value.ToLowerInvariant()

    let private stringEquals left right =
        String.Equals(left, right, StringComparison.OrdinalIgnoreCase)

    let kindName =
        function
        | Directive -> "directive"
        | Claim -> "claim"
        | Decision -> "decision"
        | Commitment -> "commitment"
        | Episode -> "episode"

    let private tryKind value =
        match normalizeKey value with
        | "directive" -> Some Directive
        | "claim" -> Some Claim
        | "decision" -> Some Decision
        | "commitment" -> Some Commitment
        | "episode" -> Some Episode
        | _ -> None

    let scopeName =
        function
        | Session -> "session"
        | User -> "user"
        | Workspace -> "workspace"
        | Global -> "global"

    let private tryScope value =
        match normalizeKey value with
        | "session" -> Some Session
        | "user" -> Some User
        | "workspace" -> Some Workspace
        | "global" -> Some Global
        | _ -> None

    let statusName =
        function
        | Current -> "current"
        | Superseded -> "superseded"
        | Retracted -> "retracted"
        | Expired -> "expired"
        | Uncertain -> "uncertain"

    let private tryStatus value =
        match normalizeKey value with
        | "current" -> Some Current
        | "superseded" -> Some Superseded
        | "retracted" -> Some Retracted
        | "expired" -> Some Expired
        | "uncertain" -> Some Uncertain
        | _ -> None

    let sensitivityName =
        function
        | Normal -> "normal"
        | Personal -> "personal"
        | Sensitive -> "sensitive"
        | Secret -> "secret"

    let private trySensitivity value =
        match normalizeKey value with
        | "normal" -> Some Normal
        | "personal" -> Some Personal
        | "sensitive" -> Some Sensitive
        | "secret" -> Some Secret
        | _ -> None

    let temporalModeName =
        function
        | CurrentOnly -> "current_only"
        | AsOfTime -> "as_of_time"
        | ChangedSince -> "changed_since"
        | IncludeHistory -> "include_history"

    let budgetName =
        function
        | Fast -> "fast"
        | Medium -> "medium"
        | Large -> "large"
        | ConflictProbe -> "conflict_probe"

    let controllerPathName =
        function
        | ZeroController -> "zero_controller"
        | SmallController -> "small_controller"
        | LargeController -> "large_controller"
        | HybridSmallThenLarge -> "hybrid_small_then_large"

    let conflictName =
        function
        | Duplicate -> "duplicate"
        | Refinement -> "refinement"
        | Contradiction -> "contradiction"
        | Unrelated -> "unrelated"
        | Retraction -> "retraction"

    let private clamp01 value =
        if Double.IsNaN value then 0.0
        elif value < 0.0 then 0.0
        elif value > 1.0 then 1.0
        else value

    let private sourceIdsFrom (provenance: MemoryProvenance) =
        provenance.sourceIds
        |> nonNullList
        |> List.filter (String.IsNullOrWhiteSpace >> not)

    let private indexTextFor
        (kind: MemoryKind)
        (title: string)
        (summary: string)
        (text: string)
        (entities: string list)
        (tags: string list)
        =
        [ yield $"{kindName kind}: {title}"
          yield summary
          yield text
          yield! entities
          yield! tags ]
        |> List.choose Text.notEmpty
        |> String.concat " "
        |> Text.normalizeWhitespace

    let private dtoFromRecord (record: MemoryRecord) : MemoryRecordDto =
        { memoryId = record.memoryId
          kind = kindName record.kind
          title = record.title
          text = record.text
          summary = record.summary
          scope = scopeName record.scope
          namespaceId = record.namespaceId
          entities = record.entities
          tags = record.tags
          status = statusName record.status
          confidence = record.confidence
          importance = record.importance
          sensitivity = sensitivityName record.sensitivity
          observedAt = record.temporal.observedAt.ToString("O")
          validFrom = record.temporal.validFrom.ToString("O")
          validTo = record.temporal.validTo |> Option.map _.ToString("O") |> optionToString
          lastConfirmedAt = record.temporal.lastConfirmedAt.ToString("O")
          sourceType = record.provenance.sourceType
          sourceIds = record.provenance.sourceIds
          toolName = record.provenance.toolName |> optionToString
          toolArgsHash = record.provenance.toolArgsHash |> optionToString
          resultHash = record.provenance.resultHash |> optionToString
          supersedes = record.relations.supersedes
          supersededBy = record.relations.supersededBy |> optionToString
          conflictsWith = record.relations.conflictsWith
          derivedFrom = record.relations.derivedFrom
          indexText = record.retrieval.indexText
          colbertDocIds = record.retrieval.colbertDocIds
          indexedAt = record.retrieval.indexedAt |> Option.map _.ToString("O") |> optionToString
          embeddingModel = record.retrieval.embeddingModel |> optionToString
          version = record.version }

    let private recordFromDto (dto: MemoryRecordDto) : MemoryRecord option =
        match tryKind dto.kind, tryScope dto.scope, tryStatus dto.status, trySensitivity dto.sensitivity with
        | Some kind, Some scope, Some status, Some sensitivity ->
            let now = DateTimeOffset.UtcNow
            let title = dto.title |> Option.ofObj |> Option.defaultValue ""
            let text = dto.text |> Option.ofObj |> Option.defaultValue ""
            let summary = dto.summary |> Option.ofObj |> Option.defaultValue ""
            let entities = dto.entities |> nonNullList
            let tags = dto.tags |> nonNullList

            let namespaceId =
                dto.namespaceId |> Option.ofObj |> Option.defaultValue defaultNamespace

            let indexText =
                dto.indexText
                |> Option.ofObj
                |> Option.bind Text.notEmpty
                |> Option.defaultValue (indexTextFor kind title summary text entities tags)

            Some
                { memoryId =
                    dto.memoryId
                    |> Option.ofObj
                    |> Option.bind Text.notEmpty
                    |> Option.defaultValue (Guid.NewGuid().ToString("N"))
                  kind = kind
                  title = title
                  text = text
                  summary = summary
                  scope = scope
                  namespaceId = namespaceId
                  entities = entities
                  tags = tags
                  status = status
                  confidence = clamp01 dto.confidence
                  importance = clamp01 dto.importance
                  sensitivity = sensitivity
                  temporal =
                    { observedAt = parseDate now dto.observedAt
                      validFrom = parseDate now dto.validFrom
                      validTo = parseOptionDate dto.validTo
                      lastConfirmedAt = parseDate now dto.lastConfirmedAt }
                  provenance =
                    { sourceType =
                        dto.sourceType
                        |> Option.ofObj
                        |> Option.bind Text.notEmpty
                        |> Option.defaultValue "unknown"
                      sourceIds = dto.sourceIds |> nonNullList
                      toolName = nonEmptyOption dto.toolName
                      toolArgsHash = nonEmptyOption dto.toolArgsHash
                      resultHash = nonEmptyOption dto.resultHash }
                  relations =
                    { supersedes = dto.supersedes |> nonNullList
                      supersededBy = nonEmptyOption dto.supersededBy
                      conflictsWith = dto.conflictsWith |> nonNullList
                      derivedFrom = dto.derivedFrom |> nonNullList }
                  retrieval =
                    { indexText = indexText
                      colbertDocIds = dto.colbertDocIds |> nonNullList
                      indexedAt = parseOptionDate dto.indexedAt
                      embeddingModel = nonEmptyOption dto.embeddingModel }
                  version = max 1 dto.version }
        | _ -> None

    let private recomputeEpochs (records: MemoryRecord list) : Map<MemoryKind, int> =
        records
        |> List.filter (fun record -> record.status = Current || record.status = Uncertain)
        |> List.groupBy _.kind
        |> List.map (fun (kind, records) ->
            let versionTotal = records |> List.sumBy _.version
            kind, max records.Length versionTotal)
        |> Map.ofList

    let private defaultPath () =
        Path.Combine(FileSystem.AppDataDirectory, "memory", "durable-memory-v1.json")

    let load (path: string) : Store * string list =
        try
            if File.Exists path then
                let dto = JsonSerializer.Deserialize<StoreDto>(File.ReadAllText path, jsonOptions)

                let records = dto.records |> nonNullList |> List.choose recordFromDto

                { path = Some path
                  records = records
                  hotOverlayIds = records |> List.map _.memoryId |> Set.ofList
                  colbertIndex = None
                  indexedAt = None
                  epochs = recomputeEpochs records },
                [ $"Loaded {records.Length} durable memory record(s)." ]
            else
                empty (Some path), [ "No durable memory store exists yet." ]
        with ex ->
            empty (Some path), [ $"Durable memory store could not be loaded: {ex.Message}" ]

    let loadDefault () = load (defaultPath ())

    let private save (store: Store) =
        match store.path with
        | None -> []
        | Some path ->
            try
                match Path.GetDirectoryName path with
                | null
                | "" -> ()
                | dir -> Directory.CreateDirectory dir |> ignore

                let dto =
                    { schemaVersion = 1
                      records = store.records |> List.map dtoFromRecord }

                File.WriteAllText(path, JsonSerializer.Serialize(dto, jsonOptions))
                []
            with ex ->
                [ $"Durable memory store could not be saved: {ex.Message}" ]

    let private currentRecordIds (records: MemoryRecord list) =
        records
        |> List.filter (fun record -> record.status = Current || record.status = Uncertain)
        |> List.map _.memoryId
        |> Set.ofList

    let buildMainIndex (report: string -> unit) (encoder: FsColbert.OnnxColbertEncoder) (store: Store) =
        async {
            let searchableRecords =
                store.records
                |> List.filter (fun record ->
                    record.status <> Retracted
                    && record.status <> Expired
                    && record.sensitivity <> Secret)

            if List.isEmpty searchableRecords then
                return
                    { store with
                        colbertIndex = None
                        indexedAt = Some DateTimeOffset.UtcNow },
                    []
            else
                try
                    let passages =
                        searchableRecords
                        |> List.mapi (fun index record ->
                            let passage: FsColbert.PassageRef =
                                { sourceId = record.memoryId
                                  sourceDisplayName = $"{kindName record.kind}: {record.title}"
                                  sourceLocation = $"memory://{record.memoryId}"
                                  index = index
                                  text = record.retrieval.indexText }

                            passage)

                    let! index =
                        FsColbert.IndexBuilder.createFromPassages
                            encoder
                            FsColbert.ChunkOptions.fsKameDefaults
                            passages
                            None

                    report $"Indexed {passages.Length} durable memory passage(s) with FsColbert."

                    return
                        { store with
                            colbertIndex = Some index
                            indexedAt = Some DateTimeOffset.UtcNow
                            hotOverlayIds = Set.empty },
                        []
                with ex ->
                    return store, [ $"Durable memory ColBERT index could not be built: {ex.Message}" ]
        }

    let private emptyRisk =
        { memoryMutation = false
          sensitive = false
          conflictLikely = false }

    let private recallBudgetMaxCandidates =
        function
        | Fast -> 20
        | Medium -> 60
        | Large -> 120
        | ConflictProbe -> 30

    let private recallBudgetLatency =
        function
        | Fast -> TimeSpan.FromMilliseconds 90.
        | Medium -> TimeSpan.FromMilliseconds 180.
        | Large -> TimeSpan.FromMilliseconds 360.
        | ConflictProbe -> TimeSpan.FromMilliseconds 160.

    let private lowerContainsAny (patterns: string list) (value: string) =
        let lower = value.ToLowerInvariant()
        patterns |> List.exists lower.Contains

    let private riskFromText (text: string) : RiskFlags =
        let memoryMutation =
            lowerContainsAny
                [ "remember that"
                  "remember to"
                  "forget"
                  "don't remember"
                  "do not remember"
                  "save this"
                  "note that"
                  "my preference"
                  "i prefer"
                  "we decided"
                  "actually" ]
                text

        let sensitive =
            lowerContainsAny
                [ "password"
                  "secret"
                  "token"
                  "api key"
                  "ssn"
                  "social security"
                  "credit card" ]
                text

        let conflictLikely =
            lowerContainsAny [ "actually"; "correction"; "instead"; "no longer"; "not anymore"; "don't use" ] text

        { memoryMutation = memoryMutation
          sensitive = sensitive
          conflictLikely = conflictLikely }

    let private acceptedRealtimeJudgement (judgement: RealtimeJudgement option) =
        match judgement with
        | Some judgement ->
            judgement.confidence >= 0.75
            && not judgement.riskFlags.memoryMutation
            && not judgement.riskFlags.sensitive
            && not judgement.riskFlags.conflictLikely
        | None -> false

    let private kindsForQuery (text: string) (risk: RiskFlags) =
        let lower = text.ToLowerInvariant()

        if risk.memoryMutation || risk.conflictLikely then
            [ Directive; Decision; Commitment; Claim; Episode ]
        elif lowerContainsAny [ "decided"; "decision"; "choose"; "chosen"; "agreed" ] lower then
            [ Decision; Directive; Claim; Episode ]
        elif lowerContainsAny [ "todo"; "follow up"; "remind"; "pending"; "open loop"; "commitment" ] lower then
            [ Commitment; Decision; Directive; Episode ]
        elif lowerContainsAny [ "preference"; "instruction"; "always"; "never"; "policy"; "directive" ] lower then
            [ Directive; Decision; Claim; Episode ]
        elif lowerContainsAny [ "earlier"; "history"; "what changed"; "before"; "previously" ] lower then
            [ Episode; Decision; Claim; Directive; Commitment ]
        else
            [ Directive; Decision; Claim; Commitment; Episode ]

    let private temporalModeFor (text: string) (risk: RiskFlags) =
        let lower = text.ToLowerInvariant()

        if lowerContainsAny [ "what changed"; "changed since"; "since then" ] lower then
            ChangedSince
        elif lowerContainsAny [ "earlier"; "original"; "at that time"; "previously"; "before" ] lower then
            IncludeHistory
        elif risk.conflictLikely then
            IncludeHistory
        else
            CurrentOnly

    let createSupervisorDecision
        (snapshot: TranscriptSnapshot)
        (judgement: RealtimeJudgement option)
        : SupervisorDecision =
        let text = snapshot.text |> Text.normalizeWhitespace
        let textRisk = riskFromText text

        let risk =
            match judgement with
            | None -> textRisk
            | Some judgement ->
                { memoryMutation = textRisk.memoryMutation || judgement.riskFlags.memoryMutation
                  sensitive = textRisk.sensitive || judgement.riskFlags.sensitive
                  conflictLikely = textRisk.conflictLikely || judgement.riskFlags.conflictLikely }

        let accepted = acceptedRealtimeJudgement judgement

        let budget, path, reason =
            match judgement with
            | Some j when
                accepted
                && (j.topicContinuity
                    |> Option.exists (fun value -> stringEquals value "same_topic"))
                && (j.needsExternalContext |> Option.exists not)
                ->
                Fast, ZeroController, "Accepted low-risk realtime same-topic judgement; using fast typed recall."
            | _ when risk.memoryMutation || risk.sensitive || risk.conflictLikely ->
                Large,
                LargeController,
                "Escalated deterministic scheduler because the turn may mutate, expose, or correct memory."
            | Some j when j.confidence < 0.45 ->
                Large,
                LargeController,
                "Escalated deterministic scheduler because realtime judgement confidence was low."
            | _ -> Medium, SmallController, "Using deterministic small-controller policy for normal typed recall."

        let spec =
            { query = text
              kinds = kindsForQuery text risk
              scopes = [ Session; User; Workspace; Global ]
              namespaceId = defaultNamespace
              temporalMode = temporalModeFor text risk
              temporalReference =
                match temporalModeFor text risk with
                | ChangedSince -> Some(snapshot.receivedAt.AddHours(-12.))
                | _ -> None
              includeSuperseded = risk.conflictLikely || temporalModeFor text risk = IncludeHistory
              recallBudget = budget
              maxCandidates = recallBudgetMaxCandidates budget
              minScore = None
              latencyBudget = recallBudgetLatency budget }

        { controllerPath = path
          recallSpec = spec
          acceptedRealtimeJudgement = accepted
          riskFlags = risk
          reason = reason }

    let renderRecallSpec decision =
        let spec = decision.recallSpec
        let kinds = spec.kinds |> List.map kindName |> String.concat ", "
        let scopes = spec.scopes |> List.map scopeName |> String.concat ", "

        [ $"strategy: {controllerPathName decision.controllerPath}"
          $"reason: {decision.reason}"
          $"accepted_realtime_judgement: {decision.acceptedRealtimeJudgement}"
          $"kinds: {kinds}"
          $"scopes: {scopes}"
          $"temporal_mode: {temporalModeName spec.temporalMode}"
          $"budget: {budgetName spec.recallBudget}"
          $"max_candidates: {spec.maxCandidates}" ]
        |> String.concat "\n"

    let private eligibleBySpec (spec: RecallSpec) (record: MemoryRecord) =
        let now = DateTimeOffset.UtcNow

        let statusOk =
            if spec.includeSuperseded then
                record.status <> Retracted && record.status <> Expired
            else
                record.status = Current || record.status = Uncertain

        let temporalOk =
            match spec.temporalMode, spec.temporalReference with
            | CurrentOnly, _ ->
                match record.temporal.validTo with
                | Some validTo when validTo <= now -> false
                | _ -> true
            | AsOfTime, Some at ->
                record.temporal.validFrom <= at
                && (record.temporal.validTo |> Option.forall (fun validTo -> at <= validTo))
            | ChangedSince, Some since ->
                record.temporal.observedAt >= since || record.temporal.lastConfirmedAt >= since
            | IncludeHistory, _ -> true
            | _, None -> true

        statusOk
        && temporalOk
        && record.sensitivity <> Secret
        && (List.isEmpty spec.kinds || List.contains record.kind spec.kinds)
        && (List.isEmpty spec.scopes || List.contains record.scope spec.scopes)
        && stringEquals record.namespaceId spec.namespaceId

    let private termSet value = Text.terms value |> Set.ofList

    let private lexicalScore query value =
        let queryTerms = termSet query

        if Set.isEmpty queryTerms then
            0.0f
        else
            let valueTerms = termSet value
            let matched = Set.intersect queryTerms valueTerms
            let overlap = float32 matched.Count / float32 (max 1 queryTerms.Count)

            let exactBoost =
                let normalizedQuery = normalizeKey query
                let normalizedValue = normalizeKey value

                if normalizedQuery.Length >= 12 && normalizedValue.Contains normalizedQuery then
                    0.5f
                else
                    0.0f

            overlap + exactBoost

    let private recencyBoost (record: MemoryRecord) =
        let age = DateTimeOffset.UtcNow - record.temporal.lastConfirmedAt

        if age.TotalDays <= 1. then 0.12f
        elif age.TotalDays <= 7. then 0.08f
        elif age.TotalDays <= 30. then 0.04f
        else 0.0f

    let private kindBoost =
        function
        | Directive -> 0.22f
        | Decision -> 0.20f
        | Commitment -> 0.18f
        | Claim -> 0.12f
        | Episode -> 0.10f

    let private statusBoost =
        function
        | Current -> 0.16f
        | Uncertain -> -0.04f
        | Superseded -> -0.18f
        | Expired -> -0.35f
        | Retracted -> -1.0f

    let private sensitivityPenalty =
        function
        | Normal -> 0.0f
        | Personal -> -0.04f
        | Sensitive -> -0.12f
        | Secret -> -1.0f

    let private adjustedHit (spec: RecallSpec) (baseScore: float32) (reasons: string list) (record: MemoryRecord) =
        let importance = float32 record.importance * 0.12f

        let finalScore =
            baseScore
            + kindBoost record.kind
            + statusBoost record.status
            + recencyBoost record
            + importance
            + sensitivityPenalty record.sensitivity

        { record = record
          score = finalScore
          reasons = reasons }

    let private searchHotOverlay (spec: RecallSpec) (store: Store) : MemoryRecallHit list =
        let hotRecords =
            if Option.isSome store.colbertIndex then
                store.records
                |> List.filter (fun record -> store.hotOverlayIds.Contains record.memoryId)
            else
                store.records

        hotRecords
        |> List.filter (eligibleBySpec spec)
        |> List.choose (fun record ->
            let score = lexicalScore spec.query record.retrieval.indexText

            if score <= 0.0f && spec.recallBudget <> ConflictProbe then
                None
            else
                Some(adjustedHit spec score [ "hot_overlay"; "lexical" ] record))

    let private searchMainIndex
        (encoder: FsColbert.OnnxColbertEncoder option)
        (spec: RecallSpec)
        (store: Store)
        : Async<MemoryRecallHit list> =
        async {
            match encoder, store.colbertIndex with
            | Some encoder, Some index ->
                try
                    let options =
                        { FsColbert.SearchOptions.defaults with
                            maxResults = spec.maxCandidates
                            candidateLimit = max FsColbert.SearchOptions.defaults.candidateLimit spec.maxCandidates
                            useLexicalFilter = true
                            useRRF = true
                            denseWeight = 1.0f
                            lexicalWeight = 0.15f }

                    let recordsById =
                        store.records |> List.map (fun record -> record.memoryId, record) |> Map.ofList

                    let! hits = FsColbert.Search.query encoder options index spec.query

                    return
                        hits
                        |> List.choose (fun hit ->
                            recordsById
                            |> Map.tryFind hit.reference.sourceId
                            |> Option.filter (eligibleBySpec spec)
                            |> Option.map (adjustedHit spec hit.score [ "colbert" ]))
                with _ ->
                    return []
            | _ -> return []
        }

    let recall (encoder: FsColbert.OnnxColbertEncoder option) (spec: RecallSpec) (store: Store) =
        async {
            let! mainHits = searchMainIndex encoder spec store
            let hotHits = searchHotOverlay spec store

            let merged =
                mainHits @ hotHits
                |> List.groupBy _.record.memoryId
                |> List.map (fun (_, hits) ->
                    hits
                    |> List.maxBy _.score
                    |> fun hit ->
                        { hit with
                            reasons = hits |> List.collect _.reasons |> List.distinct })
                |> List.filter (fun hit -> spec.minScore |> Option.forall (fun minScore -> hit.score >= minScore))
                |> List.sortByDescending (fun hit -> hit.score, hit.record.importance)
                |> List.truncate spec.maxCandidates

            return merged
        }

    let renderRecall (hits: MemoryRecallHit list) =
        if List.isEmpty hits then
            "No durable typed memory records matched."
        else
            hits
            |> List.truncate 12
            |> List.mapi (fun index hit ->
                let record = hit.record
                let reason = hit.reasons |> String.concat ", "

                $"[{index + 1}] {kindName record.kind}/{statusName record.status} score={hit.score:F2} ({reason})\n{record.title}\n{Text.truncate 650 record.text}")
            |> String.concat "\n\n"

    let private mergeSourceIds (left: string list) (right: string list) =
        Seq.append left right
        |> Seq.filter (String.IsNullOrWhiteSpace >> not)
        |> Seq.distinctBy _.ToLowerInvariant()
        |> Seq.toList

    let private replaceRecord (updated: MemoryRecord) (records: MemoryRecord list) =
        records
        |> List.map (fun record ->
            if stringEquals record.memoryId updated.memoryId then
                updated
            else
                record)

    let private newMemoryId () =
        let id = Guid.NewGuid().ToString("N")
        $"mem_{id}"

    let private createRecord
        (status: MemoryStatus)
        (supersedes: string list)
        (conflicts: string list)
        (proposal: MemoryWriteProposal)
        : MemoryRecord =
        let now = DateTimeOffset.UtcNow
        let memoryId = newMemoryId ()
        let title = proposal.title |> Text.truncate 90
        let text = proposal.text |> Text.normalizeWhitespace

        let summary =
            proposal.summary
            |> Text.normalizeWhitespace
            |> fun value ->
                if String.IsNullOrWhiteSpace value then
                    Text.truncate 160 text
                else
                    value

        { memoryId = memoryId
          kind = proposal.kind
          title = title
          text = text
          summary = summary
          scope = proposal.scope
          namespaceId = proposal.namespaceId
          entities = proposal.entities |> List.distinctBy _.ToLowerInvariant()
          tags = proposal.tags |> List.distinctBy _.ToLowerInvariant()
          status = status
          confidence = clamp01 proposal.confidence
          importance = clamp01 proposal.importance
          sensitivity = proposal.sensitivity
          temporal =
            { observedAt = proposal.observedAt
              validFrom = proposal.observedAt
              validTo = None
              lastConfirmedAt = now }
          provenance =
            { proposal.provenance with
                sourceIds = sourceIdsFrom proposal.provenance }
          relations =
            { supersedes = supersedes
              supersededBy = None
              conflictsWith = conflicts
              derivedFrom = proposal.provenance.sourceIds }
          retrieval =
            { indexText = indexTextFor proposal.kind title summary text proposal.entities proposal.tags
              colbertDocIds = []
              indexedAt = None
              embeddingModel = None }
          version = 1 }

    let private overlapScore (left: string) (right: string) =
        let leftTerms = termSet left
        let rightTerms = termSet right

        if Set.isEmpty leftTerms || Set.isEmpty rightTerms then
            0.0f
        else
            let intersection = Set.intersect leftTerms rightTerms
            float32 intersection.Count / float32 (min leftTerms.Count rightTerms.Count)

    let private sameKindCandidates (proposal: MemoryWriteProposal) (records: MemoryRecord list) =
        records
        |> List.filter (fun record ->
            record.kind = proposal.kind
            && record.scope = proposal.scope
            && stringEquals record.namespaceId proposal.namespaceId
            && (record.status = Current || record.status = Uncertain))
        |> List.map (fun record -> record, overlapScore proposal.text record.retrieval.indexText)
        |> List.sortByDescending snd

    let private hasContradictionCue (text: string) =
        lowerContainsAny
            [ "actually"
              "instead"
              "no longer"
              "not anymore"
              "don't"
              "do not"
              "correction" ]
            text

    let private classifyConflict (proposal: MemoryWriteProposal) (records: MemoryRecord list) =
        match sameKindCandidates proposal records with
        | [] -> Unrelated, []
        | candidates ->
            let sameText =
                candidates
                |> List.tryFind (fun (record, _) -> normalizeKey record.text = normalizeKey proposal.text)

            match sameText with
            | Some(record, _) when not proposal.explicitCorrection -> Duplicate, [ record ]
            | _ ->
                let strong =
                    candidates |> List.filter (fun (_, score) -> score >= 0.86f) |> List.map fst

                let related =
                    candidates |> List.filter (fun (_, score) -> score >= 0.55f) |> List.map fst

                let weakRelated =
                    candidates |> List.filter (fun (_, score) -> score >= 0.20f) |> List.map fst

                if proposal.explicitCorrection && not (List.isEmpty weakRelated) then
                    Contradiction, weakRelated |> List.truncate 3
                elif hasContradictionCue proposal.text && not (List.isEmpty related) then
                    Contradiction, related |> List.truncate 3
                elif not (List.isEmpty strong) then
                    Duplicate, strong |> List.truncate 1
                elif not (List.isEmpty related) then
                    Refinement, related |> List.truncate 2
                else
                    Unrelated, []

    let private bumpEpoch (kind: MemoryKind) (store: Store) =
        let next = store.epochs |> Map.tryFind kind |> Option.defaultValue 0 |> (+) 1

        { store with
            epochs = store.epochs |> Map.add kind next }

    let commitProposal (store: Store) (proposal: MemoryWriteProposal) =
        let outcome, matches = classifyConflict proposal store.records
        let now = DateTimeOffset.UtcNow

        let store, update =
            match outcome, matches with
            | Duplicate, first :: _ ->
                let updated =
                    { first with
                        confidence = max first.confidence proposal.confidence
                        importance = max first.importance proposal.importance
                        temporal =
                            { first.temporal with
                                lastConfirmedAt = now }
                        provenance =
                            { first.provenance with
                                sourceIds = mergeSourceIds first.provenance.sourceIds proposal.provenance.sourceIds }
                        version = first.version + 1 }

                let store =
                    { store with
                        records = replaceRecord updated store.records
                        hotOverlayIds = store.hotOverlayIds.Add updated.memoryId }
                    |> bumpEpoch updated.kind

                store,
                { proposal = proposal
                  outcome = Duplicate
                  committedRecord = Some updated
                  affectedMemoryIds = [ updated.memoryId ]
                  message = $"Merged duplicate durable memory into {updated.memoryId}." }
            | Refinement, (_ :: _ as targets)
            | Contradiction, (_ :: _ as targets) ->
                let supersededIds = targets |> List.map _.memoryId

                let records =
                    store.records
                    |> List.map (fun record ->
                        if List.contains record.memoryId supersededIds then
                            { record with
                                status = Superseded
                                temporal =
                                    { record.temporal with
                                        validTo = Some now }
                                relations =
                                    { record.relations with
                                        supersededBy = None }
                                version = record.version + 1 }
                        else
                            record)

                let newRecord = createRecord Current supersededIds [] proposal

                let records =
                    records
                    |> List.map (fun record ->
                        if List.contains record.memoryId supersededIds then
                            { record with
                                relations =
                                    { record.relations with
                                        supersededBy = Some newRecord.memoryId } }
                        else
                            record)

                let store =
                    { store with
                        records = newRecord :: records
                        hotOverlayIds = store.hotOverlayIds.Add newRecord.memoryId }
                    |> bumpEpoch proposal.kind

                store,
                { proposal = proposal
                  outcome = outcome
                  committedRecord = Some newRecord
                  affectedMemoryIds = newRecord.memoryId :: supersededIds
                  message =
                    $"Committed {conflictName outcome} as {newRecord.memoryId}; superseded {supersededIds.Length} prior record(s)." }
            | Contradiction, [] ->
                let newRecord = createRecord Uncertain [] [] proposal

                let store =
                    { store with
                        records = newRecord :: store.records
                        hotOverlayIds = store.hotOverlayIds.Add newRecord.memoryId }
                    |> bumpEpoch proposal.kind

                store,
                { proposal = proposal
                  outcome = Contradiction
                  committedRecord = Some newRecord
                  affectedMemoryIds = [ newRecord.memoryId ]
                  message = $"Committed uncertain durable memory {newRecord.memoryId}." }
            | Unrelated, _
            | Refinement, []
            | Retraction, _
            | Duplicate, [] ->
                let newRecord = createRecord Current [] [] proposal

                let store =
                    { store with
                        records = newRecord :: store.records
                        hotOverlayIds = store.hotOverlayIds.Add newRecord.memoryId }
                    |> bumpEpoch proposal.kind

                store,
                { proposal = proposal
                  outcome = Unrelated
                  committedRecord = Some newRecord
                  affectedMemoryIds = [ newRecord.memoryId ]
                  message = $"Committed new durable memory {newRecord.memoryId}." }

        let saveLogs = save store
        store, update, saveLogs

    let commitProposals (store: Store) (proposals: MemoryWriteProposal list) =
        ((store, [], []), proposals)
        ||> List.fold (fun (store, updates, logs) proposal ->
            let store, update, saveLogs = commitProposal store proposal
            store, updates @ [ update ], logs @ saveLogs)

    let private firstRegexGroup (pattern: string) (input: string) =
        let m = Regex.Match(input, pattern, RegexOptions.IgnoreCase)

        if m.Success then
            m.Groups[1].Value |> Text.notEmpty
        else
            None

    let private tryForgetQuery (snapshot: TranscriptSnapshot) =
        let text = snapshot.text |> Text.normalizeWhitespace

        [ @"\b(?:forget|delete)\s+(?:that\s+|about\s+)?(.+)$"
          @"\b(?:don't|do not)\s+remember\s+(?:that\s+|about\s+)?(.+)$" ]
        |> List.tryPick (fun pattern -> firstRegexGroup pattern text)

    let retractFromTurn (store: Store) (snapshot: TranscriptSnapshot) =
        match tryForgetQuery snapshot with
        | None -> store, []
        | Some query ->
            let matches =
                store.records
                |> List.filter (fun record -> record.status = Current || record.status = Uncertain)
                |> List.map (fun record -> record, overlapScore query record.retrieval.indexText)
                |> List.filter (fun (_, score) -> score >= 0.2f)
                |> List.sortByDescending snd
                |> List.truncate 5
                |> List.map fst

            if List.isEmpty matches then
                store, [ $"No durable memory record matched forget request: {Text.truncate 120 query}" ]
            else
                let now = DateTimeOffset.UtcNow
                let ids = matches |> List.map _.memoryId |> Set.ofList
                let affectedKinds = matches |> List.map _.kind |> List.distinct

                let records =
                    store.records
                    |> List.map (fun record ->
                        if ids.Contains record.memoryId then
                            { record with
                                status = Retracted
                                temporal =
                                    { record.temporal with
                                        validTo = Some now }
                                version = record.version + 1 }
                        else
                            record)

                let store =
                    { store with
                        records = records
                        hotOverlayIds = Set.difference store.hotOverlayIds ids }

                let store =
                    affectedKinds |> List.fold (fun store kind -> bumpEpoch kind store) store

                let logs = save store

                store,
                [ $"Retracted {matches.Length} durable memory record(s) for forget request." ]
                @ logs

    let private titleFrom (text: string) =
        text
        |> Text.normalizeWhitespace
        |> Text.truncate 72
        |> fun title ->
            if String.IsNullOrWhiteSpace title then
                "Untitled memory"
            else
                title

    let private wordsAsEntities (text: string) =
        Regex.Matches(text, @"\b[A-Z][\p{L}\p{N}_-]{2,}(?:\s+[A-Z][\p{L}\p{N}_-]{2,})?\b")
        |> Seq.cast<Match>
        |> Seq.map _.Value
        |> Seq.distinctBy _.ToLowerInvariant()
        |> Seq.truncate 8
        |> Seq.toList

    let private baseProvenance (sourceType: string) (sourceIds: string list) : MemoryProvenance =
        { sourceType = sourceType
          sourceIds = sourceIds
          toolName = None
          toolArgsHash = None
          resultHash = None }

    let private proposal
        (kind: MemoryKind)
        (title: string)
        (text: string)
        (summary: string)
        (confidence: float)
        (importance: float)
        (tags: string list)
        (sourceIds: string list)
        (observedAt: DateTimeOffset)
        (explicitCorrection: bool)
        : MemoryWriteProposal =
        { kind = kind
          title = title
          text = text
          summary = summary
          scope = Session
          namespaceId = defaultNamespace
          entities = wordsAsEntities text
          tags = tags
          confidence = confidence
          importance = importance
          sensitivity = if (riskFromText text).sensitive then Sensitive else Normal
          provenance = baseProvenance "user_turn" sourceIds
          observedAt = observedAt
          explicitCorrection = explicitCorrection }

    let private tryExplicitMemoryProposal (snapshot: TranscriptSnapshot) =
        let text = snapshot.text |> Text.normalizeWhitespace
        let lower = text.ToLowerInvariant()
        let sourceIds = [ snapshot.turnId ]
        let observedAt = snapshot.receivedAt

        let mk kind (body: string) tags explicitCorrection =
            let body = body |> Text.normalizeWhitespace

            proposal
                kind
                (titleFrom body)
                body
                (Text.truncate 180 body)
                0.88
                0.78
                tags
                sourceIds
                observedAt
                explicitCorrection

        match firstRegexGroup @"\bremember\s+to\s+(.+)$" text with
        | Some body -> Some(mk Commitment body [ "explicit"; "follow-up" ] false)
        | None ->
            match firstRegexGroup @"\b(?:remember|note)\s+that\s+(.+)$" text with
            | Some body when body.ToLowerInvariant().StartsWith("i prefer") ->
                Some(mk Directive body [ "explicit"; "preference" ] false)
            | Some body -> Some(mk Claim body [ "explicit" ] false)
            | None ->
                if
                    lower.StartsWith("i prefer ")
                    || lower.StartsWith("please always ")
                    || lower.StartsWith("never ")
                then
                    Some(mk Directive text [ "preference" ] false)
                elif
                    lowerContainsAny [ "we decided"; "we agreed"; "let's use"; "use " ] lower
                    && lowerContainsAny [ "decided"; "agreed"; "let's use" ] lower
                then
                    Some(mk Decision text [ "decision" ] false)
                elif
                    lowerContainsAny
                        [ "actually"; "correction"; "instead"; "no longer"; "don't use"; "do not use" ]
                        lower
                then
                    let kind =
                        if
                            lowerContainsAny [ "should"; "use"; "prefer"; "always"; "never"; "don't"; "do not" ] lower
                        then
                            Directive
                        else
                            Claim

                    Some(mk kind text [ "correction" ] true)
                else
                    None

    let private looksTrivialTurn (text: string) =
        let lower = text.ToLowerInvariant()

        lower.Length < 80
        || lowerContainsAny
            [ "what time"
              "what's the time"
              "current time"
              "today's date"
              "hello"
              "hi"
              "thanks"
              "thank you" ]
            lower

    let private episodeProposal (snapshot: TranscriptSnapshot) (answer: string) =
        let userText = snapshot.text |> Text.normalizeWhitespace
        let answer = answer |> Text.normalizeWhitespace

        if looksTrivialTurn userText || userText.Length + answer.Length < 360 then
            None
        else
            let text =
                $"User asked: {userText}\nOracle answered: {Text.truncate 600 answer}"
                |> Text.normalizeWhitespace

            Some(
                proposal
                    Episode
                    (titleFrom userText)
                    text
                    (Text.truncate 220 text)
                    0.72
                    0.48
                    [ "episode"; "conversation" ]
                    [ snapshot.turnId; $"oracle:{snapshot.turnId}" ]
                    snapshot.receivedAt
                    false
            )

    let proposalsFromExchange (snapshot: TranscriptSnapshot) (answer: string) =
        [ match tryExplicitMemoryProposal snapshot with
          | Some proposal -> yield proposal
          | None ->
              match episodeProposal snapshot answer with
              | Some proposal -> yield proposal
              | None -> () ]

    let hashText (value: string) =
        use sha = SHA256.Create()

        Encoding.UTF8.GetBytes value
        |> sha.ComputeHash
        |> Convert.ToHexString
        |> fun hash -> hash.ToLowerInvariant()
