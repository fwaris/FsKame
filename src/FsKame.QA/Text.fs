namespace FsKame.QA

open System
open System.Text.RegularExpressions

module Text =
    let notEmpty (value: string) =
        if String.IsNullOrWhiteSpace value then
            None
        else
            Some(value.Trim())

    let splitLines (value: string) =
        if String.IsNullOrWhiteSpace value then
            []
        else
            value.Replace("\r\n", "\n").Split('\n')
            |> Seq.choose notEmpty
            |> Seq.distinct
            |> Seq.toList

    let normalizeWhitespace (value: string) =
        let dehyphenated =
            Regex.Replace(defaultArg (Option.ofObj value) "", @"(\p{L})-\s+(\p{Ll})", "$1$2")

        Regex.Replace(dehyphenated, @"\s+", " ").Trim()

    let stripHtml (html: string) =
        let withoutScripts = Regex.Replace(html, "(?is)<(script|style).*?</\\1>", " ")
        let withoutTags = Regex.Replace(withoutScripts, "(?is)<[^>]+>", " ")
        System.Net.WebUtility.HtmlDecode(withoutTags) |> normalizeWhitespace

    let terms (value: string) =
        Regex.Matches(value.ToLowerInvariant(), "[a-z0-9][a-z0-9_-]{2,}")
        |> Seq.cast<Match>
        |> Seq.map _.Value
        |> Seq.distinct
        |> Seq.toList

    let truncate maxChars (value: string) =
        if String.IsNullOrEmpty value || value.Length <= maxChars then
            value
        else
            value.Substring(0, maxChars).TrimEnd() + "..."

    type ProcessedQuery =
        { normalizedQuery: string
          searchTerms: string list
          rewrittenQueries: string list }

    module QueryPostProcessing =
        let private regexOptions = RegexOptions.IgnoreCase ||| RegexOptions.CultureInvariant

        let private replace (pattern: string) (replacement: string) (value: string) =
            Regex.Replace(value, pattern, replacement, regexOptions)

        let private applyReplacements replacements value =
            replacements
            |> List.fold (fun current (pattern, replacement) -> replace pattern replacement current) value

        let private fillerReplacements =
            [ @"\b(?:um+|uh+|er+|ah+|hmm)\b[,\s]*", " "
              @"\b(?:you\s+know|i\s+mean)\b[,\s]*", " "
              @"^\s*(?:okay|ok|hey|so)\b[,\s]*", ""
              @"^\s*(?:can|could|would)\s+you\s+(?:please\s+)?", "" ]

        let private profileReplacements (profile: QaUseCaseProfile) =
            let profile = QaUseCaseProfile.sanitize profile

            profile.voiceReplacements
            |> List.map (fun replacement -> replacement.pattern, replacement.replacement)

        let private boundary phrase =
            @"(?<![\p{L}\p{N}])" + Regex.Escape(phrase) + @"(?![\p{L}\p{N}])"

        let private containsPhrase (phrase: string) (value: string) =
            Regex.IsMatch(value, boundary phrase, regexOptions)

        let private expansionTerms profile normalized =
            let profile = QaUseCaseProfile.sanitize profile

            profile.queryExpansionRules
            |> List.collect (fun rule ->
                if rule.triggers |> List.exists (fun trigger -> containsPhrase trigger normalized) then
                    rule.terms
                else
                    [])
            |> Seq.distinctBy _.ToLowerInvariant()
            |> Seq.toList

        let forVoiceLikeRetrievalWithProfile profile query =
            let normalized =
                defaultArg (Option.ofObj query) ""
                |> normalizeWhitespace
                |> applyReplacements (profileReplacements profile)
                |> applyReplacements fillerReplacements
                |> normalizeWhitespace

            let fallback =
                if String.IsNullOrWhiteSpace normalized then
                    normalizeWhitespace query
                else
                    normalized

            let expandedTerms = expansionTerms profile fallback

            let searchTerms =
                seq {
                    yield fallback
                    yield! expandedTerms
                }
                |> Seq.collect terms
                |> Seq.distinct
                |> Seq.truncate 32
                |> Seq.toList

            let expandedQuery =
                if List.isEmpty expandedTerms then
                    fallback
                else
                    let expansionText = String.concat " " expandedTerms
                    normalizeWhitespace $"{fallback} {expansionText}"

            { normalizedQuery = fallback
              searchTerms = searchTerms
              rewrittenQueries =
                [ fallback; expandedQuery; normalizeWhitespace query ]
                |> Seq.choose notEmpty
                |> Seq.distinctBy _.ToLowerInvariant()
                |> Seq.truncate 3
                |> Seq.toList }

        let forVoiceLikeRetrieval query =
            forVoiceLikeRetrievalWithProfile QaUseCaseProfile.generic query

module ModelCapabilities =
    let private temperatureUnsupportedPrefixes = [ "gpt-5.5" ]

    let supportsTemperature (modelId: string) =
        let modelId =
            modelId
            |> Option.ofObj
            |> Option.defaultValue ""
            |> fun value -> value.Trim().ToLowerInvariant()

        temperatureUnsupportedPrefixes
        |> List.exists (fun prefix -> modelId = prefix || modelId.StartsWith(prefix + "-"))
        |> not

module QaDefaults =
    let nanoModel = "gpt-5-nano"
    let answerModel = "gpt-5.5"
    let memoryCandidateChunks = 14
    let maxContextChunks = 12
    let neighborSeeds = 4
