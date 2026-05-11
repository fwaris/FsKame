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

        let private voiceReplacements =
            [ @"\be[\s.\-]+o[\s.\-]+i\b", "EOI"
              @"\bl[\s.\-]+t[\s.\-]+c\b", "LTC"
              @"\bc[\s.\-]+m[\s.\-]+s\b", "CMS"
              @"\ba[\s.\-]+c[\s.\-]+a\b", "ACA"
              @"\bh[\s.\-]+m[\s.\-]+o\b", "HMO"
              @"\bp[\s.\-]+p[\s.\-]+o\b", "PPO"
              @"\bh[\s.\-]+s[\s.\-]+a\b", "HSA"
              @"\bf[\s.\-]+s[\s.\-]+a\b", "FSA"
              @"\bs[\s.\-]+s[\s.\-]+d[\s.\-]+i\b", "SSDI"
              @"\bc[\s.\-]+o[\s.\-]+b[\s.\-]+r[\s.\-]+a\b", "COBRA"
              @"\bmed\s+(?:a\s+)?gap\b", "Medigap"
              @"\bmedicare\s+gap\b", "Medigap"
              @"\brenter['’]?s\b", "renters"
              @"\bhome\s+owner['’]?s\b", "homeowners"
              @"\bpit\s*bull\b", "pitbull"
              @"\bfifteen\s+hundred\b", "1500"
              @"\bone\s+five\s+zero\s+zero\b", "1500"
              @"\bform\s+fifteen\s+hundred\b", "form 1500" ]

        let private fillerReplacements =
            [ @"\b(?:um+|uh+|er+|ah+|hmm)\b[,\s]*", " "
              @"\b(?:you\s+know|i\s+mean)\b[,\s]*", " "
              @"^\s*(?:okay|ok|hey|so)\b[,\s]*", ""
              @"^\s*(?:can|could|would)\s+you\s+(?:please\s+)?", "" ]

        let private boundary phrase =
            @"(?<![\p{L}\p{N}])" + Regex.Escape(phrase) + @"(?![\p{L}\p{N}])"

        let private containsPhrase (phrase: string) (value: string) =
            Regex.IsMatch(value, boundary phrase, regexOptions)

        let private expansionRules =
            [ [ "eoi"; "evidence of insurability" ], [ "evidence"; "insurability"; "underwriting"; "proof"; "health" ]
              [ "ltc"; "long term care" ],
              [ "long"
                "term"
                "care"
                "ltc"
                "policy"
                "benefits"
                "claims"
                "paying"
                "ability"
                "financial"
                "strength"
                "contractual"
                "language"
                "nursing"
                "home"
                "custodial"
                "assisted"
                "living" ]
              [ "medigap"; "medicare supplement" ], [ "medigap"; "medicare"; "supplement"; "plan"; "part"; "coverage" ]
              [ "cms 1500"; "claim form 1500"; "form 1500" ], [ "cms"; "1500"; "claim"; "form"; "health"; "insurance" ]
              [ "workers comp"; "workers compensation" ],
              [ "workers"; "compensation"; "disability"; "work"; "injury"; "benefits" ]
              [ "renters insurance"; "renters"; "rental insurance" ],
              [ "renters"
                "renter"
                "tenant"
                "apartment"
                "personal"
                "property"
                "liability"
                "deductible"
                "covered"
                "loss"
                "fire"
                "smoke"
                "lightning"
                "claim" ]
              [ "homeowners"; "home insurance"; "property insurance" ],
              [ "homeowners"
                "home"
                "property"
                "dwelling"
                "liability"
                "coverage"
                "exclusion"
                "deductible"
                "agent" ]
              [ "pitbull"; "dog bite"; "dog breed" ],
              [ "pitbull"
                "dog"
                "breed"
                "animal"
                "bite"
                "liability"
                "exclude"
                "restrict"
                "deny"
                "coverage"
                "surcharge" ]
              [ "health insurance"; "affordable care act"; "obamacare"; "aca" ],
              [ "health"
                "insurance"
                "affordable"
                "care"
                "act"
                "aca"
                "obamacare"
                "enrollment"
                "marketplace"
                "exchange"
                "subsidy"
                "medicaid"
                "mandate"
                "coverage"
                "broker" ]
              [ "pregnant"; "pregnancy"; "maternity" ],
              [ "pregnant"
                "pregnancy"
                "maternity"
                "medicaid"
                "special"
                "enrollment"
                "essential"
                "benefits"
                "coverage" ]
              [ "claim investigated"; "investigated"; "investigation"; "red flags"; "fraud" ],
              [ "claim"
                "investigation"
                "investigated"
                "fraud"
                "red"
                "flags"
                "proof"
                "residency"
                "receipts"
                "bank"
                "statements"
                "due"
                "diligence" ]
              [ "disability insurance"; "disability" ],
              [ "disability"
                "income"
                "replacement"
                "work"
                "social"
                "security"
                "ssdi"
                "short"
                "term"
                "long"
                "benefits"
                "employer"
                "group" ]
              [ "corporation"; "business" ],
              [ "corporation"
                "business"
                "employer"
                "sponsored"
                "group"
                "executive"
                "deduct"
                "premiums"
                "taxable"
                "benefits" ]
              [ "tree falling"; "tree"; "falling on my car" ],
              [ "tree"
                "falling"
                "car"
                "auto"
                "comprehensive"
                "homeowners"
                "deductible"
                "coverage"
                "exclusion" ]
              [ "quote"; "quotes" ],
              [ "quote"
                "quotes"
                "rate"
                "premium"
                "estimate"
                "agent"
                "broker"
                "coverage"
                "cost"
                "customer"
                "service"
                "replacement" ]
              [ "variable life" ],
              [ "variable"
                "life"
                "whole"
                "universal"
                "risk"
                "cash"
                "value"
                "policy"
                "expenses"
                "performance"
                "payments" ]
              [ "file a renters insurance claim"
                "submit a renters insurance claim"
                "renters insurance claim" ],
              [ "file"
                "submit"
                "claim"
                "renters"
                "agent"
                "broker"
                "insurer"
                "damaged"
                "stolen"
                "items"
                "liability" ]
              [ "buying life insurance"; "life insurance" ],
              [ "life"
                "insurance"
                "guarantees"
                "price"
                "income"
                "assets"
                "beneficiaries"
                "affordable"
                "budget"
                "agent"
                "coverage" ]
              [ "least expensive auto"; "least expensive car"; "cheap auto"; "cheap car" ],
              [ "least"
                "expensive"
                "cheap"
                "inexpensive"
                "low"
                "cost"
                "auto"
                "car"
                "rates"
                "premium"
                "compare" ]
              [ "biggest car insurance"; "largest car insurance" ],
              [ "biggest"
                "largest"
                "car"
                "auto"
                "insurance"
                "state"
                "farm"
                "allstate"
                "progressive"
                "farmers"
                "geico"
                "usaa" ] ]

        let private expansionTerms normalized =
            expansionRules
            |> List.collect (fun (triggers, terms) ->
                if triggers |> List.exists (fun trigger -> containsPhrase trigger normalized) then
                    terms
                else
                    [])
            |> Seq.distinctBy _.ToLowerInvariant()
            |> Seq.toList

        let forVoiceLikeRetrieval query =
            let normalized =
                defaultArg (Option.ofObj query) ""
                |> normalizeWhitespace
                |> applyReplacements voiceReplacements
                |> applyReplacements fillerReplacements
                |> normalizeWhitespace

            let fallback =
                if String.IsNullOrWhiteSpace normalized then
                    normalizeWhitespace query
                else
                    normalized

            let expandedTerms = expansionTerms fallback

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
