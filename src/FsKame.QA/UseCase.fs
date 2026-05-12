namespace FsKame.QA

open System
open System.IO
open System.Security.Cryptography
open System.Text
open System.Text.Json

[<CLIMutable>]
type TextReplacement =
    { pattern: string; replacement: string }

[<CLIMutable>]
type QueryExpansionRule =
    { triggers: string list
      terms: string list }

[<CLIMutable>]
type QaUseCaseProfile =
    { id: string
      displayName: string
      description: string option
      voiceReplacements: TextReplacement list
      queryExpansionRules: QueryExpansionRule list
      keywordInstruction: string option
      keywordHints: string list
      answerSystemInstruction: string option
      judgeSystemInstruction: string option }

module QaUseCaseProfile =
    let private notEmpty (value: string) =
        if String.IsNullOrWhiteSpace value then
            None
        else
            Some(value.Trim())

    let generic =
        { id = "generic"
          displayName = "Generic QA"
          description = Some "General-purpose knowledge and memory question answering."
          voiceReplacements = []
          queryExpansionRules = []
          keywordInstruction = None
          keywordHints = []
          answerSystemInstruction = None
          judgeSystemInstruction = None }

    let private jsonOptions =
        let options = JsonSerializerOptions(PropertyNameCaseInsensitive = true)
        options.AllowTrailingCommas <- true
        options.ReadCommentHandling <- JsonCommentHandling.Skip
        options

    let private cleanList values =
        values
        |> Option.ofObj
        |> Option.defaultValue []
        |> List.choose notEmpty
        |> List.distinctBy _.ToLowerInvariant()

    let sanitize (profile: QaUseCaseProfile) =
        let id = profile.id |> notEmpty |> Option.defaultValue generic.id

        let displayName =
            profile.displayName |> notEmpty |> Option.defaultValue generic.displayName

        { profile with
            id = id
            displayName = displayName
            description = profile.description |> Option.bind notEmpty
            voiceReplacements =
                profile.voiceReplacements
                |> Option.ofObj
                |> Option.defaultValue []
                |> List.choose (fun replacement ->
                    match notEmpty replacement.pattern with
                    | None -> None
                    | Some pattern ->
                        Some
                            { pattern = pattern
                              replacement = replacement.replacement |> Option.ofObj |> Option.defaultValue "" })
            queryExpansionRules =
                profile.queryExpansionRules
                |> Option.ofObj
                |> Option.defaultValue []
                |> List.choose (fun rule ->
                    let triggers = cleanList rule.triggers
                    let terms = cleanList rule.terms

                    if List.isEmpty triggers || List.isEmpty terms then
                        None
                    else
                        Some { triggers = triggers; terms = terms })
            keywordInstruction = profile.keywordInstruction |> Option.bind notEmpty
            keywordHints = cleanList profile.keywordHints
            answerSystemInstruction = profile.answerSystemInstruction |> Option.bind notEmpty
            judgeSystemInstruction = profile.judgeSystemInstruction |> Option.bind notEmpty }

    let fromJsonText (json: string) =
        JsonSerializer.Deserialize<QaUseCaseProfile>(json, jsonOptions)
        |> Option.ofObj
        |> Option.defaultValue generic
        |> sanitize

    let load path = File.ReadAllText path |> fromJsonText

    let tryLoad path =
        try
            if String.IsNullOrWhiteSpace path || not (File.Exists path) then
                None
            else
                Some(load path)
        with _ ->
            None

    let renderHints (profile: QaUseCaseProfile) =
        let profile = sanitize profile

        if List.isEmpty profile.keywordHints then
            ""
        else
            profile.keywordHints |> String.concat ", "

    let fingerprint (profile: QaUseCaseProfile) =
        let profile = sanitize profile

        let canonical =
            JsonSerializer.Serialize(
                {| id = profile.id
                   displayName = profile.displayName
                   description = profile.description
                   voiceReplacements = profile.voiceReplacements
                   queryExpansionRules = profile.queryExpansionRules
                   keywordInstruction = profile.keywordInstruction
                   keywordHints = profile.keywordHints
                   answerSystemInstruction = profile.answerSystemInstruction
                   judgeSystemInstruction = profile.judgeSystemInstruction |},
                jsonOptions
            )

        use sha = SHA256.Create()
        let bytes = Encoding.UTF8.GetBytes canonical

        sha.ComputeHash bytes
        |> Convert.ToHexString
        |> fun hash -> hash.ToLowerInvariant()
