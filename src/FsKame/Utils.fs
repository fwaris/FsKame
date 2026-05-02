namespace FsKame

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
        Regex.Replace(defaultArg (Option.ofObj value) "", @"\s+", " ").Trim()

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
