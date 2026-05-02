namespace FsKame.WorkFlow

open System
open System.IO
open FsKame
open UglyToad.PdfPig.Content
open UglyToad.PdfPig
open UglyToad.PdfPig.DocumentLayoutAnalysis.TextExtractor

module KnowledgeSources =
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
        let text = Text.normalizeWhitespace text

        if String.IsNullOrWhiteSpace text then
            []
        else
            let step = max 1 (size - overlap)

            let rec loop index offset acc =
                if offset >= text.Length then
                    List.rev acc
                else
                    let len = min size (text.Length - offset)
                    let snip = text.Substring(offset, len).Trim()
                    let acc = if snip = "" then acc else (index, snip) :: acc
                    loop (index + 1) (offset + step) acc

            loop 0 0 []

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
                              score = 0 }
                        )
                | Error err -> errors.Add err

            return List.ofSeq chunks, List.ofSeq errors
        }

    let rank (query: string) (maxResults: int) (chunks: SourceChunk list) : SourceChunk list =
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
                    Some { chunk with score = score })
            |> List.sortByDescending (fun (c: SourceChunk) -> c.score, c.text.Length * -1)
            |> List.truncate maxResults

    let renderContext (chunks: SourceChunk list) =
        if List.isEmpty chunks then
            "No external source context was available."
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
