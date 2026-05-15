namespace FsKame.QA

open System
open System.Diagnostics
open System.IO
open System.Net.Http
open System.Security.Cryptography
open Microsoft.ML.OnnxRuntime
open Microsoft.ML.OnnxRuntime.Tensors
open FsColbert
open RapidOcrNet
open SkiaSharp

type DoclingHybridOptions =
    { minNativeCharsPerPage: int
      ocrDedupeOverlapThreshold: float
      rasterDpi: int
      enableOcr: bool
      enableLayoutAnalysis: bool
      enableFigureClassification: bool }

module DoclingHybrid =
    let defaults =
        { minNativeCharsPerPage = 24
          ocrDedupeOverlapThreshold = 0.75
          rasterDpi = 96
          enableOcr = true
          enableLayoutAnalysis = true
          enableFigureClassification = false }

    let mutable private activeDefaults = defaults

    let setDefaultOptions options = activeDefaults <- options

    let resetDefaultOptions () = activeDefaults <- defaults

    let private timed report label operation =
        async {
            let timer = Stopwatch.StartNew()
            report $"{label}..."

            try
                let! result = operation
                timer.Stop()
                report (sprintf "%s completed in %.1fs." label timer.Elapsed.TotalSeconds)
                return result
            with ex ->
                timer.Stop()
                report (sprintf "%s failed after %.1fs: %s" label timer.Elapsed.TotalSeconds ex.Message)
                return raise ex
        }

    let private withTimeout timeoutMs errorMessage operation =
        async {
            try
                let! child = Async.StartChild(operation, timeoutMs)
                return! child
            with :? TimeoutException ->
                return Error errorMessage
        }

    let private fsColbertRoot storageRoot =
        let path = Path.Combine(storageRoot, "FsKame", "FsColbert")
        Directory.CreateDirectory path |> ignore
        path

    let private doclingModelFolder storageRoot name =
        Path.Combine(fsColbertRoot storageRoot, "Models", name)

    [<Literal>]
    let private ppDocLayoutSModelFileName = "pp-doclayout-s.onnx"

    [<Literal>]
    let private ppDocLayoutSModelUrl =
        "https://github.com/GreatV/oar-ocr/releases/download/v0.3.0/pp-doclayout-s.onnx"

    [<Literal>]
    let private ppDocLayoutSModelSha256 =
        "c2336493a0a13cd9b9b457ca68aea370b327c362a4a7da4917c2bba96029bceb"

    let private rapidOcrModelFolders storageRoot =
        [ Environment.GetEnvironmentVariable "FSKAME_RAPIDOCR_MODELS"
          Path.Combine(fsColbertRoot storageRoot, "Models", "rapidocr")
          Path.Combine(storageRoot, "models", "rapidocr") ]
        |> List.choose Text.notEmpty
        |> List.distinctBy _.ToLowerInvariant()

    let private toBitmap (image: DoclingRgbImage) =
        DoclingRgbImage.validate image

        let bitmap =
            new SKBitmap(image.width, image.height, SKColorType.Rgba8888, SKAlphaType.Opaque)

        for y = 0 to image.height - 1 do
            for x = 0 to image.width - 1 do
                let offset = (y * image.width + x) * 3

                bitmap.SetPixel(
                    x,
                    y,
                    SKColor(image.pixels[offset], image.pixels[offset + 1], image.pixels[offset + 2], 255uy)
                )

        bitmap

    let mutable private rasterizerFactory: (DoclingHybridOptions -> IDoclingPageRasterizer) option =
        None

    let setRasterizerFactory factory = rasterizerFactory <- Some factory

    let clearRasterizerFactory () = rasterizerFactory <- None

    let private createRasterizer options =
        match rasterizerFactory with
        | Some factory -> factory options
        | None ->
            { new IDoclingPageRasterizer with
                member _.RasterizeAsync path =
                    async {
                        return
                            Error
                                $"No Docling PDF rasterizer is registered for '{path}'. Reference FsKame.PdfRasterization and call PdfRasterizer.register() before using Hybrid PDF parsing."
                    } }

    type private RapidOcrProvider(ocr: RapidOcr) =
        let gate = obj ()

        let textBlockCell (block: TextBlock) =
            let text = block.GetText() |> Text.normalizeWhitespace

            if
                String.IsNullOrWhiteSpace text
                || isNull block.BoxPoints
                || block.BoxPoints.Length = 0
            then
                None
            else
                let xs = block.BoxPoints |> Array.map _.X
                let ys = block.BoxPoints |> Array.map _.Y

                Some
                    { text = text
                      bbox =
                        DoclingGeometry.topLeftBox
                            (float (Array.min xs))
                            (float (Array.min ys))
                            (float (Array.max xs))
                            (float (Array.max ys))
                      confidence = Some(float block.BoxScore) }

        interface IDoclingOcrProvider with
            member _.RecognizeAsync page =
                async {
                    try
                        let cells =
                            lock gate (fun () ->
                                use bitmap = toBitmap page.image
                                let result = ocr.Detect(bitmap, RapidOcrOptions.Default)

                                if isNull result.TextBlocks then
                                    []
                                else
                                    result.TextBlocks |> Array.choose textBlockCell |> Array.toList)

                        return Ok cells
                    with ex ->
                        return Error $"RapidOCR failed on page {page.pageNo}: {ex.Message}"
                }

        interface IDisposable with
            member _.Dispose() =
                try
                    ocr.Dispose()
                with _ ->
                    ()

    let private findFile predicate folder =
        if Directory.Exists folder then
            Directory.EnumerateFiles(folder, "*", SearchOption.AllDirectories)
            |> Seq.tryFind (fun path -> predicate (Path.GetFileName(path).ToLowerInvariant()))
        else
            None

    let private findRapidOcrFiles folder =
        let isOnnx (name: string) =
            name.EndsWith(".onnx", StringComparison.OrdinalIgnoreCase)

        let det = findFile (fun name -> isOnnx name && name.Contains "det") folder
        let cls = findFile (fun name -> isOnnx name && name.Contains "cls") folder
        let recModel = findFile (fun name -> isOnnx name && name.Contains "rec") folder

        let keys =
            findFile
                (fun name ->
                    name.EndsWith(".txt", StringComparison.OrdinalIgnoreCase)
                    && (name.Contains "dict" || name.Contains "keys"))
                folder
            |> Option.orElseWith (fun () ->
                findFile (fun name -> name.EndsWith(".txt", StringComparison.OrdinalIgnoreCase)) folder)

        match det, cls, recModel, keys with
        | Some det, Some cls, Some recModel, Some keys -> Some(det, cls, recModel, keys)
        | _ -> None

    let private tryCreateRapidOcrProvider options storageRoot =
        if not options.enableOcr then
            Ok None
        else
            rapidOcrModelFolders storageRoot
            |> List.tryPick (fun folder -> findRapidOcrFiles folder |> Option.map (fun files -> folder, files))
            |> function
                | None -> Ok None
                | Some(_, (det, cls, recModel, keys)) ->
                    let ocr = new RapidOcr()

                    try
                        ocr.InitModels(det, cls, recModel, keys, max 1 (Environment.ProcessorCount / 2))
                        Ok(Some(new RapidOcrProvider(ocr) :> IDoclingOcrProvider))
                    with ex ->
                        try
                            ocr.Dispose()
                        with _ ->
                            ()

                        Error $"RapidOCR model setup failed: {ex.Message}"

    let private sha256Hex (bytes: byte[]) =
        use sha = SHA256.Create()

        sha.ComputeHash bytes
        |> Array.map (fun value -> value.ToString("x2"))
        |> String.concat ""

    let private ensurePpDocLayoutSDownloadedAsync (client: HttpClient) storageRoot =
        async {
            let folder = doclingModelFolder storageRoot "pp-doclayout-s-onnx-v0.3.0"
            Directory.CreateDirectory folder |> ignore
            let path = Path.Combine(folder, ppDocLayoutSModelFileName)

            let isValidModel path =
                File.Exists path
                && String.Equals(sha256Hex (File.ReadAllBytes path), ppDocLayoutSModelSha256, StringComparison.Ordinal)

            if isValidModel path then
                return Ok path
            else
                try
                    let! bytes = client.GetByteArrayAsync(ppDocLayoutSModelUrl) |> Async.AwaitTask
                    let actualHash = sha256Hex bytes

                    if not (String.Equals(actualHash, ppDocLayoutSModelSha256, StringComparison.Ordinal)) then
                        return
                            Error
                                $"PP-DocLayout-S ONNX hash mismatch. Expected {ppDocLayoutSModelSha256}, got {actualHash}."
                    else
                        let tempPath = $"{path}.tmp"
                        File.WriteAllBytes(tempPath, bytes)

                        if File.Exists path then
                            File.Delete path

                        File.Move(tempPath, path)
                        return Ok path
                with ex ->
                    return Error $"Unable to download PP-DocLayout-S ONNX model: {ex.Message}"
        }

    type private PpDocLayoutSOnnx(modelPath: string) =
        let threshold = 0.3f
        let width = 480
        let height = 480
        let scaleFactor = 1.0f / 255.0f

        let labels =
            [ 0, "paragraph_title"
              1, "image"
              2, "text"
              3, "number"
              4, "abstract"
              5, "content"
              6, "figure_title"
              7, "formula"
              8, "table"
              9, "table_title"
              10, "reference"
              11, "doc_title"
              12, "footnote"
              13, "header"
              14, "algorithm"
              15, "footer"
              16, "seal"
              17, "chart_title"
              18, "chart"
              19, "formula_number"
              20, "header_image"
              21, "footer_image"
              22, "aside_text" ]
            |> Map.ofList

        let session = new InferenceSession(modelPath)
        let gate = obj ()

        let inputName name =
            session.InputMetadata.Keys
            |> Seq.tryFind (fun key -> String.Equals(key, name, StringComparison.OrdinalIgnoreCase))
            |> Option.defaultWith (fun () -> failwith $"PP-DocLayout-S ONNX input '{name}' was not found.")

        let imageInputName = inputName "image"
        let scaleFactorInputName = inputName "scale_factor"

        let primaryOutputName =
            session.OutputMetadata.Keys
            |> Seq.tryFind (fun key -> String.Equals(key, "fetch_name_0", StringComparison.OrdinalIgnoreCase))
            |> Option.orElseWith (fun () -> session.OutputMetadata.Keys |> Seq.tryHead)
            |> Option.defaultWith (fun () -> failwith "PP-DocLayout-S ONNX model has no outputs.")

        let resize (image: DoclingRgbImage) =
            use source = toBitmap image

            let resized = new SKBitmap(width, height, SKColorType.Rgba8888, SKAlphaType.Opaque)

            use canvas = new SKCanvas(resized)
            canvas.Clear(SKColors.White)
            canvas.DrawBitmap(source, SKRect(0f, 0f, float32 width, float32 height))

            resized

        let imageInput (image: DoclingRgbImage) =
            use bitmap = resize image
            let values = Array.zeroCreate<float32> (3 * width * height)
            let plane = width * height

            for y = 0 to height - 1 do
                for x = 0 to width - 1 do
                    let color = bitmap.GetPixel(x, y)
                    let offset = y * width + x
                    values[offset] <- float32 color.Red * scaleFactor
                    values[plane + offset] <- float32 color.Green * scaleFactor
                    values[(2 * plane) + offset] <- float32 color.Blue * scaleFactor

            let tensor = DenseTensor<float32>(values, [| 1; 3; height; width |])
            NamedOnnxValue.CreateFromTensor(imageInputName, tensor)

        let scaleFactorInput (image: DoclingRgbImage) =
            let values =
                [| float32 height / float32 image.height; float32 width / float32 image.width |]

            let tensor = DenseTensor<float32>(values, [| 1; 2 |])
            NamedOnnxValue.CreateFromTensor(scaleFactorInputName, tensor)

        let outputTensor (outputs: IDisposableReadOnlyCollection<DisposableNamedOnnxValue>) =
            outputs
            |> Seq.tryFind (fun output -> String.Equals(output.Name, primaryOutputName, StringComparison.Ordinal))
            |> Option.defaultWith (fun () ->
                failwith $"PP-DocLayout-S ONNX output '{primaryOutputName}' was not returned.")
            |> _.AsTensor<float32>()

        let labelFor label =
            match label with
            | "paragraph_title" -> DoclingLabel.SectionHeader
            | "doc_title" -> DoclingLabel.Title
            | "image"
            | "header_image"
            | "footer_image" -> DoclingLabel.Picture
            | "chart" -> DoclingLabel.Chart
            | "figure_title"
            | "table_title"
            | "chart_title" -> DoclingLabel.Caption
            | "formula"
            | "formula_number" -> DoclingLabel.Formula
            | "table" -> DoclingLabel.Table
            | "header" -> DoclingLabel.PageHeader
            | "footer" -> DoclingLabel.PageFooter
            | "footnote" -> DoclingLabel.Footnote
            | "algorithm" -> DoclingLabel.Code
            | _ -> DoclingLabel.Text

        let thresholdFor label =
            match label with
            | "paragraph_title"
            | "formula" -> 0.3f
            | "text" -> 0.4f
            | "seal" -> 0.45f
            | _ -> 0.5f

        let clamp limit value =
            Math.Min(float limit, Math.Max(0.0, float value))

        let boxFor (image: DoclingRgbImage) x1 y1 x2 y2 =
            let normalized = x2 <= 1.05f && y2 <= 1.05f && x1 >= -0.05f && y1 >= -0.05f

            let l, t, r, b =
                if normalized then
                    float x1 * float image.width,
                    float y1 * float image.height,
                    float x2 * float image.width,
                    float y2 * float image.height
                else
                    float x1, float y1, float x2, float y2

            { l = clamp image.width l
              t = clamp image.height t
              r = clamp image.width r
              b = clamp image.height b
              coordOrigin = DoclingCoordinateOrigin.TopLeft }

        let boxArea bbox =
            max 0.0 (bbox.r - bbox.l) * max 0.0 (bbox.b - bbox.t)

        let intersectionOverUnion left right =
            let l = max left.l right.l
            let t = max left.t right.t
            let r = min left.r right.r
            let b = min left.b right.b
            let intersection = max 0.0 (r - l) * max 0.0 (b - t)
            let union = boxArea left + boxArea right - intersection

            if union <= 0.0 then 0.0 else intersection / union

        let applyNms clusters =
            let sorted = clusters |> List.sortByDescending (fun cluster -> cluster.confidence)

            let rec loop kept remaining =
                match remaining with
                | [] -> List.rev kept
                | cluster :: rest ->
                    let shouldKeep =
                        kept
                        |> List.forall (fun keptCluster ->
                            let threshold = if keptCluster.label = cluster.label then 0.6 else 0.98

                            intersectionOverUnion keptCluster.bbox cluster.bbox < threshold)

                    if shouldKeep then
                        loop (cluster :: kept) rest
                    else
                        loop kept rest

            loop [] sorted

        let predictOne (page: DoclingPageInput) =
            let imageInput = imageInput page.image
            let scaleFactorInput = scaleFactorInput page.image

            use outputs = session.Run([ imageInput; scaleFactorInput ])
            let tensor = outputTensor outputs
            let values = tensor |> Seq.toArray

            let boxCount =
                if tensor.Dimensions.Length >= 2 then
                    tensor.Dimensions[0]
                else
                    values.Length / 6

            let clusters =
                [ for index = 0 to boxCount - 1 do
                      let offset = index * 6

                      if offset + 5 < values.Length then
                          let classId = int (MathF.Round values[offset])
                          let score = values[offset + 1]

                          match labels |> Map.tryFind classId with
                          | Some rawLabel when score >= threshold && score >= thresholdFor rawLabel ->
                              let bbox =
                                  boxFor
                                      page.image
                                      values[offset + 2]
                                      values[offset + 3]
                                      values[offset + 4]
                                      values[offset + 5]

                              if bbox.r > bbox.l && bbox.b > bbox.t then
                                  { id = index
                                    label = labelFor rawLabel
                                    confidence = score
                                    bbox = bbox
                                    cells = [] }
                          | _ -> () ]
                |> applyNms
                |> List.truncate 100

            { pageNo = page.pageNo
              clusters = clusters }

        interface IDoclingLayoutPredictor with
            member _.PredictLayoutAsync pages =
                async {
                    try
                        let predictions = lock gate (fun () -> pages |> List.map predictOne)
                        return Ok predictions
                    with ex ->
                        return Error $"PP-DocLayout-S ONNX prediction failed: {ex.Message}"
                }

        interface IDisposable with
            member _.Dispose() = session.Dispose()

    let private loadLayoutPredictor storageRoot =
        async {
            try
                use client = new HttpClient()
                let! modelPath = ensurePpDocLayoutSDownloadedAsync client storageRoot

                match modelPath with
                | Error err -> return Error err
                | Ok modelPath ->
                    let predictor = new PpDocLayoutSOnnx(modelPath)
                    return Ok(predictor :> IDoclingLayoutPredictor, predictor :> IDisposable)
            with ex ->
                return Error $"Unable to initialize PP-DocLayout-S ONNX model: {ex.Message}"
        }

    let private loadFigureClassifier options storageRoot =
        async {
            if not options.enableFigureClassification then
                return Ok(None, None)
            else
                try
                    use client = new HttpClient()

                    let! files =
                        ModelCatalog.ensureDoclingOnnxDownloadedAsync
                            client
                            (doclingModelFolder storageRoot "docling-figure-classifier-v2.5")
                            ModelCatalog.doclingDocumentFigureClassifierV25Onnx

                    let classifier = new DoclingFigureClassifierOnnx(files)

                    return Ok(Some(classifier :> IDoclingFigureClassifier), Some(classifier :> IDisposable))
                with ex ->
                    return Error $"Unable to initialize Docling figure classifier ONNX model: {ex.Message}"
        }

    let buildPageInputs
        (options: DoclingHybridOptions)
        (report: string -> unit)
        (path: string)
        (rasterizer: IDoclingPageRasterizer)
        (nativeTextProvider: string -> Async<Result<DoclingNativePageText list, string>>)
        (ocrProvider: IDoclingOcrProvider option)
        =
        async {
            let! rasterized =
                rasterizer.RasterizeAsync path
                |> timed report $"Docling hybrid rasterizing {Path.GetFileName path} at {max 36 options.rasterDpi} DPI"
                |> withTimeout
                    45000
                    $"Docling hybrid rasterization timed out for {Path.GetFileName path}; falling back to legacy PDF parsing."

            match rasterized with
            | Error err -> return Error err
            | Ok(rasterized: DoclingRasterPage list) ->
                report $"Docling hybrid rasterized {rasterized.Length} page(s) for {Path.GetFileName path}."

                let! nativeResult =
                    nativeTextProvider path
                    |> timed report $"Docling hybrid reading native PDF text for {Path.GetFileName path}"
                    |> withTimeout
                        30000
                        $"Docling hybrid native text extraction timed out for {Path.GetFileName path}; falling back to legacy PDF parsing."

                match nativeResult with
                | Error err -> return Error err
                | Ok nativePages ->
                    report $"Docling hybrid read native text from {nativePages.Length} page(s)."

                    let nativeByPage =
                        nativePages |> List.map (fun page -> page.pageNo, page) |> Map.ofList

                    let rec loop (pages: DoclingRasterPage list) (acc: DoclingPageInput list) =
                        async {
                            match pages with
                            | [] -> return Ok(List.rev acc)
                            | (page: DoclingRasterPage) :: rest ->
                                let nativePage =
                                    nativeByPage
                                    |> Map.tryFind page.pageNo
                                    |> Option.defaultValue
                                        { pageNo = page.pageNo
                                          size =
                                            { width = float page.image.width
                                              height = float page.image.height }
                                          cells = [] }

                                let nativeCells =
                                    DoclingCells.scaleCellsToImage nativePage.size page.image nativePage.cells

                                if DoclingCells.hasEnoughText options.minNativeCharsPerPage nativeCells then
                                    let input: DoclingPageInput =
                                        { pageNo = page.pageNo
                                          image = page.image
                                          ocrCells = nativeCells }

                                    return! loop rest (input :: acc)
                                else
                                    match ocrProvider with
                                    | None ->
                                        return
                                            Error
                                                $"Page {page.pageNo} has insufficient native PDF text and no RapidOCR model is configured."
                                    | Some ocrProvider ->
                                        let! ocrResult =
                                            ocrProvider.RecognizeAsync page
                                            |> timed report $"Docling hybrid OCR page {page.pageNo}"
                                            |> withTimeout
                                                30000
                                                $"Docling hybrid OCR timed out on page {page.pageNo}; falling back to legacy PDF parsing."

                                        match ocrResult with
                                        | Error err -> return Error err
                                        | Ok ocrCells ->
                                            let merged =
                                                DoclingCells.mergePreferPrimary
                                                    (float page.image.height)
                                                    options.ocrDedupeOverlapThreshold
                                                    nativeCells
                                                    ocrCells

                                            let input: DoclingPageInput =
                                                { pageNo = page.pageNo
                                                  image = page.image
                                                  ocrCells = merged }

                                            return! loop rest (input :: acc)
                        }

                    return! loop rasterized []
        }

    let private convertPageInputs
        report
        chunkOptions
        passageSource
        (path: string)
        layoutPredictor
        figureClassifier
        (pageInputs: DoclingPageInput list)
        =
        async {
            let documentName = Path.GetFileNameWithoutExtension path
            report $"Docling hybrid converting {pageInputs.Length} page(s) for {Path.GetFileName path}."

            let! document =
                DoclingStandardHybrid.convertPagesWithOptions
                    DoclingConversionOptions.defaults
                    documentName
                    (Some(Path.GetFileName path))
                    layoutPredictor
                    figureClassifier
                    pageInputs
                |> timed report $"Docling hybrid layout conversion for {Path.GetFileName path}"
                |> withTimeout
                    45000
                    $"Docling hybrid layout conversion timed out for {Path.GetFileName path}; falling back to legacy PDF parsing."

            return document |> Result.map (DoclingPassages.toPassages chunkOptions passageSource)
        }

    let private convertNativeTextOnly
        report
        chunkOptions
        passageSource
        (path: string)
        (pageInputs: DoclingPageInput list)
        =
        let documentName = Path.GetFileNameWithoutExtension path

        let pages =
            pageInputs
            |> List.map (fun page ->
                page.pageNo,
                { pageNo = page.pageNo
                  size =
                    { width = float page.image.width
                      height = float page.image.height } })
            |> Map.ofList

        let textItems =
            pageInputs
            |> List.mapi (fun index page ->
                let text =
                    page.ocrCells
                    |> List.map _.text
                    |> String.concat "\n"
                    |> Text.normalizeWhitespace

                match Text.notEmpty text with
                | None -> None
                | Some text ->
                    let selfRef = $"#/texts/{index}"

                    Some
                        { selfRef = selfRef
                          parent = "#/body"
                          label = DoclingLabel.Text
                          text = text
                          orig = text
                          contentLayer = DoclingContentLayer.Body
                          prov =
                            [ { pageNo = page.pageNo
                                bbox =
                                  { l = 0.0
                                    t = 0.0
                                    r = float page.image.width
                                    b = float page.image.height
                                    coordOrigin = DoclingCoordinateOrigin.TopLeft }
                                charSpan = None } ]
                          keywords = []
                          sourceId = None
                          sourceDisplayName = None })
            |> List.choose id

        if List.isEmpty textItems then
            Error $"Docling hybrid native-text conversion found no readable text in {Path.GetFileName path}."
        else
            report
                $"Docling hybrid using fast native-text conversion for {Path.GetFileName path}; skipping layout ONNX."

            { name = documentName
              originFileName = Some(Path.GetFileName path)
              originMimeType = Some "application/pdf"
              pages = pages
              texts = textItems
              tables = []
              pictures = []
              bodyChildren = textItems |> List.map _.selfRef
              furnitureChildren = [] }
            |> DoclingPassages.toPassages chunkOptions passageSource
            |> Ok

    let readPdfPassagesWithProviders
        options
        report
        chunkOptions
        passageSource
        (path: string)
        rasterizer
        nativeTextProvider
        ocrProvider
        layoutPredictor
        figureClassifier
        =
        async {
            let! pageInputs = buildPageInputs options report path rasterizer nativeTextProvider ocrProvider

            match pageInputs with
            | Error err -> return Error err
            | Ok pageInputs ->
                if options.enableLayoutAnalysis then
                    return!
                        convertPageInputs
                            report
                            chunkOptions
                            passageSource
                            path
                            layoutPredictor
                            figureClassifier
                            pageInputs
                else
                    return convertNativeTextOnly report chunkOptions passageSource path pageInputs
        }

    let private readPdfPassages options report storageRoot chunkOptions passageSource (path: string) =
        async {
            let rasterizer = createRasterizer options
            report $"Docling hybrid PDF parser starting for {Path.GetFileName path}."

            let ocrProviderResult = tryCreateRapidOcrProvider options storageRoot

            report (
                match ocrProviderResult with
                | Ok(Some _) -> "Docling hybrid OCR provider is enabled."
                | Ok None -> "Docling hybrid OCR provider is not configured; native PDF text will be required."
                | Error err -> $"Docling hybrid OCR provider setup failed: {err}"
            )

            match ocrProviderResult with
            | Error err -> return Error err
            | Ok ocrProvider ->
                let disposableOcr =
                    ocrProvider |> Option.map (fun provider -> provider :?> IDisposable)

                try
                    let! pageInputs =
                        buildPageInputs options report path rasterizer DoclingPdfNative.readPageCells ocrProvider

                    match pageInputs with
                    | Error err -> return Error err
                    | Ok pageInputs ->
                        if options.enableLayoutAnalysis then
                            let! layout =
                                loadLayoutPredictor storageRoot
                                |> timed report "Docling hybrid preparing layout ONNX model"
                                |> withTimeout
                                    60000
                                    "Docling hybrid layout model preparation timed out; falling back to legacy PDF parsing."

                            match layout with
                            | Error err -> return Error err
                            | Ok(layoutPredictor, layoutDisposable) ->
                                try
                                    let! figure =
                                        loadFigureClassifier options storageRoot
                                        |> timed report "Docling hybrid preparing figure classifier"
                                        |> withTimeout
                                            30000
                                            "Docling hybrid figure classifier preparation timed out; falling back to legacy PDF parsing."

                                    match figure with
                                    | Error err -> return Error err
                                    | Ok(figureClassifier, figureDisposable) ->
                                        try
                                            return!
                                                convertPageInputs
                                                    report
                                                    chunkOptions
                                                    passageSource
                                                    path
                                                    layoutPredictor
                                                    figureClassifier
                                                    pageInputs
                                        finally
                                            figureDisposable |> Option.iter (fun disposable -> disposable.Dispose())
                                finally
                                    layoutDisposable.Dispose()
                        else
                            return convertNativeTextOnly report chunkOptions passageSource path pageInputs
                finally
                    disposableOcr |> Option.iter (fun disposable -> disposable.Dispose())
        }

    let fallbackToLegacy
        (report: string -> unit)
        (path: string)
        (hybrid: Async<Result<PassageRef list, string>>)
        (legacyReader: unit -> Async<Result<PassageRef list, string>>)
        =
        async {
            match! hybrid with
            | Ok passages ->
                report $"Docling hybrid PDF parser produced {passages.Length} passage(s) for {Path.GetFileName path}."
                return Ok passages
            | Error hybridError ->
                report $"Docling hybrid PDF parser failed for {Path.GetFileName path}: {hybridError}"
                report $"Falling back to PdfPig text extraction for {Path.GetFileName path}."
                let! legacy = legacyReader ()

                return
                    match legacy with
                    | Ok passages -> Ok passages
                    | Error legacyError ->
                        Error
                            $"Docling hybrid PDF parser failed: {hybridError}{Environment.NewLine}Legacy PdfPig parser also failed: {legacyError}"
        }

    let readPdfPassagesWithFallback
        storageRoot
        report
        chunkOptions
        passageSource
        path
        (legacyReader: unit -> Async<Result<PassageRef list, string>>)
        =
        async {
            let hybrid =
                async {
                    try
                        return! readPdfPassages activeDefaults report storageRoot chunkOptions passageSource path
                    with ex ->
                        return Error $"Docling hybrid PDF parser could not start: {ex.Message}"
                }

            return! fallbackToLegacy report path hybrid legacyReader
        }
