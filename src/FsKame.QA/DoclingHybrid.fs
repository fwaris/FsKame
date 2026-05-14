namespace FsKame.QA

open System
open System.IO
open System.Net.Http
open Microsoft.ML.OnnxRuntime
open Microsoft.ML.OnnxRuntime.Tensors
open FsColbert
open PDFtoImage
open RapidOcrNet
open SkiaSharp

type DoclingHybridOptions =
    { minNativeCharsPerPage: int
      ocrDedupeOverlapThreshold: float
      rasterDpi: int
      enableOcr: bool
      enableFigureClassification: bool }

module DoclingHybrid =
    let defaults =
        { minNativeCharsPerPage = 24
          ocrDedupeOverlapThreshold = 0.75
          rasterDpi = 144
          enableOcr = true
          enableFigureClassification = false }

    let private fsColbertRoot storageRoot =
        let path = Path.Combine(storageRoot, "FsKame", "FsColbert")
        Directory.CreateDirectory path |> ignore
        path

    let private doclingModelFolder storageRoot name =
        Path.Combine(fsColbertRoot storageRoot, "Models", name)

    let private doclingLayoutHeronFloat32Onnx =
        let revision = "ff99ff93d6b0185279bc00ad60f688ca1ac2c44a"

        let baseUrl =
            $"https://huggingface.co/docling-project/docling-layout-heron-onnx/resolve/{revision}"

        { id = "docling-project/docling-layout-heron-onnx-float32-ff99ff9"
          modelUrl = $"{baseUrl}/model.onnx"
          configUrl = $"{baseUrl}/config.json"
          preprocessorConfigUrl = $"{baseUrl}/preprocessor_config.json"
          modelFileName = "model.onnx"
          configFileName = "config.json"
          preprocessorConfigFileName = "preprocessor_config.json" }

    let private rapidOcrModelFolders storageRoot =
        [ Environment.GetEnvironmentVariable "FSKAME_RAPIDOCR_MODELS"
          Path.Combine(fsColbertRoot storageRoot, "Models", "rapidocr")
          Path.Combine(storageRoot, "models", "rapidocr") ]
        |> List.choose Text.notEmpty
        |> List.distinctBy _.ToLowerInvariant()

    let private toRgbImage (bitmap: SKBitmap) =
        let pixels = Array.zeroCreate<byte> (bitmap.Width * bitmap.Height * 3)

        for y = 0 to bitmap.Height - 1 do
            for x = 0 to bitmap.Width - 1 do
                let color = bitmap.GetPixel(x, y)
                let offset = (y * bitmap.Width + x) * 3
                pixels[offset] <- color.Red
                pixels[offset + 1] <- color.Green
                pixels[offset + 2] <- color.Blue

        DoclingRgbImage.create bitmap.Width bitmap.Height pixels

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

    type PdfToImageRasterizer(options: DoclingHybridOptions) =
        interface IDoclingPageRasterizer with
            member _.RasterizeAsync path =
                async {
                    try
                        let renderOptions =
                            RenderOptions(Dpi = max 36 options.rasterDpi, WithAnnotations = true, WithFormFill = true)

                        let pages: DoclingRasterPage list =
                            Conversion.ToImages(path, "", renderOptions)
                            |> Seq.mapi (fun index bitmap ->
                                use bitmap = bitmap

                                { pageNo = index + 1
                                  image = toRgbImage bitmap })
                            |> Seq.toList

                        if List.isEmpty pages then
                            return Error $"No rasterized pages were produced for '{path}'."
                        else
                            return Ok pages
                    with ex ->
                        return Error $"Unable to rasterize PDF '{path}' for Docling hybrid parsing: {ex.Message}"
                }

    let mutable private rasterizerFactory: (DoclingHybridOptions -> IDoclingPageRasterizer) option =
        None

    let setRasterizerFactory factory = rasterizerFactory <- Some factory

    let clearRasterizerFactory () = rasterizerFactory <- None

    let private createRasterizer options =
        match rasterizerFactory with
        | Some factory -> factory options
        | None -> PdfToImageRasterizer(options) :> IDoclingPageRasterizer

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

    type private DoclingLayoutHeronOnnx(files: DoclingOnnxModelFiles) =
        let threshold = 0.3f
        let width = 640
        let height = 640
        let rescaleFactor = 0.00392156862745098f

        let labels =
            [ 0, "caption"
              1, "footnote"
              2, "formula"
              3, "list_item"
              4, "page_footer"
              5, "page_header"
              6, "picture"
              7, "section_header"
              8, "table"
              9, "text"
              10, "title"
              11, "document_index"
              12, "code"
              13, "checkbox_selected"
              14, "checkbox_unselected"
              15, "form"
              16, "key_value_region" ]
            |> Map.ofList

        let session = new InferenceSession(files.modelPath)
        let gate = obj ()

        let inputName name =
            session.InputMetadata.Keys
            |> Seq.tryFind (fun key -> String.Equals(key, name, StringComparison.OrdinalIgnoreCase))
            |> Option.defaultWith (fun () -> failwith $"Docling layout ONNX input '{name}' was not found.")

        let imagesInputName = inputName "images"
        let originalTargetSizesInputName = inputName "orig_target_sizes"

        let outputName name =
            session.OutputMetadata.Keys
            |> Seq.tryFind (fun key -> String.Equals(key, name, StringComparison.OrdinalIgnoreCase))
            |> Option.defaultWith (fun () -> failwith $"Docling layout ONNX output '{name}' was not found.")

        let labelsOutputName = outputName "labels"
        let boxesOutputName = outputName "boxes"
        let scoresOutputName = outputName "scores"

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
                    values[offset] <- float32 color.Red * rescaleFactor
                    values[plane + offset] <- float32 color.Green * rescaleFactor
                    values[(2 * plane) + offset] <- float32 color.Blue * rescaleFactor

            let tensor = DenseTensor<float32>(values, [| 1; 3; height; width |])
            NamedOnnxValue.CreateFromTensor(imagesInputName, tensor)

        let originalTargetSizesInput (image: DoclingRgbImage) =
            let tensor =
                DenseTensor<int64>([| int64 image.height; int64 image.width |], [| 1; 2 |])

            NamedOnnxValue.CreateFromTensor(originalTargetSizesInputName, tensor)

        let outputTensor name (outputs: IDisposableReadOnlyCollection<DisposableNamedOnnxValue>) =
            outputs
            |> Seq.tryFind (fun output -> String.Equals(output.Name, name, StringComparison.Ordinal))
            |> Option.defaultWith (fun () -> failwith $"Docling layout ONNX output '{name}' was not returned.")
            |> _.AsTensor<'T>()

        let labelFor labelId =
            labels
            |> Map.tryFind labelId
            |> Option.defaultValue $"label_{labelId}"
            |> DoclingLabels.ofJsonValue

        let clamp limit value =
            Math.Min(float limit, Math.Max(0.0, float value))

        let boxFor (image: DoclingRgbImage) (box: float32[]) =
            let l = clamp image.width box[0]
            let t = clamp image.height box[1]
            let r = clamp image.width box[2]
            let b = clamp image.height box[3]

            { l = l
              t = t
              r = r
              b = b
              coordOrigin = DoclingCoordinateOrigin.TopLeft }

        let predictOne (page: DoclingPageInput) =
            let imageInput = imageInput page.image
            let targetSizesInput = originalTargetSizesInput page.image

            use outputs = session.Run([ imageInput; targetSizesInput ])

            let labelsTensor: Tensor<int64> = outputTensor labelsOutputName outputs
            let boxesTensor: Tensor<float32> = outputTensor boxesOutputName outputs
            let scoresTensor: Tensor<float32> = outputTensor scoresOutputName outputs

            let labelValues = labelsTensor |> Seq.toArray
            let boxValues = boxesTensor |> Seq.toArray
            let scoreValues = scoresTensor |> Seq.toArray

            let queryCount =
                if scoresTensor.Dimensions.Length >= 2 then
                    scoresTensor.Dimensions[1]
                else
                    scoreValues.Length

            let clusters =
                [ for index = 0 to queryCount - 1 do
                      let score = scoreValues[index]

                      if score >= threshold then
                          let label = labelFor (int labelValues[index])
                          let boxOffset = index * 4

                          let bbox =
                              boxFor
                                  page.image
                                  [| boxValues[boxOffset]
                                     boxValues[boxOffset + 1]
                                     boxValues[boxOffset + 2]
                                     boxValues[boxOffset + 3] |]

                          { id = index
                            label = label
                            confidence = score
                            bbox = bbox
                            cells = [] } ]

            { pageNo = page.pageNo
              clusters = clusters }

        interface IDoclingLayoutPredictor with
            member _.PredictLayoutAsync pages =
                async {
                    try
                        let predictions = lock gate (fun () -> pages |> List.map predictOne)
                        return Ok predictions
                    with ex ->
                        return Error $"Docling layout ONNX prediction failed: {ex.Message}"
                }

        interface IDisposable with
            member _.Dispose() = session.Dispose()

    let private loadLayoutPredictor storageRoot =
        async {
            try
                use client = new HttpClient()

                let! files =
                    ModelCatalog.ensureDoclingOnnxDownloadedAsync
                        client
                        (doclingModelFolder storageRoot "docling-layout-heron-float32-ff99ff9")
                        doclingLayoutHeronFloat32Onnx

                let predictor = new DoclingLayoutHeronOnnx(files)
                return Ok(predictor :> IDoclingLayoutPredictor, predictor :> IDisposable)
            with ex ->
                return Error $"Unable to initialize Docling layout ONNX model: {ex.Message}"
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
        (path: string)
        (rasterizer: IDoclingPageRasterizer)
        (nativeTextProvider: string -> Async<Result<DoclingNativePageText list, string>>)
        (ocrProvider: IDoclingOcrProvider option)
        =
        async {
            let! rasterized = rasterizer.RasterizeAsync path

            match rasterized with
            | Error err -> return Error err
            | Ok(rasterized: DoclingRasterPage list) ->
                let! nativeResult = nativeTextProvider path

                match nativeResult with
                | Error err -> return Error err
                | Ok nativePages ->
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
                                        let! ocrResult = ocrProvider.RecognizeAsync page

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

    let readPdfPassagesWithProviders
        options
        chunkOptions
        passageSource
        path
        rasterizer
        nativeTextProvider
        ocrProvider
        layoutPredictor
        figureClassifier
        =
        async {
            let! pageInputs = buildPageInputs options path rasterizer nativeTextProvider ocrProvider

            match pageInputs with
            | Error err -> return Error err
            | Ok pageInputs ->
                let documentName = Path.GetFileNameWithoutExtension path

                let! document =
                    DoclingStandardHybrid.convertPagesWithOptions
                        DoclingConversionOptions.defaults
                        documentName
                        (Some(Path.GetFileName path))
                        layoutPredictor
                        figureClassifier
                        pageInputs

                return document |> Result.map (DoclingPassages.toPassages chunkOptions passageSource)
        }

    let private readPdfPassages options storageRoot chunkOptions passageSource path =
        async {
            let rasterizer = createRasterizer options

            let ocrProviderResult = tryCreateRapidOcrProvider options storageRoot

            match ocrProviderResult with
            | Error err -> return Error err
            | Ok ocrProvider ->
                let disposableOcr =
                    ocrProvider |> Option.map (fun provider -> provider :?> IDisposable)

                try
                    let! layout = loadLayoutPredictor storageRoot

                    match layout with
                    | Error err -> return Error err
                    | Ok(layoutPredictor, layoutDisposable) ->
                        try
                            let! figure = loadFigureClassifier options storageRoot

                            match figure with
                            | Error err -> return Error err
                            | Ok(figureClassifier, figureDisposable) ->
                                try
                                    return!
                                        readPdfPassagesWithProviders
                                            options
                                            chunkOptions
                                            passageSource
                                            path
                                            rasterizer
                                            DoclingPdfNative.readPageCells
                                            ocrProvider
                                            layoutPredictor
                                            figureClassifier
                                finally
                                    figureDisposable |> Option.iter (fun disposable -> disposable.Dispose())
                        finally
                            layoutDisposable.Dispose()
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
                        return! readPdfPassages defaults storageRoot chunkOptions passageSource path
                    with ex ->
                        return Error $"Docling hybrid PDF parser could not start: {ex.Message}"
                }

            return! fallbackToLegacy report path hybrid legacyReader
        }
