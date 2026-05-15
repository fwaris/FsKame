namespace FsKame.PdfRasterization

open System
open System.Runtime.InteropServices
open CoreGraphics
open FsColbert
open FsKame.QA

module PdfRasterizer =
    let private bgraToRgbImage width height (buffer: byte[]) =
        let pixels = Array.zeroCreate<byte> (width * height * 3)

        for y = 0 to height - 1 do
            for x = 0 to width - 1 do
                let sourceOffset = (y * width + x) * 4
                let offset = (y * width + x) * 3
                pixels[offset] <- buffer[sourceOffset + 2]
                pixels[offset + 1] <- buffer[sourceOffset + 1]
                pixels[offset + 2] <- buffer[sourceOffset]

        DoclingRgbImage.create width height pixels

    let private renderPage (options: DoclingHybridOptions) (page: CGPDFPage) =
        let bounds = page.GetBoxRect(CGPDFBox.Crop)
        let scale = float (max 36 options.rasterDpi) / 72.0
        let width = max 1 (int (Math.Ceiling(double bounds.Width * scale)))
        let height = max 1 (int (Math.Ceiling(double bounds.Height * scale)))
        let bytesPerPixel = 4
        let bytesPerRow = width * bytesPerPixel
        let buffer = Array.zeroCreate<byte> (bytesPerRow * height)
        let handle = GCHandle.Alloc(buffer, GCHandleType.Pinned)

        try
            use colorSpace = CGColorSpace.CreateDeviceRGB()

            let bitmapInfo =
                CGBitmapFlags.PremultipliedFirst ||| CGBitmapFlags.ByteOrder32Little

            use context =
                new CGBitmapContext(handle.AddrOfPinnedObject(), width, height, 8, bytesPerRow, colorSpace, bitmapInfo)

            context.SetFillColorSpace(colorSpace)
            context.SetFillColor([| NFloat 1.0; NFloat 1.0; NFloat 1.0; NFloat 1.0 |])
            context.FillRect(CGRect(NFloat 0.0, NFloat 0.0, NFloat width, NFloat height))
            context.SaveState()

            let targetRect = CGRect(NFloat 0.0, NFloat 0.0, NFloat width, NFloat height)
            let transform = page.GetDrawingTransform(CGPDFBox.Crop, targetRect, 0, true)
            context.ConcatCTM(transform)
            context.DrawPDFPage(page)
            context.RestoreState()

            Ok(bgraToRgbImage width height buffer)
        finally
            handle.Free()

    type private Rasterizer(options: DoclingHybridOptions) =
        interface IDoclingPageRasterizer with
            member _.RasterizeAsync path =
                async {
                    try
                        use document = CGPDFDocument.FromFile path

                        if isNull document then
                            return Error $"CoreGraphics could not open '{path}'."
                        elif document.IsEncrypted && not document.IsUnlocked then
                            return Error $"CoreGraphics could not open locked PDF '{path}'."
                        else
                            let pages =
                                [ 1 .. int document.Pages ]
                                |> List.map (fun pageNumber ->
                                    use page = document.GetPage(pageNumber)

                                    if isNull page then
                                        Error $"CoreGraphics could not load page {pageNumber} from '{path}'."
                                    else
                                        renderPage options page
                                        |> Result.map (fun image ->
                                            ({ pageNo = pageNumber; image = image }: DoclingRasterPage)))

                            let errors =
                                pages
                                |> List.choose (function
                                    | Error err -> Some err
                                    | Ok _ -> None)

                            if not (List.isEmpty errors) then
                                return Error(String.concat Environment.NewLine errors)
                            else
                                let rasterized =
                                    pages
                                    |> List.choose (function
                                        | Ok page -> Some page
                                        | Error _ -> None)

                                if List.isEmpty rasterized then
                                    return Error $"No rasterized pages were produced for '{path}'."
                                else
                                    return Ok rasterized
                    with ex ->
                        return Error $"Unable to rasterize PDF '{path}' with CoreGraphics: {ex.Message}"
                }

    let register () =
        DoclingHybrid.setDefaultOptions
            { DoclingHybrid.defaults with
                enableLayoutAnalysis = true }

        DoclingHybrid.setRasterizerFactory (fun options -> Rasterizer(options) :> IDoclingPageRasterizer)
