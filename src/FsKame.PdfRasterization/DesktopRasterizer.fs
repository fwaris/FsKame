namespace FsKame.PdfRasterization

open System.IO
open FsColbert
open FsKame.QA
open PDFtoImage
open SkiaSharp

module PdfRasterizer =
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

    type private Rasterizer(options: DoclingHybridOptions) =
        interface IDoclingPageRasterizer with
            member _.RasterizeAsync path =
                async {
                    try
                        let renderOptions =
                            RenderOptions(Dpi = max 36 options.rasterDpi, WithAnnotations = true, WithFormFill = true)

                        let pdfBytes = File.ReadAllBytes path

                        let pages: DoclingRasterPage list =
                            Conversion.ToImages(pdfBytes, "", renderOptions)
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

    let register () =
        DoclingHybrid.setDefaultOptions
            { DoclingHybrid.defaults with
                enableLayoutAnalysis = true }

        DoclingHybrid.setRasterizerFactory (fun options -> Rasterizer(options) :> IDoclingPageRasterizer)
