namespace FsKame.PdfRasterization

open System
open Android.Graphics
open Android.Graphics.Pdf
open Android.OS
open FsColbert
open FsKame.QA

module PdfRasterizer =
    let private bitmapToRgbImage (bitmap: Bitmap) =
        let width = bitmap.Width
        let height = bitmap.Height
        let colors = Array.zeroCreate<int> (width * height)

        bitmap.GetPixels(colors, 0, width, 0, 0, width, height)

        let pixels = Array.zeroCreate<byte> (width * height * 3)

        for index = 0 to colors.Length - 1 do
            let color = colors[index]
            let offset = index * 3
            pixels[offset] <- byte ((color >>> 16) &&& 0xff)
            pixels[offset + 1] <- byte ((color >>> 8) &&& 0xff)
            pixels[offset + 2] <- byte (color &&& 0xff)

        DoclingRgbImage.create width height pixels

    let private renderPage (options: DoclingHybridOptions) (pageNumber: int) (page: PdfRenderer.Page) =
        let scale = float32 (max 36 options.rasterDpi) / 72.0f
        let width = max 1 (int (MathF.Ceiling(float32 page.Width * scale)))
        let height = max 1 (int (MathF.Ceiling(float32 page.Height * scale)))

        use bitmap = Bitmap.CreateBitmap(width, height, Bitmap.Config.Argb8888)
        use canvas = new Canvas(bitmap)
        canvas.DrawColor(Color.White)

        let destination = new Rect(0, 0, width, height)
        page.Render(bitmap, destination, null, PdfRenderMode.ForDisplay)

        { pageNo = pageNumber
          image = bitmapToRgbImage bitmap }

    type private Rasterizer(options: DoclingHybridOptions) =
        interface IDoclingPageRasterizer with
            member _.RasterizeAsync path =
                async {
                    try
                        use descriptor =
                            ParcelFileDescriptor.Open(new Java.IO.File(path), ParcelFileMode.ReadOnly)

                        use renderer = new PdfRenderer(descriptor)

                        let pages =
                            [ 0 .. renderer.PageCount - 1 ]
                            |> List.map (fun index ->
                                use page = renderer.OpenPage index
                                renderPage options (index + 1) page)

                        if List.isEmpty pages then
                            return Error $"No rasterized pages were produced for '{path}'."
                        else
                            return Ok pages
                    with ex ->
                        return Error $"Unable to rasterize PDF '{path}' with Android PdfRenderer: {ex.Message}"
                }

    let register () =
        DoclingHybrid.setDefaultOptions
            { DoclingHybrid.defaults with
                enableLayoutAnalysis = true }

        DoclingHybrid.setRasterizerFactory (fun options -> Rasterizer(options) :> IDoclingPageRasterizer)
