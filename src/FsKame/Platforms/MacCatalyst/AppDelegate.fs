namespace FsKame

open Foundation
open Microsoft.Maui

[<Register("AppDelegate")>]
type AppDelegate() =
    inherit MauiUIApplicationDelegate()

    override this.CreateMauiApp() =
        PdfKitRasterizer.register ()
        MauiProgram.CreateMauiApp()
