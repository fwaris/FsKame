namespace FsKame

open Foundation
open Microsoft.Maui

[<Register("AppDelegate")>]
type AppDelegate() =
    inherit MauiUIApplicationDelegate()

    override _.CreateMauiApp() =
        PdfKitRasterizer.register ()
        MauiProgram.CreateMauiApp()
