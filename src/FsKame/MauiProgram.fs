namespace FsKame

open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open Microsoft.Maui.Hosting
open Fabulous.Maui

type MauiProgram =
    static member CreateMauiApp() =
        let builder =
            MauiApp
                .CreateBuilder()
                .UseFabulousApp(App.view)
                .ConfigureFonts(fun fonts ->
                    fonts
                        .AddFont("OpenSans-Regular.ttf", "OpenSansRegular")
                        .AddFont("OpenSans-Semibold.ttf", "OpenSansSemibold")
                        .AddFont("MaterialSymbols.ttf", "MaterialSymbols")
                    |> ignore)

        builder.Services.AddLogging(fun logging ->
            logging.AddConsole() |> ignore
            logging.AddFilter("FsKameLog", LogLevel.Information) |> ignore
            logging.AddFilter("RTOpenAILog", LogLevel.Information) |> ignore)
        |> ignore

        builder.Logging.AddConsole() |> ignore

        let app = builder.Build()
        FSharp.DI.DI.init app.Services
        app
