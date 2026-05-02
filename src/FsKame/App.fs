namespace FsKame

open Fabulous
open Fabulous.Maui
open type Fabulous.Maui.View

module App =
    let private page model =
        match model.currentPage with
        | Main -> Views.MainView.contentPage model
        | Settings -> Views.SettingsView.contentPage model

    let program =
        Program.statefulWithCmd Update.init Update.update
        |> Program.withSubscription Update.subscribeMailbox

    let view () =
        Component("FsKame") {
            let! model = Context.Mvu(program)

            Application() { Window(page model) }
        }
