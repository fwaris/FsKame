namespace FsKame.Views

open Fabulous.Maui
open FsKame
open Microsoft.Maui
open Microsoft.Maui.Controls
open Microsoft.Maui.Graphics
open type Fabulous.Maui.View

module ToolbarView =
    let private connectionColor model =
        match model.sessionState with
        | RTOpenAI.WebRTC.State.Connecting -> Colors.Orange
        | RTOpenAI.WebRTC.State.Connected -> Colors.Magenta
        | _ -> Colors.Gray

    let main model =
        (Grid([ Dimension.Absolute 52.; Dimension.Star; Dimension.Absolute 52. ], [ Dimension.Absolute 50. ]) {
            (ViewControls.iconButton Icons.settings Settings_Show)
                .isEnabled(not (model.sessionState = RTOpenAI.WebRTC.State.Connecting))
                .alignStartHorizontal()
                .gridColumn (0)

            Label("FsKame")
                .font(size = 20., attributes = FontAttributes.Bold)
                .centerHorizontal()
                .centerVertical()
                .gridColumn (1)

            (ViewControls.iconButton Icons.power StartStop)
                .isEnabled(not model.isBusy)
                .textColor(connectionColor model)
                .alignEndHorizontal()
                .gridColumn (2)
        })

    let settings =
        (Grid([ Dimension.Absolute 52.; Dimension.Star; Dimension.Absolute 52. ], [ Dimension.Absolute 50. ]) {
            (ViewControls.iconButton Icons.back Settings_Close).alignStartHorizontal().gridColumn (0)

            Label("Settings")
                .font(size = 20., attributes = FontAttributes.Bold)
                .centerHorizontal()
                .centerVertical()
                .gridColumn (1)
        })
