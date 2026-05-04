namespace FsKame.Views

open Fabulous.Maui
open FsKame
open Microsoft.Maui
open Microsoft.Maui.Controls
open Microsoft.Maui.Graphics
open type Fabulous.Maui.View

module ViewControls =
    let iconButton icon (msg: Msg) =
        Button(icon, msg)
            .font(size = 29., fontFamily = C.FONT_SYMBOLS)
            .background(Colors.Transparent)
            .textColor(Colors.Magenta)
            .width(44.)
            .height(44.)
            .padding (0.)

    let compactIconButton icon (msg: Msg) =
        Button(icon, msg)
            .font(size = 24., fontFamily = C.FONT_SYMBOLS)
            .background(Colors.Transparent)
            .textColor(Colors.Magenta)
            .width(42.)
            .height(42.)
            .padding (0.)

    let compactDangerIconButton icon (msg: Msg) =
        Button(icon, msg)
            .font(size = 24., fontFamily = C.FONT_SYMBOLS)
            .background(Colors.Transparent)
            .textColor(Colors.Firebrick)
            .width(42.)
            .height(42.)
            .padding (0.)

    let formLabel text row =
        Label(text)
            .font(size = 13., attributes = FontAttributes.Bold)
            .alignEndHorizontal()
            .centerVertical()
            .gridRow(row)
            .gridColumn(0)
            .margin (2.)

    let sourceEditor (title: string) (value: string) (msg: string -> Msg) =
        Border(
            (Grid([ Dimension.Absolute 26.; Dimension.Star ], [ Dimension.Star ]) {
                Label(title).font(size = 13., attributes = FontAttributes.Bold).gridRow (0)

                Editor(value, msg)
                    .autoSize(EditorAutoSizeOption.TextChanges)
                    .font(size = 13.)
                    .placeholder("One per line")
                    .gridRow (1)
            })
                .padding (10.)
        )
            .stroke(SolidColorBrush(Colors.LightGray))
            .strokeThickness(1.)
            .strokeShape(RoundRectangle(CornerRadius(8.)))
            .margin (0., 4.)
