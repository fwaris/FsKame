namespace FsKame.Views

open Fabulous.Maui
open FsKame
open Microsoft.Maui
open Microsoft.Maui.Controls
open Microsoft.Maui.Graphics
open type Fabulous.Maui.View

module MainView =
    let private logView model =
        Border(
            (Grid([ Dimension.Star; Dimension.Absolute 46. ], [ Dimension.Absolute 44.; Dimension.Star ]) {
                Label("Activity")
                    .font(size = 15., attributes = FontAttributes.Bold)
                    .centerVertical()
                    .gridColumn(0)
                    .gridRow (0)

                (ViewControls.compactIconButton Icons.clear Log_Clear).gridColumn(1).gridRow (0)

                (CollectionView (model.log) (fun item ->
                    Label(item).font(size = 12.).lineBreakMode(LineBreakMode.WordWrap).padding (0., 4.)))
                    .gridColumnSpan(2)
                    .gridRow (1)
            })
                .padding (10.)
        )
            .stroke(SolidColorBrush(Colors.LightGray))
            .strokeThickness(1.)
            .strokeShape (RoundRectangle(CornerRadius(8.)))

    let contentPage (model: Model) =
        ContentPage(
            (Grid(
                [ Dimension.Star ],
                [ Dimension.Absolute 54.
                  Dimension.Absolute 44.
                  Dimension.Absolute 230.
                  Dimension.Star ]
            ) {
                (ToolbarView.main model).gridRow (0)

                Label("Realtime QA with selected PDF sources")
                    .font(size = 14.)
                    .textColor(Colors.DimGray)
                    .centerVertical()
                    .gridRow (1)

                (PdfSourcesView.view model).gridRow (2)

                (logView model).gridRow (3)
            })
                .padding (18.)
        )
            .title ("Realtime QA over PDFs")
