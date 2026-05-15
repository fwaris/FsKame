namespace FsKame.Views

open Fabulous.Maui
open FsKame
open Microsoft.Maui
open Microsoft.Maui.Controls
open Microsoft.Maui.Graphics
open type Fabulous.Maui.View

module PdfSourcesView =
    let private isRealtimeActive model =
        model.bundle.IsSome || model.sessionState <> RTOpenAI.WebRTC.State.Disconnected

    let private canMutateDocuments model =
        not model.isBusy && not (isRealtimeActive model)

    let private canChangeSourceSelection model =
        not model.isBusy && not (isRealtimeActive model)

    let private addPdfButton model =
        Button(Icons.add, PickPdfs)
            .font(size = 22., fontFamily = C.FONT_SYMBOLS)
            .background(Colors.Magenta)
            .textColor(Colors.White)
            .cornerRadius(17)
            .width(34.)
            .height(34.)
            .padding(0.)
            .margin(0, -2, 2, 0)
            .isEnabled(canMutateDocuments model)
            .alignEndHorizontal()
            .centerVertical ()

    let private statusColor doc =
        match doc.status with
        | Ready -> Colors.SeaGreen
        | Processing -> Colors.DarkOrange
        | Queued -> Colors.DimGray
        | Failed -> Colors.Firebrick

    let private emptyView =
        Border(
            Label("No documents added")
                .font(size = 13.)
                .textColor(Colors.DimGray)
                .centerHorizontal()
                .centerVertical()
                .padding (12.)
        )
            .stroke(SolidColorBrush(Colors.LightGray))
            .strokeThickness(1.)
            .strokeShape (RoundRectangle(CornerRadius(8.)))

    let private row processingActive canMutateDocuments canChangeSourceSelection (doc: PdfDocumentSource) =
        let deleteIcon =
            if processingActive then
                Icons.deleteForever
            else
                Icons.delete

        let deleteMessage =
            if processingActive then
                CancelPdfProcessing
            else
                DeletePdf doc.id

        Border(
            (Grid(
                [ Dimension.Absolute 42.
                  Dimension.Star
                  Dimension.Absolute 48.
                  Dimension.Absolute 48. ],
                [ Dimension.Absolute 28.; Dimension.Absolute 28. ]
            ) {
                CheckBox(doc.selected, fun selected -> PdfSelectionChanged(doc.id, selected))
                    .isEnabled(canChangeSourceSelection && PdfDocuments.canSelect doc)
                    .centerVertical()
                    .gridRowSpan(2)
                    .gridColumn (0)

                Label(doc.displayName)
                    .font(size = 14., attributes = FontAttributes.Bold)
                    .lineBreakMode(LineBreakMode.MiddleTruncation)
                    .centerVertical()
                    .gridColumn(1)
                    .gridRow (0)

                Label(PdfDocuments.statusText doc)
                    .font(size = 12.)
                    .textColor(statusColor doc)
                    .lineBreakMode(LineBreakMode.TailTruncation)
                    .centerVertical()
                    .gridColumn(1)
                    .gridRow (1)

                if doc.status = Failed then
                    (ViewControls.compactIconButton Icons.play (RetryPdfProcessing doc.id))
                        .isEnabled(canMutateDocuments)
                        .gridColumn(2)
                        .gridRowSpan (2)

                (ViewControls.compactDangerIconButton deleteIcon deleteMessage)
                    .isEnabled(processingActive || canMutateDocuments)
                    .gridColumn(3)
                    .gridRowSpan (2)
            })
                .padding (8.)
        )
            .stroke(SolidColorBrush(Colors.LightGray))
            .strokeThickness(1.)
            .strokeShape(RoundRectangle(CornerRadius(8.)))
            .margin (0., 0., 0., 6.)

    let view model =
        let canMutateDocuments = canMutateDocuments model
        let canChangeSourceSelection = canChangeSourceSelection model
        let processingActive = model.documentProcessingCancellation.IsSome

        Border(
            (Grid([ Dimension.Star; Dimension.Absolute 40. ], [ Dimension.Absolute 44.; Dimension.Star ]) {
                Label("Document Sources")
                    .font(size = 15., attributes = FontAttributes.Bold)
                    .centerVertical()
                    .gridColumn(0)
                    .gridRow (0)

                (addPdfButton model).gridColumn(1).gridRow (0)

                if List.isEmpty model.pdfDocuments then
                    emptyView.gridColumnSpan(2).gridRow (1)
                else
                    (CollectionView
                        (model.pdfDocuments)
                        (row processingActive canMutateDocuments canChangeSourceSelection))
                        .gridColumnSpan(2)
                        .gridRow (1)
            })
                .padding (10.)
        )
            .stroke(SolidColorBrush(Colors.LightGray))
            .strokeThickness(1.)
            .strokeShape (RoundRectangle(CornerRadius(8.)))
