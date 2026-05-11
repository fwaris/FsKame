namespace FsKame.Views

open Fabulous.Maui
open FsKame
open Microsoft.Maui
open Microsoft.Maui.Controls
open Microsoft.Maui.Graphics
open type Fabulous.Maui.View

module SettingsView =
    let private isRealtimeActive model =
        model.bundle.IsSome || model.sessionState <> RTOpenAI.WebRTC.State.Disconnected

    let private canEditSettings model =
        not model.isBusy && not (isRealtimeActive model)

    let private retrievalModeToggled enabled =
        if enabled then
            RetrievalModeChanged FsColbertWithFallback
        else
            RetrievalModeChanged InternalDocumentIndex

    let private settingsForm model =
        let canEditSettings = canEditSettings model

        Grid(
            [ Dimension.Absolute 112.; Dimension.Star; Dimension.Absolute 48. ],
            [ Dimension.Absolute 48.
              Dimension.Absolute 48.
              Dimension.Absolute 48.
              Dimension.Absolute 48.
              Dimension.Absolute 48.
              Dimension.Absolute 48.
              Dimension.Absolute 48. ]
        ) {
            ViewControls.formLabel "OpenAI key" 0

            Entry(model.openAiKey, OpenAiKeyChanged)
                .isPassword(model.hideSecrets)
                .placeholder("OpenAI API key")
                .isEnabled(canEditSettings)
                .gridRow(0)
                .gridColumn(1)
                .margin (2.)

            (ViewControls.compactIconButton
                (if model.hideSecrets then
                     Icons.visible
                 else
                     Icons.visibilityOff)
                ToggleSecretVisibility)
                .isEnabled(canEditSettings)
                .gridRow(0)
                .gridColumn (2)

            ViewControls.formLabel "Oracle model" 1

            Entry(model.oracleModel, OracleModelChanged)
                .placeholder("Oracle model")
                .isEnabled(canEditSettings)
                .gridRow(1)
                .gridColumn(1)
                .gridColumnSpan(2)
                .margin (2.)

            ViewControls.formLabel "Retrieval" 2

            (HStack(spacing = 8.) {
                Switch(model.retrievalMode = FsColbertWithFallback, retrievalModeToggled)
                    .isEnabled(canEditSettings)
                    .centerVertical ()

                Label(RetrievalModes.displayName model.retrievalMode).font(size = 13.).centerVertical ()
            })
                .gridRow(2)
                .gridColumn(1)
                .gridColumnSpan(2)
                .margin (2.)

            ViewControls.formLabel "Log Expansions" 3

            Switch(model.logExpansions, LogExpansionsToggled)
                .isEnabled(canEditSettings)
                .gridRow(3)
                .gridColumn(1)
                .centerVertical ()

            ViewControls.formLabel "Log Chunks" 4

            Switch(model.logChunks, LogChunksToggled)
                .isEnabled(canEditSettings)
                .gridRow(4)
                .gridColumn(1)
                .centerVertical ()

            ViewControls.formLabel "Lexical Filter" 5

            Switch(model.useLexicalFilter, UseLexicalFilterToggled)
                .isEnabled(canEditSettings)
                .gridRow(5)
                .gridColumn(1)
                .centerVertical ()

            ViewControls.formLabel "Index Keywords" 6

            Switch(model.elaborateIndexKeywords, ElaborateIndexKeywordsToggled)
                .isEnabled(canEditSettings)
                .gridRow(6)
                .gridColumn(1)
                .centerVertical ()
        }

    let contentPage (model: Model) =
        ContentPage(
            ScrollView(
                (VStack(spacing = 12.) {
                    ToolbarView.settings

                    settingsForm model

                })
                    .padding (18.)
            )
        )
            .title ("Settings")
