namespace FsKame.Views

open Fabulous.Maui
open FsKame
open Microsoft.Maui
open Microsoft.Maui.Controls
open Microsoft.Maui.Graphics
open type Fabulous.Maui.View

module SettingsView =
    let private settingsForm model =
        Grid(
            [ Dimension.Absolute 112.; Dimension.Star; Dimension.Absolute 48. ],
            [ Dimension.Absolute 48.; Dimension.Absolute 48.; Dimension.Absolute 48. ]
        ) {
            ViewControls.formLabel "OpenAI key" 0

            Entry(model.openAiKey, OpenAiKeyChanged)
                .isPassword(model.hideSecrets)
                .placeholder("OpenAI API key")
                .gridRow(0)
                .gridColumn(1)
                .margin (2.)

            (ViewControls.compactIconButton
                (if model.hideSecrets then
                     Icons.visible
                 else
                     Icons.visibilityOff)
                ToggleSecretVisibility)
                .gridRow(0)
                .gridColumn (2)

            ViewControls.formLabel "Oracle model" 1

            Entry(model.oracleModel, OracleModelChanged)
                .placeholder("Oracle model")
                .gridRow(1)
                .gridColumn(1)
                .gridColumnSpan(2)
                .margin (2.)

            ViewControls.formLabel "Status" 2

            Label(Update.statusText model)
                .textColor(Update.statusColor model)
                .font(size = 13., attributes = FontAttributes.Bold)
                .centerVertical()
                .gridRow(2)
                .gridColumn(1)
                .gridColumnSpan(2)
                .margin (2.)
        }

    let contentPage (model: Model) =
        ContentPage(
            ScrollView(
                (VStack(spacing = 12.) {
                    ToolbarView.settings

                    settingsForm model

                    HStack(spacing = 8.) {
                        (ViewControls.compactIconButton Icons.save ApplySources).isEnabled (not model.isBusy)
                    }
                })
                    .padding (18.)
            )
        )
            .title ("Settings")
