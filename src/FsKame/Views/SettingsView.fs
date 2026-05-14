namespace FsKame.Views

open System
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

    let private roleLabel role =
        $"{FsKame.QA.ModelRole.storageName role} model"

    let private roleValue model role =
        model.modelRoleOverrides
        |> Map.tryFind role
        |> Option.defaultValue (FsKame.QA.UseCaseDefinition.model role model.activeUseCase).modelId

    let private facetValue model (field: FsKame.QA.UseCaseSettingsField) =
        model.useCaseSettings
        |> Map.tryFind field.key
        |> Option.orElse field.defaultValue
        |> Option.defaultValue ""

    let private parseBool (value: string) =
        match Boolean.TryParse(value) with
        | true, parsed -> parsed
        | false, _ -> false

    let private isBoolFacet (field: FsKame.QA.UseCaseSettingsField) =
        match (defaultArg (Option.ofObj field.kind) "").Trim().ToLowerInvariant() with
        | "bool"
        | "boolean"
        | "toggle"
        | "switch" -> true
        | _ -> false

    let private settingsForm model =
        let canEditSettings = canEditSettings model
        let roles = FsKame.QA.ModelRole.all
        let baseRows = 8 + roles.Length
        let facetRows = model.activeUseCase.settingsFacets.Length

        let rowDefinitions =
            List.init (baseRows + facetRows) (fun _ -> Dimension.Absolute 48.)

        Grid([ Dimension.Absolute 112.; Dimension.Star; Dimension.Absolute 48. ], rowDefinitions) {
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

            ViewControls.formLabel "Use Case" 1

            Label($"{model.activeUseCase.displayName} ({model.activeUseCase.id})")
                .font(size = 13.)
                .centerVertical()
                .gridRow(1)
                .gridColumn(1)
                .gridColumnSpan(2)
                .margin (2.)

            for row, role in roles |> List.indexed do
                let row = row + 2

                ViewControls.formLabel (roleLabel role) row

                Entry(roleValue model role, fun value -> ModelRoleModelChanged(role, value))
                    .placeholder(roleLabel role)
                    .isEnabled(canEditSettings)
                    .gridRow(row)
                    .gridColumn(1)
                    .gridColumnSpan(2)
                    .margin (2.)

            let runtimeRow = 2 + roles.Length

            ViewControls.formLabel "Retrieval" runtimeRow

            (HStack(spacing = 8.) {
                Switch(model.retrievalMode = FsColbertWithFallback, retrievalModeToggled)
                    .isEnabled(canEditSettings)
                    .centerVertical ()

                Label(RetrievalModes.displayName model.retrievalMode).font(size = 13.).centerVertical ()
            })
                .gridRow(runtimeRow)
                .gridColumn(1)
                .gridColumnSpan(2)
                .margin (2.)

            ViewControls.formLabel "Log Expansions" (runtimeRow + 1)

            Switch(model.logExpansions, LogExpansionsToggled)
                .isEnabled(canEditSettings)
                .gridRow(runtimeRow + 1)
                .gridColumn(1)
                .centerVertical ()

            ViewControls.formLabel "Log Chunks" (runtimeRow + 2)

            Switch(model.logChunks, LogChunksToggled)
                .isEnabled(canEditSettings)
                .gridRow(runtimeRow + 2)
                .gridColumn(1)
                .centerVertical ()

            ViewControls.formLabel "Lexical Filter" (runtimeRow + 3)

            Switch(model.useLexicalFilter, UseLexicalFilterToggled)
                .isEnabled(canEditSettings)
                .gridRow(runtimeRow + 3)
                .gridColumn(1)
                .centerVertical ()

            ViewControls.formLabel "Index Keywords" (runtimeRow + 4)

            Switch(model.elaborateIndexKeywords, ElaborateIndexKeywordsToggled)
                .isEnabled(canEditSettings)
                .gridRow(runtimeRow + 4)
                .gridColumn(1)
                .centerVertical ()

            ViewControls.formLabel "PDF Parser" (runtimeRow + 5)

            (HStack(spacing = 8.) {
                Switch(model.useHybridPdfParsing, UseHybridPdfParsingToggled)
                    .isEnabled(canEditSettings)
                    .centerVertical ()

                Label(if model.useHybridPdfParsing then "Hybrid" else "Legacy").font(size = 13.).centerVertical ()
            })
                .gridRow(runtimeRow + 5)
                .gridColumn(1)
                .gridColumnSpan(2)
                .margin (2.)

            for row, field in model.activeUseCase.settingsFacets |> List.indexed do
                let row = runtimeRow + 6 + row

                ViewControls.formLabel field.label row

                if isBoolFacet field then
                    Switch(
                        parseBool (facetValue model field),
                        fun value -> UseCaseSettingChanged(field.key, string value)
                    )
                        .isEnabled(canEditSettings)
                        .gridRow(row)
                        .gridColumn(1)
                        .centerVertical ()
                else
                    Entry(facetValue model field, fun value -> UseCaseSettingChanged(field.key, value))
                        .placeholder(field.label)
                        .isEnabled(canEditSettings)
                        .gridRow(row)
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

                })
                    .padding (18.)
            )
        )
            .title ("Settings")
