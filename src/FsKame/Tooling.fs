namespace FsKame.WorkFlow

open System
open System.Collections.Concurrent
open System.ComponentModel
open System.IO
open System.Reflection
open System.Runtime.InteropServices
open System.Threading.Tasks
open FsKame
open FsKame.Tools
open Microsoft.Maui.Storage
open Microsoft.SemanticKernel

type ToolHostContext(report: string -> unit, blackboard: ConcurrentQueue<MemoryCard>) =
    member val Retrieval = KnowledgeSources.emptyIndex with get, set
    member val LogExpansions = false with get, set
    member val LogChunks = false with get, set
    member val UseLexicalFilter = true with get, set
    member _.Report message = report message
    member _.Blackboard = blackboard

    member this.RankSelectedSources(question: string, maxResults: int) =
        KnowledgeSources.rank
            None
            this.LogExpansions
            this.LogChunks
            this.UseLexicalFilter
            this.Report
            question
            maxResults
            this.Retrieval

type IToolProvider =
    abstract ContractVersion: int
    abstract GetToolTypes: unit -> Type list
    abstract GetPlugins: ToolHostContext -> KernelPlugin list

type ToolCatalog =
    { plugins: KernelPlugin list
      logs: string list }

module ToolCatalog =
    let empty = { plugins = []; logs = [] }

type SelectedSourceSearchTool(context: ToolHostContext) =
    [<KernelFunction("selected_source_search")>]
    [<Description("Searches the currently selected PDF and Markdown sources for passages relevant to the request.")>]
    member _.Search
        (
            [<Description("The user's source-related question or standalone retrieval request.")>] question: string,
            [<Description("The maximum number of source passages to return.")>] maxResults: int
        ) =
        task {
            let maxResults = if maxResults <= 0 then 6 else min maxResults 12
            let! chunks = context.RankSelectedSources(question, maxResults) |> Async.StartAsTask
            return KnowledgeSources.renderContext chunks
        }

type SourceInventoryTool(context: ToolHostContext) =
    [<KernelFunction("source_inventory")>]
    [<Description("Lists the selected PDF and Markdown sources available to this FsKame session.")>]
    member _.Inventory() =
        context.Retrieval.sources |> KnowledgeSources.renderInventory

type BlackboardSearchTool(context: ToolHostContext) =
    [<KernelFunction("blackboard_search")>]
    [<Description("Searches recent task-board and blackboard cards for follow-up context.")>]
    member _.Search([<Description("A word or phrase to search for in recent memory cards.")>] query: string) =
        let query = query |> FsKame.Text.normalizeWhitespace

        let cards =
            context.Blackboard.ToArray()
            |> Array.rev
            |> Array.filter (fun card ->
                String.IsNullOrWhiteSpace query
                || card.title.Contains(query, StringComparison.OrdinalIgnoreCase)
                || card.content.Contains(query, StringComparison.OrdinalIgnoreCase))
            |> Array.truncate 8

        if Array.isEmpty cards then
            "No matching blackboard cards were found."
        else
            cards
            |> Array.mapi (fun index card -> $"[{index + 1}] {card.kind}: {card.title}\n{card.content}")
            |> String.concat "\n\n"

module ToolLoader =
    let contractVersion = 1

    let defaultProviderFolder () =
        Path.Combine(FileSystem.AppDataDirectory, "tool-providers")

    let private builtInToolTypes =
        [ typeof<SelectedSourceSearchTool>
          typeof<SourceInventoryTool>
          typeof<BlackboardSearchTool>
          typeof<CurrentTimeTool> ]

    let private ctorAcceptsContext (toolType: Type) =
        toolType.GetConstructor([| typeof<ToolHostContext> |]) |> isNull |> not

    let private createTool context (toolType: Type) =
        try
            if ctorAcceptsContext toolType then
                Activator.CreateInstance(toolType, [| context :> obj |]) |> Ok
            else
                match toolType.GetConstructor(Type.EmptyTypes) with
                | null ->
                    Error
                        $"Tool type {toolType.FullName} must expose a public constructor that accepts ToolHostContext or a public parameterless constructor."
                | ctor -> ctor.Invoke(Array.empty) |> Ok
        with ex ->
            Error $"Unable to create tool type {toolType.FullName}: {ex.Message}"

    let private createPlugin context pluginName (toolTypes: Type list) =
        let tools = ResizeArray<obj>()
        let errors = ResizeArray<string>()

        for toolType in toolTypes do
            match createTool context toolType with
            | Ok tool -> tools.Add tool
            | Error err -> errors.Add err

        if tools.Count = 0 then
            None, List.ofSeq errors
        else
            try
                let functions =
                    tools
                    |> Seq.collect (fun tool -> KernelPluginFactory.CreateFromObject(tool, pluginName))
                    |> Seq.toList

                Some(KernelPluginFactory.CreateFromFunctions(pluginName, functions)), List.ofSeq errors
            with ex ->
                None, $"Unable to create plugin {pluginName}: {ex.Message}" :: List.ofSeq errors

    let private instantiateProvider (providerType: Type) =
        try
            match providerType.GetConstructor(Type.EmptyTypes) with
            | null -> Error $"Provider {providerType.FullName} must expose a public parameterless constructor."
            | ctor -> ctor.Invoke(Array.empty) :?> IToolProvider |> Ok
        with ex ->
            Error $"Unable to create provider {providerType.FullName}: {ex.Message}"

    let private providerTypes (assembly: Assembly) =
        try
            assembly.GetTypes()
            |> Array.filter (fun t -> t.IsPublic && not t.IsAbstract && typeof<IToolProvider>.IsAssignableFrom t)
            |> Array.toList
        with ex ->
            ignore ex
            []

    let private loadExternalProviderAssemblies folder =
        if RuntimeInformation.IsOSPlatform(OSPlatform.Create("IOS")) then
            [], [ "Dynamic tool provider loading is disabled on iOS; bundled providers can still be scanned." ]
        elif not (Directory.Exists folder) then
            [], [ $"Tool provider folder does not exist: {folder}" ]
        else
            let assemblies = ResizeArray<Assembly>()
            let errors = ResizeArray<string>()

            for path in Directory.EnumerateFiles(folder, "*.dll", SearchOption.TopDirectoryOnly) do
                try
                    assemblies.Add(Assembly.LoadFrom path)
                with ex ->
                    errors.Add $"Skipping tool provider assembly {path}: {ex.Message}"

            List.ofSeq assemblies, List.ofSeq errors

    let private pluginFunctionKeys (plugin: KernelPlugin) =
        plugin |> Seq.map (fun fn -> $"{plugin.Name}.{fn.Name}") |> Seq.toList

    let private validateUniqueFunctions (plugins: KernelPlugin list) =
        let keys = plugins |> List.collect pluginFunctionKeys

        keys
        |> List.countBy id
        |> List.choose (fun (key, count) ->
            if count > 1 then
                Some $"Duplicate Semantic Kernel function registered: {key}"
            else
                None)

    let private dedupePlugins (plugins: KernelPlugin seq) =
        let seen = Collections.Generic.HashSet<string>(StringComparer.OrdinalIgnoreCase)
        let accepted = ResizeArray<KernelPlugin>()
        let logs = ResizeArray<string>()

        for plugin in plugins do
            if seen.Add plugin.Name then
                accepted.Add plugin
            else
                logs.Add $"Duplicate Semantic Kernel plugin skipped: {plugin.Name}"

        List.ofSeq accepted, List.ofSeq logs

    let load context providerFolder =
        let plugins = ResizeArray<KernelPlugin>()
        let logs = ResizeArray<string>()

        let builtInPlugin, builtInErrors =
            createPlugin context "FsKameTools" builtInToolTypes

        builtInErrors |> List.iter logs.Add
        builtInPlugin |> Option.iter plugins.Add

        let assemblies, assemblyLogs = loadExternalProviderAssemblies providerFolder
        assemblyLogs |> List.iter logs.Add

        for providerType in assemblies |> List.collect providerTypes do
            match instantiateProvider providerType with
            | Error err -> logs.Add err
            | Ok provider when provider.ContractVersion <> contractVersion ->
                logs.Add
                    $"Skipping provider {providerType.FullName}: contract version {provider.ContractVersion} is not supported."
            | Ok provider ->
                try
                    let providerPlugins = provider.GetPlugins context
                    providerPlugins |> List.iter plugins.Add
                with ex ->
                    logs.Add $"Provider {providerType.FullName} failed to create plugins: {ex.Message}"

                let plugin, errors =
                    createPlugin context providerType.Name (provider.GetToolTypes())

                errors |> List.iter logs.Add
                plugin |> Option.iter plugins.Add

        let plugins, duplicatePluginLogs = dedupePlugins plugins
        duplicatePluginLogs |> List.iter logs.Add
        validateUniqueFunctions plugins |> List.iter logs.Add

        { plugins = plugins
          logs = List.ofSeq logs }
