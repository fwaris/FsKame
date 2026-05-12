namespace FsKame.QA

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Threading
open System.Threading.Tasks

type QaToolCatalog =
    { tools: IQaTool list
      logs: string list }

module QaToolCatalog =
    let empty = { tools = []; logs = [] }

module ToolArguments =
    let tryString name (args: IReadOnlyDictionary<string, string>) =
        match args.TryGetValue name with
        | true, value -> Text.notEmpty value
        | false, _ -> None

    let tryInt name fallback (args: IReadOnlyDictionary<string, string>) =
        match tryString name args with
        | Some value ->
            match Int32.TryParse value with
            | true, parsed -> parsed
            | false, _ -> fallback
        | None -> fallback

type private SourceSearchTool(host: IQaToolHost) =
    interface IQaTool with
        member _.PluginName = "FsKameTools"
        member _.Name = "selected_source_search"

        member _.Description =
            "Searches the currently selected PDF and Markdown sources for passages relevant to the request."

        member _.Parameters =
            [ { name = "question"
                description = "The source-related question or standalone retrieval request."
                required = true }
              { name = "max_results"
                description = "Maximum number of source passages to return."
                required = false } ]

        member _.InvokeAsync(args, cancellationToken) =
            task {
                let question = ToolArguments.tryString "question" args |> Option.defaultValue ""
                let maxResults = ToolArguments.tryInt "max_results" 6 args |> max 1 |> min 12
                let! content = host.SearchKnowledgeAsync(question, maxResults, cancellationToken)
                return QaToolResult.text content
            }

type private SourceInventoryTool(host: IQaToolHost) =
    interface IQaTool with
        member _.PluginName = "FsKameTools"
        member _.Name = "source_inventory"

        member _.Description =
            "Lists the selected PDF and Markdown sources available to this QA session."

        member _.Parameters = []

        member _.InvokeAsync(_, cancellationToken) =
            task {
                let! content = host.SourceInventoryAsync cancellationToken
                return QaToolResult.text content
            }

type private DurableMemorySearchTool(host: IQaToolHost) =
    interface IQaTool with
        member _.PluginName = "FsKameTools"
        member _.Name = "durable_memory_search"
        member _.Description = "Searches typed durable memory records."

        member _.Parameters =
            [ { name = "query"
                description = "The user's question or standalone memory search query."
                required = true }
              { name = "max_results"
                description = "Maximum number of durable memory records to return."
                required = false } ]

        member _.InvokeAsync(args, cancellationToken) =
            task {
                let query = ToolArguments.tryString "query" args |> Option.defaultValue ""
                let maxResults = ToolArguments.tryInt "max_results" 12 args |> max 1 |> min 30
                let! content = host.SearchMemoryAsync(query, maxResults, cancellationToken)
                return QaToolResult.text content
            }

type private BlackboardSearchTool(host: IQaToolHost) =
    interface IQaTool with
        member _.PluginName = "FsKameTools"
        member _.Name = "blackboard_search"
        member _.Description = "Searches recent QA tool observations for follow-up context."

        member _.Parameters =
            [ { name = "query"
                description = "A word or phrase to search for in recent observations."
                required = true } ]

        member _.InvokeAsync(args, cancellationToken) =
            task {
                let query = ToolArguments.tryString "query" args |> Option.defaultValue ""
                let! content = host.SearchBlackboardAsync(query, cancellationToken)
                return QaToolResult.text content
            }

module QaToolLoader =
    let contractVersion = 1

    let builtInTools host =
        [ SourceSearchTool(host) :> IQaTool
          SourceInventoryTool(host)
          DurableMemorySearchTool(host)
          BlackboardSearchTool(host) ]

    let private providerTypes (assembly: Assembly) =
        try
            assembly.GetTypes()
            |> Array.filter (fun t -> t.IsPublic && not t.IsAbstract && typeof<IQaToolProvider>.IsAssignableFrom t)
            |> Array.toList
        with _ ->
            []

    let private instantiateProvider (providerType: Type) =
        try
            match providerType.GetConstructor(Type.EmptyTypes) with
            | null -> Error $"Provider {providerType.FullName} must expose a public parameterless constructor."
            | ctor -> ctor.Invoke(Array.empty) :?> IQaToolProvider |> Ok
        with ex ->
            Error $"Unable to create provider {providerType.FullName}: {ex.Message}"

    let private loadAssemblies folder =
        if String.IsNullOrWhiteSpace folder || not (Directory.Exists folder) then
            [], [ $"Tool provider folder does not exist: {folder}" ]
        else
            let assemblies = ResizeArray<Assembly>()
            let logs = ResizeArray<string>()

            for path in Directory.EnumerateFiles(folder, "*.dll", SearchOption.TopDirectoryOnly) do
                try
                    assemblies.Add(Assembly.LoadFrom path)
                with ex ->
                    logs.Add $"Skipping tool provider assembly {path}: {ex.Message}"

            List.ofSeq assemblies, List.ofSeq logs

    let private dedupeTools (tools: IQaTool seq) =
        let seen = HashSet<string>(StringComparer.OrdinalIgnoreCase)
        let accepted = ResizeArray<IQaTool>()
        let logs = ResizeArray<string>()

        for tool in tools do
            let key = $"{tool.PluginName}.{tool.Name}"

            if seen.Add key then
                accepted.Add tool
            else
                logs.Add $"Duplicate QA tool skipped: {key}"

        List.ofSeq accepted, List.ofSeq logs

    let loadWithProviders host (providerFolder: string option) (extraProviders: IQaToolProvider list) =
        let tools = ResizeArray<IQaTool>()
        let logs = ResizeArray<string>()

        builtInTools host |> List.iter tools.Add

        for provider in extraProviders do
            try
                if provider.ContractVersion <> contractVersion then
                    logs.Add
                        $"Skipping provider {provider.GetType().FullName}: contract version {provider.ContractVersion} is not supported."
                else
                    provider.GetTools host |> List.iter tools.Add
            with ex ->
                logs.Add $"Provider {provider.GetType().FullName} failed to create tools: {ex.Message}"

        match providerFolder with
        | None -> ()
        | Some folder ->
            let assemblies, assemblyLogs = loadAssemblies folder
            assemblyLogs |> List.iter logs.Add

            for providerType in assemblies |> List.collect providerTypes do
                match instantiateProvider providerType with
                | Error err -> logs.Add err
                | Ok provider when provider.ContractVersion <> contractVersion ->
                    logs.Add
                        $"Skipping provider {providerType.FullName}: contract version {provider.ContractVersion} is not supported."
                | Ok provider ->
                    try
                        provider.GetTools host |> List.iter tools.Add
                    with ex ->
                        logs.Add $"Provider {providerType.FullName} failed to create tools: {ex.Message}"

        let tools, duplicateLogs = dedupeTools tools
        duplicateLogs |> List.iter logs.Add

        { tools = tools
          logs = List.ofSeq logs }

    let load host (providerFolder: string option) =
        loadWithProviders host providerFolder []
