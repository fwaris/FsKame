namespace FsKame

open System
open System.IO
open System.Reflection
open System.Runtime.InteropServices
open Microsoft.Maui.Storage

type LoadedUseCase =
    { plugin: FsKame.QA.IUseCasePlugin
      definition: FsKame.QA.UseCaseDefinition }

module UseCaseHost =
    let private pluginFolder () =
        Path.Combine(FileSystem.AppDataDirectory, "use-case-plugins")

    let private supportsFolderPlugins () =
        not (RuntimeInformation.IsOSPlatform(OSPlatform.Create("IOS")))

    let private bundledAssemblies () =
        [ typeof<FsKame.QA.GenericQaUseCasePlugin>.Assembly
          Assembly.GetExecutingAssembly() ]
        |> List.distinctBy _.FullName

    let private pluginTypes (assembly: Assembly) =
        try
            assembly.GetTypes()
            |> Array.filter (fun t ->
                t.IsPublic
                && not t.IsAbstract
                && typeof<FsKame.QA.IUseCasePlugin>.IsAssignableFrom t)
            |> Array.toList
        with ex ->
            ignore ex
            []

    let private instantiatePlugin (pluginType: Type) =
        try
            match pluginType.GetConstructor(Type.EmptyTypes) with
            | null -> Error $"Use-case plugin {pluginType.FullName} must expose a public parameterless constructor."
            | ctor -> Ok(ctor.Invoke(Array.empty) :?> FsKame.QA.IUseCasePlugin)
        with ex ->
            Error $"Unable to create use-case plugin {pluginType.FullName}: {ex.Message}"

    let private loadAssembly path =
        try
            Assembly.LoadFrom path |> Ok
        with ex ->
            Error $"Skipping use-case plugin assembly {path}: {ex.Message}"

    let private folderAssemblies () =
        if not (supportsFolderPlugins ()) then
            [], [ "Use-case plugin folder loading is disabled on iOS; bundled plugins can still be scanned." ]
        else
            let folder = pluginFolder ()

            if not (Directory.Exists folder) then
                [], []
            else
                let assemblies = ResizeArray<Assembly>()
                let logs = ResizeArray<string>()

                for path in Directory.EnumerateFiles(folder, "*.dll", SearchOption.TopDirectoryOnly) do
                    match loadAssembly path with
                    | Ok assembly -> assemblies.Add assembly
                    | Error err -> logs.Add err

                List.ofSeq assemblies, List.ofSeq logs

    let private loadFromAssembly (logs: ResizeArray<string>) (assembly: Assembly) =
        assembly
        |> pluginTypes
        |> List.choose (fun pluginType ->
            match instantiatePlugin pluginType with
            | Error err ->
                logs.Add err
                None
            | Ok plugin when plugin.ContractVersion <> FsKame.QA.UseCaseDefinition.currentContractVersion ->
                logs.Add
                    $"Skipping use-case plugin {pluginType.FullName}: contract version {plugin.ContractVersion} is not supported."

                None
            | Ok plugin ->
                Some
                    { plugin = plugin
                      definition = FsKame.QA.UseCaseDefinition.sanitize plugin.Definition })

    let loadAll () =
        let logs = ResizeArray<string>()
        let bundled = bundledAssemblies () |> List.collect (loadFromAssembly logs)
        let folderAssemblies, folderLogs = folderAssemblies ()
        folderLogs |> List.iter logs.Add
        let folderPlugins = folderAssemblies |> List.collect (loadFromAssembly logs)
        let seen = Collections.Generic.HashSet<string>(StringComparer.OrdinalIgnoreCase)
        let accepted = ResizeArray<LoadedUseCase>()

        for loaded in bundled @ folderPlugins do
            if seen.Add loaded.definition.id then
                accepted.Add loaded
            else
                logs.Add $"Duplicate use-case plugin skipped: {loaded.definition.id}"

        List.ofSeq accepted, List.ofSeq logs

    let loadActive activeUseCaseId =
        let plugins, logs = loadAll ()

        let active =
            activeUseCaseId
            |> Option.bind (fun id ->
                plugins
                |> List.tryFind (fun loaded ->
                    String.Equals(loaded.definition.id, id, StringComparison.OrdinalIgnoreCase)))
            |> Option.orElse (
                plugins
                |> List.tryFind (fun loaded -> loaded.definition.id = FsKame.QA.UseCaseDefinition.generic.id)
            )
            |> Option.defaultWith (fun () ->
                { plugin = FsKame.QA.GenericQaUseCasePlugin() :> FsKame.QA.IUseCasePlugin
                  definition = FsKame.QA.UseCaseDefinition.generic })

        active, logs

module UseCaseComposer =
    let toQaRetrievalMode (mode: RetrievalMode) =
        match mode with
        | InternalDocumentIndex -> FsKame.QA.InternalDocumentIndex
        | FsColbertWithFallback -> FsKame.QA.FsColbertWithFallback

    let fromQaRetrievalMode (mode: FsKame.QA.RetrievalMode) =
        match mode with
        | FsKame.QA.InternalDocumentIndex -> InternalDocumentIndex
        | FsKame.QA.FsColbertWithFallback -> FsColbertWithFallback

    let withHostOverrides
        (modelOverrides: Map<FsKame.QA.ModelRole, string>)
        retrievalMode
        useLexicalFilter
        elaborateIndexKeywords
        (definition: FsKame.QA.UseCaseDefinition)
        =
        let definition = FsKame.QA.UseCaseDefinition.sanitize definition

        let models =
            modelOverrides
            |> Map.fold
                (fun models role modelId ->
                    match Text.notEmpty modelId with
                    | None -> models
                    | Some modelId ->
                        let current =
                            definition.models
                            |> Map.tryFind role
                            |> Option.defaultValue (FsKame.QA.UseCaseDefinition.model role definition)

                        models |> Map.add role { current with modelId = modelId })
                definition.models

        { definition with
            models = models
            runtime =
                { definition.runtime with
                    retrievalMode = toQaRetrievalMode retrievalMode
                    useLexicalFilter = useLexicalFilter
                    elaborateIndexKeywords = elaborateIndexKeywords } }
