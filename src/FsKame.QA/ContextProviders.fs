namespace FsKame.QA

open System
open System.Threading
open System.Threading.Tasks
open Microsoft.Extensions.AI

type FsColbertContextProviderOptions =
    { storageRoot: string
      retrievalMode: RetrievalMode
      sources: KnowledgeSource list
      queryExpansionClient: IChatClient option
      disposeQueryExpansionClient: bool
      useCaseProfile: QaUseCaseProfile
      keywordModelId: string
      elaborateIndexKeywords: bool
      logExpansions: bool
      logChunks: bool
      useLexicalFilter: bool
      report: string -> unit }

module FsColbertContextProviderOptions =
    let create storageRoot retrievalMode sources =
        { storageRoot = storageRoot
          retrievalMode = retrievalMode
          sources = sources
          queryExpansionClient = None
          disposeQueryExpansionClient = false
          useCaseProfile = QaUseCaseProfile.generic
          keywordModelId = QaDefaults.nanoModel
          elaborateIndexKeywords = true
          logExpansions = false
          logChunks = false
          useLexicalFilter = true
          report = ignore }

type FsColbertContextProvider(options: FsColbertContextProviderOptions) =
    let mutable retrieval = KnowledgeSources.emptyIndex

    member _.Retrieval = retrieval

    interface IQaContextProvider with
        member _.ProviderId = "fskame.fscolbert"

        member _.DisplayName = RetrievalModes.displayName options.retrievalMode

        member _.Sources = retrieval.sources

        member _.LoadAsync cancellationToken =
            task {
                let sources = options.sources |> List.filter _.enabled

                options.report
                    $"Loading {sources.Length} knowledge source(s) with {RetrievalModes.displayName options.retrievalMode}."

                let loadWork =
                    match options.retrievalMode with
                    | InternalDocumentIndex -> KnowledgeSources.loadInternalIndex sources
                    | FsColbertWithFallback ->
                        let keywordOptions =
                            { KnowledgeSources.KeywordGenerationOptions.defaults with
                                enabled = options.elaborateIndexKeywords
                                client = options.queryExpansionClient
                                modelId = options.keywordModelId
                                useCaseProfile = options.useCaseProfile }

                        KnowledgeSources.loadIndex options.storageRoot options.report keywordOptions sources

                let! loaded, errors = Async.StartAsTask(loadWork, cancellationToken = cancellationToken)

                KnowledgeSources.disposeIndex retrieval
                retrieval <- loaded
                options.report $"Loaded {retrieval.chunks.Length} source chunk(s)."

                return errors
            }

        member _.RetrieveAsync(request, cancellationToken) =
            KnowledgeSources.rankWithProfile
                options.useCaseProfile
                options.queryExpansionClient
                options.logExpansions
                options.logChunks
                options.useLexicalFilter
                options.report
                request.query
                request.maxResults
                retrieval
            |> fun work -> Async.StartAsTask(work, cancellationToken = cancellationToken)

        member _.InventoryAsync _ =
            KnowledgeSources.renderInventory retrieval.sources |> Task.FromResult

        member _.DisposeAsync() =
            KnowledgeSources.disposeIndex retrieval

            if options.disposeQueryExpansionClient then
                options.queryExpansionClient |> Option.iter _.Dispose()

            ValueTask()
