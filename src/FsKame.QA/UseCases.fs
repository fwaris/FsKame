namespace FsKame.QA

type GenericQaUseCasePlugin() =
    interface IUseCasePlugin with
        member _.ContractVersion = UseCaseDefinition.currentContractVersion
        member _.Definition = UseCaseDefinition.generic
        member _.GetToolProviders() = []
        member _.GetContextProviders _ = []
