namespace FsKame

type PdfProcessingStatus =
    | Queued
    | Processing
    | Ready
    | Failed

type PdfDocumentSource =
    { id: string
      displayName: string
      storedPath: string
      originalPath: string
      selected: bool
      status: PdfProcessingStatus
      chunkCount: int
      error: string option }

type PdfProcessResult =
    { id: string
      chunkCount: int
      error: string option }

type PdfDeleteResult =
    { id: string
      displayName: string
      removedFile: bool
      removedIndexCount: int
      indexErrors: string list }

type RetrievalMode =
    | InternalDocumentIndex
    | FsColbertWithFallback

module RetrievalModes =
    let labels = [ "Internal document index"; "FsColbert index with fallback" ]

    let toStorageValue mode =
        match mode with
        | InternalDocumentIndex -> "internal"
        | FsColbertWithFallback -> "fscolbert-with-fallback"

    let ofStorageValue value =
        match (defaultArg (Option.ofObj value) "").Trim().ToLowerInvariant() with
        | "internal"
        | "internal-document-index" -> InternalDocumentIndex
        | _ -> FsColbertWithFallback

    let toIndex mode =
        match mode with
        | InternalDocumentIndex -> 0
        | FsColbertWithFallback -> 1

    let ofIndex index =
        match index with
        | 0 -> InternalDocumentIndex
        | _ -> FsColbertWithFallback

    let displayName mode = labels |> List.item (toIndex mode)

module PdfDocuments =
    let isReady doc =
        match doc.status with
        | Ready -> true
        | _ -> false

    let canSelect doc = isReady doc

    let selectedReady docs =
        docs |> List.filter (fun doc -> doc.selected && isReady doc)

    let statusText doc =
        match doc.status with
        | Queued -> "Queued"
        | Processing -> "Processing..."
        | Ready -> $"Ready - {doc.chunkCount} chunk(s)"
        | Failed -> defaultArg doc.error "Failed"
