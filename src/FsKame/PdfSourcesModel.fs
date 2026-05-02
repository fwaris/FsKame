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
