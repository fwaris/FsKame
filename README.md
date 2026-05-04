# FsKame

FsKame is an F#/.NET MAUI realtime voice question-answering app for selected PDF sources.

It builds a mobile shell around a small collection of realtime agents using RTOpenAI/RTFlow. PDFs are copied into app-owned storage, processed, persisted as a checkable source list, and only selected ready PDFs are used for answers. FsKame runs in strict document mode: it answers from the selected PDFs and declines when the selected documents do not contain the answer.

## Features

- F# .NET MAUI app targeting Android, iOS, and Mac Catalyst.
- Realtime voice mode using `gpt-realtime-1.5`.
- Backend oracle guidance using `gpt-5.4` by default.
- Persisted PDF source library with processing, ready, failed, retry, and selected states.
- PDF deletion from the source library, including cleanup of app-owned PDF files and persisted FsColbert indexes.
- PDF text extraction with FsColbert-backed local semantic retrieval and persisted on-device indexes.
- Configurable retrieval mode: internal document index, or FsColbert index with internal fallback.
- Embedded PDF image detection notes are added to the source index so image-heavy pages are represented.
- Voice answers can list which selected PDFs are currently available.
- Strict document-only answering; no general-knowledge fallback.

## Build

Restore packages first, then build a target framework:

```bash
dotnet build src/FsKame/FsKame.fsproj -f net10.0-android --no-restore --nologo
dotnet build src/FsKame/FsKame.fsproj -f net10.0-ios --no-restore --nologo
```

FsKame references the sibling FsColbert project at `../FsColbert/src/FsColbert/FsColbert.fsproj`.
Android builds require minSdk 24 because ONNX Runtime's Android package declares that minimum.

## Configuration

Open Settings in the app and provide:

- OpenAI API key
- Oracle model, defaulting to `gpt-5.4`
- Retrieval mode, either internal document index or FsColbert index with fallback

Add PDFs from the main view using the floating `+` button. Successfully processed PDFs are auto-selected and can be unchecked to exclude them from question answering.

The first selected-source load downloads the FsColbert ONNX model into app data, builds a local index for the selected PDFs, and reuses that index until the source files change. If FsColbert cannot initialize, FsKame falls back to lexical source matching and logs the reason.

## License

MIT. See [LICENSE](LICENSE).
