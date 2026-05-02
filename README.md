# FsKame

FsKame is an F#/.NET MAUI realtime voice question-answering app for selected PDF sources.

It builds a mobile shell around a small collection of realtime agents using RTOpenAI/RTFlow. PDFs are copied into app-owned storage, processed, persisted as a checkable source list, and only selected ready PDFs are used for answers.

## Features

- F# .NET MAUI app targeting Android, iOS, and Mac Catalyst.
- Realtime voice mode using `gpt-realtime-1.5`.
- Backend oracle guidance using `gpt-5.4` by default.
- Persisted PDF source library with processing, ready, failed, retry, and selected states.
- PDF text extraction and chunking for low-latency source retrieval.
- Embedded PDF image detection notes are added to the source index so image-heavy pages are represented.
- Voice answers can list which selected PDFs are currently available.

## Build

Restore packages first, then build a target framework:

```bash
dotnet build src/FsKame/FsKame.fsproj -f net10.0-android --no-restore --nologo
dotnet build src/FsKame/FsKame.fsproj -f net10.0-ios --no-restore --nologo
```

## Configuration

Open Settings in the app and provide:

- OpenAI API key
- Oracle model, defaulting to `gpt-5.4`

Add PDFs from the main view using the floating `+` button. Successfully processed PDFs are auto-selected and can be unchecked to exclude them from question answering.

## License

MIT. See [LICENSE](LICENSE).
