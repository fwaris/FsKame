namespace FsKame

module C =
    let SETTINGS_OPENAI_KEY = "FsKame.OpenAIKey"
    let SETTINGS_PDF_LIBRARY = "FsKame.PdfLibrary"
    let SETTINGS_ORACLE_MODEL = "FsKame.OracleModel"
    let SETTINGS_RETRIEVAL_MODE = "FsKame.RetrievalMode"
    let SETTINGS_LOG_EXPANSIONS = "FsKame.LogExpansions"
    let SETTINGS_LOG_CHUNKS = "FsKame.LogChunks"
    let SETTINGS_USE_LEXICAL_FILTER = "FsKame.UseLexicalFilter"
    let SETTINGS_ELABORATE_INDEX_KEYWORDS = "FsKame.ElaborateIndexKeywords"
    let SETTINGS_ACTIVE_USE_CASE = "FsKame.ActiveUseCase"

    let DEFAULT_ORACLE_MODEL = "gpt-5.5"
    let DEFAULT_REALTIME_MODEL = "gpt-realtime-2"
    let NANO_MODEL = "gpt-5-nano"
    let REALTIME_MEMORY_TIMEOUT_MS = 1200
    let REALTIME_MEMORY_CANDIDATE_CHUNKS = 14
    let REALTIME_MEMORY_MAX_CONTEXT_CHUNKS = 12
    let REALTIME_MEMORY_NEIGHBOR_SEEDS = 4
    let MAX_LOG = 250
    let FONT_REG = "OpenSansRegular"
    let FONT_BOLD = "OpenSansSemibold"
    let FONT_SYMBOLS = "MaterialSymbols"
