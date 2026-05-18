###########################################################
# confideR – Package configuration and registries
#
# Central definitions for AI package blocklists, API key
# registries, and IDE detection patterns. Extend these
# vectors to match your institutional environment.
#
# Last reviewed: 2026-04
############################################################
 
# -------------------------------------------------------------------
# AI-adjacent packages to unload/block in confidential mode.
# Intentionally conservative; add domain-specific packages as needed.
#
# NOTE: GitHub Copilot is an IDE-level integration, not an R package.
# It cannot be blocked here — it is handled by audit_ide() instead.
#
# NOTE: some experimental pkgs are included but may not be on CRAN.
# The hook installation will silently succeed for non-existent
# packages; they are listed here for future-proofing.
# -------------------------------------------------------------------

.confider_ai_packages <- c(

  # ---- LLM interface packages ----
  "ellmer",          # Core R LLM interface (Claude, GPT, Gemini, Ollama, Bedrock)
  "chattr",          # RStudio chat widget; auto-enriches prompts with env context
  "gptstudio",       # RStudio add-in for GPT chat and code completion
  "gander",          # Inline code suggestions via keyboard shortcut
  "chores",          # LLM assistants for repetitive coding tasks
  "tidyllm",         # Tidy-style LLM interface supporting multiple providers
  "gemini.R",        # Google Gemini interface
  "instructor",      # Structured outputs from LLMs

  # ---- Packages that send data values to LLMs ----
  "mall",            # Row-wise LLM operations on data frame columns
 
  # ---- Local model interfaces ----
  # These communicate with Ollama running on localhost.
  # Although local, they still represent an active AI connection
  # and may be misconfigured to use a remote endpoint.
  "rollama",         # Ollama interface
  "ollamar",         # Alternative Ollama interface
 
  # ---- Agent / tool-use frameworks ----
  "side",            # Full AI assistant in RStudio sidebar; broad file/env access
  "deputy",          # AI agent framework
  "mcptools",        # Model Context Protocol tools for R
 
  # ---- General LLM API wrappers ----
  "openai",          # OpenAI API wrapper
  "chatgpt",         # ChatGPT interface
  "theOpenAIR",      # Alternative OpenAI wrapper
 
  # ---- RAG / embedding packages ----
  "ragnar",          # R RAG framework (uses ellmer under the hood)
  "text",            # NLP package; can send text to HuggingFace models
 
  # ---- Telemetry / experiment tracking ----
  # These may phone home with session data, model outputs, or metrics.
  "wandb",           # Weights & Biases experiment tracking
  "mlflow",          # MLflow tracking (can log to remote tracking servers)
  "vetiver"          # MLOps deployment; can push to external registries
)

# -------------------------------------------------------------------
# API key environment variables to clear in confidential mode.
# Keys are backed up in options() and restored by confidential_mode_off().
# -------------------------------------------------------------------
.confider_api_keys <- c(
 
  # ---- Major cloud AI providers ----
  "OPENAI_API_KEY",
  "ANTHROPIC_API_KEY",
  "GEMINI_API_KEY",
  "GOOGLE_API_KEY",          # Alternative key name used by some Google AI packages
  "VERTEX_API_KEY",          # Google Vertex AI (distinct from GOOGLE_API_KEY)
  "AZURE_OPENAI_KEY",
  "AZURE_OPENAI_API_KEY",
  "XAI_API_KEY",             # xAI Grok
 
  # ---- Model hosting / hub platforms ----
  "HUGGINGFACE_TOKEN",
  "HF_TOKEN",                # Alternative HuggingFace token name
  "REPLICATE_API_TOKEN",     # Replicate — runs open models via API
  "NVIDIA_API_KEY",          # NVIDIA NIM — hosts Llama, Mistral, and others
 
  # ---- Routing and aggregation platforms ----
  "OPENROUTER_API_KEY",      # Routes to multiple providers; one key accesses many models
  "TOGETHER_API_KEY",        # Together AI — hosts Llama, Qwen, and other open models
  "FIREWORKS_API_KEY",       # Fireworks AI — multi-model hosting
  "CEREBRAS_API_KEY",        # Cerebras fast inference
  "CLOUDFLARE_AI_TOKEN",     # Cloudflare Workers AI
 
  # ---- Specialist / embedding providers ----
  "MISTRAL_API_KEY",
  "COHERE_API_KEY",
  "VOYAGE_API_KEY",          # Voyage AI — embedding models used in RAG pipelines
  "PERPLEXITY_API_KEY",
  "DEEPSEEK_API_KEY",
  "GROQ_API_KEY",
 
  # ---- Enterprise platforms ----
  "DATABRICKS_TOKEN"         # Databricks has extensive LLM/AI features via AI Gateway
)

# -------------------------------------------------------------------
# .Rprofile option patterns that indicate AI auto-connections.
# These are checked by audit_rprofile() against each non-comment
# line in the user and project .Rprofile files.
#
# Patterns use grepl() with perl = TRUE. Escape special regex
# characters with double backslash (\\).
# -------------------------------------------------------------------
.confider_rprofile_patterns <- c(
 
  # ---- ellmer-based chat options ----
  # ellmer is the foundation for many other AI packages
  "\\.chattr_chat",          # {chattr} auto-connection
  "\\.mall_chat",            # {mall} auto-connection
  "\\.gander_chat",          # {gander} auto-connection
  "ellmer::",                # Any direct ellmer:: call
  "chat_openai",             # ellmer::chat_openai()
  "chat_anthropic",          # ellmer::chat_anthropic()
  "chat_claude",             # ellmer::chat_claude()
  "chat_ollama",             # ellmer::chat_ollama()
  "chat_gemini",             # ellmer::chat_gemini()
  "chat_mistral",            # ellmer::chat_mistral()
  "chat_groq",               # ellmer::chat_groq()
  "chat_openrouter",         # ellmer::chat_openrouter()
  "chat_bedrock",            # ellmer::chat_bedrock()
  "chat_vertex",             # ellmer::chat_vertex()
 
  # ---- Package library() calls ----
  # Catches packages loaded at startup that establish AI connections
  "library\\(ellmer\\)",
  "library\\(chattr\\)",
  "library\\(mall\\)",
  "library\\(rollama\\)",
  "library\\(ollamar\\)",
  "library\\(tidyllm\\)",
  "require\\(ellmer\\)",
  "require\\(chattr\\)",
 
  # ---- Package-specific namespace calls ----
  "ellmer::",
  "chattr::",
  "mall::",
  "rollama::",
  "ollamar::",
  "tidyllm::",
 
  # ---- IDE AI feature options ----
  "gptstudio",  #broad catch for loading or comments
  "copilot",  #catching comments about copilot only
 
  # ---- API key assignments ----
  # Catches both KEY = "value" (.Renviron style) and
  # Sys.setenv(KEY = ...) / options(KEY = ...) style
  "OPENAI_API_KEY",
  "ANTHROPIC_API_KEY",
  "GEMINI_API_KEY",
  "HUGGINGFACE_TOKEN",
  "OPENROUTER_API_KEY",
  "TOGETHER_API_KEY",
  "GROQ_API_KEY",
  "MISTRAL_API_KEY",
 
  # ---- Broad catch patterns ----
  # These catch programmatic key-setting and option-based connections
  # that the specific patterns above might miss
  "Sys\\.setenv.*API",       # Sys.setenv(SOME_API_KEY = ...)
  "Sys\\.setenv.*TOKEN",     # Sys.setenv(SOME_TOKEN = ...)
  "options\\(.*chat",        # options(.chattr_chat = ...) or similar
  "options\\(.*llm",         # options(.tidyllm_... = ...) or similar
  "options\\(.*model"        # options(default_model = ...) or similar
)

# -------------------------------------------------------------------
# VS Code extension directory name patterns.
# Matched case-insensitively against subdirectory names in
# ~/.vscode/extensions/ by .scan_vscode_extensions().
# -------------------------------------------------------------------
.confider_vscode_ext_patterns <- c(
  "copilot",              # GitHub Copilot and Copilot Chat
  "claude",               # Claude / Anthropic extensions
  "codeium",              # Codeium
  "tabnine",              # Tabnine
  "continue\\.continue",  # Continue
  "cody",                 # Sourcegraph Cody
  "amazonq",              # Amazon Q
  "codewhisperer",        # Amazon CodeWhisperer (legacy name)
  "cursor"                # Cursor AI features
)

# -------------------------------------------------------------------
# Process name fragments for system process table scanning.
# Matched case-insensitively against full ps/tasklist output.
# Returns AMBER — presence on machine does not confirm the process
# is connected to the current R session.
# -------------------------------------------------------------------
.confider_ai_processes <- c(
  "copilot-agent",           # GitHub Copilot language server
  "copilot_language_server", # Alternative Copilot process name
  "codeium-agent",           # Codeium extension agent
  "tabnine-agent",           # Tabnine agent
  "continue-server",         # Continue extension server
  "cody-agent",              # Sourcegraph Cody agent
  "amazon-q-agent",          # Amazon Q agent
  "cursor-agent"             # Cursor agent
)

# -------------------------------------------------------------------
# R option name patterns indicating AI package configuration set
# earlier in the session (even if the package is now unloaded).
# -------------------------------------------------------------------
.confider_ai_option_patterns <- c(
  "\\.chattr_chat",
  "\\.mall_chat",
  "\\.gander_chat",
  "ellmer",
  "tidyllm",
  "rollama",
  "ollamar",
  "openai",
  "anthropic",
  "gemini"
)

# -------------------------------------------------------------------
# Null coalescing operator
# -------------------------------------------------------------------
`%||%` <- function(a, b) if (!is.null(a)) a else b
