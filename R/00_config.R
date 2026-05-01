############################################################
# confideR – Package configuration and registries
#
# Central definitions for AI package blocklists, API key
# registries, and IDE detection patterns. Extend these
# vectors to match your institutional environment.
############################################################

# -------------------------------------------------------------------
# AI-adjacent packages to unload/block in confidential mode.
# Intentionally conservative; add domain-specific packages as needed.
# -------------------------------------------------------------------
.confider_ai_packages <- c(

  # IDE integrations
  "copilot",

  # LLM interface packages
  "ellmer",
  "chattr",
  "gptstudio",
  "gander",
  "chores",

  # Packages that send data values to LLMs

  "mall",

  # Agent / tool-use frameworks
  "side",
  "deputy",
  "mcptools",

  # General LLM wrappers
  "openai",
  "chatgpt",
  "theOpenAIR",

  # Telemetry / experiment tracking (may phone home)
  "wandb"
)

# -------------------------------------------------------------------
# API key environment variables to clear in confidential mode.
# -------------------------------------------------------------------
.confider_api_keys <- c(
  "OPENAI_API_KEY",
  "ANTHROPIC_API_KEY",
  "GEMINI_API_KEY",
  "AZURE_OPENAI_KEY",
  "AZURE_OPENAI_API_KEY",
  "HUGGINGFACE_TOKEN",
  "HF_TOKEN",
  "MISTRAL_API_KEY",
  "PERPLEXITY_API_KEY",
  "COHERE_API_KEY",
  "DEEPSEEK_API_KEY",
  "GROQ_API_KEY"
)

# -------------------------------------------------------------------
# .Rprofile option patterns that indicate AI auto-connections.
# These are checked by audit_rprofile().
# -------------------------------------------------------------------
.confider_rprofile_patterns <- c(
  "\\.chattr_chat",
  "\\.mall_chat",
  "\\.gander_chat",
  "ellmer::",
  "chat_openai",
  "chat_anthropic",
  "chat_claude",
  "chat_ollama",
  "chat_gemini",
  "OPENAI_API_KEY",
  "ANTHROPIC_API_KEY",
  "gptstudio",
  "copilot"
)

# -------------------------------------------------------------------
# Null coalescing operator
# -------------------------------------------------------------------
`%||%` <- function(a, b) if (!is.null(a)) a else b
