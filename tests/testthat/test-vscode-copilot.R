# Recent VS Code ships GitHub Copilot inside the application rather than in
# ~/.vscode/extensions/, with its features on by default once signed in to
# GitHub. audit_session() previously could not see it at all.

test_that("built-in Copilot is found via VSCODE_GIT_ASKPASS_MAIN", {
  ext <- file.path(tempfile("vscode"), "resources", "app", "extensions")
  dir.create(file.path(ext, "copilot"), recursive = TRUE)
  dir.create(file.path(ext, "git", "dist"), recursive = TRUE)
  askpass <- file.path(ext, "git", "dist", "askpass-main.js")
  file.create(askpass)

  hits <- .find_vscode_builtin_copilot(askpass_main = askpass, candidate_dirs = character(0))
  expect_length(hits, 1)
  expect_match(hits, "copilot$")
})

test_that("built-in Copilot is found in a standard install location", {
  ext <- file.path(tempfile("vscode"), "resources", "app", "extensions")
  dir.create(file.path(ext, "copilot"), recursive = TRUE)
  expect_length(.find_vscode_builtin_copilot(askpass_main = "", candidate_dirs = ext), 1)
})

test_that("no built-in Copilot is reported when absent", {
  expect_length(.find_vscode_builtin_copilot(askpass_main = "", candidate_dirs = tempfile()), 0)
})

test_that("settings reader detects the AI master switch and ignores comments", {
  p <- tempfile(fileext = ".json")
  writeLines(c(
    "{",
    '  // "chat.disableAIFeatures": false,',
    '  "chat.disableAIFeatures": true,',
    '  "github.copilot.enable": { "*": false, "markdown": true },',
    "}"
  ), p)
  s <- .read_vscode_ai_settings(p)
  expect_true(s$ai_disabled)
  expect_true(s$copilot_off)

  writeLines('{ "editor.fontSize": 14 }', p)
  s <- .read_vscode_ai_settings(p)
  expect_true(is.na(s$ai_disabled))
  expect_true(is.na(s$copilot_off))

  expect_true(is.na(.read_vscode_ai_settings(tempfile())$ai_disabled))
})

test_that("audit warns about built-in Copilot unless AI features are disabled", {
  ide <- list(name = "VS Code", is_positron = FALSE, is_rstudio = FALSE, is_vscode = TRUE)
  local_mocked_bindings(
    .scan_vscode_extensions      = function() character(0),
    .find_vscode_builtin_copilot = function(...) "C:/fake/resources/app/extensions/copilot"
  )

  local_mocked_bindings(.read_vscode_ai_settings = function(...) list(ai_disabled = NA, copilot_off = NA))
  expect_true(any(grepl("built into this version of VS Code", .ide_warnings(ide))))

  local_mocked_bindings(.read_vscode_ai_settings = function(...) list(ai_disabled = TRUE, copilot_off = NA))
  expect_false(any(grepl("built into this version of VS Code", .ide_warnings(ide))))
})

test_that("OpenAI ChatGPT/Codex extensions match the pattern list", {
  hit <- function(x) any(vapply(.confider_vscode_ext_patterns, function(p)
    grepl(p, x, ignore.case = TRUE, perl = TRUE), logical(1)))
  expect_true(hit("openai.chatgpt-26.903.61454-win32-x64"))
  expect_true(hit("github.copilot-chat-0.30.0"))
  expect_false(hit("reditorsupport.r-2.8.8"))
  expect_false(hit("ms-python.python-2026.4.0-win32-x64"))
  expect_false(hit("eamodio.gitlens-19.1.0"))
})
