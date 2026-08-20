# confideR <img src="man/figures/logo.png" align="right" height="139" />

> Protect confidential research data when using AI coding assistants

## Overview

**confideR** is a companion R package to the paper *"Protecting confidential research data when using AI coding assistants: A practical guide"*. It is designed for researchers in ecological, environmental, fisheries, agricultural, and social science settings who use AI tools for code development but cannot expose the underlying data.

It provides four core capabilities:

1. **Confidential mode**: Activate session-level protection that clears AI API keys, unloads AI packages, and blocks them from being loaded.

2. **Session auditing**: Detect your IDE (RStudio, Positron, VS Code), scan `.Rprofile` for AI auto-connections, check for AI API keys, scan the VS Code extension directory, and query the system process table for active AI agent processes.

3. **Data fingerprinting**: Extract a privacy-safe structural summary of your confidential dataset (column names, types, distributions) without exposing any raw values. Supports three obfuscation levels. Auto-detects confidential columns across fisheries, ecological, environmental, agricultural, and social science naming conventions.

4. **Data simulation**: Generate realistic simulated survey datasets with group effects, strata effects, seasonal patterns, observer coverage, and secondary observations — safe to use with any AI tool. Supports the *develop on simulated data, run on real data* workflow.

## Installation and Example

```r
# From GitHub
remotes::install_github("smithja16/confideR")

# Quick start
library(confideR)

# 1. Audit your session FIRST
audit_session()

# 2. Activate confidential mode (clears AI keys, blocks AI packages)
confidential_mode_on()

# 3. Simulate data for AI-assisted development
sim_data <- simulate_data(
  n_obs    = 2000,
  n_groups = 30,
  include_observer  = TRUE,
  include_secondary = TRUE )

# 4. Fingerprint a dataset - structural summary only, no raw values.
#    In real use, point this at YOUR confidential data instead:
#    fp_real <- fingerprint(real_data, mode = "summary", obfuscation = "partial")
fp_sim <- fingerprint(sim_data, mode = "summary", obfuscation = "partial")

# 5. Format fingerprint for copy-pasting into a browser AI chat
format_for_prompt(fp_sim)
# This summary is safe to paste into an AI tool (after checking)

# 6. The "fingerprint -> simulate" round trip: turn a fingerprint back into a
#    synthetic dataset that mirrors its structure and distributions.
#    (Runnable here on fp_sim; in real use you'd pass fp_real.)
sim_real <- simulate_from_fingerprint(fp_sim)

# 7. Write simulated data for your AI workspace
# write.csv(sim_real, "data/simulated_data.csv", row.names = FALSE)
# This file is safe to use with an AI tool (after checking)

# NOTE: with obfuscation, simulated columns carry alias names (ID_1, Coord_1,
# Var1, ...). Keep those aliases while developing with AI. Only on the secure
# machine, for the final run against real data, restore the original names:
# sim_real <- restore_names(sim_real, fp_real)
# Never paste restored names - or code/errors referencing them - into an AI tool.

# 8. Check if object contains raw data
library(mgcv)
M <- gam(log(response) ~ stratum + s(month, bs="cc") + s(year) +
           s(covariate_1) + s(group_id, bs="re"), data = sim_data)
contains_data_like(M)  #TRUE
contains_data_like(sim_data)  #TRUE
contains_data_like(fp_sim)  #FALSE

# 9. When done, turn off confidential mode
confidential_mode_off()
```

## The "develop on fake, run on real" workflow with physical separation

```
  AI Machine (workshop)          Secure Machine (vault)
  ========================       ========================
  simulated_data.csv             real_data.csv
  + AI tools enabled             + NO AI tools
  + develop analysis code        + run final analysis
           |                              ^
           |--- transfer code only -------|
           |  (Git, paste, file transfer) |
           v                              |
  Code tested on fake data       Code runs on real data
```

## Functions

### Confidential mode
- `confidential_mode_on()` — activate protection
- `confidential_mode_off()` — deactivate and restore
- `is_confidential_mode()` — check status
- `restore_api_keys()` — recover keys from a session that didn't close cleanly (refused while confidential mode is active)
- `api_key_status()` — show which AI keys are live, backed up, or on disk

### Auditing
- `audit_session()` — comprehensive audit report
- `audit_ide()` — detect IDE and AI features
- `audit_rprofile()` — scan .Rprofile for AI config
- `audit_packages()` — check for loaded AI packages and option residuals
- `audit_env_keys()` — check for AI API keys
- `audit_processes()` — scan system process table for AI agent processes
- `confider_status()` — one-line status summary

### Fingerprinting
- `fingerprint()` — create structural summary
- `is_fingerprint()` — test whether an object is a fingerprint
- `alias_map()` — view alias-to-original mapping (local only)
- `restore_names()` — rename simulated alias columns back to originals (local only)
- `format_for_prompt()` — format for AI chat

### Simulation
- `simulate_data()` — generate survey data from parameters
- `simulate_from_fingerprint()` — generate synthetic data from a `fingerprint()` (the "fingerprint → simulate" round trip)

### Leak detection
- `contains_data_like()` — check if object contains raw data
- `ensure_no_data_leakage()` — error if raw data detected

### Script scanning
- `scan_script()` — check .R/.Rmd/.qmd for data exposure patterns
- `check_notebook_outputs()` — check for rendered output in notebooks

## Citation

If you use confideR in your research, please cite:

```
Smith J.A., Roff A., Brown C.J. (2026). Protecting confidential data when using AI
coding assistants: A practical guide. EcoEvoRxiv (preprint). DOI: https://doi.org/10.32942/X2FM30
```
