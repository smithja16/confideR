# safecatch <img src="man/figures/logo.png" align="right" height="139" />

> Protect confidential fisheries data when using AI coding assistants

## Overview

**safecatch** is a companion R package to the paper *"Protecting confidential fisheries data when using AI coding assistants: A practical guide"*. It provides four core capabilities:

1. **Confidential mode** — Activate session-level protection that clears AI API keys, unloads AI packages, and blocks them from being loaded.

2. **Session auditing** — Detect your IDE (RStudio, Positron, VS Code), scan `.Rprofile` for AI auto-connections, report loaded AI packages and active API keys.

3. **Data fingerprinting** — Extract a privacy-safe structural summary of your confidential dataset (column names, types, distributions) without exposing any raw values. Supports three obfuscation levels.

4. **Fisheries data simulation** — Generate realistic simulated CPUE datasets with vessel effects, seasonal patterns, spatial areas, observer data, and bycatch — safe to use with any AI tool.

## Installation

```r
# From GitHub (when published)
# remotes::install_github("[user]/safecatch")

# For now, source from local directory
devtools::load_all("path/to/safecatch")
```

## Quick start

```r
library(safecatch)

# 1. Audit your session FIRST
audit_session()

# 2. Activate confidential mode (clears AI keys, blocks AI packages)
confidential_mode_on()

# 3. Simulate data for AI-assisted development
sim_data <- simulate_fisheries_cpue(
  n_trips  = 2000,
  n_vessels = 30,
  include_observer = TRUE,
  include_bycatch  = TRUE
)

# 4. Fingerprint your REAL data (structural summary only)
# fp <- fingerprint(real_data, mode = "summary", obfuscation = "partial")

# 5. Write simulated data for your AI workspace
write.csv(sim_data, "data/simulated_cpue.csv", row.names = FALSE)
# This file is safe to use with any AI tool.

# 6. Format fingerprint for copy-pasting into a browser AI chat
# format_for_prompt(fp)

# 7. When done, turn off confidential mode
confidential_mode_off()
```

## The "develop on fake, run on real" workflow with physcial separation

```
  AI Machine (workshop)          Secure Machine (vault)
  ========================       ========================
  simulated_cpue.csv             real_cpue.csv
  + AI tools enabled             + NO AI tools
  + develop analysis code        + run final analysis
           |                              ^
           |--- transfer code only -------|
           |   (Git, USB, file transfer)  |
           v                              |
  Code tested on fake data       Code runs on real data
```

## Functions

### Confidential mode
- `confidential_mode_on()` — activate protection
- `confidential_mode_off()` — deactivate and restore
- `is_confidential_mode()` — check status

### Auditing
- `audit_session()` — comprehensive audit report
- `audit_ide()` — detect IDE and AI features
- `audit_rprofile()` — scan .Rprofile for AI config
- `audit_packages()` — check for loaded AI packages
- `audit_env_keys()` — check for AI API keys
- `safecatch_status()` — one-line status summary

### Fingerprinting
- `fingerprint()` — create structural summary
- `alias_map()` — view alias-to-original mapping (local only)
- `format_for_prompt()` — format for AI chat

### Simulation
- `simulate_fisheries_cpue()` — generate CPUE data from parameters
- `simulate_from_fingerprint()` — generate data from a fingerprint

### Leak detection
- `contains_data_like()` — check if object contains raw data
- `ensure_no_data_leakage()` — error if raw data detected

## Citation

If you use safecatch in your research, please cite:

```
[Authors] (2026). Protecting confidential data when using AI
coding assistants: A practical guide. [Journal]. DOI: [pending]
```

