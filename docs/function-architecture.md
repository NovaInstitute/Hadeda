# Phase 1 – Step 2: Package Scaffolding & Client Architecture

## Goals
- Establish an installable R package skeleton managed with `renv`,
  `devtools`, and `testthat` so future work can iterate quickly.
- Introduce a set of pure helper functions that derive REST and gRPC
  transport settings without relying on mutable client objects.
- Document helper responsibilities for configuration merging,
  transport selection, and diagnostics ahead of endpoint-specific
  implementation.

## Package Layout
The package now follows the conventional structure expected by
`devtools::check()`:

- `DESCRIPTION`, `NAMESPACE`, and `LICENSE` establish the package
  identity and declare dependencies used across transports.
- `R/` contains source files. The initial module introduces
  configuration helpers that compute transport settings.
- `tests/testthat/` holds unit tests configured for `testthat`
  edition 3. Early tests exercise the configuration helpers and
  merging utilities to ensure consistent behaviour.
- `.github/workflows/R-CMD-check.yaml` provisions continuous
  integration using the r-lib actions so every commit executes
  `R CMD check` on Ubuntu devel, release, and oldrel builds.
- `renv.lock` plus `renv/activate.R` pin dependency versions and make
  it easy for contributors to reproduce the development environment.

## Functional configuration helpers
Rather than exposing an OO-style client, the package provides a
collection of pure helpers:

- `hadeda_config()` – returns a named list describing the resolved
  network, REST, and gRPC settings plus a default transport selection.
- `hadeda_merge_config()` – shallow-merges user overrides into the
  network defaults.
- `hadeda_resolve_transport()` – resolves and validates the transport
  preference, raising a typed error for unsupported values.
- `hadeda_network_defaults()` – central lookup for base URLs and gRPC
  endpoints across Hedera networks.

Because the helpers are ordinary functions that take inputs and return
lists, callers can comfortably use them within tidyverse pipelines or
pass the resulting configuration into specific verbs as needed.

## Testing Strategy
The initial test suite focuses on guarding the scaffolding:

1. Configurations built for supported networks expose the expected
   structure and default transport.
2. Default transport inference behaves deterministically when only one
   transport is configured.
3. Configuration overrides replace defaults without mutating unrelated
   fields.

As new utilities arrive (pagination, validators, signing helpers), they
will receive corresponding unit tests under `tests/testthat/` so that CI
runs provide quick feedback.

## Next Steps
1. Expand helper coverage with argument validators (identifier
   normalisers, pagination cursors, signer abstractions).
2. Implement HTTP and gRPC request builders that honour the
   configuration helpers while producing tidy tibbles.
3. Introduce documentation via Roxygen2 for the configuration helpers and
   publish rendered Rd files in `man/`.
4. Begin implementing Phase 1, Step 3 utilities (HTTP/gRPC helper
   layer) using the scaffolding delivered here.
