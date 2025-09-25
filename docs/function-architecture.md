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

## Phase 2 progress

Phase 2 focused on wiring core Mirror Node endpoints to the transport
helpers introduced in Phase 1. The following user-facing verbs now ship
with REST backends and pagination-aware parsers:

- `accounts_list()`, `accounts_get()`, and `accounts_balance()` return
  tidy account and balance records with parsed timestamps and key
  metadata.
- `transactions_list()`, `transactions_get()`, and `topics_messages()`
  expose transaction history and topic message retrieval with shared
  timestamp parsing utilities.
- `tokens_get()`, `tokens_balances()`, `contracts_get()`, and
  `contracts_bytecode()` wrap token and contract metadata endpoints,
  normalising nested payloads into list-columns when appropriate.

Each helper honours the configuration list returned by
`hadeda_config()`, accepts an optional `.transport` override, and routes
requests through common REST builders. The shared pagination helper
collects `links$next` cursors to surface multi-page responses as a
single tibble. Unit tests mock the REST layer so parsers and query
construction behave deterministically.

## Next Steps
1. Expand helper coverage with argument validators (identifier
   normalisers, pagination cursors, signer abstractions).
2. Implement HTTP and gRPC request builders that honour the
   configuration helpers while producing tidy tibbles.
3. Introduce documentation via Roxygen2 for the configuration helpers and
   publish rendered Rd files in `man/`.
4. Begin implementing Phase 1, Step 3 utilities (HTTP/gRPC helper
   layer) using the scaffolding delivered here.

## Phase 3 progress

The Phase 3 buildout introduces initial CryptoService coverage alongside the
Consensus submit helper delivered earlier. Newly implemented verbs include:

- `crypto_create_account()` – chooses between a gRPC mutation and the existing
  REST `accounts_create()` fallback, returning tidy acknowledgement metadata.
- `crypto_transfer()` – normalises hbar and token transfer payloads before
  delegating to the configured gRPC transport and parsing the resulting
  receipt.
- `crypto_update_account_keys()` – updates account signing keys via gRPC while
  enforcing minimal validation around account identifiers and supplied key
  material.

Supporting internals convert tidy data frames into the list-based payloads
expected by gRPC handlers and collapse mutation responses into consistent
tibbles. The accompanying test suite exercises normalisation paths, transport
selection, and error handling so future transport implementations can plug in
with confidence.
