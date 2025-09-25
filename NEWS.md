# Hadeda 0.0.0.9000

## Phase 4 quality and documentation update

- Added transport contract tests that ensure REST fallbacks and gRPC helpers
  expose identical tibble schemas for key account and token workflows.
- Introduced snapshot tests for parser helpers and integration test harnesses
  guarded by environment variables for live Hedera testnet validation.
- Authored a vignette series covering getting started, topics, transactions,
  tokens, and contracts to guide typical user journeys across transports.
- Configured lintr, pkgdown navigation, and release collateral to support
  consistent style enforcement and site generation ahead of publication.
