# Workplan

## Phase 1 – Foundations

1. **Landscape analysis**
   * Review Hedera SDKs (Go, Java, JavaScript) and protobuf definitions to catalog available operations.
   * Confirm REST endpoint behavior via Mirror Node API documentation and sample responses.
2. **Package scaffolding**
   * Initialize R package structure with `usethis::create_package()`.
   * Configure `renv`, `devtools`, `testthat`, and CI workflows.
   * Define the `.client` S3 class, including REST and gRPC configuration slots.
3. **Utility layer**
   * Implement HTTP client helpers using `httr2` for REST and `grpc`/`RProtoBuf` for RPC calls.
   * Create shared validators, pagination helpers, and response normalizers that output tibbles.

## Phase 2 – Core endpoints

4. **Accounts domain**
   * Implement `accounts_list()`, `accounts_get()`, and `accounts_balance()` with full parameter coverage.
   * Add fixtures and mocked tests for pagination and filtering logic.
5. **Transactions & topics**
   * Implement transaction query helpers and topic message readers.
   * Ensure timestamp parsing with `lubridate` and type-stable returns.
6. **Tokens & contracts**
   * Cover token metadata, treasury relationships, contract info, and bytecode retrieval.
   * Validate list-column structures for nested token associations.

## Phase 3 – RPC interactions

7. **Consensus service**
   * Implement `consensus_submit_message()` with message chunking and acknowledgement handling.
8. **Crypto service**
   * Implement account creation, key management, and transfer helpers, leveraging protobuf builders.
9. **Token service**
   * Implement token creation, association, and transfer RPC helpers with explicit fee controls.
10. **Smart contract service**
    * Provide `contract_call()` and deployment utilities, including contract parameter encoding.

## Phase 4 – Quality & documentation

11. **Testing & validation**
    * Set up integration tests against Hedera testnet (guarded by env vars) and snapshot-based unit tests.
    * Measure coverage and enforce style with `lintr` and `styler`.
12. **Documentation**
    * Use Roxygen2 to document each function with links to Hedera specifications.
    * Build vignettes demonstrating tidyverse pipelines for analytics and operations.
13. **Release preparation**
    * Finalize README, NEWS, and pkgdown site.
    * Submit to CRAN or publish binaries as appropriate.
