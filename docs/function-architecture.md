# Phase 1 – Step 2: Clientless Signing & Function Architecture Plan

## Goals
- Adopt an idiomatic R interface that accepts signatures (or signers) instead of forcing users to manage a persistent client object.
- Ensure transport-agnostic function families can delegate signing, transport selection, and asynchronous execution concerns in a predictable way.
- Identify shared helper functions that can be implemented and tested early to reduce duplication in subsequent work packages.

## Signature Handling Strategy
- **Primary argument**: each call that needs authentication receives a `signature` argument.
  - Accepts a character vector (already-prepared signature string) or a function returning a character scalar when called with no arguments.
  - Helper functions (see below) return closures for secure retrieval, e.g. `signature = signer_env("HEDERA_PRIVATE_KEY")`.
  - Lazy evaluation lets us defer sensitive key retrieval until just-in-time execution, reducing accidental exposure in logs.
- **Validation**: a shared `validate_signature(signature)` helper coerces character inputs, confirms length/format, or invokes signer functions with error handling that masks secrets in messages.
- **Derived arguments**: top-level verbs accept `account_id`, `network`, optional `public_key`. Builders for transactions embed these fields alongside signing metadata, so downstream helpers (e.g. `submit_transaction()`) do not need to know which variant produced the signature.

## Function Hierarchy & Responsibilities
- **Top-level verbs** (e.g. `accounts_create()`, `token_transfer()`): user-facing wrappers.
  - Collect domain-specific parameters, normalize them into a transaction spec list, call shared builders.
  - For REST-capable operations, accept `.transport = c("rest", "grpc", "auto")` with default set via package option.
  - Provide `.async` argument that toggles between synchronous execution (returning parsed response) and asynchronous execution (returning a promise).
- **Transaction builders** (`build_transaction_*`): create protobuf payloads or REST request bodies.
  - Receive validated primitives (IDs, amounts, memo) and the **signature provider**; produce a structured object with fields `payload`, `signer`, `transport_requirements`.
  - Do not perform network I/O.
- **Submission helpers** (`submit_transaction()`): handle transport selection, signing, retries.
  - Invoke `resolve_signer()` to materialize the signature just before transport-specific encoding.
  - For gRPC, attach signatures to protobuf objects; for REST, include in headers/body as required.
  - For `.async = TRUE`, wrap network request in `future::future()` or `promises::future_promise()` depending on chosen async backend.
- **Polling/Receipt helpers** (`await_receipt()`, `stream_topic()`): abstract async completion flows.
  - Accept objects returned by submission helpers and poll endpoints or consume streams until confirmation.

## Asynchronous Considerations
- Use a pluggable backend via a package option (e.g. `options(hedera.future_plan = future::multisession)`), defaulting to synchronous execution when `.async = FALSE`.
- Shared helper `maybe_async(expr, async = FALSE, scheduler = NULL)` returns either the evaluated result or a promise/future.
- Streaming helpers (e.g. topic message subscription) expose generator-style iterators that yield tibbles; asynchronous consumption relies on helper wrappers from `promises` and `later`.

## Cross-Cutting Helpers (Build First)
1. `is_signer(x)` / `validate_signature(x)` – accept character or function, provide informative errors without revealing secrets.
2. `signer_env(var, decrypt = identity)` – returns function reading from `Sys.getenv(var)` and optionally piping through a decryptor (e.g. `askpass::askpass`).
3. `signer_keyring(service, key = NULL)` – closure that fetches keys from `keyring::key_get()` on demand.
4. `resolve_signer(signature)` – calls `validate_signature()` and forces function inputs.
5. `maybe_async(expr, async = FALSE, scheduler = getOption("hedera.async_scheduler", NULL))` – standardize async toggling.
6. `match_transport(.transport, default = getOption("hedera.transport", "auto"))` – centralize transport selection logic.
7. `normalize_account_id(x)` / `normalize_token_id(x)` – reused across builders and verbs.
8. `build_request()` scaffolding that transforms builder output into REST/gRPC objects while deferring execution.

Implementing and testing helpers (1–7) upfront enables consistent argument validation and behavior across domains before domain-specific verbs are introduced.

## Testing Approach for Helpers
- Unit tests in `tests/testthat/test-signers.R` verifying:
  - Direct character signatures pass through untouched.
  - Signer functions are only evaluated once and errors are masked.
  - `signer_env()` respects `NA` / missing environment variables with informative errors.
  - Keyring helpers skip gracefully when `keyring` is not installed (skip on missing namespace).
- Async helper tests in `tests/testthat/test-async.R` covering synchronous vs asynchronous returns using the `promises` test helpers.
- Transport matcher tests ensuring `.transport` argument accepts partial matches and respects package options.

## Next Steps
1. Implement signer helpers (`validate_signature()`, `resolve_signer()`, `signer_env()`, `signer_keyring()`).
2. Implement transport matching helper and ID normalizers.
3. Introduce `maybe_async()` with placeholder synchronous backend.
4. Draft skeletons for transaction builders and submission helper to confirm interfaces before implementing domain verbs.
