# Hadeda

Hadeda is an R package that aims to provide idiomatic tidyverse wrappers around the Hashgraph Hedera network APIs. The package will expose a consistent set of high-level functions that map directly to Hedera REST and gRPC endpoints, returning tidy data structures to simplify downstream analysis, monitoring, and workflow automation in R.

## Installation

Project setup is automated via the install scripts in `scripts/`:

- **macOS / Linux**  
  ```bash
  ./scripts/install_hadeda.sh
  ```
  The script bootstraps Pandoc, ensures `pkg-config` and the gRPC native toolchain are present (via Homebrew or apt), restores the `renv` lockfile, and builds/install the package into a project-local library.

- **Windows (PowerShell)**  
  ```powershell
  powershell -ExecutionPolicy Bypass -File .\scripts\install_hadeda.ps1
  ```
  This variant handles Pandoc, installs `pkg-config` and gRPC libraries (via Chocolatey/winget + vcpkg), restores dependencies, and builds/install the package.

If you prefer managing dependencies manually, review the scripts for the exact prerequisites (Pandoc, pkg-config, gRPC headers/libraries) before running `renv::restore()` yourself.

## Design goals

* **Parity with Hedera SDKs** – provide coverage for the core feature set available in the official Hedera SDKs and protobuf definitions.
* **Tidyverse ergonomics** – adopt expressive verbs, snake_case naming, and tibble-based results to integrate naturally with the tidyverse ecosystem.
* **Explicit endpoint mapping** – every exported function will correspond to a single REST or RPC endpoint, with documentation that references the upstream Hedera specification.
* **Dual transport support** – favor a single set of user-facing verbs that can transparently call either REST or gRPC transports, keeping the function names and arguments identical regardless of network protocol.
* **Composable workflows** – functions will favor pure data transformations with minimal side effects, enabling chaining with `%>%` or `|>`.

## Endpoint coverage summary

Phase 1, Step 1 in [`docs/landscape-analysis.md`](docs/landscape-analysis.md) documents
the complete parity matrix between Hadeda functions, Hedera Mirror Node REST
endpoints, and the gRPC RPCs surfaced by the official Go, Java, and JavaScript
SDKs. The highlights are summarised below.

### Mirror Node REST endpoints

Hadeda will expose verbs for every Mirror Node endpoint that the reference SDKs
wrap today, grouped by functional area:

* **Accounts and balances** – `accounts_list()`, `accounts_get()`,
  `accounts_balance()`, `accounts_allowances_crypto()`,
  `accounts_allowances_tokens()`, `accounts_allowances_nfts()`, rewards history, and `balances_list()`
  for timestamped snapshots.
  Mirror node REST APIs are read-only for accounts, so creation and
  lifecycle management flow exclusively through the gRPC helpers.
* **Blocks and contracts** – block pagination plus full contract metadata,
  state, result listings, and per-transaction execution lookups.
* **Network and governance** – exchange rates via `network_exchange_rate()`,
  fee schedules with `network_fees()`, node address book via `network_nodes()`,
  staking snapshots through `network_stake()`, and circulating supply metrics
  using `network_supply()` to match the Java SDK helper utilities.
* **Tokens and NFTs** – token metadata, balances, NFT inventory and
  transactions, and token-level allowance views.
* **Topics and transactions** – transaction queries and state proofs alongside
  topic message history and future streaming helpers.
* **Schedules and proofs** – schedule listings, detail lookups, and
  `/stateproofs/{transactionId}` to support regulatory workflows.

### gRPC services

Every RPC defined in the Hedera protobufs that is exposed by the Go, Java, and
JavaScript SDKs has a corresponding Hadeda verb:

* **CryptoService** – account lifecycle management, hbar/token transfers, and
  allowance administration (`crypto_create_account()`, `crypto_transfer()`,
  `crypto_delete_allowances()`, `crypto_transaction_record()` for
  `getTxRecordByTxID`, etc.).
* **ConsensusService** – topic creation, updates, deletions, message
  submission (chunked and single), and topic metadata queries.
* **FileService** – file creation, updates, appends, deletes, and metadata
  queries.
* **SmartContractService** – contract deployment, updates, on-ledger and local
  calls, deletions, and record retrieval (including the deprecated
  `getContractTxRecordByID`).
* **TokenService** – token lifecycle operations, treasury interactions,
  association helpers, NFT-specific queries, and the newer airdrop/pause APIs
  slated for future Hadeda helpers.
* **ScheduleService** – schedule creation, signing, deletion, and info queries.
* **Network/Util/Freeze services** – network metadata, PRNG utility (with
  planned support for the new `atomicBatch` transaction), and freeze
  orchestration endpoints required for node operators.

Additional streaming helpers such as `topics_messages_stream()` and
`consensus_topic_subscribe()` will wrap the Mirror Node streaming APIs so that R
workflows remain on par with JavaScript SDK capabilities.

The initial gRPC surface now spans the full CryptoService (`crypto_create_account()`,
`crypto_update_account()`, `crypto_update_account_keys()`, `crypto_transfer()`,
`crypto_delete()`, allowance helpers, and the updated
`crypto_transaction_record()` / `crypto_transaction_records()` pair that now
surface duplicate and child records), alongside TokenService helpers
(`tokens_create()`, `tokens_associate()`, `tokens_transfer()`),
SmartContractService deployment and execution utilities (`contract_deploy()`,
`contract_call()`), and the REST-backed `consensus_submit_message()`. These
functions accept tidy inputs, delegate to user-provided gRPC handlers, and
return acknowledgement tibbles that mirror the rest of the package.

## Naming and argument conventions

* **Function names**
  * Use a `<domain>_<verb>()` pattern (e.g., `accounts_list()`, `crypto_transfer()`), mirroring tidyverse verbs such as `list`, `get`, `create`, `update`, and `delete`.
  * REST query helpers use plural domains (`accounts`, `tokens`, `transactions`), while RPC mutation helpers use the service namespace (`crypto`, `token`, `consensus`).
  * For dual-transport verbs, the same function name orchestrates the request builder for both REST and gRPC, with the transport inferred from the Hadeda configuration or an explicit `.transport` argument.
* **Argument order**
  * The first argument is always `config`, a list created by `hadeda_config()` that contains REST settings (base URL, headers, rate limits) and gRPC handlers.
  * Endpoint-specific arguments follow, using snake_case names that match API query parameters or protobuf field names.
  * A `.transport` argument defaults to `NULL`, enabling auto-selection between REST and gRPC while letting advanced users force a protocol.
  * Dots (`...`) are reserved for future extensions such as pagination cursors or request options.
* **Argument types**
  * Identifiers (`account_id`, `token_id`, `transaction_id`) are stored as character vectors in the Hedera format (for example the account returned by `crypto_create_account()` might be `"0.0.4891617"`).
  * Monetary amounts (`amount`, `fee`, `max_fee`) use 64-bit integers via the `bit64::integer64` class to preserve precision.
  * Boolean toggles use bare logicals (`TRUE`/`FALSE`).
  * Timestamps and durations use `POSIXct` and `lubridate::duration` objects respectively.
  * Collections of filters accept tidy-select semantics where possible (e.g., `dplyr::vars_select` style for field inclusion).
* **Return values**
  * REST queries return tibbles (`tibble::tibble`) with one row per entity and list-columns for nested structures.
  * RPC calls that mutate state return a tibble with a single row summarizing the response metadata along with nested list-columns for receipts and records.
  * When both transports are available, the function standardizes column names and types so that downstream tidyverse code receives consistent schemas regardless of protocol.
  * Side-effecting functions invisibly return the response tibble to encourage piping.

## Dual transport orchestration

Hadeda functions negotiate between REST and gRPC using the configuration object returned by `hadeda_config()`, which stores both REST settings (base URL, rate limits) and gRPC configuration (channel, TLS, signing keys). Each verb performs the following steps:

1. Determine the preferred transport by inspecting `config$default_transport` and honoring an optional `.transport` override passed by the caller.
2. Build a tidy list of request parameters using the same snake_case arguments for either transport.
3. Dispatch to `hadeda_request_rest()` or `hadeda_request_grpc()` internals that know how to translate the parameter list into HTTP query strings or protobuf messages.
4. Normalize the response into a tibble using shared post-processing helpers so that pipelines such as `accounts_list(...) |> select(account_id, balance_hbar)` behave identically.

This design allows analytical workflows to stay idiomatic:

```r
cfg <- hadeda_config(network = "testnet")

accounts_list(cfg) |>
  mutate(balance_hbar = balance_tinybar / 1e8) |>
  arrange(desc(balance_hbar))

crypto_transfer(cfg, from_account, to_account, amount) |>
  tidyr::unnest_wider(receipt)
```

In both examples the same verbs work whether `config` points to a REST-only mirror node, a gRPC-enabled node, or a hybrid configuration.

## Supporting utilities

To keep the API consistent, internal helpers will:

* Convert snake_case R arguments into the mixed-case names expected by Hedera APIs or protobuf fields.
* Validate identifier formats with regex checks and informative errors (`cli::cli_abort`).
* Offer pagination helpers (`paginate_accounts()`) that standardize cursor handling across endpoints.
* Provide request builders that serialize inputs into protobuf messages using `RProtoBuf`.
* Harmonize REST JSON payloads and gRPC protobuf responses into shared tidy schemas via `vctrs::vec_rbind()` builders.

## Signing and key management plan

Interacting with Hedera's gRPC services requires clients to sign transactions with their private keys. Hadeda will provide a
layered approach so that R users can pick a credential workflow that balances convenience and security:

* **Dedicated key stores** – Prefer storing private keys in secure OS-backed vaults managed by packages such as
  [`keyring`](https://cran.r-project.org/package=keyring) or [`askpass`](https://cran.r-project.org/package=askpass). Hadeda will
  ship helpers (e.g., `hadeda_key_store_set()` / `hadeda_key_store_get()`) that wrap these packages to register a key once and
  retrieve it on demand during signing. Keys are never persisted to disk by Hadeda; the package only requests in-memory
  materialization for the duration of a request.
* **Ephemeral session keys** – Advanced users can supply `openssl::read_key()` or `sodium` key objects directly to the
  configuration helpers when constructing a request pipeline. The resolved configuration will hold raw key material in-memory
  only and wipe it with `openssl::zeroize()` after use to avoid lingering secrets.
* **Environment variables (opt-in)** – For scripts that must run unattended (e.g., CI pipelines), Hadeda will look for
  `HADEDA_PRIVATE_KEY`, `HADEDA_OPERATOR_ID`, and related variables via `Sys.getenv()`. Documentation will clearly warn that
  environment variables are only as secure as the host configuration: storing keys in `.Renviron`, shell history, or shared
  servers is discouraged. Guidance will recommend using environment variables exclusively in locked-down automation contexts
  (GitHub Actions secrets, HashiCorp Vault injections) and pairing them with `renv::load_dot_env()` or `withr::with_envvar()` to
  minimize exposure.
* **Safety prompts** – Whenever a user attempts to register a key through a plain-text mechanism (e.g., pasting into the R
  console), Hadeda will issue a `cli::cli_alert_warning()` explaining the risks and pointing to the secure storage helpers. The
  package vignette will include a "Choosing a signing strategy" section that compares the security posture of each option.

Internally, signing will be mediated by a `hadeda_sign_transaction()` helper that accepts a generic key provider interface. The
provider will resolve to one of the strategies above, normalize the key into the protobuf-required format, and return an
`openssl::signature` object ready for gRPC submission. This abstraction keeps endpoint wrappers agnostic to how credentials are
supplied while making it straightforward to add support for hardware wallets or external agents in the future.

## Example workflow

```r
library(hadeda)
library(dplyr)

accounts <- accounts_list(hadeda_config(network = "testnet")) %>%
  filter(balance_hbar > 100) %>%
  arrange(desc(balance_hbar))

grpc_config <- hadeda_config(
  network = "testnet",
  grpc = list(
    create_account = function(config,
                               public_key,
                               initial_balance,
                               alias,
                               key_type,
                               memo,
                               auto_renew_period,
                               key_list,
                               wait_for_record) {
      list(
        transaction_id = "0.0.5005-1700000000-000000000",
        status = "OK",
        receipt = list(status = "SUCCESS", accountId = "0.0.5005")
      )
    },
    crypto_transfer = function(config,
                               transfers,
                               token_transfers,
                               memo,
                               transaction_valid_duration,
                               max_fee,
                               wait_for_receipt) {
      list(
        transaction_id = "0.0.5005-1700000001-000000001",
        status = "OK",
        receipt = list(status = "SUCCESS"),
        consensusTimestamp = "1700000001.000000001"
      )
    }
  ),
  default_transport = "grpc"
)

new_account <- crypto_create_account(
  grpc_config,
  public_key = "302a300506032b6570032100...",
  initial_balance = 10
)

crypto_transfer(
  grpc_config,
  transfers = tibble::tribble(
    ~account_id, ~amount,
    "0.0.5005", bit64::as.integer64(-1e8),
    new_account$account_id, bit64::as.integer64(1e8)
  ),
  memo = "Example transfer"
)
```

## Project structure

* `R/` – Endpoint wrappers and internal helpers.
* `man/` – Roxygen2-generated documentation referencing Hedera specs.
* `tests/` – Testthat suites mocking REST and gRPC requests.
* `vignettes/` – Usage guides and walkthroughs for common workflows.
* `pkgdown/` – Site configuration for publishing reference documentation.

## Documentation

Rendered reference documentation and user journey vignettes are published via
pkgdown. Build the site locally with:

```r
devtools::document()
pkgdown::build_site()
```

The article index covers getting started, topics, transactions, tokens, and
contracts so teams can follow end-to-end workflows across transports.

## Token service RPC helpers

Hadeda now surfaces TokenService mutations alongside the existing REST
wrappers:

* `tokens_create()` supports `.transport = "grpc"` with `max_fee` and
  `wait_for_receipt` controls when paired with a gRPC handler such as
  `config$grpc$token_create`.
* `tokens_associate()` maps to the `associateTokens` RPC, normalising token
  identifiers before submitting the request.
* `tokens_transfer()` issues multi-party transfers with tidy data frame or list
  inputs and explicit fee controls.

When using the gRPC transport, configure handlers via
`hadeda_config(grpc = list(token_create = function(...) {...}))` or integrate an
SDK-backed client within the `config$grpc` list.

## Contributing

1. Fork and clone the repository.
2. Use `renv` to manage dependencies and reproduce environments.
3. Run `devtools::test()` and `lintr::lint_package()` before submitting pull requests.
4. Follow the tidyverse style guide and keep coverage parity with the upstream SDK features you modify.

## License

MIT License.
