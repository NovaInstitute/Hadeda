# Hadeda

Hadeda is an R package that aims to provide idiomatic tidyverse wrappers around the Hashgraph Hedera network APIs. The package will expose a consistent set of high-level functions that map directly to Hedera REST and gRPC endpoints, returning tidy data structures to simplify downstream analysis, monitoring, and workflow automation in R.

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
  `accounts_balance()`, allowance readers, rewards history, and `balances_list()`
  for timestamped snapshots.
* **Blocks and contracts** – block pagination plus full contract metadata,
  state, result listings, and per-transaction execution lookups.
* **Network and governance** – exchange rates, fee schedules, node address
  book, and supply metrics to match the Java SDK helper utilities.
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
  `crypto_delete_allowances()`, etc.).
* **ConsensusService** – topic message submission (chunked and single) and
  topic metadata queries.
* **FileService** – file creation, updates, appends, deletes, and metadata
  queries.
* **SmartContractService** – contract deployment, updates, on-ledger and local
  calls, deletions, and record retrieval.
* **TokenService** – token lifecycle operations, treasury interactions,
  association helpers, and NFT-specific queries.
* **ScheduleService** – schedule creation, signing, deletion, and info queries.
* **Network/Util/Freeze services** – network metadata, PRNG utility, and freeze
  orchestration endpoints required for node operators.

Additional streaming helpers such as `topics_messages_stream()` and
`consensus_topic_subscribe()` will wrap the Mirror Node streaming APIs so that R
workflows remain on par with JavaScript SDK capabilities.

## Naming and argument conventions

* **Function names**
  * Use a `<domain>_<verb>()` pattern (e.g., `accounts_list()`, `crypto_transfer()`), mirroring tidyverse verbs such as `list`, `get`, `create`, `update`, and `delete`.
  * REST query helpers use plural domains (`accounts`, `tokens`, `transactions`), while RPC mutation helpers use the service namespace (`crypto`, `token`, `consensus`).
  * For dual-transport verbs, the same function name orchestrates the request builder for both REST and gRPC, with the transport inferred from the `.client` configuration or an explicit `.transport` argument.
* **Argument order**
  * The first argument is always `.client`, representing an S3 class containing connection settings (REST base URL, gRPC channel, credentials, throttling options).
  * Endpoint-specific arguments follow, using snake_case names that match API query parameters or protobuf field names.
  * A `.transport` argument defaults to `NULL`, enabling auto-selection between REST and gRPC while letting advanced users force a protocol.
  * Dots (`...`) are reserved for future extensions such as pagination cursors or request options.
* **Argument types**
  * Identifiers (`account_id`, `token_id`, `transaction_id`) are stored as character vectors in the Hedera format (e.g., `"0.0.1001"`).
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

Hadeda functions negotiate between REST and gRPC using the `.client` object, which stores both REST configuration (base URL, rate limits) and gRPC configuration (channel, TLS, signing keys). Each verb performs the following steps:

1. Determine the preferred transport by inspecting `.client$default_transport` and honoring an optional `.transport` override passed by the caller.
2. Build a tidy list of request parameters using the same snake_case arguments for either transport.
3. Dispatch to `hadeda_request_rest()` or `hadeda_request_grpc()` internals that know how to translate the parameter list into HTTP query strings or protobuf messages.
4. Normalize the response into a tibble using shared post-processing helpers so that pipelines such as `accounts_list(...) |> select(account_id, balance_hbar)` behave identically.

This design allows analytical workflows to stay idiomatic:

```r
accounts_list(client) |> 
  mutate(balance_hbar = balance_tinybar / 1e8) |> 
  arrange(desc(balance_hbar))

crypto_transfer(client, from_account, to_account, amount) |> 
  tidyr::unnest_wider(receipt)
```

In both examples the same verbs work whether `client` points to a REST-only mirror node, a gRPC-enabled node, or a hybrid configuration.

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
* **Ephemeral session keys** – Advanced users can supply `openssl::read_key()` or `sodium` key objects directly to
  `hadeda_client()` when constructing a session. The client object will hold raw key material in-memory only and wipe it with
  `openssl::zeroize()` when the session is closed to avoid lingering secrets.
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

accounts <- accounts_list(hadeda_client(network = "testnet")) %>%
  filter(balance_hbar > 100) %>%
  arrange(desc(balance_hbar))

new_transfer <- crypto_transfer(
  hadeda_client(network = "testnet"),
  from_account = "0.0.1001",
  to_account = "0.0.2002",
  amount = bit64::as.integer64(1e8)
)
```

## Project structure

* `R/` – Endpoint wrappers and internal helpers.
* `man/` – Roxygen2-generated documentation referencing Hedera specs.
* `tests/` – Testthat suites mocking REST and gRPC requests.
* `inst/vignettes/` – Usage guides and walkthroughs for common workflows.

## Contributing

1. Fork and clone the repository.
2. Use `renv` to manage dependencies and reproduce environments.
3. Run `devtools::test()` and `lintr::lint_package()` before submitting pull requests.
4. Follow the tidyverse style guide and keep coverage parity with the upstream SDK features you modify.

## License

MIT License.
