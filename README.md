# Hadeda

Hadeda is an R package that aims to provide idiomatic tidyverse wrappers around the Hashgraph Hedera network APIs. The package will expose a consistent set of high-level functions that map directly to Hedera REST and gRPC endpoints, returning tidy data structures to simplify downstream analysis, monitoring, and workflow automation in R.

## Design goals

* **Parity with Hedera SDKs** – provide coverage for the core feature set available in the official Hedera SDKs and protobuf definitions.
* **Tidyverse ergonomics** – adopt expressive verbs, snake_case naming, and tibble-based results to integrate naturally with the tidyverse ecosystem.
* **Explicit endpoint mapping** – every exported function will correspond to a single REST or RPC endpoint, with documentation that references the upstream Hedera specification.
* **Dual transport support** – favor a single set of user-facing verbs that can transparently call either REST or gRPC transports, keeping the function names and arguments identical regardless of network protocol.
* **Composable workflows** – functions will favor pure data transformations with minimal side effects, enabling chaining with `%>%` or `|>`.

## Target service endpoints

The following table lists the initial set of Hedera network endpoints that Hadeda functions will cover. REST endpoints align with the Hedera Mirror Node API, while RPC endpoints use the Hedera gRPC services as defined in the protobuf specifications. Each verb is designed to dispatch to REST or gRPC implementations based on the capabilities of the configured client while sharing the same tidyverse-friendly signature.

| Domain | Endpoint | Method / RPC | Primary use | Planned function(s) |
| --- | --- | --- | --- | --- |
| Accounts | `/api/v1/accounts` | GET | List accounts with optional filtering | `accounts_list()` → REST; `accounts_list(mode = "grpc")` → gRPC mirror (future) |
| Accounts | `/api/v1/accounts/{accountId}` | GET | Retrieve a single account | `accounts_get()` (REST/gRPC shared signature) |
| Accounts | `/api/v1/accounts/{accountId}/balance` | GET | Fetch account balance | `accounts_balance()` (auto-select transport) |
| Tokens | `/api/v1/tokens` | GET | List tokens with metadata | `tokens_list()` (REST first, gRPC when mirror available) |
| Tokens | `/api/v1/tokens/{tokenId}` | GET | Retrieve token details | `tokens_get()` |
| Transactions | `/api/v1/transactions` | GET | Query transactions with filters | `transactions_list()` |
| Transactions | `/api/v1/transactions/{transactionId}` | GET | Get transaction record | `transactions_get()` |
| Topics | `/api/v1/topics/{topicId}/messages` | GET | Read topic messages | `topics_messages()` (REST) |
| Network | `/api/v1/network/nodes` | GET | List available network nodes | `network_nodes()` (REST) |
| Smart Contracts | `/api/v1/contracts` | GET | List smart contracts | `contracts_list()` |
| Smart Contracts | `/api/v1/contracts/{contractId}` | GET | Retrieve contract bytecode & metadata | `contracts_get()` |
| Consensus | `ConsensusService/SubmitMessage` | gRPC | Submit HCS message | `consensus_submit_message()` (gRPC primary, REST fallback) |
| Crypto | `CryptoService/CreateAccount` | gRPC | Create new account | `crypto_create_account()` |
| Crypto | `CryptoService/TransferCrypto` | gRPC | Transfer hbars or tokens | `crypto_transfer()` |
| SmartContract | `SmartContractService/CallContract` | gRPC | Execute smart contract function | `contract_call()` |
| Token | `TokenService/CreateToken` | gRPC | Create fungible or NFT token | `token_create()` |

This set will expand as additional Hedera services and protobuf operations are incorporated.

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
