# Hadeda

Hadeda is an R package that aims to provide idiomatic tidyverse wrappers around the Hashgraph Hedera network APIs. The package will expose a consistent set of high-level functions that map directly to Hedera REST and gRPC endpoints, returning tidy data structures to simplify downstream analysis, monitoring, and workflow automation in R.

## Design goals

* **Parity with Hedera SDKs** – provide coverage for the core feature set available in the official Hedera SDKs and protobuf definitions.
* **Tidyverse ergonomics** – adopt expressive verbs, snake_case naming, and tibble-based results to integrate naturally with the tidyverse ecosystem.
* **Explicit endpoint mapping** – every exported function will correspond to a single REST or RPC endpoint, with documentation that references the upstream Hedera specification.
* **Composable workflows** – functions will favor pure data transformations with minimal side effects, enabling chaining with `%>%` or `|>`.

## Target service endpoints

The following table lists the initial set of Hedera network endpoints that Hadeda functions will cover. REST endpoints align with the Hedera Mirror Node API, while RPC endpoints use the Hedera gRPC services as defined in the protobuf specifications.

| Domain | Endpoint | Method / RPC | Primary use | Planned function prefix |
| --- | --- | --- | --- | --- |
| Accounts | `/api/v1/accounts` | GET | List accounts with optional filtering | `accounts_list()` |
| Accounts | `/api/v1/accounts/{accountId}` | GET | Retrieve a single account | `accounts_get()` |
| Accounts | `/api/v1/accounts/{accountId}/balance` | GET | Fetch account balance | `accounts_balance()` |
| Tokens | `/api/v1/tokens` | GET | List tokens with metadata | `tokens_list()` |
| Tokens | `/api/v1/tokens/{tokenId}` | GET | Retrieve token details | `tokens_get()` |
| Transactions | `/api/v1/transactions` | GET | Query transactions with filters | `transactions_list()` |
| Transactions | `/api/v1/transactions/{transactionId}` | GET | Get transaction record | `transactions_get()` |
| Topics | `/api/v1/topics/{topicId}/messages` | GET | Read topic messages | `topics_messages()` |
| Network | `/api/v1/network/nodes` | GET | List available network nodes | `network_nodes()` |
| Smart Contracts | `/api/v1/contracts` | GET | List smart contracts | `contracts_list()` |
| Smart Contracts | `/api/v1/contracts/{contractId}` | GET | Retrieve contract bytecode & metadata | `contracts_get()` |
| RPC: Consensus | `ConsensusService/SubmitMessage` | gRPC | Submit HCS message | `consensus_submit_message()` |
| RPC: Crypto | `CryptoService/CreateAccount` | gRPC | Create new account | `crypto_create_account()` |
| RPC: Crypto | `CryptoService/TransferCrypto` | gRPC | Transfer hbars or tokens | `crypto_transfer()` |
| RPC: SmartContract | `SmartContractService/CallContract` | gRPC | Execute smart contract function | `contract_call()` |
| RPC: Token | `TokenService/CreateToken` | gRPC | Create fungible or NFT token | `token_create()` |

This set will expand as additional Hedera services and protobuf operations are incorporated.

## Naming and argument conventions

* **Function names**
  * Use a `<domain>_<verb>()` pattern (e.g., `accounts_list()`, `crypto_transfer()`), mirroring tidyverse verbs such as `list`, `get`, `create`, `update`, and `delete`.
  * REST query helpers use plural domains (`accounts`, `tokens`, `transactions`), while RPC mutation helpers use the service namespace (`crypto`, `token`, `consensus`).
* **Argument order**
  * The first argument is always `.client`, representing an S3 class containing connection settings (REST base URL, gRPC channel, credentials, throttling options).
  * Endpoint-specific arguments follow, using snake_case names that match API query parameters or protobuf field names.
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
  * Side-effecting functions invisibly return the response tibble to encourage piping.

## Supporting utilities

To keep the API consistent, internal helpers will:

* Convert snake_case R arguments into the mixed-case names expected by Hedera APIs.
* Validate identifier formats with regex checks and informative errors (`cli::cli_abort`).
* Offer pagination helpers (`paginate_accounts()`) that standardize cursor handling across endpoints.
* Provide request builders that serialize inputs into protobuf messages using `RProtoBuf`.

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
