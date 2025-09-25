# Phase 1 – Step 1: Landscape analysis

This document catalogs the Hedera Mirror Node REST endpoints and gRPC service RPCs
covered by the official Go, Java, and JavaScript SDKs so that Hadeda can expose a
functionally equivalent surface area. It will guide future implementation work and
serve as the source of truth for endpoint-to-function mappings.

All planned Hadeda verbs share the signature conventions described in the README.
Each entry below references the upstream capability and the corresponding
Hadeda function we will implement.

## Coverage snapshot

- **Mirror Node REST:** 16 of 38 documented endpoints now have corresponding Hadeda helpers (≈42% coverage), spanning accounts, blocks, contracts, network metadata, tokens, topics, and transactions.
- **gRPC services:** Implementation work has not yet started; all RPCs below remain planned stubs.

## Mirror Node REST endpoints

The Mirror Node API is documented at <https://docs.hedera.com/hedera/mirror-node-api>.
The table below consolidates the endpoints actively supported by the SDKs and maps
them to Hadeda functions. When a Mirror Node endpoint corresponds to a gRPC
query, Hadeda will still provide an HTTP implementation because REST is the most
widely available transport today.

| Domain | Endpoint | Purpose | Hadeda function | Status |
| --- | --- | --- | --- | --- |
| Accounts | `/api/v1/accounts` | List accounts with optional filtering, pagination, and staking metadata | `accounts_list()` | ✅ Implemented |
| Accounts | `/api/v1/accounts/{accountId}` | Retrieve a single account, including key, memo, staking info | `accounts_get()` | ✅ Implemented |
| Accounts | `/api/v1/accounts/{accountId}/balance` | Fetch the latest balance snapshot for an account | `accounts_balance()` | ✅ Implemented |
| Accounts | `/api/v1/accounts/{accountId}/allowances/crypto` | View approved HBAR allowances | `accounts_allowances_crypto()` | 🚧 Planned |
| Accounts | `/api/v1/accounts/{accountId}/allowances/tokens` | View approved fungible token allowances | `accounts_allowances_tokens()` | 🚧 Planned |
| Accounts | `/api/v1/accounts/{accountId}/allowances/nfts` | View approved NFT allowances | `accounts_allowances_nfts()` | 🚧 Planned |
| Accounts | `/api/v1/accounts/{accountId}/rewards` | Retrieve staking reward history | `accounts_rewards()` | 🚧 Planned |
| Accounts | `/api/v1/balances` | List account balances at a consensus timestamp | `balances_list()` | 🚧 Planned |
| Blocks | `/api/v1/blocks` | Enumerate blocks with hash, number, and timestamp | `blocks_list()` | ✅ Implemented |
| Blocks | `/api/v1/blocks/{blockNumberOrHash}` | Retrieve details for a specific block | `blocks_get()` | ✅ Implemented |
| Contracts | `/api/v1/contracts` | List smart contracts and metadata | `contracts_list()` | 🚧 Planned |
| Contracts | `/api/v1/contracts/{contractId}` | Fetch contract metadata | `contracts_get()` | ✅ Implemented |
| Contracts | `/api/v1/contracts/{contractId}/bytecode` | Retrieve smart contract bytecode | `contracts_bytecode()` | ✅ Implemented |
| Contracts | `/api/v1/contracts/{contractId}/results` | List execution results for a contract | `contracts_results()` | 🚧 Planned |
| Contracts | `/api/v1/contracts/results` | Query contract call results across contracts | `contracts_results_list()` | 🚧 Planned |
| Contracts | `/api/v1/contracts/results/{transactionId}` | Fetch a single contract execution result | `contracts_results_get()` | 🚧 Planned |
| Contracts | `/api/v1/contracts/{contractId}/state` | Get key-value state entries for a contract | `contracts_state()` | 🚧 Planned |
| Network | `/api/v1/network/exchangerate` | Retrieve current and next exchange rates | `network_exchange_rate()` | 🚧 Planned |
| Network | `/api/v1/network/fees` | Fetch fee schedules | `network_fees()` | 🚧 Planned |
| Network | `/api/v1/network/nodes` | List mirror and network nodes | `network_nodes()` | ✅ Implemented |
| Network | `/api/v1/network/stake` | Retrieve network stake metadata | `network_stake()` | ✅ Implemented |
| Network | `/api/v1/network/supply` | Retrieve supply metrics (hbar total/circulating) | `network_supply()` | 🚧 Planned |
| Schedules | `/api/v1/schedules` | List scheduled transactions | `schedules_list()` | 🚧 Planned |
| Schedules | `/api/v1/schedules/{scheduleId}` | Get a scheduled transaction | `schedules_get()` | 🚧 Planned |
| State Proof | `/api/v1/stateproofs/{transactionId}` | Retrieve a transaction state proof | `stateproof_get()` | 🚧 Planned |
| Tokens | `/api/v1/tokens` | List tokens with filters | `tokens_list()` | 🚧 Planned |
| Tokens | `/api/v1/tokens/{tokenId}` | Retrieve token metadata | `tokens_get()` | ✅ Implemented |
| Tokens | `/api/v1/tokens/{tokenId}/balances` | List token balances by account | `tokens_balances()` | ✅ Implemented |
| Tokens | `/api/v1/tokens/{tokenId}/nfts` | Enumerate NFTs for a token | `tokens_nfts()` | ✅ Implemented |
| Tokens | `/api/v1/tokens/{tokenId}/nfts/{serialNumber}` | Fetch a single NFT | `tokens_nfts_get()` | 🚧 Planned |
| Tokens | `/api/v1/tokens/{tokenId}/nfts/{serialNumber}/transactions` | NFT transfer history | `tokens_nft_transactions()` | 🚧 Planned |
| Tokens | `/api/v1/tokens/{tokenId}/allowances` | Allowance list for a token | `tokens_allowances()` | 🚧 Planned |
| Topics | `/api/v1/topics/{topicId}` | Retrieve topic metadata | `topics_get()` | ✅ Implemented |
| Topics | `/api/v1/topics/{topicId}/messages` | Stream topic messages | `topics_messages()` | ✅ Implemented |
| Transactions | `/api/v1/transactions` | List transactions with filters | `transactions_list()` | ✅ Implemented |
| Transactions | `/api/v1/transactions/{transactionId}` | Fetch transaction record and metadata | `transactions_get()` | ✅ Implemented |
| Transactions | `/api/v1/transactions/{transactionId}/stateproof` | Retrieve state proof for a transaction | `transactions_stateproof()` | 🚧 Planned |
| Transactions | `/api/v1/transactions/{transactionId}/record` | Retrieve detailed record | `transactions_record()` | 🚧 Planned |

Additional REST helpers planned for parity with official SDK utilities:

* `network_address_book()` – combine `/network/nodes` and key data for operator setup.
* `accounts_relationships()` – flatten token relationship list columns like the Java
  SDK helper utilities.
* `topics_messages_stream()` – wrap the Mirror Node SSE/WebSocket endpoints once
  available so R users can subscribe to live topics similar to JavaScript SDKs.

## gRPC service coverage

The official SDKs expose the Hedera gRPC services defined in
`hedera-protobufs`. Hadeda will mirror these operations with tidyverse-friendly
functions. Implementation has not begun yet; the table below tracks the planned
surface so we can mark progress as gRPC helpers land. Each RPC will correspond to
a Hadeda verb accepting a `.client` configured with signing keys and channel
information.

### Crypto Service (`CryptoService`)

| RPC | Description | Planned Hadeda function |
| --- | --- | --- |
| `createAccount` | Create an account | `crypto_create_account()` |
| `updateAccount` | Update mutable account properties | `crypto_update_account()` |
| `cryptoTransfer` | Transfer hbar or tokens between accounts | `crypto_transfer()` |
| `cryptoDelete` | Delete an account and transfer remaining balance | `crypto_delete()` |
| `approveAllowances` | Approve fungible/NFT allowances | `crypto_approve_allowances()` |
| `deleteAllowances` | Revoke allowances | `crypto_delete_allowances()` |
| `getAccountRecords` | Query account transaction records | `crypto_account_records()` |
| `getAccountBalance` | Query account balance | `crypto_account_balance()` |
| `getAccountInfo` | Query account metadata | `crypto_account_info()` |
| `getTransactionReceipts` | Get receipts by transaction ID | `crypto_transaction_receipts()` |
| `getTransactionRecord` | Get a single transaction record | `crypto_transaction_record()` |
| `getTransactionRecords` | Get paged transaction records | `crypto_transaction_records()` |
| `getAccountDetails` | Rich account metadata (HIP-623) | `crypto_account_details()` |
| Deprecated live hash RPCs | Exposed for completeness but throw `NOT_SUPPORTED` | `crypto_livehash_*()` (internal stubs) |

### Consensus Service (`ConsensusService`)

| RPC | Description | Planned Hadeda function |
| --- | --- | --- |
| `submitMessage` | Submit a topic message | `consensus_submit_message()` |
| `submitMessageChunk` | Submit chunked message segments | `consensus_submit_message_chunk()` |
| `getTopicInfo` | Query topic metadata | `consensus_topic_info()` |

Hadeda will also surface the Mirror Consensus Service streaming subscription via
`consensus_topic_subscribe()` to maintain parity with SDK streaming APIs.

### File Service (`FileService`)

| RPC | Description | Planned Hadeda function |
| --- | --- | --- |
| `createFile` | Create a new file | `file_create()` |
| `updateFile` | Update file contents or keys | `file_update()` |
| `deleteFile` | Delete a file | `file_delete()` |
| `appendContent` | Append contents to a file | `file_append()` |
| `getFileContent` | Download file contents | `file_content()` |
| `getFileInfo` | Retrieve file metadata | `file_info()` |

### Smart Contract Service (`SmartContractService`)

| RPC | Description | Planned Hadeda function |
| --- | --- | --- |
| `createContract` | Deploy a smart contract | `contract_create()` |
| `updateContract` | Update contract properties | `contract_update()` |
| `contractCallMethod` | Execute a contract call | `contract_call()` |
| `contractCallLocalMethod` | Local query (no state change) | `contract_call_local()` |
| `deleteContract` | Delete a contract | `contract_delete()` |
| `getContractInfo` | Query contract metadata | `contract_info()` |
| `getContractRecords` | Fetch contract transaction records | `contract_records()` |
| `getTxRecordByContractID` | Fetch records by contract ID | `contract_tx_record_by_id()` |

### Token Service (`TokenService`)

| RPC | Description | Planned Hadeda function |
| --- | --- | --- |
| `createToken` | Create fungible/NFT tokens | `token_create()` |
| `updateToken` | Update token properties | `token_update()` |
| `mintToken` | Mint new token units/NFTs | `token_mint()` |
| `burnToken` | Burn token units/NFTs | `token_burn()` |
| `deleteToken` | Delete a token | `token_delete()` |
| `wipeTokenAccount` | Wipe an account's token balance | `token_wipe_account()` |
| `freezeTokenAccount` | Freeze token for an account | `token_freeze_account()` |
| `unfreezeTokenAccount` | Unfreeze token for an account | `token_unfreeze_account()` |
| `grantKycToTokenAccount` | Grant KYC | `token_grant_kyc()` |
| `revokeKycFromTokenAccount` | Revoke KYC | `token_revoke_kyc()` |
| `associateTokens` | Associate fungible tokens | `token_associate()` |
| `dissociateTokens` | Dissociate fungible tokens | `token_dissociate()` |
| `approveTokenAllowance` | Approve token allowance | `token_approve_allowance()` |
| `deleteTokenAllowance` | Revoke token allowance | `token_delete_allowance()` |
| `getTokenInfo` | Query token metadata | `token_info()` |
| `getTokenNftInfo` | Query NFT metadata | `token_nft_info()` |
| `getTokenNftInfos` | List NFTs | `token_nft_infos()` |
| `getAccountNftInfos` | List NFTs owned by an account | `token_account_nft_infos()` |
| `getTokenNftTransferHistory` | NFT transfer history | `token_nft_transfer_history()` |
| `getTokenRelationships` | Query account-token relationships | `token_relationships()` |

### Schedule Service (`ScheduleService`)

| RPC | Description | Planned Hadeda function |
| --- | --- | --- |
| `createSchedule` | Create a scheduled transaction | `schedule_create()` |
| `deleteSchedule` | Delete a schedule | `schedule_delete()` |
| `getScheduleInfo` | Query schedule metadata | `schedule_info()` |
| `signSchedule` | Add signature to a schedule | `schedule_sign()` |

### Network, Util, and Freeze services

| Service | RPC | Planned function |
| --- | --- | --- |
| `NetworkService` | `getVersionInfo` | `network_version_info()` |
| `NetworkService` | `getExecutionTime` | `network_execution_time()` |
| `NetworkService` | `getAccountDetails` (HIP-623) | `network_account_details()` |
| `FreezeService` | `freeze` | `freeze_network()` |
| `FreezeService` | `freezeUpgrade` | `freeze_upgrade()` |
| `FreezeService` | `freezeAbort` | `freeze_abort()` |
| `UtilService` | `prng` | `util_prng()` |

## SDK parity checklist

* ✅ Mirror Node REST endpoint coverage documented above.
* ✅ gRPC RPC coverage mapped to Hadeda function names.
* ✅ Streaming utilities (Mirror topic subscribe, SDK helper parity) captured for
  follow-up implementation.

This checklist will be revisited at the end of Phase 1 to ensure any new Hedera
API releases are incorporated before implementation begins.
