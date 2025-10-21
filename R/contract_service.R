#' Manage deployed Hedera smart contracts via gRPC
#'
#' These helpers expose the remaining SmartContractService RPCs via tidy
#' interfaces that normalise acknowledgements and query responses for downstream
#' pipelines. All verbs currently require the gRPC transport.
#'
#' @name contract_service
NULL

#' Update mutable smart contract properties
#'
#' Submit a SmartContractService `updateContract` transaction via the configured
#' gRPC transport. At least one mutable attribute must be supplied. The helper
#' normalises the acknowledgement into a tidy tibble and preserves the raw
#' response for inspection.
#'
#' @inheritParams contract_call
#' @param admin_key Optional admin key definition accepted by the gRPC handler.
#' @param auto_renew_account Optional auto-renew account identifier.
#' @param auto_renew_period Optional auto-renew period expressed in seconds.
#' @param expiration_time Optional expiration timestamp expressed as POSIXct, a
#'   numeric epoch second value, or a string timestamp.
#' @param memo Optional contract memo.
#' @param staked_account_id Optional staking account identifier.
#' @param staked_node_id Optional staking node identifier.
#' @param decline_staking_reward Optional logical controlling whether staking
#'   rewards should be declined.
#' @param max_token_associations Optional ceiling for automatic token
#'   associations.
#' @param wait_for_record Logical toggling whether the handler should await a
#'   transaction record acknowledgement.
#'
#' @return A tibble summarising the transaction metadata.
#'
#' @examples
#' mirror <- hadeda_config(network = "testnet")
#' \dontrun{
#'   contract_update(
#'     mirror,
#'     contract_id = "0.0.9001",
#'     memo = "Updated contract"
#'   )
#' }
#'
#' @export
contract_update <- function(config,
                            contract_id,
                            admin_key = NULL,
                            auto_renew_account = NULL,
                            auto_renew_period = NULL,
                            expiration_time = NULL,
                            memo = NULL,
                            staked_account_id = NULL,
                            staked_node_id = NULL,
                            decline_staking_reward = NULL,
                            max_token_associations = NULL,
                            max_fee = NULL,
                            wait_for_record = FALSE,
                            .transport = NULL) {
  transport <- hadeda_choose_transport(
    config,
    .transport,
    rest_supported = FALSE,
    grpc_supported = TRUE
  )
  if (!identical(transport, "grpc")) {
    cli::cli_abort("contract_update() currently supports only the gRPC transport.")
  }

  if (rlang::is_missing(contract_id) || is.null(contract_id) || !nzchar(as.character(contract_id)[[1]])) {
    cli::cli_abort("`contract_id` must be supplied when updating a contract.")
  }

  fields <- list(admin_key, auto_renew_account, auto_renew_period, expiration_time,
                 memo, staked_account_id, staked_node_id, decline_staking_reward,
                 max_token_associations)
  if (all(vapply(fields, is.null, logical(1), USE.NAMES = FALSE))) {
    cli::cli_abort("Provide at least one mutable attribute when updating a contract.")
  }

  expiry <- hadeda_normalise_contract_expiration(expiration_time)

  response <- hadeda_grpc_contract_update(
    config = config,
    contract_id = contract_id,
    admin_key = admin_key,
    auto_renew_account = auto_renew_account,
    auto_renew_period = auto_renew_period,
    expiration_time = expiry$value,
    memo = memo,
    staked_account_id = staked_account_id,
    staked_node_id = staked_node_id,
    decline_staking_reward = decline_staking_reward,
    max_token_associations = max_token_associations,
    max_fee = max_fee,
    wait_for_record = wait_for_record
  )

  tbl <- hadeda_parse_grpc_mutation_response(
    response,
    extra = list(
      contract_id = as.character(contract_id),
      admin_key = list(admin_key),
      auto_renew_account = if (is.null(auto_renew_account)) NA_character_ else as.character(auto_renew_account),
      auto_renew_period = if (is.null(auto_renew_period)) NA_real_ else as.numeric(auto_renew_period),
      expiration_time = expiry$column,
      memo = if (is.null(memo)) NA_character_ else as.character(memo),
      staked_account_id = if (is.null(staked_account_id)) NA_character_ else as.character(staked_account_id),
      staked_node_id = if (is.null(staked_node_id)) NA_character_ else as.character(staked_node_id),
      decline_staking_reward = if (is.null(decline_staking_reward)) NA else rlang::is_true(decline_staking_reward),
      max_token_associations = if (is.null(max_token_associations)) NA_real_ else as.numeric(max_token_associations)
    )
  )

  tbl$max_fee <- list(max_fee)
  tbl
}

#' @keywords internal
hadeda_grpc_contract_update <- function(config,
                                        contract_id,
                                        admin_key = NULL,
                                        auto_renew_account = NULL,
                                        auto_renew_period = NULL,
                                        expiration_time = NULL,
                                        memo = NULL,
                                        staked_account_id = NULL,
                                        staked_node_id = NULL,
                                        decline_staking_reward = NULL,
                                        max_token_associations = NULL,
                                        max_fee = NULL,
                                        wait_for_record = FALSE) {
  grpc <- hadeda_require_grpc(config)
  handler <- grpc$contract_update %||% grpc$update_contract %||% grpc$contractUpdate %||%
    grpc$contract_update_transaction
  if (!rlang::is_function(handler)) {
    cli::cli_abort(
      "No gRPC SmartContractService update handler configured.\nProvide `config$grpc$contract_update` to enable contract updates.",
      class = "hadeda_grpc_not_configured"
    )
  }

  handler(
    config = config,
    contract_id = contract_id,
    admin_key = admin_key,
    auto_renew_account = auto_renew_account,
    auto_renew_period = auto_renew_period,
    expiration_time = expiration_time,
    memo = memo,
    staked_account_id = staked_account_id,
    staked_node_id = staked_node_id,
    decline_staking_reward = decline_staking_reward,
    max_token_associations = max_token_associations,
    max_fee = max_fee,
    wait_for_record = wait_for_record
  )
}

#' Execute a local smart contract query
#'
#' Invoke the SmartContractService `contractCallLocalMethod` RPC which executes a
#' contract function without submitting a transaction to the network. Callers can
#' provide either ABI parameters via `function_signature`/`parameters` or raw
#' pre-encoded `call_data`.
#'
#' @inheritParams contract_call
#' @param max_result_size Optional ceiling for the response payload size accepted
#'   by the handler.
#'
#' @return A tibble with the call metadata and `ContractFunctionResult` fields.
#'
#' @examples
#' mirror <- hadeda_config(network = "testnet")
#' \dontrun{
#'   contract_call_local(
#'     mirror,
#'     contract_id = "0.0.9001",
#'     function_signature = "balanceOf(address)",
#'     parameters = list("0.0.1234")
#'   )
#' }
#'
#' @export
contract_call_local <- function(config,
                                 contract_id,
                                 function_signature = NULL,
                                 parameters = list(),
                                 call_data = NULL,
                                 gas = NULL,
                                 sender_account_id = NULL,
                                 max_result_size = NULL,
                                 .transport = NULL) {
  transport <- hadeda_choose_transport(
    config,
    .transport,
    rest_supported = FALSE,
    grpc_supported = TRUE
  )
  if (!identical(transport, "grpc")) {
    cli::cli_abort("contract_call_local() currently supports only the gRPC transport.")
  }

  if (rlang::is_missing(contract_id) || is.null(contract_id) || !nzchar(as.character(contract_id)[[1]])) {
    cli::cli_abort("`contract_id` must be supplied when executing a local query.")
  }

  if (is.null(call_data)) {
    if (rlang::is_missing(function_signature) || is.null(function_signature)) {
      cli::cli_abort("Provide either `call_data` or `function_signature` for contract_call_local().")
    }
    call_data <- contract_encode_parameters(function_signature, parameters)
  } else {
    call_data <- hadeda_contract_normalise_hex(call_data)
  }

  response <- hadeda_grpc_contract_call_local(
    config = config,
    contract_id = contract_id,
    gas = gas,
    call_data = call_data,
    sender_account_id = sender_account_id,
    max_result_size = max_result_size
  )

  call_result <- response$call_result %||% response$result %||%
    response$contract_call_result %||% response$contractCallResult %||% list()

  return_data <- call_result$return_data %||% call_result$returnData %||% call_result$result %||% NULL
  gas_used <- call_result$gas_used %||% call_result$gasUsed %||% NA_real_
  error_message <- call_result$error_message %||% call_result$errorMessage %||% NA_character_

  tibble::tibble(
    contract_id = as.character(contract_id),
    function_signature = if (is.null(function_signature)) NA_character_ else as.character(function_signature),
    call_data = call_data,
    gas = if (is.null(gas)) NA_real_ else as.numeric(gas),
    sender_account_id = if (is.null(sender_account_id)) NA_character_ else as.character(sender_account_id),
    max_result_size = if (is.null(max_result_size)) NA_real_ else as.numeric(max_result_size),
    gas_used = if (is.null(gas_used)) NA_real_ else as.numeric(gas_used),
    error_message = if (is.null(error_message)) NA_character_ else as.character(error_message),
    return_data = list(return_data),
    call_result = list(call_result),
    response = list(response)
  )
}

#' @keywords internal
hadeda_grpc_contract_call_local <- function(config,
                                            contract_id,
                                            gas = NULL,
                                            call_data,
                                            sender_account_id = NULL,
                                            max_result_size = NULL) {
  grpc <- hadeda_require_grpc(config)
  handler <- grpc$contract_call_local %||% grpc$contract_call_local_method %||% grpc$contractCallLocal %||%
    grpc$call_contract_local
  if (!rlang::is_function(handler)) {
    cli::cli_abort(
      "No gRPC SmartContractService local call handler configured.\nProvide `config$grpc$contract_call_local` to enable local queries.",
      class = "hadeda_grpc_not_configured"
    )
  }

  handler(
    config = config,
    contract_id = contract_id,
    gas = gas,
    call_data = call_data,
    sender_account_id = sender_account_id,
    max_result_size = max_result_size
  )
}

#' Delete a smart contract
#'
#' Submit a SmartContractService `deleteContract` transaction. Callers may
#' optionally specify beneficiary contract or account identifiers for remaining
#' hbar transfers.
#'
#' @inheritParams contract_call
#' @param transfer_account_id Optional account identifier that should receive
#'   remaining contract balance.
#' @param transfer_contract_id Optional contract identifier that should receive
#'   remaining contract balance.
#'
#' @return A tibble summarising the deletion acknowledgement.
#'
#' @examples
#' mirror <- hadeda_config(network = "testnet")
#' \dontrun{
#'   contract_delete(mirror, contract_id = "0.0.9001")
#' }
#'
#' @export
contract_delete <- function(config,
                            contract_id,
                            transfer_account_id = NULL,
                            transfer_contract_id = NULL,
                            max_fee = NULL,
                            wait_for_record = FALSE,
                            .transport = NULL) {
  transport <- hadeda_choose_transport(
    config,
    .transport,
    rest_supported = FALSE,
    grpc_supported = TRUE
  )
  if (!identical(transport, "grpc")) {
    cli::cli_abort("contract_delete() currently supports only the gRPC transport.")
  }

  if (rlang::is_missing(contract_id) || is.null(contract_id) || !nzchar(as.character(contract_id)[[1]])) {
    cli::cli_abort("`contract_id` must be supplied when deleting a contract.")
  }

  response <- hadeda_grpc_contract_delete(
    config = config,
    contract_id = contract_id,
    transfer_account_id = transfer_account_id,
    transfer_contract_id = transfer_contract_id,
    max_fee = max_fee,
    wait_for_record = wait_for_record
  )

  tbl <- hadeda_parse_grpc_mutation_response(
    response,
    extra = list(
      contract_id = as.character(contract_id),
      transfer_account_id = if (is.null(transfer_account_id)) NA_character_ else as.character(transfer_account_id),
      transfer_contract_id = if (is.null(transfer_contract_id)) NA_character_ else as.character(transfer_contract_id)
    )
  )
  tbl$max_fee <- list(max_fee)
  tbl
}

#' @keywords internal
hadeda_grpc_contract_delete <- function(config,
                                        contract_id,
                                        transfer_account_id = NULL,
                                        transfer_contract_id = NULL,
                                        max_fee = NULL,
                                        wait_for_record = FALSE) {
  grpc <- hadeda_require_grpc(config)
  handler <- grpc$contract_delete %||% grpc$delete_contract %||% grpc$contractDelete %||%
    grpc$delete_contract_transaction
  if (!rlang::is_function(handler)) {
    cli::cli_abort(
      "No gRPC SmartContractService delete handler configured.\nProvide `config$grpc$contract_delete` to enable contract deletion.",
      class = "hadeda_grpc_not_configured"
    )
  }

  handler(
    config = config,
    contract_id = contract_id,
    transfer_account_id = transfer_account_id,
    transfer_contract_id = transfer_contract_id,
    max_fee = max_fee,
    wait_for_record = wait_for_record
  )
}

#' Retrieve contract metadata via gRPC
#'
#' Query the SmartContractService `getContractInfo` RPC and normalise the result
#' into a tidy tibble.
#'
#' @inheritParams contract_call
#'
#' @return A tibble describing the contract and associated staking metadata.
#'
#' @examples
#' mirror <- hadeda_config(network = "testnet")
#' \dontrun{
#'   contract_info(mirror, contract_id = "0.0.9001")
#' }
#'
#' @export
contract_info <- function(config,
                          contract_id,
                          .transport = NULL) {
  transport <- hadeda_choose_transport(
    config,
    .transport,
    rest_supported = FALSE,
    grpc_supported = TRUE
  )
  if (!identical(transport, "grpc")) {
    cli::cli_abort("contract_info() currently supports only the gRPC transport.")
  }

  if (rlang::is_missing(contract_id) || is.null(contract_id) || !nzchar(as.character(contract_id)[[1]])) {
    cli::cli_abort("`contract_id` must be supplied when requesting contract info.")
  }

  response <- hadeda_grpc_contract_info(
    config = config,
    contract_id = contract_id
  )

  hadeda_parse_grpc_contract_info(response)
}

#' @keywords internal
hadeda_grpc_contract_info <- function(config, contract_id) {
  grpc <- hadeda_require_grpc(config)
  handler <- grpc$get_contract_info %||% grpc$contract_get_info %||% grpc$contractInfo %||%
    grpc$contract_info
  if (!rlang::is_function(handler)) {
    cli::cli_abort(
      "No gRPC SmartContractService info handler configured.\nProvide `config$grpc$get_contract_info` to enable info queries.",
      class = "hadeda_grpc_not_configured"
    )
  }

  handler(
    config = config,
    contract_id = contract_id
  )
}

#' Retrieve recent contract transaction records
#'
#' Invoke the SmartContractService `getContractRecords` RPC to fetch recent
#' transaction records associated with a contract.
#'
#' @inheritParams contract_call
#'
#' @return A tibble of transaction records.
#'
#' @examples
#' mirror <- hadeda_config(network = "testnet")
#' \dontrun{
#'   contract_records(mirror, contract_id = "0.0.9001")
#' }
#'
#' @export
contract_records <- function(config,
                             contract_id,
                             .transport = NULL) {
  transport <- hadeda_choose_transport(
    config,
    .transport,
    rest_supported = FALSE,
    grpc_supported = TRUE
  )
  if (!identical(transport, "grpc")) {
    cli::cli_abort("contract_records() currently supports only the gRPC transport.")
  }

  if (rlang::is_missing(contract_id) || is.null(contract_id) || !nzchar(as.character(contract_id)[[1]])) {
    cli::cli_abort("`contract_id` must be supplied when requesting contract records.")
  }

  response <- hadeda_grpc_contract_records(
    config = config,
    contract_id = contract_id
  )

  tbl <- hadeda_parse_grpc_transaction_records(response)
  tbl$contract_id <- rep(as.character(contract_id), nrow(tbl))
  tbl
}

#' @keywords internal
hadeda_grpc_contract_records <- function(config, contract_id) {
  grpc <- hadeda_require_grpc(config)
  handler <- grpc$get_contract_records %||% grpc$contract_get_records %||% grpc$contractRecords %||%
    grpc$contract_records
  if (!rlang::is_function(handler)) {
    cli::cli_abort(
      "No gRPC SmartContractService records handler configured.\nProvide `config$grpc$get_contract_records` to enable record queries.",
      class = "hadeda_grpc_not_configured"
    )
  }

  handler(
    config = config,
    contract_id = contract_id
  )
}

#' Retrieve a transaction record by contract and transaction ID
#'
#' Call the SmartContractService `getTxRecordByContractID` RPC which returns the
#' transaction record for a specific contract/transaction combination.
#'
#' @inheritParams contract_call
#' @param transaction_id Transaction identifier string.
#'
#' @return A tibble with a single transaction record.
#'
#' @examples
#' mirror <- hadeda_config(network = "testnet")
#' \dontrun{
#'   contract_tx_record_by_id(
#'     mirror,
#'     contract_id = "0.0.9001",
#'     transaction_id = "0.0.1234@1672531200.000000000"
#'   )
#' }
#'
#' @export
contract_tx_record_by_id <- function(config,
                                     contract_id,
                                     transaction_id,
                                     .transport = NULL) {
  transport <- hadeda_choose_transport(
    config,
    .transport,
    rest_supported = FALSE,
    grpc_supported = TRUE
  )
  if (!identical(transport, "grpc")) {
    cli::cli_abort("contract_tx_record_by_id() currently supports only the gRPC transport.")
  }

  if (rlang::is_missing(contract_id) || is.null(contract_id) || !nzchar(as.character(contract_id)[[1]])) {
    cli::cli_abort("`contract_id` must be supplied when requesting a contract transaction record.")
  }
  if (rlang::is_missing(transaction_id) || is.null(transaction_id) || !nzchar(as.character(transaction_id)[[1]])) {
    cli::cli_abort("`transaction_id` must be supplied when requesting a contract transaction record.")
  }

  response <- hadeda_grpc_contract_tx_record_by_id(
    config = config,
    contract_id = contract_id,
    transaction_id = transaction_id
  )

  record <- hadeda_parse_grpc_transaction_record(response)
  record$contract_id <- as.character(contract_id)
  record
}

#' @keywords internal
hadeda_grpc_contract_tx_record_by_id <- function(config,
                                                 contract_id,
                                                 transaction_id) {
  grpc <- hadeda_require_grpc(config)
  handler <- grpc$get_contract_tx_record_by_id %||% grpc$contract_get_tx_record_by_id %||%
    grpc$contractTxRecordById %||% grpc$get_tx_record_by_contract_id %||%
    grpc$contract_transaction_record_by_id
  if (!rlang::is_function(handler)) {
    cli::cli_abort(
      "No gRPC SmartContractService transaction record handler configured.\nProvide `config$grpc$get_contract_tx_record_by_id` to enable targeted record queries.",
      class = "hadeda_grpc_not_configured"
    )
  }

  handler(
    config = config,
    contract_id = contract_id,
    transaction_id = transaction_id
  )
}

#' @keywords internal
hadeda_normalise_contract_expiration <- function(expiration_time) {
  if (rlang::is_missing(expiration_time) || is.null(expiration_time)) {
    return(list(value = NULL, column = lubridate::as_datetime(NA_real_)))
  }

  if (inherits(expiration_time, "POSIXt")) {
    return(list(value = expiration_time, column = expiration_time))
  }

  if (is.numeric(expiration_time)) {
    dt <- lubridate::as_datetime(expiration_time)
    return(list(value = expiration_time, column = dt))
  }

  if (is.character(expiration_time)) {
    parsed <- hadeda_parse_timestamp(expiration_time)
    return(list(value = expiration_time, column = parsed))
  }

  cli::cli_abort("`expiration_time` must be NULL, POSIXct, numeric seconds, or a timestamp string.")
}
