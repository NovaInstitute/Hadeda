#' Execute a Hedera smart contract call
#'
#' Submit a SmartContractService `contractCallMethod` transaction via the gRPC
#' transport. The helper normalises call data, supports optional ABI encoding of
#' function parameters, and returns a tidy acknowledgement tibble.
#'
#' @param config Configuration created by [hadeda_config()].
#' @param contract_id Target contract identifier.
#' @param function_signature Canonical function signature used when encoding
#'   parameters (for example `"transfer(address,uint256)"`).
#' @param parameters Optional list or atomic vector of parameters supplied when
#'   `function_signature` is provided.
#' @param call_data Optional hexadecimal string containing pre-encoded call
#'   data. When supplied `function_signature` and `parameters` are ignored.
#' @param gas Optional gas limit forwarded to the gRPC handler.
#' @param sender_account_id Optional account submitting the transaction.
#' @param payable_amount Optional hbar amount forwarded as the transaction
#'   payment (expressed in tinybars).
#' @param max_fee Optional fee ceiling expressed in tinybars.
#' @param wait_for_record Logical indicating whether the gRPC handler should
#'   wait for a transaction record in addition to the receipt acknowledgement.
#' @param .transport Optional transport override. Only the gRPC transport is
#'   supported.
#'
#' @return A tibble summarising the transaction metadata and call result.
#'
#' @details When automatic ABI encoding is requested Hadeda computes the
#'   function selector using Keccak-256. If the hashing backend does not support
#'   Keccak-256 the function aborts with guidance to provide pre-encoded
#'   `call_data`.
#'
#' @export
contract_call <- function(config,
                          contract_id,
                          function_signature = NULL,
                          parameters = list(),
                          call_data = NULL,
                          gas = NULL,
                          sender_account_id = NULL,
                          payable_amount = NULL,
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
    cli::cli_abort("contract_call() currently supports only the gRPC transport.")
  }

  if (rlang::is_missing(contract_id) || is.null(contract_id) || !nzchar(as.character(contract_id)[[1]])) {
    cli::cli_abort("`contract_id` must be supplied when calling a contract.")
  }

  if (is.null(call_data)) {
    if (rlang::is_missing(function_signature) || is.null(function_signature)) {
      cli::cli_abort("Provide either `call_data` or `function_signature`.")
    }
    call_data <- contract_encode_parameters(function_signature, parameters)
  } else {
    call_data <- hadeda_contract_normalise_hex(call_data)
  }

  response <- hadeda_grpc_contract_call(
    config = config,
    contract_id = contract_id,
    gas = gas,
    call_data = call_data,
    sender_account_id = sender_account_id,
    payable_amount = payable_amount,
    max_fee = max_fee,
    wait_for_record = wait_for_record
  )

  tbl <- hadeda_parse_grpc_mutation_response(
    response,
    extra = list(
      contract_id = as.character(contract_id),
      function_signature = if (is.null(function_signature)) NA_character_ else as.character(function_signature),
      call_data = call_data,
      gas = if (is.null(gas)) NA_real_ else as.numeric(gas),
      sender_account_id = if (is.null(sender_account_id)) NA_character_ else as.character(sender_account_id),
      payable_amount = list(payable_amount)
    )
  )

  call_result <- response$call_result %||% response$result %||%
    response$contract_call_result %||% response$contractCallResult %||%
    response$record$contract_call_result %||% response$record$contractCallResult %||%
    list()

  return_data <- call_result$return_data %||% call_result$returnData %||% call_result$result %||% NULL
  gas_used <- call_result$gas_used %||% call_result$gasUsed %||% NA_real_

  tbl$call_result <- list(call_result)
  tbl$return_data <- list(return_data)
  tbl$gas_used <- if (is.null(gas_used)) NA_real_ else as.numeric(gas_used)

  tbl
}

#' @keywords internal
hadeda_grpc_contract_call <- function(config,
                                       contract_id,
                                       gas = NULL,
                                       call_data,
                                       sender_account_id = NULL,
                                       payable_amount = NULL,
                                       max_fee = NULL,
                                       wait_for_record = FALSE) {
  grpc <- hadeda_require_grpc(config)
  handler <- grpc$contract_call %||% grpc$contract_call_method %||% grpc$contractCall
  if (!rlang::is_function(handler)) {
    cli::cli_abort(
      "No gRPC SmartContractService call handler configured.\nProvide `config$grpc$contract_call` to enable contract execution.",
      class = "hadeda_grpc_not_configured"
    )
  }

  handler(
    config = config,
    contract_id = contract_id,
    gas = gas,
    call_data = call_data,
    sender_account_id = sender_account_id,
    payable_amount = payable_amount,
    max_fee = max_fee,
    wait_for_record = wait_for_record
  )
}
