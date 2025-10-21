#' Deploy a smart contract via gRPC
#'
#' Submit a SmartContractService `createContract` transaction using the gRPC
#' transport. Callers can supply raw bytecode or a bytecode file identifier and
#' optionally encode constructor parameters using
#' [contract_encode_parameters()].
#'
#' @param config Configuration created by [hadeda_config()].
#' @param bytecode Contract bytecode supplied as a hexadecimal string or raw
#'   vector. Provide either `bytecode` or `bytecode_file_id`.
#' @param bytecode_file_id Optional file identifier referencing uploaded
#'   bytecode on the Hedera network.
#' @param constructor_signature Optional constructor parameter signature used
#'   when encoding `constructor_args`.
#' @param constructor_args Optional constructor arguments paired with
#'   `constructor_signature`.
#' @param gas Optional gas limit forwarded to the gRPC handler.
#' @param initial_balance Optional initial balance to send to the contract in
#'   tinybars.
#' @param admin_key Optional admin key definition accepted by the gRPC handler.
#' @param auto_renew_account Optional auto-renew account identifier.
#' @param auto_renew_period Optional auto-renew period expressed in seconds.
#' @param memo Optional transaction memo.
#' @param max_fee Optional fee ceiling expressed in tinybars.
#' @param wait_for_record Logical indicating whether the gRPC handler should
#'   wait for a transaction record acknowledgement.
#' @param .transport Optional transport override. Only the gRPC transport is
#'   supported.
#'
#' @return A tibble summarising the deployment transaction metadata.
#'
#' @examples
#' mirror <- hadeda_config(network = "testnet")
#' bytecode <- "608060405234801561001057600080fd5b..."
#' \dontrun{
#'   contract_deploy(mirror, bytecode = bytecode, gas = 2e5)
#' }
#'
#' @export
contract_deploy <- function(config,
                            bytecode = NULL,
                            bytecode_file_id = NULL,
                            constructor_signature = NULL,
                            constructor_args = list(),
                            gas = NULL,
                            initial_balance = NULL,
                            admin_key = NULL,
                            auto_renew_account = NULL,
                            auto_renew_period = NULL,
                            memo = NULL,
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
    cli::cli_abort("contract_deploy() currently supports only the gRPC transport.")
  }

  if (is.null(bytecode) && is.null(bytecode_file_id)) {
    cli::cli_abort("Provide either `bytecode` or `bytecode_file_id` when deploying a contract.")
  }

  if (!is.null(bytecode) && !is.null(bytecode_file_id)) {
    cli::cli_abort("Supply only one of `bytecode` or `bytecode_file_id`, not both.")
  }

  bytecode_hex <- if (!is.null(bytecode)) hadeda_contract_normalise_hex(bytecode) else NULL

  constructor_parameters <- NULL
  if (!rlang::is_missing(constructor_signature) && !is.null(constructor_signature)) {
    constructor_parameters <- contract_encode_parameters(
      constructor_signature,
      constructor_args,
      include_selector = FALSE
    )
  } else if (!is.null(constructor_args) && length(constructor_args)) {
    cli::cli_abort("`constructor_args` requires a corresponding `constructor_signature`.")
  }

  response <- hadeda_grpc_contract_deploy(
    config = config,
    bytecode = bytecode_hex,
    bytecode_file_id = bytecode_file_id,
    constructor_parameters = constructor_parameters,
    gas = gas,
    initial_balance = initial_balance,
    admin_key = admin_key,
    auto_renew_account = auto_renew_account,
    auto_renew_period = auto_renew_period,
    memo = memo,
    max_fee = max_fee,
    wait_for_record = wait_for_record
  )

  receipt <- response$receipt %||% response$transaction_receipt %||% response$transactionReceipt %||% list()
  contract_id <- receipt$contract_id %||% receipt$contractId %||% receipt$contractID %||%
    response$contract_id %||% response$contractId %||% response$contractID

  tbl <- hadeda_parse_grpc_mutation_response(
    response,
    extra = list(
      contract_id = if (is.null(contract_id)) NA_character_ else as.character(contract_id),
      bytecode = list(bytecode_hex),
      bytecode_file_id = if (is.null(bytecode_file_id)) NA_character_ else as.character(bytecode_file_id),
      constructor_parameters = list(constructor_parameters)
    )
  )

  tbl$gas <- if (is.null(gas)) NA_real_ else as.numeric(gas)
  tbl$initial_balance <- list(initial_balance)
  tbl$admin_key <- list(admin_key)
  tbl$auto_renew_account <- if (is.null(auto_renew_account)) NA_character_ else as.character(auto_renew_account)
  tbl$auto_renew_period <- list(auto_renew_period)
  tbl$memo <- if (is.null(memo)) NA_character_ else as.character(memo)

  tbl
}

#' @keywords internal
hadeda_grpc_contract_deploy <- function(config,
                                        bytecode = NULL,
                                        bytecode_file_id = NULL,
                                        constructor_parameters = NULL,
                                        gas = NULL,
                                        initial_balance = NULL,
                                        admin_key = NULL,
                                        auto_renew_account = NULL,
                                        auto_renew_period = NULL,
                                        memo = NULL,
                                        max_fee = NULL,
                                        wait_for_record = FALSE) {
  grpc <- hadeda_require_grpc(config)
  handler <- grpc$contract_deploy %||% grpc$create_contract %||% grpc$contract_create
  if (!rlang::is_function(handler)) {
    cli::cli_abort(
      "No gRPC SmartContractService deploy handler configured.\nProvide `config$grpc$contract_deploy` to enable contract deployment.",
      class = "hadeda_grpc_not_configured"
    )
  }

  handler(
    config = config,
    bytecode = bytecode,
    bytecode_file_id = bytecode_file_id,
    constructor_parameters = constructor_parameters,
    gas = gas,
    initial_balance = initial_balance,
    admin_key = admin_key,
    auto_renew_account = auto_renew_account,
    auto_renew_period = auto_renew_period,
    memo = memo,
    max_fee = max_fee,
    wait_for_record = wait_for_record
  )
}
