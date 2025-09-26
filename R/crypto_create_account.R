#' Create a Hedera account via the CryptoService
#'
#' Submit a CryptoService account creation transaction using the gRPC transport.
#' Mirror node REST APIs are read-only for accounts, so this helper delegates to
#' a user-supplied gRPC handler that signs and submits the transaction.
#'
#' @inheritParams accounts_list
#' @param public_key Public key to register for the account. Some gRPC handlers
#'   can derive a default from the operator configuration, but supplying a key
#'   explicitly is the most portable option.
#' @param initial_balance Optional initial balance in tinybars.
#' @param alias Optional alias string for the new account.
#' @param key_type The key type to generate. Either "ED25519" or "ECDSA".
#' @param memo Optional memo associated with the account.
#' @param auto_renew_period Optional auto-renew period expressed in seconds.
#' @param key_list Optional list describing a threshold or key list structure
#'   recognised by the underlying gRPC handler.
#' @param wait_for_record Logical indicating whether the gRPC handler should
#'   wait for a transaction record in addition to the receipt acknowledgement.
#'
#' @return A tibble containing the new account identifier alongside transaction
#'   metadata.
#'
#' @examples
#' grpc_config <- hadeda_config(
#'   network = "testnet",
#'   grpc = list(
#'     create_account = function(config,
#'                                public_key,
#'                                initial_balance,
#'                                alias,
#'                                key_type,
#'                                memo,
#'                                auto_renew_period,
#'                                key_list,
#'                                wait_for_record) {
#'       # Delegate to an SDK client here. This stub illustrates the
#'       # expected shape of the return value.
#'       list(
#'         transaction_id = "0.0.5005-1700000000-000000000",
#'         status = "OK",
#'         receipt = list(status = "SUCCESS", accountId = "0.0.5005"),
#'         accountInfo = list(public_key = public_key)
#'       )
#'     }
#'   ),
#'   default_transport = "grpc"
#' )
#'
#' \dontrun{
#'   new_account <- crypto_create_account(
#'     grpc_config,
#'     public_key = "302a300506032b6570032100...",
#'     initial_balance = 10,
#'     memo = "Example account"
#'   )
#'   new_account$account_id
#' }
#'
#' @export
crypto_create_account <- function(config,
                                   public_key = NULL,
                                   initial_balance = NULL,
                                   alias = NULL,
                                   key_type = c("ED25519", "ECDSA"),
                                   memo = NULL,
                                   auto_renew_period = NULL,
                                   key_list = NULL,
                                   wait_for_record = FALSE,
                                   .transport = NULL) {
  hadeda_choose_transport(
    config,
    .transport,
    rest_supported = FALSE,
    grpc_supported = TRUE
  )

  key_type <- rlang::arg_match(key_type)

  response <- hadeda_grpc_crypto_create_account(
    config = config,
    public_key = public_key,
    initial_balance = initial_balance,
    alias = alias,
    key_type = key_type,
    memo = memo,
    auto_renew_period = auto_renew_period,
    key_list = key_list,
    wait_for_record = wait_for_record
  )

  receipt <- response$receipt %||% response$transaction_receipt %||% response$transactionReceipt %||% list()
  account_id <- receipt$account_id %||% receipt$accountId %||% receipt$accountID %||%
    response$account_id %||% response$accountId %||% response$accountID

  metadata <- response$account_info %||% response$accountInfo %||% list()

  hadeda_parse_grpc_mutation_response(
    response,
    extra = list(
      account_id = if (is.null(account_id)) NA_character_ else as.character(account_id),
      metadata = list(metadata)
    )
  )
}

#' @keywords internal
hadeda_grpc_crypto_create_account <- function(config,
                                              public_key = NULL,
                                              initial_balance = NULL,
                                              alias = NULL,
                                              key_type = NULL,
                                              memo = NULL,
                                              auto_renew_period = NULL,
                                              key_list = NULL,
                                              wait_for_record = FALSE) {
  grpc <- hadeda_require_grpc(config)
  handler <- grpc$create_account %||% grpc$crypto_create_account
  if (!rlang::is_function(handler)) {
    cli::cli_abort(
      "No gRPC CryptoService account creation handler configured.\nProvide `config$grpc$create_account` to enable account creation.",
      class = "hadeda_grpc_not_configured"
    )
  }

  handler(
    config = config,
    public_key = public_key,
    initial_balance = initial_balance,
    alias = alias,
    key_type = key_type,
    memo = memo,
    auto_renew_period = auto_renew_period,
    key_list = key_list,
    wait_for_record = wait_for_record
  )
}
