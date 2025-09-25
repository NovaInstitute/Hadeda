#' Create a Hedera account via the CryptoService
#'
#' Submit a CryptoService account creation transaction using either the gRPC
#' transport or, when explicitly requested, fall back to the REST
#' `accounts_create()` helper for mirror node providers such as Hashio.
#'
#' @inheritParams accounts_create
#' @param public_key Optional public key to register for the account. When
#'   omitted and the REST transport is selected the mirror node will generate
#'   new keys on your behalf.
#' @param auto_renew_period Optional auto-renew period expressed in seconds.
#' @param key_list Optional list describing a threshold or key list structure
#'   recognised by the underlying gRPC handler.
#' @param wait_for_record Logical indicating whether the gRPC handler should
#'   wait for a transaction record in addition to the receipt acknowledgement.
#'
#' @return A tibble containing the new account identifier alongside transaction
#'   metadata.
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
  transport <- hadeda_choose_transport(
    config,
    .transport,
    rest_supported = TRUE,
    grpc_supported = TRUE
  )

  key_type <- rlang::arg_match(key_type)

  if (identical(transport, "rest")) {
    if (!rlang::is_missing(public_key) && !is.null(public_key)) {
      cli::cli_abort("REST fallback cannot register an explicit `public_key`. Use the gRPC transport instead.")
    }

    rest_tbl <- accounts_create(
      config,
      initial_balance = initial_balance,
      alias = alias,
      key_type = key_type,
      memo = memo,
      .transport = "rest"
    )

    metadata <- purrr::pmap(
      rest_tbl[c("evm_address", "public_key", "private_key", "mnemonic")],
      function(evm_address, public_key, private_key, mnemonic) {
        list(
          evm_address = evm_address %||% NA_character_,
          public_key = public_key %||% NA_character_,
          private_key = private_key %||% NA_character_,
          mnemonic = mnemonic %||% character()
        )
      }
    )

    tibble::tibble(
      transaction_id = rep(NA_character_, nrow(rest_tbl)),
      status = rep(NA_character_, nrow(rest_tbl)),
      receipt_status = rep(NA_character_, nrow(rest_tbl)),
      consensus_timestamp = rep(lubridate::as_datetime(NA_real_), nrow(rest_tbl)),
      receipt = vector("list", nrow(rest_tbl)),
      response = rest_tbl$response,
      account_id = rest_tbl$account,
      metadata = metadata
    )
  } else {
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
