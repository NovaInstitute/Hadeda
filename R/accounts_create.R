#' Create a new Hedera account
#'
#' Submit an account creation request via a REST transport such as Hashio.
#'
#' @inheritParams accounts_list
#' @param initial_balance Optional initial balance in tinybars.
#' @param alias Optional alias string for the new account.
#' @param key_type The key type to generate. Either "ED25519" or "ECDSA".
#' @param memo Optional memo associated with the account.
#'
#' @return A tibble containing the newly created account identifier and metadata.
#'
#' @examples
#' hashio <- hadeda_config(
#'   network = "testnet",
#'   rest = list(
#'     base_url = "https://testnet.hashio.io/api/v1",
#'     headers = list(`X-API-Key` = Sys.getenv("HASHIO_API_KEY"))
#'   ),
#'   default_transport = "rest"
#' )
#' \dontrun{
#' new_account <- accounts_create(hashio, initial_balance = 10)
#' new_account$account
#' }
#'
#' @export
accounts_create <- function(config,
                             initial_balance = NULL,
                             alias = NULL,
                             key_type = c("ED25519", "ECDSA"),
                             memo = NULL,
                             .transport = NULL) {
  transport <- hadeda_choose_transport(config, .transport, rest_supported = TRUE, grpc_supported = FALSE)
  if (!identical(transport, "rest")) {
    cli::cli_abort("accounts_create() currently supports only the REST transport.")
  }

  key_type <- rlang::arg_match(key_type)

  body <- hadeda_drop_null(list(
    initialBalance = initial_balance,
    alias = alias,
    keyType = key_type,
    memo = memo
  ))

  resp <- hadeda_rest_post(config, "accounts", body)

  account_id <- resp$accountId %||% resp$account_id %||% resp$account
  if (is.null(account_id)) {
    cli::cli_abort("Account creation response did not include an account identifier.")
  }

  evm_address <- resp$evmAddress %||% resp$evm_address
  public_key <- resp$publicKey %||% resp$public_key
  private_key <- resp$privateKey %||% resp$private_key
  mnemonic <- resp$mnemonic %||% resp$mnemonic

  tibble::tibble(
    account = as.character(account_id),
    evm_address = if (is.null(evm_address)) NA_character_ else as.character(evm_address),
    public_key = if (is.null(public_key)) NA_character_ else as.character(public_key),
    private_key = if (is.null(private_key)) NA_character_ else as.character(private_key),
    mnemonic = list(if (is.null(mnemonic)) character() else as.character(mnemonic)),
    response = list(resp)
  )
}
