#' Create a new Hedera token
#'
#' Submit a token creation request over the REST transport.
#'
#' @inheritParams tokens_balances
#' @param name The token name.
#' @param symbol The token symbol.
#' @param treasury_account_id The treasury account responsible for the token supply.
#' @param initial_supply Optional initial supply for fungible tokens.
#' @param decimals Optional number of decimals for fungible tokens.
#' @param token_type Token type, either "fungible_common" or "non_fungible_unique".
#' @param memo Optional token memo.
#' @param freeze_default Optional logical indicating the default freeze state.
#'
#' @return A tibble containing the newly created token identifier and metadata.
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
#' account <- accounts_create(hashio)
#' token <- tokens_create(
#'   hashio,
#'   name = "Hadeda Example",
#'   symbol = "HADEDA",
#'   treasury_account_id = account$account
#' )
#' token$token_id
#' }
#'
#' @export
tokens_create <- function(config,
                           name,
                           symbol,
                           treasury_account_id,
                           initial_supply = NULL,
                           decimals = NULL,
                           token_type = c("fungible_common", "non_fungible_unique"),
                           memo = NULL,
                           freeze_default = NULL,
                           .transport = NULL) {
  transport <- hadeda_choose_transport(config, .transport, rest_supported = TRUE, grpc_supported = FALSE)
  if (!identical(transport, "rest")) {
    cli::cli_abort("tokens_create() currently supports only the REST transport.")
  }

  if (missing(name) || missing(symbol) || missing(treasury_account_id)) {
    cli::cli_abort("name, symbol, and treasury_account_id are required to create a token.")
  }

  token_type <- rlang::arg_match(token_type)
  token_type_map <- c(
    fungible_common = "FUNGIBLE_COMMON",
    non_fungible_unique = "NON_FUNGIBLE_UNIQUE"
  )
  token_type <- token_type_map[[token_type]]

  body <- hadeda_drop_null(list(
    name = name,
    symbol = symbol,
    treasuryAccountId = treasury_account_id,
    initialSupply = initial_supply,
    decimals = decimals,
    tokenType = token_type,
    memo = memo,
    freezeDefault = freeze_default
  ))

  resp <- hadeda_rest_post(config, "tokens", body)

  token_id <- resp$tokenId %||% resp$token_id %||% resp$token
  if (is.null(token_id)) {
    cli::cli_abort("Token creation response did not include a token identifier.")
  }

  tibble::tibble(
    token_id = as.character(token_id),
    status = if (is.null(resp$status)) NA_character_ else as.character(resp$status),
    transaction_id = if (is.null(resp$transactionId) && is.null(resp$transaction_id)) {
      NA_character_
    } else {
      as.character(resp$transactionId %||% resp$transaction_id)
    },
    response = list(resp)
  )
}
