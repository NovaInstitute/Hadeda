#' Create a new Hedera token
#'
#' Submit a token creation request over the REST transport or issue a
#' TokenService `createToken` transaction via gRPC.
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
#' @param max_fee Optional fee ceiling expressed in tinybars when using the
#'   gRPC transport.
#' @param wait_for_receipt Logical indicating whether the gRPC handler should
#'   wait for a transaction receipt acknowledgement.
#'
#' @return A tibble containing the newly created token identifier and
#'   transaction metadata.
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
                           max_fee = NULL,
                           wait_for_receipt = TRUE,
                           .transport = NULL) {
  transport <- hadeda_choose_transport(
    config,
    .transport,
    rest_supported = TRUE,
    grpc_supported = TRUE
  )

  if (missing(name) || missing(symbol) || missing(treasury_account_id)) {
    cli::cli_abort("name, symbol, and treasury_account_id are required to create a token.")
  }

  token_type_choice <- rlang::arg_match(token_type)
  token_type_map <- c(
    fungible_common = "FUNGIBLE_COMMON",
    non_fungible_unique = "NON_FUNGIBLE_UNIQUE"
  )
  token_type_value <- token_type_map[[token_type_choice]]

  if (identical(transport, "rest")) {
    body <- hadeda_drop_null(list(
      name = name,
      symbol = symbol,
      treasuryAccountId = treasury_account_id,
      initialSupply = initial_supply,
      decimals = decimals,
      tokenType = token_type_value,
      memo = memo,
      freezeDefault = freeze_default
    ))

    resp <- hadeda_rest_post(config, "tokens", body)

    token_id <- resp$tokenId %||% resp$token_id %||% resp$token
    if (is.null(token_id)) {
      cli::cli_abort("Token creation response did not include a token identifier.")
    }

    wrapped <- list(
      transactionId = resp$transactionId %||% resp$transaction_id,
      status = resp$status %||% NULL
    )
    tbl <- hadeda_parse_grpc_mutation_response(
      wrapped,
      extra = list(token_id = as.character(token_id))
    )
    tbl$response <- list(resp)
    tbl
  } else {
    response <- hadeda_grpc_tokens_create(
      config = config,
      name = name,
      symbol = symbol,
      treasury_account_id = treasury_account_id,
      initial_supply = initial_supply,
      decimals = decimals,
      token_type = token_type_value,
      memo = memo,
      freeze_default = freeze_default,
      max_fee = max_fee,
      wait_for_receipt = wait_for_receipt
    )

    receipt <- response$receipt %||% response$transaction_receipt %||% response$transactionReceipt %||% list()
    token_id <- receipt$token_id %||% receipt$tokenId %||% receipt$tokenID %||%
      response$token_id %||% response$tokenId %||% response$tokenID

    hadeda_parse_grpc_mutation_response(
      response,
      extra = list(
        token_id = if (is.null(token_id)) NA_character_ else as.character(token_id)
      )
    )
  }
}

#' @keywords internal
hadeda_grpc_tokens_create <- function(config,
                                      name,
                                      symbol,
                                      treasury_account_id,
                                      initial_supply = NULL,
                                      decimals = NULL,
                                      token_type = NULL,
                                      memo = NULL,
                                      freeze_default = NULL,
                                      max_fee = NULL,
                                      wait_for_receipt = TRUE) {
  grpc <- hadeda_require_grpc(config)
  handler <- grpc$token_create %||% grpc$create_token
  if (!rlang::is_function(handler)) {
    cli::cli_abort(
      "No gRPC TokenService create handler configured.\nProvide `config$grpc$token_create` to enable token creation.",
      class = "hadeda_grpc_not_configured"
    )
  }

  handler(
    config = config,
    name = name,
    symbol = symbol,
    treasury_account_id = treasury_account_id,
    initial_supply = initial_supply,
    decimals = decimals,
    token_type = token_type,
    memo = memo,
    freeze_default = freeze_default,
    max_fee = max_fee,
    wait_for_receipt = wait_for_receipt
  )
}
