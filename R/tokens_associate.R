#' Associate tokens with an account
#'
#' Submit a TokenService `associateTokens` transaction using the gRPC
#' transport. The helper normalises token identifiers into a character vector
#' before delegating to the configured gRPC handler.
#'
#' @param config Configuration created by [hadeda_config()].
#' @param account_id The Hedera account identifier to associate with the
#'   provided tokens.
#' @param token_ids Vector, list, or data frame column containing token
#'   identifiers to associate with the account.
#' @param memo Optional transaction memo forwarded to the gRPC handler.
#' @param max_fee Optional fee ceiling expressed in tinybars.
#' @param wait_for_receipt Logical indicating whether the gRPC handler should
#'   wait for a transaction receipt acknowledgement.
#' @param .transport Optional transport override. Only the gRPC transport is
#'   supported.
#'
#' @return A tibble containing acknowledgement metadata for the association
#'   transaction.
#'
#' @export
tokens_associate <- function(config,
                             account_id,
                             token_ids,
                             memo = NULL,
                             max_fee = NULL,
                             wait_for_receipt = TRUE,
                             .transport = NULL) {
  transport <- hadeda_choose_transport(
    config,
    .transport,
    rest_supported = FALSE,
    grpc_supported = TRUE
  )
  if (!identical(transport, "grpc")) {
    cli::cli_abort("tokens_associate() currently supports only the gRPC transport.")
  }

  if (rlang::is_missing(account_id) || is.null(account_id) || identical(account_id, "")) {
    cli::cli_abort("`account_id` must be supplied to associate tokens.")
  }

  if (rlang::is_missing(token_ids) || is.null(token_ids)) {
    cli::cli_abort("`token_ids` must include at least one token identifier.")
  }

  if (tibble::is_tibble(token_ids) || is.data.frame(token_ids)) {
    if (!"token_id" %in% names(token_ids)) {
      cli::cli_abort("Data frames supplied to `token_ids` must include a `token_id` column.")
    }
    token_ids <- token_ids$token_id
  }

  tokens <- as.character(unlist(token_ids, recursive = TRUE, use.names = FALSE))
  tokens <- tokens[!is.na(tokens) & tokens != ""]
  if (!length(tokens)) {
    cli::cli_abort("`token_ids` must include at least one non-empty token identifier.")
  }

  response <- hadeda_grpc_tokens_associate(
    config = config,
    account_id = account_id,
    token_ids = tokens,
    memo = memo,
    max_fee = max_fee,
    wait_for_receipt = wait_for_receipt
  )

  hadeda_parse_grpc_mutation_response(
    response,
    extra = list(
      account_id = as.character(account_id),
      token_ids = list(tokens)
    )
  )
}

#' @keywords internal
hadeda_grpc_tokens_associate <- function(config,
                                         account_id,
                                         token_ids,
                                         memo = NULL,
                                         max_fee = NULL,
                                         wait_for_receipt = TRUE) {
  grpc <- hadeda_require_grpc(config)
  handler <- grpc$token_associate %||% grpc$associate_tokens
  if (!rlang::is_function(handler)) {
    cli::cli_abort(
      "No gRPC TokenService associate handler configured.\nProvide `config$grpc$token_associate` to enable token associations.",
      class = "hadeda_grpc_not_configured"
    )
  }

  handler(
    config = config,
    account_id = account_id,
    token_ids = token_ids,
    memo = memo,
    max_fee = max_fee,
    wait_for_receipt = wait_for_receipt
  )
}
