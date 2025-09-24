#' Retrieve token metadata
#'
#' @param config A configuration list from [hadeda_config()].
#' @param token_id Token identifier.
#' @param .transport Optional transport override.
#'
#' @return A tibble describing the token.
#' @export
tokens_get <- function(config, token_id, .transport = NULL) {
  transport <- hadeda_choose_transport(config, .transport, rest_supported = TRUE, grpc_supported = FALSE)
  if (!identical(transport, "rest")) {
    cli::cli_abort("tokens_get() currently supports only the REST transport.")
  }

  resp <- hadeda_rest_get(config, paste0("tokens/", token_id))
  hadeda_parse_tokens(list(resp))
}

#' Retrieve token balance information
#'
#' @inheritParams tokens_get
#' @param limit Optional page size.
#' @param account_id Optional filter restricting balances to a specific account.
#'
#' @return A tibble of token balances.
#' @export
tokens_balances <- function(config,
                            token_id,
                            limit = NULL,
                            account_id = NULL,
                            order = c("desc", "asc"),
                            .transport = NULL) {
  transport <- hadeda_choose_transport(config, .transport, rest_supported = TRUE, grpc_supported = FALSE)
  if (!identical(transport, "rest")) {
    cli::cli_abort("tokens_balances() currently supports only the REST transport.")
  }

  query <- hadeda_drop_null(list(
    limit = limit,
    account.id = account_id,
    order = match.arg(order)
  ))

  responses <- hadeda_rest_paginate(config, paste0("tokens/", token_id, "/balances"), query)
  records <- purrr::map(responses, "balances")
  records <- purrr::flatten(records)
  hadeda_parse_token_balances(records)
}

#' Retrieve contract information
#'
#' @param config A configuration list from [hadeda_config()].
#' @param contract_id Contract identifier.
#' @param .transport Optional transport override.
#'
#' @return A tibble describing the contract.
#' @export
contracts_get <- function(config, contract_id, .transport = NULL) {
  transport <- hadeda_choose_transport(config, .transport, rest_supported = TRUE, grpc_supported = FALSE)
  if (!identical(transport, "rest")) {
    cli::cli_abort("contracts_get() currently supports only the REST transport.")
  }

  resp <- hadeda_rest_get(config, paste0("contracts/", contract_id))
  hadeda_parse_contracts(list(resp))
}

#' Retrieve smart contract bytecode
#'
#' @inheritParams contracts_get
#'
#' @return A list with the contract identifier and bytecode.
#' @export
contracts_bytecode <- function(config, contract_id, .transport = NULL) {
  transport <- hadeda_choose_transport(config, .transport, rest_supported = TRUE, grpc_supported = FALSE)
  if (!identical(transport, "rest")) {
    cli::cli_abort("contracts_bytecode() currently supports only the REST transport.")
  }

  resp <- hadeda_rest_get(config, paste0("contracts/", contract_id, "/bytecode"))
  tibble::tibble(
    contract_id = contract_id,
    bytecode = resp$bytecode %||% NA_character_
  )
}

hadeda_parse_tokens <- function(records) {
  if (length(records) == 0) {
    return(tibble::tibble(
      token_id = character(),
      symbol = character(),
      name = character(),
      treasury_account_id = character(),
      type = character()
    ))
  }

  tibble::tibble(
    token_id = purrr::map_chr(records, "token_id", .default = NA_character_),
    symbol = purrr::map_chr(records, "symbol", .default = NA_character_),
    name = purrr::map_chr(records, "name", .default = NA_character_),
    treasury_account_id = purrr::map_chr(records, "treasury_account_id", .default = NA_character_),
    type = purrr::map_chr(records, "type", .default = NA_character_)
  )
}

hadeda_parse_token_balances <- function(records) {
  if (length(records) == 0) {
    return(tibble::tibble(
      account = character(),
      balance = numeric(),
      decimals = numeric()
    ))
  }

  tibble::tibble(
    account = purrr::map_chr(records, "account", .default = NA_character_),
    balance = purrr::map_dbl(records, "balance", .default = NA_real_),
    decimals = purrr::map_dbl(records, "decimals", .default = NA_real_)
  )
}

hadeda_parse_contracts <- function(records) {
  if (length(records) == 0) {
    return(tibble::tibble(
      contract_id = character(),
      admin_key = character(),
      auto_renew_account_id = character(),
      created_timestamp = lubridate::as_datetime(numeric()),
      memo = character()
    ))
  }

  tibble::tibble(
    contract_id = purrr::map_chr(records, "contract_id", .default = NA_character_),
    admin_key = purrr::map_chr(records, "admin_key", .default = NA_character_),
    auto_renew_account_id = purrr::map_chr(records, "auto_renew_account_id", .default = NA_character_),
    created_timestamp = hadeda_parse_timestamp(purrr::map_chr(records, "created_timestamp", .default = NA_character_)),
    memo = purrr::map_chr(records, "memo", .default = NA_character_)
  )
}
