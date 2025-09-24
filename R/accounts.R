#' List Hedera accounts
#'
#' @param config A configuration list created by [hadeda_config()].
#' @param limit Optional page size for the Mirror Node request.
#' @param account_id Optional account ID filter.
#' @param public_key Optional public key filter.
#' @param balance_min,balance_max Optional balance range filters (tinybars).
#' @param order Result ordering, either "asc" or "desc".
#' @param .transport Optional transport override ("rest" or "grpc").
#'
#' @return A tibble of accounts with balance metadata.
#' @export
accounts_list <- function(config,
                          limit = NULL,
                          account_id = NULL,
                          public_key = NULL,
                          balance_min = NULL,
                          balance_max = NULL,
                          order = c("desc", "asc"),
                          .transport = NULL) {
  transport <- hadeda_choose_transport(config, .transport, rest_supported = TRUE, grpc_supported = FALSE)
  if (!identical(transport, "rest")) {
    cli::cli_abort("accounts_list() currently supports only the REST transport.")
  }

  query <- hadeda_drop_null(list(
    limit = limit,
    account.id = account_id,
    publickey = public_key,
    balance.min = balance_min,
    balance.max = balance_max,
    order = match.arg(order)
  ))

  responses <- hadeda_rest_paginate(config, "accounts", query)
  records <- purrr::map(responses, "accounts")
  records <- purrr::flatten(records)
  hadeda_parse_accounts(records)
}

#' Retrieve a single Hedera account
#'
#' @inheritParams accounts_list
#' @param account_id The account identifier to retrieve.
#'
#' @return A tibble with a single row describing the account.
#' @export
accounts_get <- function(config, account_id, .transport = NULL) {
  transport <- hadeda_choose_transport(config, .transport, rest_supported = TRUE, grpc_supported = FALSE)
  if (!identical(transport, "rest")) {
    cli::cli_abort("accounts_get() currently supports only the REST transport.")
  }

  resp <- hadeda_rest_get(config, paste0("accounts/", account_id))
  hadeda_parse_accounts(list(resp))
}

#' Retrieve the balance for an account
#'
#' @inheritParams accounts_list
#' @param account_id The account identifier to retrieve.
#' @param timestamp Optional consensus timestamp filter.
#'
#' @return A tibble with balance and token holdings.
#' @export
accounts_balance <- function(config, account_id, timestamp = NULL, .transport = NULL) {
  transport <- hadeda_choose_transport(config, .transport, rest_supported = TRUE, grpc_supported = FALSE)
  if (!identical(transport, "rest")) {
    cli::cli_abort("accounts_balance() currently supports only the REST transport.")
  }

  query <- hadeda_drop_null(list(timestamp = timestamp))
  resp <- hadeda_rest_get(config, paste0("accounts/", account_id, "/balance"), query)
  hadeda_parse_account_balances(resp)
}

hadeda_parse_accounts <- function(records) {
  if (length(records) == 0) {
    return(tibble::tibble(
      account = character(),
      balance = numeric(),
      timestamp = lubridate::as_datetime(numeric()),
      deleted = logical(),
      public_key = character()
    ))
  }

  tibble::tibble(
    account = purrr::map_chr(records, "account", .default = NA_character_),
    balance = purrr::map_dbl(records, c("balance", "balance"), .default = NA_real_),
    timestamp = hadeda_parse_timestamp(purrr::map_chr(records, c("balance", "timestamp"), .default = NA_character_)),
    deleted = purrr::map_lgl(records, "deleted", .default = NA),
    public_key = purrr::map_chr(records, c("key", "key"), .default = NA_character_)
  )
}

hadeda_parse_account_balances <- function(resp) {
  balances <- resp$balances %||% list()
  if (length(balances) == 0) {
    return(tibble::tibble(
      account = character(),
      balance = numeric(),
      timestamp = lubridate::as_datetime(numeric()),
      tokens = list()
    ))
  }

  tibble::tibble(
    account = purrr::map_chr(balances, "account", .default = NA_character_),
    balance = purrr::map_dbl(balances, "balance", .default = NA_real_),
    timestamp = hadeda_parse_timestamp(rep(resp$timestamp %||% NA_character_, length(balances))),
    tokens = purrr::map(balances, ~ .x$tokens %||% list())
  )
}
