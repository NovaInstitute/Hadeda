#' Parse token allowance records
#'
#' Convert token allowance payloads from the Mirror Node into a tidy tibble.
#'
#' @param records A list of token allowance entries from the Mirror Node REST API.
#'
#' @return A tibble describing token allowance approvals.
#'
#' @examples
#' records <- list(list(
#'   owner = "0.0.1001",
#'   token_id = "0.0.2002",
#'   spender = "0.0.3003",
#'   amount_granted = 1000,
#'   amount = 250,
#'   timestamp = "1672531200.000000000",
#'   delegating_spender = "0.0.4004",
#'   payer_account_id = "0.0.5005",
#'   transaction_id = "0.0.5005-123456789-000000000"
#' ))
#' hadeda_parse_account_allowances_tokens(records)
#'
#' @keywords internal
hadeda_parse_account_allowances_tokens <- function(records) {
  if (length(records) == 0) {
    return(tibble::tibble(
      owner = character(),
      token_id = character(),
      spender = character(),
      amount_granted = numeric(),
      amount = numeric(),
      timestamp = hadeda_parse_timestamp(character()),
      delegating_spender = character(),
      payer_account_id = character(),
      transaction_id = character()
    ))
  }

  tibble::tibble(
    owner = purrr::map_chr(records, ~purrr::pluck(.x, "owner", .default = NA_character_)),
    token_id = purrr::map_chr(records, ~purrr::pluck(.x, "token_id", .default = NA_character_)),
    spender = purrr::map_chr(records, ~purrr::pluck(.x, "spender", .default = NA_character_)),
    amount_granted = purrr::map_dbl(records, ~as.numeric(purrr::pluck(.x, "amount_granted", .default = NA_real_))),
    amount = purrr::map_dbl(records, ~as.numeric(purrr::pluck(.x, "amount", .default = NA_real_))),
    timestamp = hadeda_parse_timestamp(purrr::map_chr(records, ~purrr::pluck(.x, "timestamp", .default = NA_character_))),
    delegating_spender = purrr::map_chr(records, ~purrr::pluck(.x, "delegating_spender", .default = NA_character_)),
    payer_account_id = purrr::map_chr(records, ~purrr::pluck(.x, "payer_account_id", .default = NA_character_)),
    transaction_id = purrr::map_chr(records, ~purrr::pluck(.x, "transaction_id", .default = NA_character_))
  )
}
