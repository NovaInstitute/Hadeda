#' Parse crypto allowance records
#'
#' Convert crypto allowance payloads from the Mirror Node into a tidy tibble.
#'
#' @param records A list of allowance entries from the Mirror Node REST API.
#'
#' @return A tibble describing allowance approvals.
#'
#' @examples
#' records <- list(list(
#'   owner = "0.0.1001",
#'   spender = "0.0.2002",
#'   amount = 1000,
#'   timestamp = "1672531200.000000000",
#'   payer_account_id = "0.0.3003",
#'   transaction_id = "0.0.3003-123456789-000000000"
#' ))
#' hadeda_parse_account_allowances_crypto(records)
#'
#' @keywords internal
hadeda_parse_account_allowances_crypto <- function(records) {
  if (length(records) == 0) {
    return(tibble::tibble(
      owner = character(),
      spender = character(),
      amount = numeric(),
      timestamp = hadeda_parse_timestamp(character()),
      delegating_spender = character(),
      payer_account_id = character(),
      transaction_id = character()
    ))
  }

  tibble::tibble(
    owner = purrr::map_chr(records, ~purrr::pluck(.x, "owner", .default = NA_character_)),
    spender = purrr::map_chr(records, ~purrr::pluck(.x, "spender", .default = NA_character_)),
    amount = purrr::map_dbl(records, ~purrr::pluck(.x, "amount", .default = NA_real_)),
    timestamp = hadeda_parse_timestamp(purrr::map_chr(records, ~purrr::pluck(.x, "timestamp", .default = NA_character_))),
    delegating_spender = purrr::map_chr(records, ~purrr::pluck(.x, "delegating_spender", .default = NA_character_)),
    payer_account_id = purrr::map_chr(records, ~purrr::pluck(.x, "payer_account_id", .default = NA_character_)),
    transaction_id = purrr::map_chr(records, ~purrr::pluck(.x, "transaction_id", .default = NA_character_))
  )
}
