#' Parse account balance responses
#'
#' Convert balance payloads returned from `accounts/{id}/balance` to a tibble.
#'
#' @param resp A list response from [hadeda_rest_get()] for the account balance endpoint.
#'
#' @return A tibble of balances by account.
#'
#' @examples
#' resp <- list(
#'   timestamp = "1700000000.000000000",
#'   balances = list(list(account = "0.0.1001", balance = 100L, tokens = list()))
#' )
#' hadeda_parse_account_balances(resp)
#'
#' @keywords internal
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
    account = purrr::map_chr(balances, ~purrr::pluck(.x, "account", .default = NA_character_)),
    balance = purrr::map_dbl(balances, ~purrr::pluck(.x, "balance", .default = NA_real_)),
    timestamp = hadeda_parse_timestamp(rep(resp$timestamp %||% NA_character_, length(balances))),
    tokens = purrr::map(balances, ~ .x$tokens %||% list())
  )
}
