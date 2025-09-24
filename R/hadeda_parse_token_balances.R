#' Parse token balance records
#'
#' Convert token balance payloads into a tidy tibble.
#'
#' @param records A list of token balance entries.
#'
#' @return A tibble of token balances.
#'
#' @examples
#' records <- list(list(account = "0.0.1001", balance = 100L, decimals = 8))
#' hadeda_parse_token_balances(records)
#'
#' @keywords internal
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
