#' Parse account records from Mirror Node responses
#'
#' Transform raw account payloads from the Mirror Node into a tibble.
#'
#' @param records A list of account records as returned by the Mirror Node REST API.
#'
#' @return A tibble with account metadata.
#'
#' @examples
#' records <- list(list(
#'   account = "0.0.1001",
#'   balance = list(balance = 100L, timestamp = "1700000000.000000000"),
#'   deleted = FALSE,
#'   key = list(key = "302a300506032b6570032100...")
#' ))
#' hadeda_parse_accounts(records)
#'
#' @keywords internal
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
