#' Parse transaction records
#'
#' Convert Mirror Node transaction payloads into a tibble.
#'
#' @param records A list of transaction records.
#'
#' @return A tibble describing transactions.
#'
#' @examples
#' records <- list(list(
#'   transaction_id = "0.0.1001-1700000000-000000000",
#'   consensus_timestamp = "1700000000.000000001",
#'   name = "CRYPTOTRANSFER",
#'   valid_start_timestamp = "1700000000.000000000",
#'   charged_tx_fee = 1000
#' ))
#' hadeda_parse_transactions(records)
#'
#' @keywords internal
hadeda_parse_transactions <- function(records) {
  if (length(records) == 0) {
    return(tibble::tibble(
      transaction_id = character(),
      consensus_timestamp = lubridate::as_datetime(numeric()),
      name = character(),
      valid_start = lubridate::as_datetime(numeric()),
      charged_tx_fee = numeric()
    ))
  }

  tibble::tibble(
    transaction_id = purrr::map_chr(records, "transaction_id", .default = NA_character_),
    consensus_timestamp = hadeda_parse_timestamp(purrr::map_chr(records, "consensus_timestamp", .default = NA_character_)),
    name = purrr::map_chr(records, "name", .default = NA_character_),
    valid_start = hadeda_parse_timestamp(purrr::map_chr(records, "valid_start_timestamp", .default = NA_character_)),
    charged_tx_fee = purrr::map_dbl(records, "charged_tx_fee", .default = NA_real_)
  )
}
