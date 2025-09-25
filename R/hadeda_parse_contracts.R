#' Parse contract metadata
#'
#' Transform raw contract records into a tibble.
#'
#' @param records A list of contract metadata entries.
#'
#' @return A tibble describing contracts.
#'
#' @examples
#' records <- list(list(
#'   contract_id = "0.0.5005",
#'   admin_key = "302a300506032b6570032100...",
#'   auto_renew_account_id = "0.0.1001",
#'   created_timestamp = "1700000000.000000000",
#'   memo = "Example contract"
#' ))
#' hadeda_parse_contracts(records)
#'
#' @keywords internal
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
    contract_id = purrr::map_chr(records, ~purrr::pluck(.x, "contract_id", .default = NA_character_)),
    admin_key = purrr::map_chr(records, ~purrr::pluck(.x, "admin_key", .default = NA_character_)),
    auto_renew_account_id = purrr::map_chr(records, ~purrr::pluck(.x, "auto_renew_account_id", .default = NA_character_)),
    created_timestamp = hadeda_parse_timestamp(purrr::map_chr(records, ~purrr::pluck(.x, "created_timestamp", .default = NA_character_))),
    memo = purrr::map_chr(records, ~purrr::pluck(.x, "memo", .default = NA_character_))
  )
}
