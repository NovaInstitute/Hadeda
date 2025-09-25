#' Parse token NFT records
#'
#' Transform raw NFT records from the Mirror Node API into a tidy tibble.
#'
#' @param records A list of NFT objects as returned by the Mirror Node REST API.
#'
#' @return A tibble describing token NFTs.
#'
#' @examples
#' records <- list(list(
#'   token_id = "0.0.1001",
#'   serial_number = 1,
#'   account_id = "0.0.2002",
#'   created_timestamp = "1700000000.000000000",
#'   metadata = "aGVsbG8="
#' ))
#' hadeda_parse_token_nfts(records)
#'
#' @keywords internal
hadeda_parse_token_nfts <- function(records) {
  if (length(records) == 0) {
    return(tibble::tibble(
      token_id = character(),
      serial_number = integer(),
      account_id = character(),
      created_timestamp = lubridate::as_datetime(numeric()),
      modified_timestamp = lubridate::as_datetime(numeric()),
      metadata = character(),
      spender_id = character()
    ))
  }

  created <- purrr::map_chr(records, ~purrr::pluck(.x, "created_timestamp", .default = NA_character_))
  modified <- purrr::map_chr(records, ~purrr::pluck(.x, "modified_timestamp", .default = NA_character_))

  tibble::tibble(
    token_id = purrr::map_chr(records, ~purrr::pluck(.x, "token_id", .default = NA_character_)),
    serial_number = purrr::map_int(records, function(record) {
      value <- record$serial_number %||% record$serialNumber %||% NA_integer_
      as.integer(value)
    }),
    account_id = purrr::map_chr(records, function(record) {
      record$account_id %||% record$accountId %||% NA_character_
    }),
    created_timestamp = hadeda_parse_timestamp(created),
    modified_timestamp = hadeda_parse_timestamp(modified),
    metadata = purrr::map_chr(records, function(record) {
      record$metadata %||% NA_character_
    }),
    spender_id = purrr::map_chr(records, function(record) {
      record$spender_id %||% record$spenderId %||% NA_character_
    })
  )
}
