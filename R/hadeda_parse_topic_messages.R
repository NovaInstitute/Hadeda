#' Parse topic message records
#'
#' Convert Mirror Node topic messages into a tibble.
#'
#' @param records A list of topic message records.
#'
#' @return A tibble describing topic messages.
#'
#' @examples
#' records <- list(list(
#'   topic_id = "0.0.2002",
#'   consensus_timestamp = "1700000000.000000001",
#'   message = "SGVsbG8=",
#'   running_hash = "abcd",
#'   sequence_number = 1
#' ))
#' hadeda_parse_topic_messages(records)
#'
#' @keywords internal
hadeda_parse_topic_messages <- function(records) {
  if (length(records) == 0) {
    return(tibble::tibble(
      topic_id = character(),
      consensus_timestamp = lubridate::as_datetime(numeric()),
      message = character(),
      running_hash = character(),
      sequence_number = numeric()
    ))
  }

  tibble::tibble(
    topic_id = purrr::map_chr(records, ~purrr::pluck(.x, "topic_id", .default = NA_character_)),
    consensus_timestamp = hadeda_parse_timestamp(purrr::map_chr(records, ~purrr::pluck(.x, "consensus_timestamp", .default = NA_character_))),
    message = purrr::map_chr(records, ~purrr::pluck(.x, "message", .default = NA_character_)),
    running_hash = purrr::map_chr(records, ~purrr::pluck(.x, "running_hash", .default = NA_character_)),
    sequence_number = purrr::map_dbl(records, ~purrr::pluck(.x, "sequence_number", .default = NA_real_))
  )
}
