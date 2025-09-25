#' Parse block summaries
#'
#' Transform raw block records from the Mirror Node API into a tibble.
#'
#' @param records A list of block objects as returned by the Mirror Node REST API.
#'
#' @return A tibble describing blocks.
#'
#' @examples
#' records <- list(list(
#'   number = 1000L,
#'   hash = "0xabc",
#'   previous_hash = "0xdef",
#'   count = 25,
#'   start_consensus_timestamp = "1700000000.000000000",
#'   end_consensus_timestamp = "1700000001.000000000"
#' ))
#' hadeda_parse_blocks(records)
#'
#' @keywords internal
hadeda_parse_blocks <- function(records) {
  if (length(records) == 0) {
    return(tibble::tibble(
      number = integer(),
      hash = character(),
      previous_hash = character(),
      count = integer(),
      gas_used = numeric(),
      logs_bloom = character(),
      hapi_version = character(),
      start_timestamp = lubridate::as_datetime(numeric()),
      end_timestamp = lubridate::as_datetime(numeric()),
      transactions = list()
    ))
  }

  start_ts <- purrr::map_chr(
    records,
    function(record) {
      record$start_consensus_timestamp %||%
        record$start_timestamp %||%
        record$from %||%
        NA_character_
    },
    .default = NA_character_
  )

  end_ts <- purrr::map_chr(
    records,
    function(record) {
      record$end_consensus_timestamp %||%
        record$end_timestamp %||%
        record$to %||%
        NA_character_
    },
    .default = NA_character_
  )

  tibble::tibble(
    number = purrr::map_int(records, function(record) {
      value <- record$number %||% record$block_number %||% NA_integer_
      as.integer(value)
    }),
    hash = purrr::map_chr(records, function(record) {
      value <- record$hash %||% record$block_hash %||% NA_character_
      as.character(value)
    }),
    previous_hash = purrr::map_chr(records, function(record) {
      value <- record$previous_hash %||% record$prev_hash %||% NA_character_
      as.character(value)
    }),
    count = purrr::map_int(records, function(record) {
      value <- record$count %||% record$transaction_count %||% NA_integer_
      as.integer(value)
    }),
    gas_used = purrr::map_dbl(records, function(record) {
      value <- record$gas_used %||% record$gasUsed %||% NA_real_
      as.numeric(value)
    }),
    logs_bloom = purrr::map_chr(records, function(record) {
      value <- record$logs_bloom %||% record$logsBloom %||% NA_character_
      as.character(value)
    }),
    hapi_version = purrr::map_chr(records, function(record) {
      value <- record$hapi_version %||% record$hapiVersion %||% NA_character_
      as.character(value)
    }),
    start_timestamp = hadeda_parse_timestamp(start_ts),
    end_timestamp = hadeda_parse_timestamp(end_ts),
    transactions = purrr::map(records, function(record) {
      txs <- record$transactions %||% list()
      if (is.null(txs)) {
        list()
      } else {
        txs
      }
    })
  )
}
