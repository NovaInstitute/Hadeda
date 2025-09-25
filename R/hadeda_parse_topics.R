#' Parse consensus topic metadata
#'
#' Convert topic records returned by the Mirror Node or transaction
#' submission endpoints into a tidy tibble.
#'
#' @param records A list of topic records.
#'
#' @return A tibble describing consensus topics.
#'
#' @keywords internal
hadeda_parse_topics <- function(records) {
  if (length(records) == 0) {
    return(tibble::tibble(
      topic_id = character(),
      memo = character(),
      auto_renew_account = character(),
      auto_renew_period = numeric(),
      expiration = lubridate::as_datetime(numeric()),
      sequence_number = numeric(),
      running_hash = character(),
      running_hash_version = integer(),
      deleted = logical(),
      admin_key = list(),
      submit_key = list(),
      ledger_id = character()
    ))
  }

  expiration_raw <- purrr::map_chr(records, ~{
    value <- purrr::pluck(
      .x,
      "expiration_timestamp",
      .default = purrr::pluck(.x, "expirationTimestamp", .default = NA_character_)
    )
    if (is.null(value)) {
      NA_character_
    } else {
      as.character(value)
    }
  })

  tibble::tibble(
    topic_id = purrr::map_chr(records, ~{
      value <- purrr::pluck(.x, "topic_id", .default = purrr::pluck(.x, "topicId", .default = NA_character_))
      if (is.null(value)) {
        NA_character_
      } else {
        as.character(value)
      }
    }),
    memo = purrr::map_chr(records, ~{
      value <- purrr::pluck(.x, "memo", .default = purrr::pluck(.x, "memoBase64", .default = NA_character_))
      if (is.null(value)) {
        NA_character_
      } else {
        as.character(value)
      }
    }),
    auto_renew_account = purrr::map_chr(records, ~{
      value <- purrr::pluck(
        .x,
        "auto_renew_account",
        .default = purrr::pluck(
          .x,
          "autoRenewAccount",
          .default = purrr::pluck(
            .x,
            "auto_renew_account_id",
            .default = purrr::pluck(.x, "autoRenewAccountId", .default = NA_character_)
          )
        )
      )
      if (is.null(value)) {
        NA_character_
      } else {
        as.character(value)
      }
    }),
    auto_renew_period = purrr::map_dbl(records, ~{
      value <- purrr::pluck(.x, "auto_renew_period", .default = purrr::pluck(.x, "autoRenewPeriod", .default = NA_real_))
      if (is.null(value)) {
        NA_real_
      } else {
        as.numeric(value)
      }
    }),
    expiration = hadeda_parse_timestamp(expiration_raw),
    sequence_number = purrr::map_dbl(records, ~{
      value <- purrr::pluck(.x, "sequence_number", .default = purrr::pluck(.x, "sequenceNumber", .default = NA_real_))
      if (is.null(value)) {
        NA_real_
      } else {
        as.numeric(value)
      }
    }),
    running_hash = purrr::map_chr(records, ~{
      value <- purrr::pluck(.x, "running_hash", .default = purrr::pluck(.x, "runningHash", .default = NA_character_))
      if (is.null(value)) {
        NA_character_
      } else {
        as.character(value)
      }
    }),
    running_hash_version = purrr::map_int(records, ~{
      value <- purrr::pluck(.x, "running_hash_version", .default = purrr::pluck(.x, "runningHashVersion", .default = NA_integer_))
      if (is.null(value)) {
        NA_integer_
      } else {
        as.integer(value)
      }
    }),
    deleted = purrr::map_lgl(records, ~{
      value <- purrr::pluck(.x, "deleted", .default = NA)
      if (is.null(value)) {
        NA
      } else {
        as.logical(value)
      }
    }),
    admin_key = purrr::map(records, ~purrr::pluck(.x, "admin_key", .default = purrr::pluck(.x, "adminKey", .default = NULL))),
    submit_key = purrr::map(records, ~purrr::pluck(.x, "submit_key", .default = purrr::pluck(.x, "submitKey", .default = NULL))),
    ledger_id = purrr::map_chr(records, ~{
      value <- purrr::pluck(.x, "ledger_id", .default = purrr::pluck(.x, "ledgerId", .default = NA_character_))
      if (is.null(value)) {
        NA_character_
      } else {
        as.character(value)
      }
    })
  )
}
