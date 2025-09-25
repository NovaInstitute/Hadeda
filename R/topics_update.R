#' Update consensus topic properties
#'
#' Submit a topic update transaction over the REST transport.
#'
#' @inheritParams topics_create
#' @param topic_id Consensus topic identifier.
#'
#' @return A tibble containing the updated topic metadata and transaction
#'   details.
#'
#' @export
topics_update <- function(config,
                          topic_id,
                          memo = NULL,
                          admin_key = NULL,
                          submit_key = NULL,
                          auto_renew_account = NULL,
                          auto_renew_period = NULL,
                          .transport = NULL) {
  transport <- hadeda_choose_transport(config, .transport, rest_supported = TRUE, grpc_supported = FALSE)
  if (!identical(transport, "rest")) {
    cli::cli_abort("topics_update() currently supports only the REST transport.")
  }

  if (missing(topic_id) || is.null(topic_id)) {
    cli::cli_abort("topic_id is required to update a topic.")
  }

  body <- hadeda_drop_null(list(
    memo = memo,
    adminKey = admin_key,
    submitKey = submit_key,
    autoRenewAccount = auto_renew_account,
    autoRenewPeriod = auto_renew_period
  ))

  if (length(body) == 0) {
    cli::cli_abort("At least one field must be supplied to update a topic.")
  }

  resp <- hadeda_rest_post(config, paste0("topics/", topic_id), body)
  record <- resp$topic %||% resp
  parsed <- hadeda_parse_topics(list(record))

  status <- resp$status %||% resp$receipt$status %||% NA_character_
  transaction_id <- resp$transactionId %||% resp$transaction_id %||% NA_character_

  parsed |>
    tibble::add_column(
      status = if (is.null(status)) NA_character_ else as.character(status),
      transaction_id = if (is.null(transaction_id)) NA_character_ else as.character(transaction_id),
      response = list(resp),
      .after = "topic_id"
    )
}
