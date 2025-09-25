#' Create a new consensus topic
#'
#' Submit a topic creation request via a REST transport such as Hashio.
#'
#' @inheritParams topics_messages
#' @param memo Optional topic memo.
#' @param admin_key Optional admin key definition.
#' @param submit_key Optional submit key definition.
#' @param auto_renew_account Optional auto renew account identifier.
#' @param auto_renew_period Optional auto renew period in seconds.
#'
#' @return A tibble containing the created topic metadata and transaction
#'   details.
#'
#' @export
topics_create <- function(config,
                          memo = NULL,
                          admin_key = NULL,
                          submit_key = NULL,
                          auto_renew_account = NULL,
                          auto_renew_period = NULL,
                          .transport = NULL) {
  transport <- hadeda_choose_transport(config, .transport, rest_supported = TRUE, grpc_supported = FALSE)
  if (!identical(transport, "rest")) {
    cli::cli_abort("topics_create() currently supports only the REST transport.")
  }

  body <- hadeda_drop_null(list(
    memo = memo,
    adminKey = admin_key,
    submitKey = submit_key,
    autoRenewAccount = auto_renew_account,
    autoRenewPeriod = auto_renew_period
  ))

  resp <- hadeda_rest_post(config, "topics", body)
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
