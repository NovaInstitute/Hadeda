#' Parse NFT allowance records
#'
#' Convert NFT allowance payloads from the Mirror Node into a tidy tibble.
#'
#' @param records A list of NFT allowance entries from the Mirror Node REST API.
#'
#' @return A tibble describing NFT allowance approvals.
#'
#' @examples
#' records <- list(list(
#'   owner = "0.0.1001",
#'   token_id = "0.0.2002",
#'   granted = list(list(
#'     spender = "0.0.3003",
#'     serial_numbers = list(1, 2),
#'     timestamp = "1672531200.000000000",
#'     payer_account_id = "0.0.4004",
#'     transaction_id = "0.0.4004-123456789-000000000"
#'   )),
#'   approved_for_all = list(list(
#'     spender = "0.0.5005",
#'     timestamp = "1672531300.000000000",
#'     delegating_spender = "0.0.6006",
#'     payer_account_id = "0.0.7007",
#'     transaction_id = "0.0.7007-123456789-000000000"
#'   ))
#' ))
#' hadeda_parse_account_allowances_nfts(records)
#'
#' @keywords internal
hadeda_parse_account_allowances_nfts <- function(records) {
  if (length(records) == 0) {
    return(tibble::tibble(
      owner = character(),
      token_id = character(),
      spender = character(),
      approved_for_all = logical(),
      serial_numbers = list(),
      timestamp = hadeda_parse_timestamp(character()),
      delegating_spender = character(),
      payer_account_id = character(),
      transaction_id = character()
    ))
  }

  rows <- purrr::map(records, function(record) {
    owner <- purrr::pluck(record, "owner", .default = NA_character_)
    token_id <- purrr::pluck(record, "token_id", .default = NA_character_)

    granted <- purrr::map(purrr::pluck(record, "granted", .default = list()), function(entry) {
      list(
        owner = owner,
        token_id = token_id,
        spender = purrr::pluck(entry, "spender", .default = purrr::pluck(record, "spender", .default = NA_character_)),
        approved_for_all = FALSE,
        serial_numbers = purrr::pluck(entry, "serial_numbers", .default = list()),
        timestamp = purrr::pluck(entry, "timestamp", .default = NA_character_),
        delegating_spender = purrr::pluck(entry, "delegating_spender", .default = NA_character_),
        payer_account_id = purrr::pluck(entry, "payer_account_id", .default = NA_character_),
        transaction_id = purrr::pluck(entry, "transaction_id", .default = NA_character_)
      )
    })

    approved_for_all <- purrr::map(purrr::pluck(record, "approved_for_all", .default = list()), function(entry) {
      list(
        owner = owner,
        token_id = token_id,
        spender = purrr::pluck(entry, "spender", .default = purrr::pluck(record, "spender", .default = NA_character_)),
        approved_for_all = TRUE,
        serial_numbers = list(),
        timestamp = purrr::pluck(entry, "timestamp", .default = NA_character_),
        delegating_spender = purrr::pluck(entry, "delegating_spender", .default = NA_character_),
        payer_account_id = purrr::pluck(entry, "payer_account_id", .default = NA_character_),
        transaction_id = purrr::pluck(entry, "transaction_id", .default = NA_character_)
      )
    })

    combined <- c(granted, approved_for_all)

    if (length(combined) == 0) {
      combined <- list(list(
        owner = owner,
        token_id = token_id,
        spender = purrr::pluck(record, "spender", .default = NA_character_),
        approved_for_all = isTRUE(purrr::pluck(record, "approved_for_all", .default = FALSE)),
        serial_numbers = purrr::pluck(record, "serial_numbers", .default = list()),
        timestamp = purrr::pluck(record, "timestamp", .default = NA_character_),
        delegating_spender = purrr::pluck(record, "delegating_spender", .default = NA_character_),
        payer_account_id = purrr::pluck(record, "payer_account_id", .default = NA_character_),
        transaction_id = purrr::pluck(record, "transaction_id", .default = NA_character_)
      ))
    }

    combined
  })

  rows <- purrr::flatten(rows)

  serial_numbers <- purrr::map(rows, function(entry) {
    values <- purrr::pluck(entry, "serial_numbers", .default = list())
    if (length(values) == 0) {
      return(integer())
    }
    if (is.list(values) && length(values) == 1 && is.list(values[[1]])) {
      values <- values[[1]]
    }
    if (!rlang::is_atomic(values)) {
      values <- unlist(values)
    }
    as.integer(values)
  })

  tibble::tibble(
    owner = purrr::map_chr(rows, ~purrr::pluck(.x, "owner", .default = NA_character_)),
    token_id = purrr::map_chr(rows, ~purrr::pluck(.x, "token_id", .default = NA_character_)),
    spender = purrr::map_chr(rows, ~purrr::pluck(.x, "spender", .default = NA_character_)),
    approved_for_all = purrr::map_lgl(rows, ~isTRUE(purrr::pluck(.x, "approved_for_all", .default = FALSE))),
    serial_numbers = serial_numbers,
    timestamp = hadeda_parse_timestamp(purrr::map_chr(rows, ~purrr::pluck(.x, "timestamp", .default = NA_character_))),
    delegating_spender = purrr::map_chr(rows, ~purrr::pluck(.x, "delegating_spender", .default = NA_character_)),
    payer_account_id = purrr::map_chr(rows, ~purrr::pluck(.x, "payer_account_id", .default = NA_character_)),
    transaction_id = purrr::map_chr(rows, ~purrr::pluck(.x, "transaction_id", .default = NA_character_))
  )
}
