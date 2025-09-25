#' Parse token metadata
#'
#' Transform raw token records from the Mirror Node into a tibble.
#'
#' @param records A list of token metadata objects.
#'
#' @return A tibble describing tokens.
#'
#' @examples
#' records <- list(list(
#'   token_id = "0.0.1001",
#'   symbol = "HED",
#'   name = "Example Token",
#'   treasury_account_id = "0.0.3",
#'   type = "FUNGIBLE_COMMON"
#' ))
#' hadeda_parse_tokens(records)
#'
#' @keywords internal
hadeda_parse_tokens <- function(records) {
  if (length(records) == 0) {
    return(tibble::tibble(
      token_id = character(),
      symbol = character(),
      name = character(),
      treasury_account_id = character(),
      type = character()
    ))
  }

  tibble::tibble(
    token_id = purrr::map_chr(records, ~purrr::pluck(.x, "token_id", .default = NA_character_)),
    symbol = purrr::map_chr(records, ~purrr::pluck(.x, "symbol", .default = NA_character_)),
    name = purrr::map_chr(records, ~purrr::pluck(.x, "name", .default = NA_character_)),
    treasury_account_id = purrr::map_chr(records, ~purrr::pluck(.x, "treasury_account_id", .default = NA_character_)),
    type = purrr::map_chr(records, ~purrr::pluck(.x, "type", .default = NA_character_))
  )
}
