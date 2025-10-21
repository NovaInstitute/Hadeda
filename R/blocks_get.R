#' Retrieve a specific Hedera block
#'
#' Fetch a single block from the Mirror Node REST API by number or hash.
#'
#' @inheritParams blocks_list
#' @param block Identifier of the block to retrieve. Can be a hash or numeric.
#'
#' @return A tibble with a single row describing the block.
#'
#' @examples
#' mirror <- hadeda_config(network = "testnet")
#' \dontrun{
#'   blocks_get(mirror, block = 123456)
#' }
#'
#' @export
blocks_get <- function(config, block, .transport = NULL) {
  transport <- hadeda_choose_transport(config, .transport, rest_supported = TRUE, grpc_supported = FALSE)
  if (!identical(transport, "rest")) {
    cli::cli_abort("blocks_get() currently supports only the REST transport.")
  }

  if (missing(block) || length(block) != 1) {
    cli::cli_abort("A single block identifier is required.")
  }

  path <- sprintf("blocks/%s", block)
  resp <- hadeda_rest_get(config, path)

  if (!is.null(resp$blocks)) {
    records <- resp$blocks
  } else {
    records <- list(resp)
  }

  tbl <- hadeda_parse_blocks(records)
  if (nrow(tbl) == 0) {
    cli::cli_abort("Block response did not contain any records.")
  }
  tibble::as_tibble(tbl[1, , drop = FALSE])
}
