#' Retrieve smart contract bytecode
#'
#' Fetch bytecode for a smart contract from the Mirror Node.
#'
#' @inheritParams contracts_get
#'
#' @return A tibble with the contract identifier and bytecode.
#'
#' @examples
#' config <- hadeda_config()
#' contract_id <- "0.0.5005"
#' bytecode <- contracts_bytecode(config, contract_id)
#' bytecode
#'
#' @export
contracts_bytecode <- function(config, contract_id, .transport = NULL) {
  transport <- hadeda_choose_transport(config, .transport, rest_supported = TRUE, grpc_supported = FALSE)
  if (!identical(transport, "rest")) {
    cli::cli_abort("contracts_bytecode() currently supports only the REST transport.")
  }

  resp <- hadeda_rest_get(config, paste0("contracts/", contract_id, "/bytecode"))
  tibble::tibble(
    contract_id = contract_id,
    bytecode = resp$bytecode %||% NA_character_
  )
}
