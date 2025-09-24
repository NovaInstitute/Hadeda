#' Retrieve contract information
#'
#' Fetch smart contract metadata from the Mirror Node.
#'
#' @param config A configuration list from [hadeda_config()].
#' @param contract_id Contract identifier.
#' @param .transport Optional transport override.
#'
#' @return A tibble describing the contract.
#'
#' @examples
#' config <- hadeda_config()
#' contract_id <- "0.0.5005"
#' contract <- contracts_get(config, contract_id)
#' contract
#'
#' @export
contracts_get <- function(config, contract_id, .transport = NULL) {
  transport <- hadeda_choose_transport(config, .transport, rest_supported = TRUE, grpc_supported = FALSE)
  if (!identical(transport, "rest")) {
    cli::cli_abort("contracts_get() currently supports only the REST transport.")
  }

  resp <- hadeda_rest_get(config, paste0("contracts/", contract_id))
  hadeda_parse_contracts(list(resp))
}
