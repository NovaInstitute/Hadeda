#' Parse contract metadata
#'
#' Transform raw contract records into a tibble.
#'
#' @param records A list of contract metadata entries.
#'
#' @return A tibble describing contracts.
#'
#' @examples
#' records <- list(list(
#'   contract_id = "0.0.5005",
#'   admin_key = "302a300506032b6570032100...",
#'   auto_renew_account_id = "0.0.1001",
#'   created_timestamp = "1700000000.000000000",
#'   memo = "Example contract"
#' ))
#' hadeda_parse_contracts(records)
#'
#' @keywords internal
hadeda_parse_contracts <- function(records) {
  if (length(records) == 0) {
    return(tibble::tibble(
      contract_id = character(),
      admin_key = character(),
      auto_renew_account_id = character(),
      created_timestamp = lubridate::as_datetime(numeric()),
      memo = character()
    ))
  }

  tibble::tibble(
    contract_id = purrr::map_chr(records, ~purrr::pluck(.x, "contract_id", .default = NA_character_)),
    admin_key = purrr::map_chr(records, ~purrr::pluck(.x, "admin_key", .default = NA_character_)),
    auto_renew_account_id = purrr::map_chr(records, ~purrr::pluck(.x, "auto_renew_account_id", .default = NA_character_)),
    created_timestamp = hadeda_parse_timestamp(purrr::map_chr(records, ~purrr::pluck(.x, "created_timestamp", .default = NA_character_))),
    memo = purrr::map_chr(records, ~purrr::pluck(.x, "memo", .default = NA_character_))
  )
}

#' @keywords internal
hadeda_parse_grpc_contract_info <- function(response) {
  if (is.null(response)) {
    response <- list()
  }

  info <- response$contract_info %||% response$info %||% response$contractInfo %||% response

  contract_id <- info$contract_id %||% info$contractId %||% info$contractID %||% NA_character_
  account_id <- info$account_id %||% info$accountId %||% info$accountID %||% NA_character_
  admin_key <- info$admin_key %||% info$adminKey %||% list()
  auto_renew_account <- info$auto_renew_account_id %||% info$autoRenewAccountId %||%
    info$auto_renew_account %||% info$autoRenewAccount %||% NA_character_

  auto_renew_period <- info$auto_renew_period %||% info$autoRenewPeriod %||% NA_real_
  if (is.list(auto_renew_period) && !is.null(auto_renew_period$seconds)) {
    auto_renew_period <- auto_renew_period$seconds
  }

  expiration_raw <- info$expiration_time %||% info$expirationTime %||% NA_character_
  expiration <- if (is.null(expiration_raw)) {
    lubridate::as_datetime(NA_real_)
  } else {
    hadeda_parse_timestamp(as.character(expiration_raw))
  }

  memo <- info$memo %||% info$memoBase64 %||% NA_character_
  balance <- info$balance %||% NA_real_
  storage <- info$storage %||% NA_real_
  evm_address <- info$evm_address %||% info$evmAddress %||% NA_character_
  contract_account_id <- info$contract_account_id %||% info$contractAccountId %||% NA_character_

  staking <- info$staking_info %||% info$stakingInfo %||% list()
  staked_account <- staking$staked_account_id %||% staking$stakedAccountId %||%
    info$staked_account_id %||% info$stakedAccountId %||% NA_character_
  staked_node <- staking$staked_node_id %||% staking$stakedNodeId %||%
    info$staked_node_id %||% info$stakedNodeId %||% NA_character_
  decline_reward <- staking$decline_reward %||% staking$declineReward %||%
    info$decline_reward %||% info$declineReward
  decline_reward <- if (is.null(decline_reward)) NA else rlang::is_true(decline_reward)

  max_auto_assoc <- info$max_automatic_token_associations %||% info$maxAutomaticTokenAssociations %||% NA_real_
  ledger_id <- info$ledger_id %||% info$ledgerId %||% NA_character_
  obtainer <- info$obtainer_id %||% info$obtainerId %||% NA_character_

  tibble::tibble(
    contract_id = as.character(contract_id),
    account_id = as.character(account_id),
    contract_account_id = as.character(contract_account_id),
    evm_address = as.character(evm_address),
    memo = if (is.null(memo)) NA_character_ else as.character(memo),
    balance = if (is.null(balance)) NA_real_ else as.numeric(balance),
    storage = if (is.null(storage)) NA_real_ else as.numeric(storage),
    auto_renew_account = if (is.null(auto_renew_account)) NA_character_ else as.character(auto_renew_account),
    auto_renew_period = if (is.null(auto_renew_period)) NA_real_ else as.numeric(auto_renew_period),
    expiration_time = expiration,
    admin_key = list(admin_key),
    staked_account_id = if (is.null(staked_account)) NA_character_ else as.character(staked_account),
    staked_node_id = if (is.null(staked_node)) NA_character_ else as.character(staked_node),
    decline_staking_reward = decline_reward,
    max_token_associations = if (is.null(max_auto_assoc)) NA_real_ else as.numeric(max_auto_assoc),
    ledger_id = if (is.null(ledger_id)) NA_character_ else as.character(ledger_id),
    obtainer_id = if (is.null(obtainer)) NA_character_ else as.character(obtainer),
    staking = list(staking %||% list()),
    info = list(info),
    response = list(response)
  )
}
