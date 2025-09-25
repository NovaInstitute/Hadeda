#' Parse network stake snapshots
#'
#' Convert network stake payloads from the Mirror Node API into a tibble.
#'
#' @param records A list of stake snapshots.
#'
#' @return A tibble summarising network stake.
#'
#' @keywords internal
hadeda_parse_network_stake <- function(records) {
  if (length(records) == 0) {
    return(tibble::tibble(
      epoch_day = integer(),
      max_stake_reward_rate_per_hbar = numeric(),
      max_staking_reward = numeric(),
      max_total_reward = numeric(),
      node_reward_fee_fraction = numeric(),
      reserved_staking_reward = numeric(),
      staking_period = character(),
      staking_period_duration = numeric(),
      staking_period_start = lubridate::as_datetime(numeric()),
      staking_period_end = lubridate::as_datetime(numeric()),
      total_stake = numeric()
    ))
  }

  start_ts <- purrr::map_chr(records, function(record) {
    record$staking_period_start %||% record$stakingPeriodStart %||% NA_character_
  }, .default = NA_character_)

  end_ts <- purrr::map_chr(records, function(record) {
    record$staking_period_end %||% record$stakingPeriodEnd %||% NA_character_
  }, .default = NA_character_)

  tibble::tibble(
    epoch_day = purrr::map_int(records, function(record) {
      value <- record$epoch_day %||% record$epochDay %||% NA_integer_
      as.integer(value)
    }),
    max_stake_reward_rate_per_hbar = purrr::map_dbl(records, function(record) {
      value <- record$max_stake_reward_rate_per_hbar %||%
        record$maxStakeRewardRatePerHbar %||% NA_real_
      as.numeric(value)
    }),
    max_staking_reward = purrr::map_dbl(records, function(record) {
      value <- record$max_staking_reward %||% record$maxStakingReward %||% NA_real_
      as.numeric(value)
    }),
    max_total_reward = purrr::map_dbl(records, function(record) {
      value <- record$max_total_reward %||% record$maxTotalReward %||% NA_real_
      as.numeric(value)
    }),
    node_reward_fee_fraction = purrr::map_dbl(records, function(record) {
      value <- record$node_reward_fee_fraction %||% record$nodeRewardFeeFraction %||% NA_real_
      as.numeric(value)
    }),
    reserved_staking_reward = purrr::map_dbl(records, function(record) {
      value <- record$reserved_staking_reward %||% record$reservedStakingReward %||% NA_real_
      as.numeric(value)
    }),
    staking_period = purrr::map_chr(records, function(record) {
      record$staking_period %||% record$stakingPeriod %||% NA_character_
    }),
    staking_period_duration = purrr::map_dbl(records, function(record) {
      value <- record$staking_period_duration %||% record$stakingPeriodDuration %||% NA_real_
      as.numeric(value)
    }),
    staking_period_start = hadeda_parse_timestamp(start_ts),
    staking_period_end = hadeda_parse_timestamp(end_ts),
    total_stake = purrr::map_dbl(records, function(record) {
      value <- record$total_stake %||% record$totalStake %||% NA_real_
      as.numeric(value)
    })
  )
}
