test_that("blocks_list parses payload", {
  cfg <- hadeda_config(network = "testnet")
  responses <- list(
    list(
      blocks = list(
        list(
          number = 1000,
          hash = "0xabc",
          previous_hash = "0xdef",
          count = 25,
          gas_used = 12345,
          logs_bloom = "0x00",
          hapi_version = "0.37.0",
          start_consensus_timestamp = "1700000000.000000000",
          end_consensus_timestamp = "1700000001.000000000",
          transactions = list("0.0.1001-1700000000-000000001")
        )
      ),
      links = list("next" = NULL)
    )
  )

  with_mocked_bindings({
    tbl <- blocks_list(cfg, limit = 1, order = "desc", block_number = 1000)
  }, hadeda_rest_paginate = function(config, path, query) {
    expect_identical(path, "blocks")
    expect_equal(query$limit, 1)
    expect_equal(query$order, "desc")
    expect_equal(query$`block.number`, 1000)
    responses
  })

  expect_equal(nrow(tbl), 1)
  expect_s3_class(tbl$start_timestamp, "POSIXct")
  expect_equal(tbl$number, 1000)
  expect_equal(tbl$transactions[[1]][[1]], "0.0.1001-1700000000-000000001")
})

test_that("blocks_get returns single row", {
  cfg <- hadeda_config(network = "testnet")
  response <- list(
    number = 1001,
    hash = "0x123",
    previous_hash = "0x456",
    count = 10,
    start_consensus_timestamp = "1700000100.000000000",
    end_consensus_timestamp = "1700000101.000000000"
  )

  with_mocked_bindings({
    tbl <- blocks_get(cfg, 1001)
  }, hadeda_rest_get = function(config, path, query = list()) {
    expect_identical(path, "blocks/1001")
    response
  })

  expect_equal(nrow(tbl), 1)
  expect_equal(tbl$number, 1001)
})

test_that("network_nodes parses node metadata", {
  cfg <- hadeda_config(network = "testnet")
  responses <- list(
    list(
      nodes = list(
        list(
          node_id = 1,
          account_id = "0.0.3",
          description = "Test node",
          stake = 100,
          min_stake = 50,
          max_stake = 150,
          service_endpoints = list(list(ip_address_v4 = "127.0.0.1", port = 50211))
        )
      ),
      links = list("next" = NULL)
    )
  )

  with_mocked_bindings({
    tbl <- network_nodes(cfg, limit = 1, node_id = 1, order = "asc")
  }, hadeda_rest_paginate = function(config, path, query) {
    expect_identical(path, "network/nodes")
    expect_equal(query$limit, 1)
    expect_equal(query$order, "asc")
    expect_equal(query$`node.id`, 1)
    responses
  })

  expect_equal(nrow(tbl), 1)
  expect_equal(tbl$node_id, 1)
  expect_equal(tbl$service_endpoints[[1]][[1]]$ip_address_v4, "127.0.0.1")
})

test_that("network_stake parses stake snapshots", {
  cfg <- hadeda_config(network = "testnet")
  responses <- list(
    list(
      stakes = list(
        list(
          epoch_day = 10,
          max_stake_reward_rate_per_hbar = 0.0001,
          max_staking_reward = 1000,
          max_total_reward = 2000,
          node_reward_fee_fraction = 0.1,
          reserved_staking_reward = 100,
          staking_period = "10",
          staking_period_duration = 86400,
          staking_period_start = "1700000200.000000000",
          staking_period_end = "1700000264.000000000",
          total_stake = 100000
        )
      ),
      links = list("next" = NULL)
    )
  )

  with_mocked_bindings({
    tbl <- network_stake(cfg, limit = 1, order = "desc")
  }, hadeda_rest_paginate = function(config, path, query) {
    expect_identical(path, "network/stake")
    expect_equal(query$limit, 1)
    expect_equal(query$order, "desc")
    responses
  })

  expect_equal(nrow(tbl), 1)
  expect_s3_class(tbl$staking_period_start, "POSIXct")
  expect_equal(tbl$total_stake, 100000)
})

test_that("network_exchange_rate parses current and next rates", {
  cfg <- hadeda_config(network = "testnet")
  response <- list(
    timestamp = "1700000300.000000000",
    current_rate = list(
      hbar_equivalent = 1,
      cent_equivalent = 12,
      expiration_time = 1700000360
    ),
    next_rate = list(
      hbar_equivalent = 2,
      cent_equivalent = 25,
      expirationTime = 1700000420
    )
  )

  with_mocked_bindings({
    tbl <- network_exchange_rate(cfg)
  }, hadeda_rest_get = function(config, path, query = list()) {
    expect_identical(path, "network/exchangerate")
    response
  })

  expect_equal(nrow(tbl), 2)
  expect_equal(sort(tbl$rate_type), c("current", "next"))
  expect_equal(tbl$cent_equivalent, c(12, 25))
  expect_s3_class(tbl$effective_timestamp, "POSIXct")
  expect_s3_class(tbl$expiration_timestamp, "POSIXct")
})

test_that("network_supply parses supply payload", {
  cfg <- hadeda_config(network = "testnet")
  response <- list(
    timestamp = "1700000400.000000000",
    released_supply = "5000000000000000",
    total_supply = 5000000000000000,
    circulating_supply = "4990000000000000",
    max_supply = 5000000000000000
  )

  with_mocked_bindings({
    tbl <- network_supply(cfg)
  }, hadeda_rest_get = function(config, path, query = list()) {
    expect_identical(path, "network/supply")
    response
  })

  expect_equal(nrow(tbl), 1)
  expect_s3_class(tbl$collected_timestamp, "POSIXct")
  expect_equal(tbl$released_supply, 5000000000000000)
  expect_equal(tbl$circulating_supply, 4990000000000000)
})

test_that("network_fees parses fee schedule payload", {
  cfg <- hadeda_config(network = "testnet")
  response <- list(
    timestamp = "1700000500.000000000",
    current_fee_schedule = list(
      timestamp = list(
        from = "1700000400.000000000",
        to = "1700000500.000000000"
      ),
      expiry_time = "1700000560.000000000",
      transaction_fee_schedule = list(
        list(
          hedera_functionality = "CryptoTransfer",
          timestamp = list(
            from = "1700000400.000000000",
            to = "1700000500.000000000"
          ),
          fees = list(
            list(
              sub_type = "DEFAULT",
              fee_data = list(
                network_data = list(constant = 10, bpt = 2),
                node_data = list(constant = 5),
                service_data = list(constant = 20)
              )
            )
          )
        )
      )
    ),
    next_fee_schedule = list(
      timestamp = list(
        from = "1700000500.000000000",
        to = "1700000600.000000000"
      ),
      expiry_time = "1700000660.000000000",
      transaction_fee_schedule = list()
    )
  )

  with_mocked_bindings({
    tbl <- network_fees(cfg)
  }, hadeda_rest_get = function(config, path, query = list()) {
    expect_identical(path, "network/fees")
    response
  })

  expect_equal(nrow(tbl), 2)
  expect_equal(sort(tbl$schedule_type), c("current", "next"))
  expect_s3_class(tbl$period_start, "POSIXct")
  expect_s3_class(tbl$expiry_time, "POSIXct")
  current_index <- which(tbl$schedule_type == "current")
  current_schedule <- tbl$transaction_fee_schedule[[current_index]]
  expect_s3_class(current_schedule, "tbl_df")
  expect_equal(current_schedule$hedera_functionality, "CryptoTransfer")
  fee_entry <- current_schedule$fees[[1]]
  expect_s3_class(fee_entry, "tbl_df")
  component_tbl <- fee_entry$fee_data[[1]]
  expect_s3_class(component_tbl, "tbl_df")
  expect_equal(component_tbl$component, c("network", "node", "service"))
})

test_that("tokens_nfts parses NFT list", {
  cfg <- hadeda_config(network = "testnet")
  token_id <- "0.0.5000"
  responses <- list(
    list(
      nfts = list(
        list(
          token_id = token_id,
          serial_number = 1,
          account_id = "0.0.1001",
          metadata = "aGVsbG8=",
          created_timestamp = "1700000300.000000000",
          modified_timestamp = "1700000310.000000000",
          spender_id = "0.0.1002"
        ),
        list(
          token_id = token_id,
          serial_number = 2,
          account_id = "0.0.1003",
          metadata = "d29ybGQ=",
          created_timestamp = "1700000400.000000000"
        )
      ),
      links = list("next" = NULL)
    )
  )

  with_mocked_bindings({
    tbl <- tokens_nfts(
      cfg,
      token_id = token_id,
      limit = 2,
      order = "asc",
      account_id = "0.0.1001",
      spender_id = "0.0.1002",
      serial_number = 1
    )
  }, hadeda_rest_paginate = function(config, path, query) {
    expect_identical(path, sprintf("tokens/%s/nfts", token_id))
    expect_equal(query$limit, 2)
    expect_equal(query$order, "asc")
    expect_equal(query$`account.id`, "0.0.1001")
    expect_equal(query$`spender.id`, "0.0.1002")
    expect_equal(query$serialnumber, 1)
    responses
  })

  expect_equal(nrow(tbl), 2)
  expect_equal(tbl$serial_number, c(1, 2))
  expect_equal(tbl$metadata, c("aGVsbG8=", "d29ybGQ="))
})

test_that("tokens_nft parses single NFT", {
  cfg <- hadeda_config(network = "testnet")
  token_id <- "0.0.5000"
  serial <- 42
  response <- list(
    token_id = token_id,
    serial_number = serial,
    account_id = "0.0.1001",
    metadata = "YWJj",
    created_timestamp = "1700000500.000000000"
  )

  with_mocked_bindings({
    tbl <- tokens_nft(cfg, token_id, serial)
  }, hadeda_rest_get = function(config, path, query = list()) {
    expect_identical(path, sprintf("tokens/%s/nfts/%s", token_id, serial))
    response
  })

  expect_equal(nrow(tbl), 1)
  expect_equal(tbl$serial_number, serial)
})
