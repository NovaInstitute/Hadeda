test_that("tokens_get parses metadata", {
  cfg <- hadeda_config(network = "testnet")
  response <- list(
    token_id = "0.0.4000",
    symbol = "HED",
    name = "Hedera",
    treasury_account_id = "0.0.2",
    type = "FUNGIBLE_COMMON"
  )

  with_mocked_bindings({
    tbl <- tokens_get(cfg, response$token_id)
  }, hadeda_rest_get = function(config, path, query = list()) {
    expect_identical(path, paste0("tokens/", response$token_id))
    response
  })

  expect_equal(nrow(tbl), 1)
  expect_equal(tbl$symbol, "HED")
})

test_that("tokens_balances returns flattened balances", {
  cfg <- hadeda_config(network = "testnet")
  balances <- list(
    list(account = "0.0.1001", balance = 10, decimals = 2),
    list(account = "0.0.1002", balance = 20, decimals = 2)
  )

  with_mocked_bindings({
    tbl <- tokens_balances(cfg, "0.0.4000", limit = 2, account_id = "0.0.1001", order = "asc")
  }, hadeda_rest_paginate = function(config, path, query) {
    expect_identical(path, "tokens/0.0.4000/balances")
    expect_equal(query$limit, 2)
    expect_equal(query$`account.id`, "0.0.1001")
    expect_equal(query$order, "asc")
    list(list(balances = balances, links = list("next" = NULL)))
  })

  expect_equal(nrow(tbl), 2)
  expect_equal(tbl$account, c("0.0.1001", "0.0.1002"))
})

test_that("tokens_create posts payload and returns identifiers", {
  cfg <- hadeda_config(network = "testnet")
  response <- list(
    tokenId = "0.0.7001",
    status = "SUCCESS",
    transactionId = "0.0.6001-1700000000-000000000"
  )

  with_mocked_bindings({
    tbl <- tokens_create(
      cfg,
      name = "Hadeda Example",
      symbol = "HADEDA",
      treasury_account_id = "0.0.6001",
      initial_supply = 1000,
      decimals = 2,
      memo = "demo"
    )
  }, hadeda_rest_post = function(config, path, body = list()) {
    expect_identical(path, "tokens")
    expect_equal(body$name, "Hadeda Example")
    expect_equal(body$symbol, "HADEDA")
    expect_equal(body$treasuryAccountId, "0.0.6001")
    expect_equal(body$initialSupply, 1000)
    expect_equal(body$decimals, 2)
    expect_equal(body$memo, "demo")
    response
  })

  expect_s3_class(tbl, "tbl_df")
  expect_equal(tbl$token_id, "0.0.7001")
  expect_equal(tbl$status, "SUCCESS")
  expect_equal(tbl$transaction_id, "0.0.6001-1700000000-000000000")
  expect_true(is.na(tbl$receipt_status))
  expect_true(is.na(tbl$consensus_timestamp))
  expect_equal(tbl$receipt, list(list()))
  expect_equal(tbl$response, list(response))
})

test_that("tokens_create delegates to gRPC when requested", {
  cfg <- hadeda_config(network = "testnet")
  response <- list(
    transactionId = "0.0.6001-1700000000-000000001",
    status = "OK",
    receipt = list(tokenId = "0.0.7002", status = "SUCCESS"),
    tokenId = "0.0.7002"
  )

  with_mocked_bindings({
    tbl <- tokens_create(
      cfg,
      name = "gRPC Token",
      symbol = "GRPC",
      treasury_account_id = "0.0.1234",
      initial_supply = 25,
      decimals = 2,
      max_fee = 1e6,
      wait_for_receipt = FALSE,
      .transport = "grpc"
    )
  }, hadeda_grpc_tokens_create = function(config,
                                           name,
                                           symbol,
                                           treasury_account_id,
                                           initial_supply,
                                           decimals,
                                           token_type,
                                           memo,
                                           freeze_default,
                                           max_fee,
                                           wait_for_receipt) {
    expect_equal(name, "gRPC Token")
    expect_equal(symbol, "GRPC")
    expect_equal(treasury_account_id, "0.0.1234")
    expect_equal(initial_supply, 25)
    expect_equal(decimals, 2)
    expect_equal(token_type, "FUNGIBLE_COMMON")
    expect_equal(max_fee, 1e6)
    expect_false(wait_for_receipt)
    response
  })

  expect_equal(tbl$token_id, "0.0.7002")
  expect_equal(tbl$status, "OK")
  expect_equal(tbl$receipt_status, "SUCCESS")
  expect_equal(tbl$transaction_id, "0.0.6001-1700000000-000000001")
  expect_equal(tbl$response, list(response))
})

test_that("tokens_associate normalises identifiers and parses responses", {
  cfg <- hadeda_config(network = "testnet")
  response <- list(
    transactionId = "0.0.1111-1700000000-000000002",
    status = "OK",
    receipt = list(status = "SUCCESS")
  )

  with_mocked_bindings({
    tbl <- tokens_associate(
      cfg,
      account_id = "0.0.2002",
      token_ids = data.frame(token_id = c("0.0.7001", "0.0.7002")),
      memo = "associate",
      max_fee = 123,
      .transport = "grpc"
    )
  }, hadeda_grpc_tokens_associate = function(config,
                                              account_id,
                                              token_ids,
                                              memo,
                                              max_fee,
                                              wait_for_receipt) {
    expect_equal(account_id, "0.0.2002")
    expect_equal(token_ids, c("0.0.7001", "0.0.7002"))
    expect_equal(memo, "associate")
    expect_equal(max_fee, 123)
    expect_true(wait_for_receipt)
    response
  })

  expect_equal(tbl$account_id, "0.0.2002")
  expect_equal(tbl$token_ids[[1]], c("0.0.7001", "0.0.7002"))
  expect_equal(tbl$receipt_status, "SUCCESS")
  expect_equal(tbl$response, list(response))
})

test_that("tokens_transfer accepts tidy transfer specs", {
  cfg <- hadeda_config(network = "testnet")
  transfer_df <- data.frame(
    token_id = c("0.0.7001", "0.0.7001"),
    account_id = c("0.0.2002", "0.0.2003"),
    amount = c(-50, 50)
  )
  response <- list(
    transactionId = "0.0.1111-1700000000-000000003",
    status = "OK",
    receipt = list(status = "SUCCESS")
  )

  with_mocked_bindings({
    tbl <- tokens_transfer(
      cfg,
      token_transfers = transfer_df,
      memo = "rebalance",
      max_fee = 456,
      wait_for_receipt = FALSE,
      .transport = "grpc"
    )
  }, hadeda_grpc_tokens_transfer = function(config,
                                             token_transfers,
                                             memo,
                                             max_fee,
                                             wait_for_receipt) {
    expect_equal(length(token_transfers), 2)
    expect_equal(token_transfers[[1]]$token_id, "0.0.7001")
    expect_equal(token_transfers[[1]]$amount, -50)
    expect_equal(memo, "rebalance")
    expect_equal(max_fee, 456)
    expect_false(wait_for_receipt)
    response
  })

  expect_equal(tbl$receipt_status, "SUCCESS")
  expect_equal(tbl$token_transfers[[1]][[1]]$account_id, "0.0.2002")
  expect_equal(tbl$response, list(response))
})

test_that("contracts_get parses contract metadata", {
  cfg <- hadeda_config(network = "testnet")
  response <- list(
    contract_id = "0.0.5000",
    admin_key = "0022",
    auto_renew_account_id = "0.0.55",
    created_timestamp = "1672531600.000000000",
    memo = "Demo contract"
  )

  with_mocked_bindings({
    tbl <- contracts_get(cfg, response$contract_id)
  }, hadeda_rest_get = function(config, path, query = list()) {
    expect_identical(path, paste0("contracts/", response$contract_id))
    response
  })

  expect_equal(nrow(tbl), 1)
  expect_equal(tbl$memo, "Demo contract")
  expect_s3_class(tbl$created_timestamp, "POSIXct")
})

test_that("contracts_bytecode returns tibble", {
  cfg <- hadeda_config(network = "testnet")
  response <- list(bytecode = "0x60016000")

  with_mocked_bindings({
    tbl <- contracts_bytecode(cfg, "0.0.5000")
  }, hadeda_rest_get = function(config, path, query = list()) {
    expect_identical(path, "contracts/0.0.5000/bytecode")
    response
  })

  expect_equal(nrow(tbl), 1)
  expect_equal(tbl$bytecode, "0x60016000")
})
