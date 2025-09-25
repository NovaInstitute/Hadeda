test_that("crypto_create_account falls back to REST", {
  cfg <- hadeda_config(network = "testnet")
  cfg$default_transport <- "rest"

  fake_response <- tibble::tibble(
    account = "0.0.4001",
    evm_address = "0xabc",
    public_key = "302a300506032b6570032100",
    private_key = NA_character_,
    mnemonic = list(c("able", "baker", "charlie")),
    response = list(list(accountId = "0.0.4001"))
  )

  with_mocked_bindings({
    tbl <- crypto_create_account(
      cfg,
      initial_balance = 10,
      memo = "rest-path"
    )
  }, accounts_create = function(config,
                                 initial_balance,
                                 alias,
                                 key_type,
                                 memo,
                                 .transport) {
    expect_identical(.transport, "rest")
    expect_equal(initial_balance, 10)
    expect_equal(memo, "rest-path")
    fake_response
  })

  expect_s3_class(tbl, "tbl_df")
  expect_equal(tbl$account_id, "0.0.4001")
  expect_true(all(is.na(tbl$transaction_id)))
  expect_equal(length(tbl$metadata), 1)
  expect_equal(tbl$metadata[[1]]$evm_address, "0xabc")
})

test_that("crypto_create_account delegates to gRPC handler", {
  cfg <- hadeda_config(network = "testnet")
  cfg$default_transport <- "grpc"

  response <- list(
    transaction_id = "0.0.5005-1700000000-000000000",
    status = "OK",
    receipt = list(status = "SUCCESS", accountId = "0.0.5005"),
    consensusTimestamp = "1700000000.000000000",
    accountInfo = list(key = list("_type" = "ED25519", key = "abcd"))
  )

  calls <- list()
  with_mocked_bindings({
    tbl <- crypto_create_account(
      cfg,
      public_key = "302a",
      initial_balance = 25,
      alias = "alias",
      memo = "memo"
    )
  }, hadeda_grpc_crypto_create_account = function(config,
                                                   public_key,
                                                   initial_balance,
                                                   alias,
                                                   key_type,
                                                   memo,
                                                   auto_renew_period,
                                                   key_list,
                                                   wait_for_record) {
    calls[[length(calls) + 1]] <<- list(
      public_key = public_key,
      initial_balance = initial_balance,
      alias = alias,
      memo = memo,
      wait_for_record = wait_for_record
    )
    response
  })

  expect_equal(length(calls), 1)
  expect_equal(calls[[1]]$public_key, "302a")
  expect_equal(calls[[1]]$initial_balance, 25)
  expect_equal(calls[[1]]$alias, "alias")
  expect_s3_class(tbl, "tbl_df")
  expect_equal(tbl$account_id, "0.0.5005")
  expect_equal(tbl$status, "OK")
  expect_equal(tbl$receipt_status, "SUCCESS")
  expect_s3_class(tbl$consensus_timestamp, "POSIXct")
  expect_equal(tbl$metadata[[1]]$key$`_type`, "ED25519")
})

test_that("crypto_transfer normalises transfer payloads", {
  cfg <- hadeda_config(network = "testnet")
  cfg$default_transport <- "grpc"

  transfers <- tibble::tibble(
    account_id = c("0.0.1", "0.0.2"),
    amount = c(-100, 100)
  )
  token_transfers <- tibble::tibble(
    token_id = "0.0.3003",
    account_id = c("0.0.1", "0.0.2"),
    amount = c(-10, 10)
  )

  response <- list(
    transaction_id = "0.0.2-1700000000-000000000",
    status = "OK",
    receipt = list(status = "SUCCESS"),
    consensusTimestamp = "1700000000.000000001"
  )

  observed <- list()
  with_mocked_bindings({
    tbl <- crypto_transfer(
      cfg,
      transfers = transfers,
      token_transfers = token_transfers,
      memo = "memo",
      max_fee = 1000,
      transaction_valid_duration = 120
    )
  }, hadeda_grpc_crypto_transfer = function(config,
                                             transfers,
                                             token_transfers,
                                             memo,
                                             transaction_valid_duration,
                                             max_fee,
                                             wait_for_receipt) {
    observed[[length(observed) + 1]] <<- list(
      transfers = transfers,
      token_transfers = token_transfers,
      memo = memo,
      transaction_valid_duration = transaction_valid_duration,
      max_fee = max_fee,
      wait_for_receipt = wait_for_receipt
    )
    response
  })

  expect_equal(length(observed), 1)
  expect_equal(length(observed[[1]]$transfers), 2)
  expect_equal(observed[[1]]$transfers[[1]]$account_id, "0.0.1")
  expect_equal(observed[[1]]$token_transfers[[1]]$token_id, "0.0.3003")
  expect_equal(tbl$status, "OK")
  expect_true(tbl$receipt_status == "SUCCESS")
  expect_equal(length(tbl$transfers[[1]]), 2)
})

test_that("crypto_transfer validates required transfers", {
  cfg <- hadeda_config(network = "testnet")
  cfg$default_transport <- "grpc"
  expect_error(crypto_transfer(cfg, transfers = NULL), "must be supplied")
})

test_that("crypto_update_account_keys delegates to handler", {
  cfg <- hadeda_config(network = "testnet")
  cfg$default_transport <- "grpc"

  response <- list(
    transaction_id = "0.0.2-1700000000-000000000",
    status = "OK",
    receipt = list(status = "SUCCESS"),
    consensusTimestamp = "1700000000.000000010"
  )

  captured <- list()
  with_mocked_bindings({
    tbl <- crypto_update_account_keys(
      cfg,
      account_id = "0.0.1234",
      public_key = "abcd",
      memo = "memo"
    )
  }, hadeda_grpc_crypto_update_account_keys = function(config,
                                                        account_id,
                                                        public_key,
                                                        key_list,
                                                        memo,
                                                        wait_for_receipt) {
    captured[[length(captured) + 1]] <<- list(
      account_id = account_id,
      public_key = public_key,
      memo = memo,
      wait_for_receipt = wait_for_receipt
    )
    response
  })

  expect_equal(length(captured), 1)
  expect_equal(captured[[1]]$account_id, "0.0.1234")
  expect_equal(tbl$account_id, "0.0.1234")
  expect_equal(tbl$status, "OK")
  expect_equal(tbl$receipt_status, "SUCCESS")
  expect_s3_class(tbl$consensus_timestamp, "POSIXct")
})

test_that("crypto_update_account_keys validates inputs", {
  cfg <- hadeda_config(network = "testnet")
  cfg$default_transport <- "grpc"
  expect_error(crypto_update_account_keys(cfg, public_key = "key"), "account_id")
  expect_error(
    crypto_update_account_keys(cfg, account_id = "0.0.1"),
    "Supply either"
  )
})
