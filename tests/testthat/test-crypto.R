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

test_that("crypto_create_account rejects unsupported transports", {
  cfg <- hadeda_config(network = "testnet")
  cfg$default_transport <- "rest"
  expect_error(
    crypto_create_account(cfg, public_key = "302a"),
    "requires the gRPC transport"
  )
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

test_that("crypto_update_account delegates to handler", {
  cfg <- hadeda_config(network = "testnet")
  cfg$default_transport <- "grpc"

  response <- list(
    transaction_id = "0.0.2-1700000000-000000010",
    status = "OK",
    receipt = list(status = "SUCCESS"),
    consensusTimestamp = "1700000000.000000050"
  )

  captured <- list()
  with_mocked_bindings({
    tbl <- crypto_update_account(
      cfg,
      account_id = "0.0.1111",
      memo = "memo",
      auto_renew_period = 7776000,
      max_token_associations = 10,
      staked_node_id = "0.0.5",
      staked_account_id = "0.0.6",
      decline_reward = TRUE,
      receiver_sig_required = FALSE
    )
  }, hadeda_grpc_crypto_update_account = function(config,
                                                   account_id,
                                                   memo,
                                                   auto_renew_period,
                                                   max_token_associations,
                                                   staked_node_id,
                                                   staked_account_id,
                                                   decline_reward,
                                                   receiver_sig_required,
                                                   wait_for_receipt) {
    captured[[length(captured) + 1]] <<- list(
      account_id = account_id,
      memo = memo,
      auto_renew_period = auto_renew_period,
      max_token_associations = max_token_associations,
      staked_node_id = staked_node_id,
      staked_account_id = staked_account_id,
      decline_reward = decline_reward,
      receiver_sig_required = receiver_sig_required,
      wait_for_receipt = wait_for_receipt
    )
    response
  })

  expect_equal(length(captured), 1)
  expect_equal(captured[[1]]$account_id, "0.0.1111")
  expect_s3_class(tbl, "tbl_df")
  expect_equal(tbl$account_id, "0.0.1111")
  expect_equal(tbl$receipt_status, "SUCCESS")
})

test_that("crypto_delete requires transfer account", {
  cfg <- hadeda_config(network = "testnet")
  cfg$default_transport <- "grpc"
  expect_error(crypto_delete(cfg, account_id = "0.0.1"), "transfer_account_id")
})

test_that("crypto_delete delegates to handler", {
  cfg <- hadeda_config(network = "testnet")
  cfg$default_transport <- "grpc"

  response <- list(
    transaction_id = "0.0.3-1700000001-000000000",
    status = "OK",
    receipt = list(status = "SUCCESS")
  )

  observed <- list()
  with_mocked_bindings({
    tbl <- crypto_delete(
      cfg,
      account_id = "0.0.2222",
      transfer_account_id = "0.0.3333"
    )
  }, hadeda_grpc_crypto_delete = function(config,
                                           account_id,
                                           transfer_account_id,
                                           wait_for_receipt) {
    observed[[length(observed) + 1]] <<- list(
      account_id = account_id,
      transfer_account_id = transfer_account_id,
      wait_for_receipt = wait_for_receipt
    )
    response
  })

  expect_equal(length(observed), 1)
  expect_equal(observed[[1]]$transfer_account_id, "0.0.3333")
  expect_equal(tbl$transfer_account_id, "0.0.3333")
  expect_equal(tbl$receipt_status, "SUCCESS")
})

test_that("crypto_approve_allowances normalises payload", {
  cfg <- hadeda_config(network = "testnet")
  cfg$default_transport <- "grpc"

  response <- list(
    transaction_id = "0.0.4-1700000002-000000000",
    status = "OK",
    receipt = list(status = "SUCCESS")
  )

  observed <- list()
  with_mocked_bindings({
    tbl <- crypto_approve_allowances(
      cfg,
      hbar_allowances = tibble::tibble(
        spender_account_id = "0.0.100",
        amount = 1000
      ),
      token_allowances = tibble::tibble(
        token_id = "0.0.200",
        spender_account_id = "0.0.101",
        amount = 50
      ),
      nft_allowances = list(list(
        token_id = "0.0.300",
        spender_account_id = "0.0.102",
        serial_numbers = c(1, 2)
      ))
    )
  }, hadeda_grpc_crypto_approve_allowances = function(config,
                                                       hbar_allowances,
                                                       token_allowances,
                                                       nft_allowances,
                                                       wait_for_receipt) {
    observed[[length(observed) + 1]] <<- list(
      hbar_allowances = hbar_allowances,
      token_allowances = token_allowances,
      nft_allowances = nft_allowances,
      wait_for_receipt = wait_for_receipt
    )
    response
  })

  expect_equal(length(observed), 1)
  expect_equal(observed[[1]]$hbar_allowances[[1]]$spender_account_id, "0.0.100")
  expect_equal(observed[[1]]$token_allowances[[1]]$token_id, "0.0.200")
  expect_equal(observed[[1]]$nft_allowances[[1]]$serial_numbers, c(1, 2))
  expect_equal(tbl$receipt_status, "SUCCESS")
})

test_that("crypto_approve_allowances validates input", {
  cfg <- hadeda_config(network = "testnet")
  cfg$default_transport <- "grpc"
  expect_error(crypto_approve_allowances(cfg), "Provide at least one allowance")
})

test_that("crypto_delete_allowances normalises payload", {
  cfg <- hadeda_config(network = "testnet")
  cfg$default_transport <- "grpc"

  response <- list(
    transaction_id = "0.0.5-1700000003-000000000",
    status = "OK",
    receipt = list(status = "SUCCESS")
  )

  observed <- list()
  with_mocked_bindings({
    tbl <- crypto_delete_allowances(
      cfg,
      token_allowances = list(list(
        token_id = "0.0.201",
        spender_account_id = "0.0.301"
      )),
      nft_allowances = tibble::tibble(
        token_id = "0.0.202",
        spender_account_id = "0.0.302",
        serial_numbers = list(c(3, 4))
      )
    )
  }, hadeda_grpc_crypto_delete_allowances = function(config,
                                                      token_allowances,
                                                      nft_allowances,
                                                      wait_for_receipt) {
    observed[[length(observed) + 1]] <<- list(
      token_allowances = token_allowances,
      nft_allowances = nft_allowances,
      wait_for_receipt = wait_for_receipt
    )
    response
  })

  expect_equal(length(observed), 1)
  expect_equal(observed[[1]]$token_allowances[[1]]$token_id, "0.0.201")
  expect_equal(observed[[1]]$nft_allowances[[1]]$serial_numbers, c(3, 4))
  expect_equal(tbl$receipt_status, "SUCCESS")
})

test_that("crypto_delete_allowances validates input", {
  cfg <- hadeda_config(network = "testnet")
  cfg$default_transport <- "grpc"
  expect_error(crypto_delete_allowances(cfg), "Provide at least one allowance")
})

test_that("crypto_account_balance parses response", {
  cfg <- hadeda_config(network = "testnet")
  cfg$default_transport <- "grpc"

  response <- list(
    accountId = "0.0.400",
    balance = 1000,
    timestamp = "1700000004.000000000",
    tokenBalances = list(list(tokenId = "0.0.500", balance = 10))
  )

  with_mocked_bindings({
    tbl <- crypto_account_balance(cfg, account_id = "0.0.400")
  }, hadeda_grpc_crypto_account_balance = function(config, account_id) {
    expect_equal(account_id, "0.0.400")
    response
  })

  expect_s3_class(tbl, "tbl_df")
  expect_equal(tbl$account, "0.0.400")
  expect_equal(tbl$balance, 1000)
  expect_equal(length(tbl$tokens[[1]]), 1)
})

test_that("crypto_account_info parses response", {
  cfg <- hadeda_config(network = "testnet")
  cfg$default_transport <- "grpc"

  response <- list(
    accountId = "0.0.401",
    memo = "memo",
    alias = "alias",
    deleted = FALSE,
    receiverSigRequired = TRUE,
    balance = 2000,
    tokenRelationships = list(list(tokenId = "0.0.10"))
  )

  with_mocked_bindings({
    tbl <- crypto_account_info(cfg, account_id = "0.0.401")
  }, hadeda_grpc_crypto_account_info = function(config, account_id) {
    expect_equal(account_id, "0.0.401")
    response
  })

  expect_equal(tbl$account_id, "0.0.401")
  expect_equal(tbl$memo, "memo")
  expect_true(tbl$receiver_sig_required)
})

test_that("crypto_account_details parses response", {
  cfg <- hadeda_config(network = "testnet")
  cfg$default_transport <- "grpc"

  response <- list(
    accountId = "0.0.402",
    memo = "memo",
    evmAddress = "0xabc",
    stakingInfo = list(nodeId = 3)
  )

  with_mocked_bindings({
    tbl <- crypto_account_details(cfg, account_id = "0.0.402")
  }, hadeda_grpc_crypto_account_details = function(config, account_id) {
    expect_equal(account_id, "0.0.402")
    response
  })

  expect_equal(tbl$account_id, "0.0.402")
  expect_equal(tbl$evm_address, "0xabc")
  expect_equal(tbl$staking[[1]]$nodeId, 3)
})

test_that("crypto_account_records parses response", {
  cfg <- hadeda_config(network = "testnet")
  cfg$default_transport <- "grpc"

  response <- list(
    records = list(
      list(
        transactionId = "0.0.1-1700000005-000000000",
        consensusTimestamp = "1700000005.000000000",
        memo = "memo"
      )
    )
  )

  with_mocked_bindings({
    tbl <- crypto_account_records(cfg, account_id = "0.0.403")
  }, hadeda_grpc_crypto_account_records = function(config, account_id) {
    expect_equal(account_id, "0.0.403")
    response
  })

  expect_equal(nrow(tbl), 1)
  expect_equal(tbl$transaction_id, "0.0.1-1700000005-000000000")
})

test_that("crypto_transaction_receipts parses response", {
  cfg <- hadeda_config(network = "testnet")
  cfg$default_transport <- "grpc"

  response <- list(
    receipts = list(
      list(status = "SUCCESS", accountId = "0.0.500", transactionId = "0.0.2-1-0")
    )
  )

  with_mocked_bindings({
    tbl <- crypto_transaction_receipts(cfg, transaction_id = "0.0.2-1-0")
  }, hadeda_grpc_crypto_transaction_receipts = function(config, transaction_id) {
    expect_equal(transaction_id, "0.0.2-1-0")
    response
  })

  expect_equal(tbl$status, "SUCCESS")
  expect_equal(tbl$account_id, "0.0.500")
})

test_that("crypto_transaction_record parses response", {
  cfg <- hadeda_config(network = "testnet")
  cfg$default_transport <- "grpc"

  response <- list(
    transactionId = "0.0.2-1-1",
    consensusTimestamp = "1700000006.000000000",
    memo = "memo"
  )

  with_mocked_bindings({
    tbl <- crypto_transaction_record(cfg, transaction_id = "0.0.2-1-1")
  }, hadeda_grpc_crypto_transaction_record = function(config, transaction_id) {
    expect_equal(transaction_id, "0.0.2-1-1")
    response
  })

  expect_equal(tbl$transaction_id, "0.0.2-1-1")
  expect_equal(tbl$memo, "memo")
})

test_that("crypto_transaction_records parses response", {
  cfg <- hadeda_config(network = "testnet")
  cfg$default_transport <- "grpc"

  response <- list(
    records = list(
      list(transactionId = "0.0.2-1-2"),
      list(transactionId = "0.0.2-1-3")
    )
  )

  with_mocked_bindings({
    tbl <- crypto_transaction_records(cfg, transaction_id = "0.0.2-1-2")
  }, hadeda_grpc_crypto_transaction_records = function(config, transaction_id) {
    expect_equal(transaction_id, "0.0.2-1-2")
    response
  })

  expect_equal(nrow(tbl), 2)
  expect_equal(tbl$transaction_id[[2]], "0.0.2-1-3")
})

test_that("crypto_livehash stubs raise error", {
  cfg <- hadeda_config(network = "testnet")
  cfg$default_transport <- "grpc"
  expect_error(crypto_livehash_add(cfg, account_id = "0.0.1"), "not supported")
  expect_error(crypto_livehash_delete(cfg, account_id = "0.0.1"), "not supported")
  expect_error(crypto_livehash_get(cfg, account_id = "0.0.1"), "not supported")
})
