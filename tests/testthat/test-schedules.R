test_that("hadeda_parse_schedules handles empty input", {
  tbl <- hadeda_parse_schedules(list())
  expect_s3_class(tbl, "tbl_df")
  expect_equal(nrow(tbl), 0)
  expect_setequal(names(tbl), c(
    "schedule_id",
    "transaction_id",
    "scheduled_transaction_id",
    "creator_account_id",
    "payer_account_id",
    "consensus_timestamp",
    "executed_timestamp",
    "expiration_time",
    "deletion_timestamp",
    "wait_for_expiry",
    "memo",
    "admin_key",
    "signatures",
    "transaction_body",
    "ledger_id",
    "deleted"
  ))
})

test_that("hadeda_parse_schedules normalises records", {
  records <- list(
    list(
      schedule_id = "0.0.8001",
      transactionId = "0.0.5005-1700000000-000000000",
      scheduledTransactionId = "0.0.5005-1700000001-000000000",
      creatorAccountId = "0.0.1001",
      payer_account_id = "0.0.2002",
      consensusTimestamp = "1700000000.000000123",
      executed_timestamp = "1700000001.000000000",
      expirationTime = "1700000100.000000000",
      deletionTimestamp = "1700000200.000000000",
      wait_for_expiry = TRUE,
      memo = "demo",
      transaction_body = "AAAA",
      signatures = list(
        list(
          public_key_prefix = "abcd",
          signature = "0102",
          type = "ED25519",
          status = "SUCCESS"
        )
      ),
      ledgerId = "00",
      deleted = FALSE
    )
  )

  tbl <- hadeda_parse_schedules(records)
  expect_equal(tbl$schedule_id, "0.0.8001")
  expect_equal(tbl$transaction_id, "0.0.5005-1700000000-000000000")
  expect_equal(tbl$scheduled_transaction_id, "0.0.5005-1700000001-000000000")
  expect_equal(tbl$creator_account_id, "0.0.1001")
  expect_equal(tbl$payer_account_id, "0.0.2002")
  expect_true(tbl$wait_for_expiry)
  expect_equal(tbl$memo, "demo")
  expect_false(tbl$deleted)
  expect_s3_class(tbl$consensus_timestamp, "POSIXct")
  expect_s3_class(tbl$executed_timestamp, "POSIXct")
  expect_s3_class(tbl$expiration_time, "POSIXct")
  expect_s3_class(tbl$deletion_timestamp, "POSIXct")
  expect_s3_class(tbl$signatures[[1]], "tbl_df")
  expect_equal(tbl$signatures[[1]]$type, "ED25519")
})

test_that("schedules_list builds REST queries", {
  cfg <- hadeda_config(network = "testnet")
  response <- list(
    list(schedules = list(list(schedule_id = "0.0.8001")))
  )

  with_mocked_bindings({
    tbl <- schedules_list(
      cfg,
      limit = 5,
      order = "asc",
      schedule_id = "0.0.8001",
      transaction_id = "0.0.5005-1700000000-000000000",
      scheduled_timestamp = "gt:1700000000.000000000",
      executed_timestamp = "lte:1700000001.000000000",
      deleted = TRUE,
      timestamp = "lt:1700000100.000000000"
    )
  }, hadeda_rest_paginate = function(config, path, query) {
    expect_equal(path, "schedules")
    expect_equal(query$limit, 5)
    expect_equal(query$order, "asc")
    expect_equal(query$`schedule.id`, "0.0.8001")
    expect_equal(query$transactionId, "0.0.5005-1700000000-000000000")
    expect_equal(query$scheduledTimestamp, "gt:1700000000.000000000")
    expect_equal(query$executedTimestamp, "lte:1700000001.000000000")
    expect_true(query$deleted)
    expect_equal(query$timestamp, "lt:1700000100.000000000")
    response
  })

  expect_equal(tbl$schedule_id, "0.0.8001")
})

test_that("schedules_get fetches details", {
  cfg <- hadeda_config(network = "testnet")
  response <- list(schedule = list(schedule_id = "0.0.8001", memo = "demo"))

  with_mocked_bindings({
    tbl <- schedules_get(cfg, schedule_id = "0.0.8001")
  }, hadeda_rest_get = function(config, path) {
    expect_equal(path, "schedules/0.0.8001")
    response
  })

  expect_equal(tbl$schedule_id, "0.0.8001")
  expect_equal(tbl$memo, "demo")
})

test_that("schedule_create normalises inputs", {
  cfg <- hadeda_config(network = "testnet")
  response <- list(
    transactionId = "0.0.5005-1700000000-000000000",
    status = "OK",
    receipt = list(scheduleId = "0.0.8001")
  )

  payload <- list(transaction = "body")
  expiry <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")

  with_mocked_bindings({
    tbl <- schedule_create(
      cfg,
      scheduled_transaction = payload,
      payer_account_id = "0.0.2002",
      admin_key = list(threshold = 1),
      memo = "demo",
      wait_for_expiry = FALSE,
      expiration_time = expiry,
      max_fee = 1000,
      wait_for_receipt = FALSE,
      .transport = "grpc"
    )
  }, hadeda_grpc_schedule_create = function(config,
                                             scheduled_transaction,
                                             payer_account_id,
                                             admin_key,
                                             memo,
                                             wait_for_expiry,
                                             expiration_time,
                                             max_fee,
                                             wait_for_receipt) {
    expect_true(is.list(config))
    expect_equal(scheduled_transaction, payload)
    expect_equal(payer_account_id, "0.0.2002")
    expect_equal(admin_key$threshold, 1)
    expect_equal(memo, "demo")
    expect_false(wait_for_expiry)
    expect_s3_class(expiration_time, "POSIXct")
    expect_equal(max_fee, 1000)
    expect_false(wait_for_receipt)
    response
  })

  expect_equal(tbl$schedule_id, "0.0.8001")
  expect_equal(tbl$payer_account_id, "0.0.2002")
  expect_equal(tbl$memo, "demo")
  expect_s3_class(tbl$expiration_time, "POSIXct")
  expect_equal(tbl$scheduled_transaction[[1]], payload)
})

test_that("schedule_sign delegates to gRPC", {
  cfg <- hadeda_config(network = "testnet")
  response <- list(transactionId = "0.0.5005-1700000000-000000001", status = "OK")
  signatures <- list(list(public_key_prefix = "abcd", signature = "0102"))

  with_mocked_bindings({
    tbl <- schedule_sign(
      cfg,
      schedule_id = "0.0.8001",
      signatures = signatures,
      max_fee = 2000,
      wait_for_receipt = FALSE,
      .transport = "grpc"
    )
  }, hadeda_grpc_schedule_sign = function(config,
                                          schedule_id,
                                          signatures,
                                          max_fee,
                                          wait_for_receipt) {
    expect_equal(schedule_id, "0.0.8001")
    expect_equal(signatures[[1]]$public_key_prefix, "abcd")
    expect_equal(max_fee, 2000)
    expect_false(wait_for_receipt)
    response
  })

  expect_equal(tbl$schedule_id, "0.0.8001")
  expect_equal(tbl$signatures[[1]][[1]]$public_key_prefix, "abcd")
})

test_that("schedule_delete returns acknowledgement", {
  cfg <- hadeda_config(network = "testnet")
  response <- list(transactionId = "0.0.5005-1700000000-000000002", status = "OK")

  with_mocked_bindings({
    tbl <- schedule_delete(
      cfg,
      schedule_id = "0.0.8001",
      max_fee = 3000,
      wait_for_receipt = FALSE,
      .transport = "grpc"
    )
  }, hadeda_grpc_schedule_delete = function(config,
                                            schedule_id,
                                            max_fee,
                                            wait_for_receipt) {
    expect_equal(schedule_id, "0.0.8001")
    expect_equal(max_fee, 3000)
    expect_false(wait_for_receipt)
    response
  })

  expect_equal(tbl$schedule_id, "0.0.8001")
})

test_that("schedule_info parses metadata", {
  cfg <- hadeda_config(network = "testnet")
  response <- list(
    scheduleInfo = list(
      scheduleId = "0.0.8001",
      creatorAccountId = "0.0.1001",
      payerAccountId = "0.0.2002",
      expirationTime = list(seconds = 1700000100, nanos = 0),
      memo = "demo",
      waitForExpiry = FALSE,
      signers = list(list(ed25519 = "abcd"))
    )
  )

  with_mocked_bindings({
    tbl <- schedule_info(cfg, schedule_id = "0.0.8001", .transport = "grpc")
  }, hadeda_grpc_schedule_info = function(config, schedule_id) {
    expect_equal(schedule_id, "0.0.8001")
    response
  })

  expect_equal(tbl$schedule_id, "0.0.8001")
  expect_equal(tbl$creator_account_id, "0.0.1001")
  expect_equal(tbl$payer_account_id, "0.0.2002")
  expect_equal(tbl$memo, "demo")
  expect_false(tbl$wait_for_expiry)
  expect_s3_class(tbl$expiration_time, "POSIXct")
  expect_equal(tbl$signatures[[1]][[1]]$ed25519, "abcd")
  expect_equal(tbl$response[[1]], response)
})
