test_that("hadeda_parse_topics handles empty input", {
  tbl <- hadeda:::hadeda_parse_topics(list())
  expect_s3_class(tbl, "tbl_df")
  expect_equal(nrow(tbl), 0)
  expect_named(tbl, c(
    "topic_id",
    "memo",
    "auto_renew_account",
    "auto_renew_period",
    "expiration",
    "sequence_number",
    "running_hash",
    "running_hash_version",
    "deleted",
    "admin_key",
    "submit_key",
    "ledger_id"
  ))
})

test_that("hadeda_parse_topics parses fields", {
  record <- list(
    topic_id = "0.0.2000",
    memo = "demo",
    auto_renew_account = "0.0.3",
    auto_renew_period = 7890000,
    expiration_timestamp = "1672531500.000000000",
    sequence_number = 42,
    running_hash = "abcd",
    running_hash_version = 2,
    deleted = FALSE,
    admin_key = list(_type = "ED25519", key = "abc"),
    submit_key = list(_type = "ED25519", key = "def"),
    ledger_id = "0x02"
  )

  tbl <- hadeda:::hadeda_parse_topics(list(record))
  expect_equal(nrow(tbl), 1)
  expect_equal(tbl$topic_id, "0.0.2000")
  expect_equal(tbl$memo, "demo")
  expect_equal(tbl$auto_renew_account, "0.0.3")
  expect_equal(tbl$auto_renew_period, 7890000)
  expect_equal(tbl$sequence_number, 42)
  expect_equal(tbl$running_hash, "abcd")
  expect_equal(tbl$running_hash_version, 2)
  expect_false(tbl$deleted)
  expect_s3_class(tbl$expiration, "POSIXct")
  expect_equal(tbl$admin_key[[1]]$key, "abc")
  expect_equal(tbl$submit_key[[1]]$key, "def")
  expect_equal(tbl$ledger_id, "0x02")
})

test_that("topics_get retrieves topic metadata", {
  cfg <- hadeda_config(network = "testnet")
  topic_id <- "0.0.2000"
  record <- list(
    topic_id = topic_id,
    memo = "demo",
    auto_renew_account = "0.0.3",
    auto_renew_period = 7890000,
    sequence_number = 1,
    running_hash = "abcd"
  )

  with_mocked_bindings({
    tbl <- topics_get(cfg, topic_id)
  }, hadeda_rest_get = function(config, path, query = list()) {
    expect_identical(path, paste0("topics/", topic_id))
    record
  })

  expect_equal(tbl$topic_id, topic_id)
  expect_equal(tbl$memo, "demo")
  expect_equal(tbl$auto_renew_account, "0.0.3")
})

test_that("topics_create submits payload", {
  cfg <- hadeda_config(network = "testnet")
  record <- list(
    topic_id = "0.0.2001",
    memo = "created",
    auto_renew_account = "0.0.3",
    auto_renew_period = 7890000
  )
  response <- list(
    topic = record,
    status = "SUCCESS",
    transactionId = "0.0.1001-1700000000-000000000"
  )

  with_mocked_bindings({
    tbl <- topics_create(
      cfg,
      memo = "created",
      admin_key = list(_type = "ED25519", key = "abc"),
      auto_renew_account = "0.0.3",
      auto_renew_period = 7890000
    )
  }, hadeda_rest_post = function(config, path, body = list()) {
    expect_identical(path, "topics")
    expect_equal(body$memo, "created")
    expect_equal(body$autoRenewAccount, "0.0.3")
    expect_equal(body$autoRenewPeriod, 7890000)
    expect_type(body$adminKey, "list")
    response
  })

  expect_equal(tbl$topic_id, "0.0.2001")
  expect_equal(tbl$status, "SUCCESS")
  expect_equal(tbl$transaction_id, "0.0.1001-1700000000-000000000")
  expect_equal(tbl$response[[1]]$status, "SUCCESS")
})

test_that("topics_update validates payload", {
  cfg <- hadeda_config(network = "testnet")
  expect_error(topics_update(cfg, "0.0.2000"), "At least one field must be supplied")

  updated <- list(
    topic_id = "0.0.2000",
    memo = "updated"
  )
  response <- list(
    topic = updated,
    status = "SUCCESS",
    transactionId = "0.0.1001-1700000000-000000000"
  )

  with_mocked_bindings({
    tbl <- topics_update(cfg, "0.0.2000", memo = "updated")
  }, hadeda_rest_post = function(config, path, body = list()) {
    expect_identical(path, "topics/0.0.2000")
    expect_equal(body$memo, "updated")
    response
  })

  expect_equal(tbl$memo, "updated")
  expect_equal(tbl$status, "SUCCESS")
})

test_that("topics_message_submit submits messages", {
  cfg <- hadeda_config(network = "testnet")
  topic_id <- "0.0.2000"
  response <- list(
    status = "SUCCESS",
    sequenceNumber = 5,
    consensusTimestamp = "1672531600.000000000"
  )

  with_mocked_bindings({
    tbl <- topics_message_submit(cfg, topic_id, message = "hello", message_type = "text", memo = "memo")
  }, hadeda_rest_post = function(config, path, body = list()) {
    expect_identical(path, paste0("topics/", topic_id, "/messages"))
    expect_equal(body$memo, "memo")
    expect_equal(body$message, jsonlite::base64_enc(charToRaw("hello")))
    response
  })

  expect_equal(tbl$topic_id, topic_id)
  expect_equal(tbl$sequence_number, 5)
  expect_equal(tbl$status, "SUCCESS")
  expect_s3_class(tbl$consensus_timestamp, "POSIXct")
})
