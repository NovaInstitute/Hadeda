test_that("file_create normalises inputs and parses responses", {
  cfg <- hadeda_config(network = "testnet")
  response <- list(
    transactionId = "0.0.5005-1700000000-000000000",
    status = "OK",
    receipt = list(fileId = "0.0.8001", status = "SUCCESS")
  )

  expiry <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  key_df <- data.frame(threshold = 1, key = I(list(list(ed25519 = "abcd"))))

  with_mocked_bindings({
    tbl <- file_create(
      cfg,
      contents = "hello",
      keys = key_df,
      memo = "demo",
      expiration_time = expiry,
      max_fee = 12345,
      wait_for_receipt = FALSE,
      .transport = "grpc"
    )
  }, hadeda_grpc_file_create = function(config,
                                         contents,
                                         keys,
                                         memo,
                                         expiration_time,
                                         max_fee,
                                         wait_for_receipt) {
    expect_true(is.list(config))
    expect_true(is.raw(contents))
    expect_equal(rawToChar(contents), "hello")
    expect_equal(keys[[1]]$threshold, 1)
    expect_s3_class(expiration_time, "POSIXt")
    expect_equal(memo, "demo")
    expect_equal(max_fee, 12345)
    expect_false(wait_for_receipt)
    response
  })

  expect_equal(tbl$file_id, "0.0.8001")
  expect_equal(rawToChar(tbl$contents[[1]]), "hello")
  expect_equal(tbl$memo, "demo")
  expect_s3_class(tbl$expiration_time, "POSIXct")
  expect_equal(tbl$keys[[1]][[1]]$threshold, 1)
})

test_that("file_update requires at least one change", {
  cfg <- hadeda_config(network = "testnet")
  expect_error(
    file_update(cfg, file_id = "0.0.8001", .transport = "grpc"),
    "Provide at least one"
  )
})

test_that("file_update delegates to gRPC", {
  cfg <- hadeda_config(network = "testnet")
  response <- list(
    transactionId = "0.0.5005-1700000000-000000001",
    status = "OK",
    receipt = list(status = "SUCCESS")
  )

  with_mocked_bindings({
    tbl <- file_update(
      cfg,
      file_id = "0.0.8001",
      contents = as.raw(1:3),
      memo = "updated",
      .transport = "grpc"
    )
  }, hadeda_grpc_file_update = function(config,
                                         file_id,
                                         contents,
                                         keys,
                                         memo,
                                         expiration_time,
                                         max_fee,
                                         wait_for_receipt) {
    expect_equal(file_id, "0.0.8001")
    expect_equal(as.integer(contents), 1:3)
    expect_equal(memo, "updated")
    expect_null(keys)
    expect_null(expiration_time)
    expect_true(wait_for_receipt)
    response
  })

  expect_equal(tbl$file_id, "0.0.8001")
  expect_equal(as.integer(tbl$contents[[1]]), 1:3)
  expect_equal(tbl$memo, "updated")
})

test_that("file_append normalises payloads", {
  cfg <- hadeda_config(network = "testnet")
  response <- list(
    transactionId = "0.0.5005-1700000000-000000002",
    status = "OK",
    receipt = list(status = "SUCCESS")
  )

  with_mocked_bindings({
    tbl <- file_append(
      cfg,
      file_id = "0.0.8001",
      contents = c(104, 105),
      .transport = "grpc"
    )
  }, hadeda_grpc_file_append = function(config,
                                         file_id,
                                         contents,
                                         max_fee,
                                         wait_for_receipt) {
    expect_equal(file_id, "0.0.8001")
    expect_true(is.raw(contents))
    expect_equal(rawToChar(contents), "hi")
    expect_null(max_fee)
    expect_true(wait_for_receipt)
    response
  })

  expect_equal(rawToChar(tbl$contents[[1]]), "hi")
})

test_that("file_delete returns acknowledgement", {
  cfg <- hadeda_config(network = "testnet")
  response <- list(
    transactionId = "0.0.5005-1700000000-000000003",
    status = "OK",
    receipt = list(status = "SUCCESS")
  )

  with_mocked_bindings({
    tbl <- file_delete(
      cfg,
      file_id = "0.0.8001",
      .transport = "grpc"
    )
  }, hadeda_grpc_file_delete = function(config,
                                         file_id,
                                         max_fee,
                                         wait_for_receipt) {
    expect_equal(file_id, "0.0.8001")
    expect_true(wait_for_receipt)
    response
  })

  expect_equal(tbl$file_id, "0.0.8001")
})

test_that("file_content parses payloads", {
  cfg <- hadeda_config(network = "testnet")
  response <- list(
    file_id = "0.0.8001",
    contents = charToRaw("hello")
  )

  with_mocked_bindings({
    tbl <- file_content(cfg, file_id = "0.0.8001", .transport = "grpc")
  }, hadeda_grpc_file_content = function(config, file_id) {
    expect_equal(file_id, "0.0.8001")
    response
  })

  expect_equal(tbl$file_id, "0.0.8001")
  expect_equal(rawToChar(tbl$contents[[1]]), "hello")
  expect_equal(tbl$size, 5)
})

test_that("file_info parses metadata", {
  cfg <- hadeda_config(network = "testnet")
  response <- list(
    fileInfo = list(
      fileId = "0.0.8001",
      size = 10,
      memo = "example",
      deleted = FALSE,
      ledgerId = "00",
      keys = list(list(ed25519 = "abcd")),
      expirationTime = list(seconds = 1700000000, nanos = 0)
    )
  )

  with_mocked_bindings({
    tbl <- file_info(cfg, file_id = "0.0.8001", .transport = "grpc")
  }, hadeda_grpc_file_info = function(config, file_id) {
    expect_equal(file_id, "0.0.8001")
    response
  })

  expect_equal(tbl$file_id, "0.0.8001")
  expect_equal(tbl$size, 10)
  expect_equal(tbl$memo, "example")
  expect_false(tbl$deleted)
  expect_equal(tbl$ledger_id, "00")
  expect_s3_class(tbl$expiration_time, "POSIXct")
  expect_equal(tbl$keys[[1]][[1]]$ed25519, "abcd")
})
