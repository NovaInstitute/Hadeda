test_that("hadeda_require_grpc validates configuration", {
  cfg <- hadeda_config(network = "testnet")
  expect_equal(hadeda:::hadeda_require_grpc(cfg)$host, cfg$grpc$host)

  broken <- cfg
  broken$grpc <- list()
  expect_error(hadeda:::hadeda_require_grpc(broken), "gRPC configuration")
})

test_that("consensus_submit_message validates transport", {
  cfg <- hadeda_config(network = "testnet")
  expect_error(consensus_submit_message(cfg, "0.0.1000", "hi"), "gRPC transport")
})

test_that("consensus_submit_message supports single chunk submissions", {
  cfg <- hadeda_config(network = "testnet")
  cfg$default_transport <- "grpc"

  payload <- "hello"
  response <- list(
    transaction_id = "0.0.1000-1700000000-000000000",
    status = "OK",
    receipt = list(status = "SUCCESS"),
    sequenceNumber = 42,
    consensusTimestamp = "1700000000.000000000"
  )

  calls <- list()
  with_mocked_bindings({
    tbl <- consensus_submit_message(
      cfg,
      topic_id = "0.0.2000",
      message = payload,
      message_type = "text",
      chunk_size = 1024L
    )
  }, hadeda_grpc_consensus_submit_message = function(config,
                                                      topic_id,
                                                      payload,
                                                      memo,
                                                      chunk_info,
                                                      wait_for_receipt) {
    calls[[length(calls) + 1]] <<- list(
      topic_id = topic_id,
      payload = payload,
      memo = memo,
      chunk_info = chunk_info,
      wait_for_receipt = wait_for_receipt
    )
    response
  })

  expect_equal(length(calls), 1)
  expect_equal(calls[[1]]$topic_id, "0.0.2000")
  expect_true(is.raw(calls[[1]]$payload))
  expect_null(calls[[1]]$chunk_info)

  expect_s3_class(tbl, "tbl_df")
  expect_equal(nrow(tbl), 1)
  expect_equal(tbl$chunk_number, 1)
  expect_equal(tbl$transaction_id, response$transaction_id)
  expect_equal(tbl$status, response$status)
  expect_equal(tbl$receipt_status, "SUCCESS")
  expect_true(tbl$acknowledged)
  expect_equal(tbl$sequence_number, 42)
  expect_s3_class(tbl$consensus_timestamp, "POSIXct")
})

test_that("consensus_submit_message chunks large payloads", {
  cfg <- hadeda_config(network = "testnet")
  cfg$default_transport <- "grpc"
  cfg$grpc$operator_account_id <- "0.0.5005"

  payload <- paste(rep("a", 8000), collapse = "")
  responses <- list(
    list(
      transaction_id = "0.0.5005-1700000000-000000000",
      status = "OK",
      receipt = list(status = "SUCCESS", sequenceNumber = 1),
      consensusTimestamp = "1700000000.000000001"
    ),
    list(
      transaction_id = "0.0.5005-1700000001-000000000",
      status = "OK",
      receipt = list(status = "SUCCESS", sequenceNumber = 2),
      consensusTimestamp = "1700000001.000000000"
    )
  )

  observed_chunks <- list()
  with_mocked_bindings({
    tbl <- consensus_submit_message(
      cfg,
      topic_id = "0.0.6006",
      message = payload,
      message_type = "text",
      chunk_size = 4096L
    )
  }, hadeda_generate_transaction_id = function(config) {
    "0.0.5005-1700000000-000000000"
  }, hadeda_grpc_consensus_submit_message = function(config,
                                                     topic_id,
                                                     payload,
                                                     memo,
                                                     chunk_info,
                                                     wait_for_receipt) {
    observed_chunks[[length(observed_chunks) + 1]] <<- list(
      topic_id = topic_id,
      payload = payload,
      chunk_info = chunk_info
    )
    responses[[length(observed_chunks)]]
  })

  expect_equal(length(observed_chunks), 2)
  expect_true(all(vapply(observed_chunks, function(call) is.raw(call$payload), logical(1))))
  expect_true(all(vapply(observed_chunks, function(call) {
    length(call$payload) <= 4096
  }, logical(1))))
  expect_true(all(vapply(seq_along(observed_chunks), function(i) {
    call <- observed_chunks[[i]]
    call$chunk_info$number == i &&
      call$chunk_info$total == length(observed_chunks) &&
      identical(call$chunk_info$initial_transaction_id, "0.0.5005-1700000000-000000000")
  }, logical(1))))

  expect_equal(nrow(tbl), 2)
  expect_equal(tbl$chunk_number, 1:2)
  expect_true(all(tbl$acknowledged))
  expect_equal(tbl$sequence_number, c(1, 2))
})

test_that("consensus_submit_message_chunk forwards chunk metadata", {
  cfg <- hadeda_config(network = "testnet")
  cfg$default_transport <- "grpc"

  response <- list(
    transaction_id = "0.0.1111-1700000000-000000000",
    status = "OK",
    receipt = list(status = "SUCCESS", sequenceNumber = 5),
    consensusTimestamp = "1700000000.000000123"
  )

  observed <- NULL
  with_mocked_bindings({
    tbl <- consensus_submit_message_chunk(
      cfg,
      topic_id = "0.0.2222",
      message = "chunk",
      chunk_number = 2,
      total_chunks = 3,
      initial_transaction_id = "0.0.1111-1700000000-000000000",
      message_type = "text",
      memo = "memo"
    )
  }, hadeda_grpc_consensus_submit_message = function(config,
                                                      topic_id,
                                                      payload,
                                                      memo,
                                                      chunk_info,
                                                      wait_for_receipt) {
    observed <<- list(
      topic_id = topic_id,
      payload = payload,
      memo = memo,
      chunk_info = chunk_info,
      wait_for_receipt = wait_for_receipt
    )
    response
  })

  expect_false(is.null(observed))
  expect_equal(observed$topic_id, "0.0.2222")
  expect_true(is.raw(observed$payload))
  expect_equal(observed$memo, "memo")
  expect_equal(observed$chunk_info$number, 2)
  expect_equal(observed$chunk_info$total, 3)
  expect_equal(observed$chunk_info$initial_transaction_id, "0.0.1111-1700000000-000000000")
  expect_true(observed$wait_for_receipt)

  expect_s3_class(tbl, "tbl_df")
  expect_equal(nrow(tbl), 1)
  expect_equal(tbl$chunk_number, 1)
  expect_equal(tbl$receipt_status, "SUCCESS")
  expect_equal(tbl$sequence_number, 5)
})

test_that("consensus_topic_info parses gRPC responses", {
  cfg <- hadeda_config(network = "testnet")
  cfg$default_transport <- "grpc"

  response <- list(
    topic_info = list(
      topicId = "0.0.3333",
      memo = "topic memo",
      autoRenewAccount = "0.0.4444",
      autoRenewPeriod = 789,
      runningHash = "abcd",
      runningHashVersion = 2,
      sequenceNumber = 42,
      deleted = FALSE,
      ledgerId = "02",
      expirationTimestamp = "1700000005.000000000"
    )
  )

  tbl <- with_mocked_bindings({
    consensus_topic_info(cfg, topic_id = "0.0.3333")
  }, hadeda_grpc_consensus_topic_info = function(config, topic_id) {
    expect_equal(topic_id, "0.0.3333")
    response
  })

  expect_s3_class(tbl, "tbl_df")
  expect_equal(nrow(tbl), 1)
  expect_equal(tbl$topic_id, "0.0.3333")
  expect_equal(tbl$memo, "topic memo")
  expect_equal(tbl$auto_renew_account, "0.0.4444")
  expect_equal(tbl$auto_renew_period, 789)
  expect_equal(tbl$running_hash, "abcd")
  expect_equal(tbl$running_hash_version, 2)
  expect_equal(tbl$sequence_number, 42)
  expect_false(tbl$deleted)
  expect_equal(tbl$ledger_id, "02")
  expect_true(inherits(tbl$expiration, "POSIXct"))
  expect_equal(tbl$response[[1]], response)
})
