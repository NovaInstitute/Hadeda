log_step <- function(message, ...) {
  cli::cli_inform(c("i" = sprintf(message, ...)))
}

find_rpc <- function(client, pattern) {
  matches <- grep(pattern, names(client), value = TRUE, fixed = TRUE)
  if (!length(matches)) {
    available <- paste(names(client), collapse = ", ")
    cli::cli_abort(c(
      "x" = sprintf("RPC matching '%s' not found", pattern),
      "i" = sprintf("Available RPCs: %s", available)
    ))
  }
  client[[matches[[1]]]]
}

proto_message_factory <- function(type) {
  descriptor <- RProtoBuf::P(type)
  function(...) {
    do.call(RProtoBuf::new, c(list(descriptor), list(...)))
  }
}

build_grpc_config <- function(version,
                              target,
                              operator,
                              proto_root,
                              crypto_stubs,
                              query_helpers,
                              sign_transaction) {
  query_wrapper <- query_helpers$query_wrapper
  crypto_get_account_info <- query_helpers$crypto_get_account_info
  query_header <- query_helpers$query_header
  to_account_id <- query_helpers$to_account_id
  transaction <- query_helpers$transaction

  crypto_service <- function(channel) {
    grpc::grpc_client(crypto_stubs, channel)
  }

  destroy_channel <- get0(
    "grpc_channel_destroy",
    envir = asNamespace("grpc"),
    inherits = FALSE
  )
  if (is.null(destroy_channel)) {
    destroy_channel <- get0(
      "channel_destroy",
      envir = asNamespace("grpc"),
      inherits = FALSE
    )
  }

  crypto_cost_query <- function(account_id, channel = NULL) {
    owns_channel <- FALSE
    if (is.null(channel)) {
      owns_channel <- TRUE
      channel <- target
    }

    on.exit({
      if (owns_channel && !is.null(destroy_channel) && inherits(channel, "grpc_channel")) {
        destroy_channel(channel)
      }
    }, add = TRUE)

    client <- crypto_service(channel)
    account_info_rpc <- find_rpc(client, "getAccountInfo")

    cost_query <- query_wrapper(
      cryptoGetAccountInfo = crypto_get_account_info(
        header = query_header(responseType = "COST_ANSWER"),
        accountID = to_account_id(account_id)
      )
    )

    response <- account_info_rpc$call(cost_query)
    as.list(response)
  }

  build_payment <- query_helpers$build_payment

  query_account_info <- function(config, account_id) {
    channel <- target
    owns_channel <- TRUE

    on.exit({
      if (owns_channel && !is.null(destroy_channel) && inherits(channel, "grpc_channel")) {
        destroy_channel(channel)
      }
    }, add = TRUE)

    cost_response <- crypto_cost_query(account_id, channel)
    cost <- cost_response$cryptoGetAccountInfo$responseHeader$cost
    payment <- build_payment(config$grpc$operator, account_id, cost)

    query_message <- query_wrapper(
      cryptoGetAccountInfo = crypto_get_account_info(
        header = query_header(
          responseType = "ANSWER_ONLY",
          payment = transaction(signedTransactionBytes = payment)
        ),
        accountID = to_account_id(account_id)
      )
    )

    client <- crypto_service(channel)
    account_info_rpc <- find_rpc(client, "getAccountInfo")
    response <- account_info_rpc$call(query_message)
    as.list(response)
  }

  grpc_config <- hadeda_config(network = "testnet", default_transport = "grpc")
  grpc_config$grpc <- modifyList(
    grpc_config$grpc,
    list(
      operator_account_id = operator$operator_account_id,
      operator = operator,
      channel = function() target,
      proto_root = proto_root,
      sign_transaction = sign_transaction,
      get_account_info = query_account_info
    ),
    keep.null = TRUE
  )

  grpc_config
}

build_query_helpers <- function(sign_transaction) {
  query_header <- proto_message_factory("proto.QueryHeader")
  account_id_message <- proto_message_factory("proto.AccountID")
  timestamp_message <- proto_message_factory("proto.Timestamp")
  duration_message <- proto_message_factory("proto.Duration")
  transaction_id_message <- proto_message_factory("proto.TransactionID")
  transfer_list_message <- proto_message_factory("proto.TransferList")
  account_amount_message <- proto_message_factory("proto.AccountAmount")
  crypto_transfer_message <- proto_message_factory("proto.CryptoTransferTransactionBody")
  signature_map_message <- proto_message_factory("proto.SignatureMap")
  signature_pair_message <- proto_message_factory("proto.SignaturePair")
  crypto_get_account_info <- proto_message_factory("proto.CryptoGetInfoQuery")
  query_wrapper <- proto_message_factory("proto.Query")
  transaction_body <- proto_message_factory("proto.TransactionBody")
  transaction <- proto_message_factory("proto.Transaction")

  to_account_id <- function(id) {
    pieces <- strsplit(id, "\\.", fixed = TRUE)[[1]]
    account_id_message(
      shardNum = as.integer(pieces[1]),
      realmNum = as.integer(pieces[2]),
      accountNum = as.integer(pieces[3])
    )
  }

  now_timestamp <- function() {
    instant <- as.integer(as.numeric(Sys.time()))
    timestamp_message(seconds = instant, nanos = 0L)
  }

  build_payment <- function(operator, account_id, cost, valid_duration = 120L) {
    pub_key <- hadeda_grpc_signer_public_key(sign_transaction)
    body <- transaction_body(
      transactionID = transaction_id_message(
        accountID = to_account_id(operator$operator_account_id),
        transactionValidStart = now_timestamp()
      ),
      nodeAccountID = to_account_id("0.0.3"),
      transactionFee = cost,
      transactionValidDuration = duration_message(seconds = as.integer(valid_duration)),
      cryptoTransfer = crypto_transfer_message(
        transfers = transfer_list_message(
          accountAmounts = list(
            account_amount_message(
              accountID = to_account_id(operator$operator_account_id),
              amount = -cost
            ),
            account_amount_message(
              accountID = to_account_id("0.0.3"),
              amount = cost
            )
          )
        )
      )
    )

    body_bytes <- RProtoBuf::serialize(body, NULL)
    sig_bytes <- sign_transaction(body_bytes)
    signature_map <- signature_map_message(
      sigPair = list(
        signature_pair_message(
          pubKeyPrefix = pub_key,
          ed25519 = sig_bytes
        )
      )
    )

    signed_transaction <- transaction(
      bodyBytes = body_bytes,
      sigMap = signature_map
    )

    RProtoBuf::serialize(signed_transaction, NULL)
  }

  list(
    query_header = query_header,
    crypto_get_account_info = crypto_get_account_info,
    query_wrapper = query_wrapper,
    to_account_id = to_account_id,
    build_payment = build_payment,
    transaction = transaction
  )
}

perform_diagnostic <- function(version = Sys.getenv("HADEDA_TEST_GRPC_PROTO_VERSION", "0.47.0"),
                               target = Sys.getenv("HADEDA_GRPC_TARGET", "testnet.hedera.com:50211")) {
  skip_on_cran()
  skip_if_not_installed("grpc")
  skip_if_not_installed("RProtoBuf")
  skip_if_not_installed("openssl")
  skip_if(Sys.getenv("HADEDA_OPERATOR_ID") == "", "Environment variable HADEDA_OPERATOR_ID is required")
  skip_if(Sys.getenv("HADEDA_OPERATOR_KEY") == "", "Environment variable HADEDA_OPERATOR_KEY is required")
  skip_if(Sys.getenv("HADEDA_TEST_GRPC_DIAGNOSTIC") == "", "Set HADEDA_TEST_GRPC_DIAGNOSTIC=1 to run gRPC diagnostics")

  log_step("Downloading Hedera protobuf bundle version %s", version)
  bundle_dir <- tempfile("hadeda-grpc-bundle-")
  on.exit(unlink(bundle_dir, recursive = TRUE), add = TRUE)
  proto_root <- hadeda_grpc_use_proto_bundle(dest = bundle_dir, version = version, overwrite = TRUE)
  log_step("Bundle extracted to %s", proto_root)

  services_dir <- file.path(proto_root, "services")
  required <- c("crypto_service.proto", "network_service.proto", "query.proto", "transaction.proto")
  missing <- required[!file.exists(file.path(services_dir, required))]
  expect_length(missing, 0, info = sprintf("Missing proto files: %s", paste(missing, collapse = ", ")))
  log_step("Verified required proto files: %s", paste(required, collapse = ", "))

  proto_paths <- unique(c(proto_root, services_dir, hadeda_find_protobuf_include()))
  log_step("Using proto search paths: %s", paste(proto_paths, collapse = "; "))

  crypto_proto <- file.path(services_dir, "crypto_service.proto")
  network_proto <- file.path(services_dir, "network_service.proto")

  crypto_stubs <- hadeda_read_services2(crypto_proto, proto_path = proto_paths)
  network_stubs <- hadeda_read_services2(network_proto, proto_path = proto_paths)
  log_step("Discovered CryptoService RPCs: %s", paste(names(crypto_stubs), collapse = ", "))
  log_step("Discovered NetworkService RPCs: %s", paste(names(network_stubs), collapse = ", "))

  expect_true(any(grepl("getAccountInfo", names(crypto_stubs), fixed = TRUE)),
    info = "CryptoService stubs must include getAccountInfo")
  expect_true(any(grepl("getVersionInfo", names(network_stubs), fixed = TRUE)),
    info = "NetworkService stubs must include getVersionInfo")

  log_step("Loading operator credentials from environment")
  operator <- hadeda_grpc_env_credentials()
  sign_transaction <- hadeda_grpc_ed25519_signer()
  operator$public_key <- hadeda_grpc_signer_public_key(sign_transaction)
  log_step("Operator %s public key length: %d bytes",
    operator$operator_account_id,
    length(operator$public_key))

  log_step("Building protobuf helper functions")
  query_helpers <- build_query_helpers(sign_transaction)

  log_step("Constructing Hadeda gRPC configuration for target %s", target)
  config <- build_grpc_config(
    version = version,
    target = target,
    operator = operator,
    proto_root = proto_root,
    crypto_stubs = crypto_stubs,
    query_helpers = query_helpers,
    sign_transaction = sign_transaction
  )

  log_step("Requesting account info for %s", operator$operator_account_id)
  info <- crypto_account_info(config = config, account_id = operator$operator_account_id)
  expect_s3_class(info, "tbl_df")
  expect_equal(info$account_id[[1]], operator$operator_account_id)
  log_step("Account info retrieved successfully")

  invisible(info)
}

test_that("gRPC proto bundle diagnostic passes", {
  perform_diagnostic()
})
