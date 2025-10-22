test_that("hadeda_read_services2 parses proto definitions outside cwd", {
  skip_if_not_installed("RProtoBuf")

  tmp <- tempfile("hadeda-proto")
  dir.create(tmp)
  old <- setwd(tmp)
  on.exit({
    setwd(old)
    unlink(tmp, recursive = TRUE)
  }, add = TRUE)

  dir.create(file.path("nested", "services"), recursive = TRUE)

  proto_path <- file.path("nested", "services", "example.proto")
  writeLines(
    c(
      "syntax = \"proto3\";",
      "package example;",
      "message PingRequest {}",
      "message PingReply {}",
      "service Demo {",
      "  rpc Ping (PingRequest) returns (PingReply);",
      "  rpc Stream (stream PingRequest) returns (stream PingReply);",
      "}",
      "service Admin {",
      "  rpc Empty (PingRequest) returns (PingReply) {};",
      "}"
    ),
    proto_path
  )

  services <- hadeda_read_services2(proto_path)

  expect_setequal(names(services), c("Ping", "Stream", "Empty"))
  expect_equal(services$Ping$RequestType$proto, "example.PingRequest")
  expect_false(services$Ping$RequestType$stream)
  expect_true(services$Stream$ResponseType$stream)
  expect_match(services$Empty$name, "/example.Admin/Empty")
})
