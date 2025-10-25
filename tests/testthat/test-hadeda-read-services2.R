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
  dir.create(file.path("nested", "includes"), recursive = TRUE)

  proto_path <- file.path("nested", "services", "example.proto")
  writeLines(
    c(
      "syntax = \"proto3\";",
      "package example;",
      "import \"support.proto\";",
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

  writeLines(
    c(
      "syntax = \"proto3\";",
      "package support;",
      "message Placeholder {}"
    ),
    file.path("nested", "includes", "support.proto")
  )

  expect_error(hadeda_read_services2(proto_path))

  services <- hadeda_read_services2(proto_path, proto_path = file.path("nested", "includes"))

  expect_setequal(names(services), c("Ping", "Stream", "Empty"))
  expect_equal(services$Ping$RequestType$proto, "example.PingRequest")
  expect_false(services$Ping$RequestType$stream)
  expect_true(services$Stream$ResponseType$stream)
  expect_match(services$Empty$name, "/example.Admin/Empty")
})

test_that("hadeda_read_services2 ignores multiline block comments", {
  skip_if_not_installed("RProtoBuf")

  tmp <- tempfile("hadeda-proto-block")
  dir.create(tmp)
  old <- setwd(tmp)
  on.exit({
    setwd(old)
    unlink(tmp, recursive = TRUE)
  }, add = TRUE)

  proto_path <- file.path(tmp, "comment.proto")
  writeLines(
    c(
      "syntax = \"proto3\";",
      "package demo;",
      "/*",
      " * Example service description spanning",
      " * multiple lines that previously confused the parser.",
      " */",
      "message PingRequest {}",
      "message PingReply {}",
      "service CommentDemo {",
      "  rpc Ping (PingRequest) returns (PingReply);",
      "}"
    ),
    proto_path
  )

  services <- hadeda_read_services2(proto_path, proto_path = dirname(proto_path))

  expect_named(services, "Ping")
  expect_match(services$Ping$name, "/demo.CommentDemo/Ping")
})
