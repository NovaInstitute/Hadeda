test_that("hadeda_find_protobuf_include honours PROTOBUF_INCLUDE", {
  tmp <- tempfile("protobuf-include-")
  dir.create(file.path(tmp, "google", "protobuf"), recursive = TRUE)

  old_env <- Sys.getenv("PROTOBUF_INCLUDE", unset = NA_character_)
  on.exit({
    if (is.na(old_env)) {
      Sys.unsetenv("PROTOBUF_INCLUDE")
    } else {
      Sys.setenv(PROTOBUF_INCLUDE = old_env)
    }
  }, add = TRUE)

  Sys.setenv(PROTOBUF_INCLUDE = tmp)

  paths <- hadeda_find_protobuf_include()

  expect_true(tmp %in% paths)
})
