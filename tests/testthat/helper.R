if (!"package:hadeda" %in% search()) {
  if (requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all(path = "../..", quiet = TRUE)
  } else {
    stop("pkgload package is required to run these tests.")
  }
}
