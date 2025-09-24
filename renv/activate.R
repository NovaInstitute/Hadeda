local({
  frame <- sys.frame(1)
  project <- if (exists("ofile", frame, inherits = FALSE)) {
    dirname(frame$ofile)
  } else {
    getwd()
  }
  if (!requireNamespace("renv", quietly = TRUE)) {
    install.packages("renv")
  }
  renv::activate(project = project)
})
