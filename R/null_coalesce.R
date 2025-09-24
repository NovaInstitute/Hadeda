#' Null coalescing helper
#'
#' Return the left-hand side if it is neither `NULL` nor length zero, otherwise return the right-hand side.
#'
#' @param x,y Values to compare.
#'
#' @return `x` when it is available, otherwise `y`.
#'
#' @examples
#' "value" %||% "fallback"
#' NULL %||% "fallback"
#'
#' @keywords internal
"%||%" <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}
