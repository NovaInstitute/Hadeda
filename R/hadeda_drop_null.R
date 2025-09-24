#' Drop NULL elements from a list
#'
#' Remove entries whose values are `NULL` from a list.
#'
#' @param x A list possibly containing `NULL` elements.
#'
#' @return The list with `NULL` elements removed.
#'
#' @examples
#' hadeda_drop_null(list(a = 1, b = NULL))
#'
#' @keywords internal
hadeda_drop_null <- function(x) {
  if (length(x) == 0) {
    return(x)
  }
  is_null <- vapply(x, is.null, logical(1))
  x[!is_null]
}
