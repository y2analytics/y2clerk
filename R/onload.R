if (getRversion() < "4.4.0") {
  `%||%` <- function(a, b) {
    if (!is.null(a)) a else b
  }
}