if (getRversion() < "4.4.0") {
  `%||%` <- function(a, b) {
    if (!is.null(a)) a else b
  }
}

.onLoad <- function(libname, pkgname) {
  # y2print holds the print methods for freq_y2 objects.
  requireNamespace("y2print", quietly = TRUE)

  invisible()
}

.onAttach <- function(libname, pkgname) {
  missing_y2print <- !requireNamespace("y2print", quietly = TRUE)
  quiet <- isTRUE(getOption("y2clerk.quiet"))

  if (missing_y2print && interactive() && !quiet) {
    packageStartupMessage(
      cli::format_message(c(
        "i" = "Printing methods for frequency tables now live in {.pkg y2print}.",
        "*" = "Install with {.run pak::pak(\"y2analytics/y2print\")}.",
        " " = "Silence this message with {.code options(y2clerk.quiet = TRUE)}."
      ))
    )
  }

  invisible()
}
