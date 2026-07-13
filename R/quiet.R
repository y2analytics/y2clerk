# quiet ----------------------------------------------------------------------

# Is the package configured to suppress informational messages?
is_quiet <- function() {
  isTRUE(getOption("y2clerk.quiet", default = FALSE))
}

# Emit an informational message via cli, unless `y2clerk.quiet` is TRUE.
# `.envir` is forwarded so glue-style interpolation resolves in the caller.
inform_quiet <- function(message, ..., .envir = parent.frame()) {
  if (is_quiet()) {
    return(invisible())
  }
  cli::cli_inform(message, ..., .envir = .envir)
}
