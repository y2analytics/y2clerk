# y2clerk-options -------------------------------------------------------

#' y2clerk package options
#'
#' @description
#' y2clerk's behavior can be controlled with the following global options,
#' set via [base::options()]. Each option has a built-in default that is used
#' when the option is unset.
#'
#' # Options
#'
#' ## `y2clerk.quiet`
#'
#' **Default:** `FALSE`
#'
#' Whether to suppress y2clerk's informational messages (e.g. progress
#' notes from [multi_freqs()] and [sig_test_y2()], or the hints emitted by
#' [freqs()] about ignored arguments). Warnings and errors are unaffected.
#' Set to `TRUE` to silence informational output.
#'
#' ```r
#' # Silence informational messages
#' options(y2clerk.quiet = TRUE)
#' ```
#'
#' ## `y2clerk.mcc_correction`
#'
#' **Default:** `"fdr"`
#'
#' The multiple comparison correction algorithm passed to [stats::p.adjust()]
#' in [sig_test_y2()]. Must be one of:
#' `r paste(stats::p.adjust.methods, collapse = ", ")`.
#'
#' ```r
#' # Use Bonferroni correction instead of the default FDR
#' options(y2clerk.mcc_correction = "bonferroni")
#' ```
#'
#' ## `y2clerk.quantile_algorithm`
#'
#' **Default:** `"hf8"`
#'
#' The quantile algorithm (`qrule`) passed to [survey::svyquantile()] when
#' `freqs()` is called with `stat = "quantile"` and a weight variable.
#' See [survey::svyquantile()] for the full list of supported rules (e.g.
#' `"hf1"` through `"hf9"`, `"math"`, `"school"`).
#'
#' ```r
#' # Use the "school" quantile rule
#' options(y2clerk.quantile_algorithm = "school")
#' ```
#'
#' @name y2clerk-options
NULL
