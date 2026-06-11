# stem() tidyselect helper -------------------------------------------------------

#' Select columns by stem prefix followed by a numeric suffix
#'
#' A tidyselect helper that matches columns whose names consist of a stem
#' immediately followed by one of the allowed separators and then a digit.
#' Useful for selecting multi-select variable sets (e.g. `Q1_1`, `Q1_2`, ...)
#' without also grabbing unrelated columns that merely start with the same stem.
#'
#' @details
#' `stem()` by default does not select column names who start with the question stem and end with text (QuestionStem_oe or QuestionStem_TEXT).
#' This is intentional. If you need those columns as well, use a plain `tidyselect::starts_with()`.
#' This function differs from `dplyr::select(tidyselect::starts_with('QuestionStem'), -ends_with('Text'))`
#' in the fact that it does not select columns names who start with the question stem, contain other information, then end with a digit.
#'
#' @param stem A string giving the variable stem to match.
#' @param separator A character vector of separator strings allowed between
#'   the stem and the numeric suffix. Defaults to `c("_", "r")`. Use `""`
#'   to allow no separator (bare digits); in that case the column name must
#'   end with the digits (e.g. `Q11`, `Q12`).
#'
#' @return A tidyselect selection, suitable for use inside `dplyr::select()`,
#'   `dplyr::across()`, `y2clerk::freqs()`, etc.
#'
#' @examples
#' df <- data.frame(
#'   Q1_1 = 1, Q1_2 = 2, Q1_3 = 3,
#'   Q10_1 = 4, Q1r1 = 5, Q11 = 6, Q1_TEXT = "open end", other = 7
#' )
#'
#' # Default (underscore or r): Q1_1, Q1_2, Q1_3, Q1r1
#' dplyr::select(df, stem("Q1"))
#'
#' # Underscore only
#' dplyr::select(df, stem("Q1", separator = "_"))
#'
#' @export
stem <- function(stem, separator = c("_", "r"), ignore.case = FALSE) {
  if (!is.character(separator)) {
    cli::cli_abort("{.arg separator} must be a character vector.")
  }

  escaped_stem <- gsub(
    "([.+*?^${}()|\\[\\]\\\\])",
    "\\\\\\1",
    stem,
    perl = TRUE
  )

  parts <- purrr::map_chr(
    separator,
    \(sep) {
      if (sep == "") {
        "\\d+$"
      } else {
        escaped_sep <- gsub(
          "([.+*?^${}()|\\[\\]\\\\])",
          "\\\\\\1",
          sep,
          perl = TRUE
        )
        paste0(escaped_sep, "\\d+$")
      }
    }
  )

  pattern <- if (length(parts) == 1L) {
    paste0("^", escaped_stem, parts)
  } else {
    paste0("^", escaped_stem, "(", paste(parts, collapse = "|"), ")")
  }

  rlang::try_fetch(
    tidyselect::matches(pattern, ignore.case = ignore.case),
    error = function(cnd) {
      cli::cli_abort(
        c(
          "{.fn stem} must be used within a *selecting* function.",
          "i" = "See {.url https://tidyselect.r-lib.org/reference/faq-selection-context.html} for mroe details."
        ),
        call = rlang::caller_env(),
        parent = NA
      )
    }
  )
}


# extract_stem (Used for old multi-freqs interface)

extract_stem <- function(nms) {
  nms |>
    stringr::str_remove("_[0-9]+_TEXT$") |>
    stringr::str_remove("_[0-9]+$") |>
    unique()
}
