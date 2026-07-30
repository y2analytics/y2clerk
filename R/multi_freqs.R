# multi_freqs() --------------------------------------------------------------------

#' Run frequencies for multiple-select variables
#'
#' `multi_freqs()` runs [freqs()] across one or more multiple-select ("select
#' all that apply") question *stems*. For each stem it selects the associated
#' columns with the [stem()] tidyselect helper, drops respondents who answered
#' none of them, then runs `freqs()`.
#'
#' @details
#' Pass the *stem* of each question, not an individual column. For a question
#' stored as `Q1_1`, `Q1_2`, `Q1_3`, pass `Q1`. Stems may be given as bare
#' symbols (`Q1`), strings (`"Q1"`), or spliced in from a character vector with
#' [tidyselect::all_of()] / [tidyselect::any_of()]. If no stems are given,
#' `multi_freqs()` runs on every stem in the dataset.
#'
#' Columns are matched with [stem()], so `_TEXT` / open-ended columns are
#' excluded automatically. If you pass a name that is itself a column in the
#' dataset (e.g. `Q1_1`), `multi_freqs()` warns: the modern interface expects
#' the stem rather than an exemplar column.
#'
#' @param dataset A dataframe.
#' @param .by <tidy-select> Variables to group by for this operation only.
#' Cannot be used when the dataset is already a grouped data frame.
#' @param ... Question stems to tabulate, given as bare symbols (`Q1`), strings
#' (`"Q1"`), or a character vector wrapped in `all_of()` / `any_of()`. If
#' nothing is specified, the function runs on every stem in the dataset.
#' @param separator Character vector of separators allowed between the stem and
#' its numeric suffix, passed through to [stem()] (default: `c("_", "r")`).
#' @param ignore.case Boolean, whether to match the stem case-insensitively,
#' passed through to [stem()] (default: FALSE).
#' @param remove_nas Boolean, after freqs is run (which always includes NAs), whether or not to filter out counts of NA value (default: TRUE).
#' @param wt The unquoted name of a weighting variable in the dataset (default: NULL).
#' @param prompt Boolean, whether or not to include the prompt in the dataset (default: FALSE).
#' @param digits Integer, number of significant digits for rounding (default: 2).
#' @param nas_group Boolean, whether or not to include NA values for the grouping variable in the tabulation (default: TRUE).
#' @param factor_group Boolean, whether or not to convert the grouping variable to a factor and use its labels instead of its underlying numeric values (default: FALSE)
#' @param unweighted_ns Boolean, whether the 'n' column in the freqs table should be UNweighted while results ARE weighted. This argument can only be used if a wt variable is used. If no weight variable is used, the 'n' column will always be unweighted (default: FALSE).
#' @param show_missing_levels Boolean, whether to keep response levels with no data (default: TRUE)
#' @return A dataframe with the variable names, prompts, values, labels, counts,
#' stats, and resulting calculations.
#' @seealso [stem()], [freqs()]
#' @examples
#'
#' df <- tibble::tibble(
#'   a = c(1, 2, 3, 1, 2, 3, 1),
#'   Q1_1 = c(1, NA, 1, 1, NA, 1, NA),
#'   Q1_2 = c(1, 1, NA, 1, NA, 1, NA),
#'   Q1_3 = c(NA, 1, 1, NA, 4, 1, NA),
#'   weights = c(0.9, 0.9, 1.1, 1.1, 1, 1, 1)
#' )
#'
#'
#' # Pass the stem, not an individual column. These give the same output:
#' multi_freqs(df, Q1)
#' df |> multi_freqs(Q1)
#' df |> multi_freqs("Q1")
#'
#'
#' # Splice stems in from a character vector
#' stems <- c("Q1")
#' df |> multi_freqs(tidyselect::all_of(stems))
#'
#'
#' # Grouped example with weights
#' df |>
#'   dplyr::group_by(a) |>
#'   multi_freqs(Q1, wt = weights)
#'
#'
#' # Group for this call only with .by
#' multi_freqs(df, Q1, .by = a)
#'
#' @export

multi_freqs <- function(
  dataset,
  ...,
  .by = NULL,
  remove_nas = TRUE,
  wt = NULL,
  prompt = FALSE,
  digits = 2,
  nas_group = TRUE,
  factor_group = FALSE,
  unweighted_ns = FALSE,
  show_missing_levels = TRUE,
  separator = c("_", "r"),
  ignore.case = FALSE
) {
  check_data_frame2(dataset)
  rlang::check_bool(remove_nas)
  rlang::check_bool(prompt)
  rlang::check_bool(nas_group)
  rlang::check_bool(factor_group)
  rlang::check_bool(unweighted_ns)
  rlang::check_bool(show_missing_levels)
  rlang::check_bool(ignore.case)
  rlang::check_number_whole(digits, min = 0)
  for (sep in separator) {
    rlang::check_string(sep, arg = "separator")
  }

  # .by grouping: resolve tidy-selection and apply as grouping
  dataset <- apply_by(dataset, rlang::enquo(.by))

  # Validate the weighting column and the unweighted_ns combination
  wt_quo <- rlang::enquo(wt)
  check_wt(dataset, wt_quo)
  check_unweighted_ns(unweighted_ns, !rlang::quo_is_null(wt_quo))

  stems <- resolve_dots(...)

  if (length(stems) > 0) {
    warn_actual_variable(dataset, stems, separator, ignore.case)
  } else {
    # No stems passed: run on every stem in the dataset
    stems <- all_stems(dataset, wt_quo)
  }

  datalist <- purrr::map(stems, function(stem) {
    cols <- stem_cols(dataset, stem, separator, ignore.case)

    # Nothing matched (e.g. an actual variable was passed) -- skip
    if (length(cols) == 0) {
      return(NULL)
    }

    warn_stem_type(dataset, cols)

    data <- freq_one_stem(
      dataset = dataset,
      cols = cols,
      wt_quo = wt_quo,
      remove_nas = remove_nas,
      prompt = prompt,
      digits = digits,
      nas_group = nas_group,
      factor_group = factor_group,
      unweighted_ns = unweighted_ns,
      show_missing_levels = show_missing_levels
    )

    inform_quiet(
      stringr::str_c('Variable stem "', stem, '" successfully freq\'d')
    )

    data
  })

  dplyr::bind_rows(datalist)
}


# Internal helpers ------------------------------------------------------------

# Turn the dots into a character vector of stems. Bare symbols and string
# literals become their name; `all_of()` / `any_of()` calls are evaluated to
# splice in a character vector of stems.
resolve_dots <- function(...) {
  quos <- rlang::enquos(...)

  if (length(quos) == 0) {
    return(character(0))
  }

  stems <- purrr::map(quos, function(q) {
    expr <- rlang::quo_get_expr(q)
    fn <- if (rlang::is_call(expr)) rlang::call_name(expr) else NULL

    if (!is.null(fn) && fn %in% c("all_of", "any_of")) {
      rlang::eval_tidy(
        rlang::call_args(expr)[[1]],
        env = rlang::quo_get_env(q)
      )
    } else {
      rlang::as_name(q)
    }
  })

  unlist(stems, use.names = FALSE)
}

# Warn (no fallback) when a passed "stem" is actually a column in the dataset --
# the historic interface took an exemplar variable; the new one takes the stem.
warn_actual_variable <- function(dataset, stems, separator, ignore.case) {
  actual <- stems[stems %in% names(dataset)]

  for (v in actual) {
    suggested <- extract_stem(v)
    matched <- stem_cols(dataset, v, separator, ignore.case)

    match_bullet <- if (length(matched) > 0) {
      c(
        "!" = "Passed to {.fn stem} as-is, {.val {v}} will match: {.val {matched}}."
      )
    } else {
      c("!" = "Passed to {.fn stem} as-is, {.val {v}} will match nothing.")
    }

    cli::cli_warn(
      c(
        "{.val {v}} appears to be an actual variable in the dataset, not a stem.",
        "i" = "{.fn multi_freqs} now selects columns with {.fn stem}; pass the
               stem instead, e.g. {.code multi_freqs(data, {suggested})}.",
        match_bullet
      )
    )
  }
}

# The stems to run when the dots are empty: every stem in the dataset, minus the
# weight and any grouping variables.
all_stems <- function(dataset, wt_quo) {
  dataset |>
    dplyr::ungroup() |>
    dplyr::select(
      -!!wt_quo,
      -tidyselect::any_of(dplyr::group_vars(dataset))
    ) |>
    names() |>
    extract_stem()
}

# Column names belonging to a stem, selected via the stem() tidyselect helper.
stem_cols <- function(dataset, stem, separator, ignore.case) {
  dataset |>
    dplyr::ungroup() |>
    dplyr::select(
      stem(!!stem, separator = !!separator, ignore.case = !!ignore.case)
    ) |>
    names()
}

# Warn when a stem points at a text variable or a single-select variable.
warn_stem_type <- function(dataset, cols, call = rlang::caller_env(4)) {
  type_check <- dataset |>
    dplyr::ungroup() |>
    dplyr::select(cols[[1]])

  num_rows <- freqs(type_check, nas = FALSE) |>
    nrow()

  if (num_rows > 1) {
    cli::cli_warn(
      c(
        "!" = 'Matrix question detected',
        "i" = "Question {.val {cols[[1]]}} contains {.val {num_rows}} response options",
        "i" = "Please make sure this is intentional"
      ),
      call = call
    )
  }
}

# Run freqs on a single stem: select its columns, drop rows where the respondent
# answered none of them, then freq.
freq_one_stem <- function(
  dataset,
  cols,
  wt_quo,
  remove_nas,
  prompt,
  digits,
  nas_group,
  factor_group,
  unweighted_ns,
  show_missing_levels
) {
  # Select grouping variables explicitly so dplyr does not silently re-add them
  # (and emit "Adding missing grouping variables") when they are absent from the
  # stem columns.
  group_cols <- dplyr::group_vars(dataset)

  data <- dataset |>
    dplyr::select(
      tidyselect::all_of(c(group_cols, cols)),
      # weight is selected if specified
      !!wt_quo
    ) |>
    # Filter out rows where none of the questions have been answered
    dplyr::mutate(
      ns = rowSums(
        dplyr::across(
          .cols = tidyselect::all_of(cols),
          .fns = \(x) !is.na(x)
        )
      )
    ) |>
    dplyr::filter(ns > 0) |>
    dplyr::select(-ns) |>
    freqs(
      nas = TRUE,
      wt = !!wt_quo,
      prompt = prompt,
      digits = digits,
      nas_group = nas_group,
      factor_group = factor_group,
      unweighted_ns = unweighted_ns,
      show_missing_levels = show_missing_levels
    )

  if (isTRUE(remove_nas)) {
    data <- data |>
      dplyr::filter(!is.na(.data$value))
  }

  data
}
