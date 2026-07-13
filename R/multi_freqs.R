# multi_freqs() --------------------------------------------------------------------

#' Run frequencies for multiple select variables
#'
#' Filters out rows that are completely NULL values (if respondent did not answer question) then runs freqs
#'
#' @param dataset A dataframe.
#' @param ... The unquoted names of a set of variables in the dataset referring to variable "stems". If nothing
#' is specified, the function runs a frequency on every column in given dataset.
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
#' @examples
#'
#' df <- data.frame(
#'   a = c(1, 2, 3, 1, 2, 3, 1),
#'   Q1_1 = c(1, NA, 1, 1, NA, 1, NA),
#'   Q1_2 = c(1, 1, NA, 1, NA, 1, NA),
#'   Q1_3 = c(NA, 1, 1, NA, 4, 1, NA),
#'   weights = c(0.9, 0.9, 1.1, 1.1, 1, 1, 1)
#' ) |>
#'   tibble::as_tibble()
#'
#'
#' # All 3 methods below give the same output
#' multi_freqs(df, Q1_1)
#' df |> multi_freqs(Q1_1)
#' df |>
#'   dplyr::select(dplyr::starts_with("Q1")) |>
#'   multi_freqs()
#'
#'
#' # Grouped examples with weights (both have same outputs)
#' df |>
#'   dplyr::group_by(a) |>
#'   multi_freqs(Q1_1, wt = weights)
#' df |>
#'   dplyr::group_by(a) |>
#'   dplyr::select(starts_with("Q1"), weights) |>
#'   multi_freqs(wt = weights)
#'
#' @export

multi_freqs <- function(
  dataset,
  ...,
  remove_nas = TRUE,
  wt = NULL,
  prompt = FALSE,
  digits = 2,
  nas_group = TRUE,
  factor_group = FALSE,
  unweighted_ns = FALSE,
  show_missing_levels = TRUE
) {
  wt_quo <- rlang::enquo(wt)

  pattern <- resolve_pattern(dataset, ..., wt_quo = wt_quo)

  datalist <- purrr::map(pattern, function(stem) {
    warn_stem_type(dataset, stem)

    data <- freq_one_stem(
      dataset = dataset,
      stem = stem,
      wt_quo = wt_quo,
      remove_nas = remove_nas,
      prompt = prompt,
      digits = digits,
      nas_group = nas_group,
      factor_group = factor_group,
      unweighted_ns = unweighted_ns,
      show_missing_levels = show_missing_levels
    )

    cli::cli_inform(
      stringr::str_c('Variable stem "', stem, '" successfully freq\'d')
    )

    data
  })

  dplyr::bind_rows(datalist)
}


# Internal helpers ------------------------------------------------------------

# Regex used to select the columns belonging to a stem (underscore + digit).
stem_regex <- function(stem) {
  stringr::str_c('^', stem, '_[0-9]')
}

# Resolve the vector of stems to freq. Uses the columns passed in `...`; if none
# were passed, falls back to every column in the dataset (minus the weight and
# any grouping variables).
resolve_pattern <- function(dataset, ..., wt_quo) {
  pattern <- dataset |>
    dplyr::ungroup() |>
    dplyr::select(...) |>
    names() |>
    extract_stem()

  if (length(pattern) > 0) {
    return(pattern)
  }

  if (!dplyr::is_grouped_df(dataset)) {
    dataset |>
      dplyr::select(-!!wt_quo) |>
      names() |>
      extract_stem()
  } else {
    dataset |>
      dplyr::ungroup() |>
      dplyr::select(
        -!!wt_quo,
        -tidyselect::all_of(dplyr::group_vars(dataset))
      ) |>
      names() |>
      extract_stem()
  }
}

# Warn when a stem points at a text variable or a single-select variable.
warn_stem_type <- function(dataset, stem) {
  type_check <- dataset |>
    dplyr::ungroup() |>
    dplyr::select(dplyr::matches(stem_regex(stem)))

  if (is.character(type_check[, 1])) {
    cli::cli_warn(
      'Text variable stem detected -- please ensure this is intentional'
    )
  }

  if (nrow(freqs(type_check |> dplyr::select(1), nas = FALSE)) > 1) {
    cli::cli_warn(
      'Single select variable stem detected -- please ensure this is intentional'
    )
  }
}

# Run freqs on a single stem: select its columns, drop rows where the respondent
# answered none of them, then freq.
freq_one_stem <- function(
  dataset,
  stem,
  wt_quo,
  remove_nas,
  prompt,
  digits,
  nas_group,
  factor_group,
  unweighted_ns,
  show_missing_levels
) {
  regex <- stem_regex(stem)

  data <- dataset |>
    dplyr::select(
      dplyr::matches(regex),
      # "_TEXT" question is always removed
      -dplyr::ends_with('_TEXT'),
      # weight is selected if specified
      !!wt_quo
    ) |>
    # Filter out rows where none of the questions have been answered
    dplyr::mutate(
      ns = rowSums(
        dplyr::across(
          .cols = dplyr::matches(regex),
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
