# Public function ---------------------------------------------------------
### pivot_freqs

#' Widen a frequencies table
#'
#' Given a grouped frequencies table, pivot_freqs will create new columns for each label level in the frequencies
#'
#' @param dataset A grouped frequencies table as produced by y2clerk::freqs()
#' @param columns_var DEFAULT = label; If label, the frequencies will be pivoted so a new column will be created for each unique level of label.
#' Can also be set to group_var to pivot the other way and create new columns for each unique level of group_var
#' @return A wide tibble of frequencies with one row for each group (by default)
#' @export
#' @examples
#'   frequencies <- forcats::gss_cat |>
#'     dplyr::group_by(year) |>
#'       y2clerk::freqs(marital) |>
#'       pivot_freqs()
pivot_freqs <- function(
  dataset,
  columns_var = label
) {
  pivot_errors(dataset)

  columns_var_nm <- rlang::as_label(rlang::enquo(columns_var))

  # Only apply empty-string → NA for character columns; Numeric types are already good.
  if (is.character(dplyr::pull(dataset, {{ columns_var }}))) {
    dataset <- dataset |>
      dplyr::mutate({{ columns_var }} := dplyr::na_if({{ columns_var }}, ''))
  }

  cfg <- pivot_config(dataset, columns_var_nm)

  if (cfg$compound_names) {
    dataset <- dataset |>
      dplyr::mutate(.names_col = paste(.data$variable, .data$label, sep = "_"))
  }

  dataset |>
    dplyr::select(tidyselect::all_of(c(
      cfg$id_cols,
      cfg$names_from,
      'result'
    ))) |>
    tidyr::pivot_wider(
      names_from = cfg$names_from,
      values_from = 'result',
      values_fill = 0
    )
}

# Private functions -------------------------------------------------------

# Resolves which columns to select and pivot on.
# Returns a list with:
#   id_cols      - character vector of row-identifier columns
#   names_from   - string name of the column to pivot into headers
#   compound_names - TRUE if a "variable_label" column needs to be created first
pivot_config <- function(dataset, columns_var_nm) {
  has_multiple_vars <- 'variable' %in%
    names(dataset) &&
    length(unique(dataset$variable)) > 1

  # Only check label uniqueness when multiple variables are present
  labels_collide <- has_multiple_vars && !pivot_labels_unique(dataset)

  if (columns_var_nm == "label") {
    if (labels_collide) {
      # e.g. every question has "Yes"/"No" — prefix with variable name
      list(
        id_cols = 'group_var',
        names_from = '.names_col',
        compound_names = TRUE
      )
    } else {
      list(id_cols = 'group_var', names_from = 'label', compound_names = FALSE)
    }
  } else {
    # columns_var = group_var: labels become row identifiers
    id_cols <- if (labels_collide) c('variable', 'label') else 'label'
    list(id_cols = id_cols, names_from = columns_var_nm, compound_names = FALSE)
  }
}

# TRUE if no label value appears in more than one variable.
pivot_labels_unique <- function(dataset) {
  dataset |>
    dplyr::distinct(.data$variable, .data$label) |>
    dplyr::count(.data$label) |>
    dplyr::pull(n) |>
    max() == 1
}

pivot_errors <- function(dataset, call = rlang::caller_env()) {
  rlang::check_data_frame(dataset, call = call)

  col_names <- names(dataset)

  if (!'label' %in% col_names) {
    cli::cli_abort(
      c(
        "x" = "Input data must contain a {.arg label} column.",
        "i" = "Ensure you are passing the output from a (.fn freqs} call."
      ),
      call = call
    )
  }
  if (!'result' %in% col_names) {
    cli::cli_abort(
      c(
        "x" = "Input data must contain a {.arg result} column.",
        "i" = "Ensure you are passing the output from a (.fn freqs} call."
      ),
      call = call
    )
  }

  if (
    unique(dataset$label)[1] == '' &&
      length(unique(dataset$label)) == 1
  ) {
    cli::cli_abort(
      c(
        "x" = "Your frequencies label column is blank. Please provide unique labels on which to pivot."
      ),
      call = call
    )
  }

  if (!('group_var' %in% col_names)) {
    cli::cli_abort(
      c(
        "x" = 'Your frequencies does not contain a {.arg group_var}.",
      "i" = "Supply a {.arg group_var} to pivot correctly.'
      )
    )
  }
}
