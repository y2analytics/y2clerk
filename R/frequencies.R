
##### Public functions #####

#' Run frequencies for multiple variables.
#'
#' @param dataset A dataframe.
#' @param ... The unquoted names of a set of variables in the dataframe.
#' @param nas Boolean, whether or not to include NAs in the tabulation.
#' @param wt The unquoted name of a weighting variable in the dataframe.
#' @param prompt Boolean, whether or not to include the prompt in the dataframe.
#' @param digits Integer, number of significant digits for rounding. Default is 2,
#' which results in an integer percentage.
#' @return A dataframe with the variable names, prompts, values, labels, counts,
#' and percents.
#' @examples
#' df <- data.frame(
#'   a = c(1, 2, 2, 3, 4, 2, NA),
#'   b = c(1, 2, 2, 3, 4, 1, NA),
#'   weights = c(0.9, 0.9, 1.1, 1.1, 1, 1, 1)
#' )
#'
#' freqs(df, a, b)
#' freqs(df, a, b, nas = FALSE)
#' freqs(df, a, b, wt = weights)
#' @export
freqs <- function(dataset, ..., nas = TRUE, wt = NULL, prompt = F, digits = 2) {
  weight = dplyr::enquo(wt)
  purrr::map_dfr(
    .x = dplyr::quos(...),
    .f = function(variable) {
      freq_var(dataset, !!variable, nas, !!weight, prompt, digits)
    }
  )
}
# Create a redundant function for convenience/backwards compatibility.
freq <- freqs

#' Run frequencies for multiple select survey questions.
#'
#' @param dataset A dataframe.
#' @param ... The unquoted names of a set of variables in the dataframe that
#' represent a single multiple select question on a survey. Each variable
#' should be encoded as non-NA for a selection, NA for no selection.
#' @param wt The unquoted name of a weighting variable in the dataframe.
#' @param var_name An optional variable name to assign to the results. If
#' omitted, it will derive a name by slicing off _[digit] suffixes.
#' @param prompt Boolean, whether or not to include the prompt in the dataframe.
#' @param digits Integer, number of significant digits for rounding. Default is 2,
#' which results in an integer percentage.
#' @return A dataframe with the variable names, prompts, values, labels, counts,
#' and percents.
#' @export
freq_ms <- function(dataset, ..., wt = NULL, var_name = NULL, prompt = F, digits = 2) {
  weight <- dplyr::enquo(wt)
  freqs(dataset, ..., wt = !!weight, prompt = prompt, digits = digits) %>%
    filter(
      !is.na(value)
    ) %>%
    mutate(
      variable = ifelse(
        is.null(var_name),
        derive_var_name(variable),
        var_name
      )
    )
}

##### Private functions #####

freq_var <- function(dataset, variable, nas = TRUE, wt = NULL, prompt = F, digits = 2) {
  variable <- dplyr::enquo(variable)
  weight <- dplyr::enquo(wt)
  ns(dataset, variable, weight, prompt) %>%
    percents(nas, digits = digits)
}

ns <- function(dataset, variable, weight, prompt) {
  counts <- if (class(dataset %>% dplyr::pull(!!variable)) == 'labelled') {
    # Metadata is better if the given variable has labels
    labelled_ns(dataset, variable, weight, prompt)
  } else {
    # Otherwise, use some sensible defaults
    unlabelled_ns(dataset, variable, weight, prompt)
  }
  # Reorder because Scotty is OCD
  if (prompt) {
    counts %>%
      dplyr::select(
        variable,
        prompt,
        value,
        label,
        n
      )
  } else {
    counts %>%
      dplyr::select(
        variable,
        value,
        label,
        n
      )
  }
}

percents <- function(counts, include_nas, digits) {
  # Filter out NAs if requested
  if (! include_nas) {
    counts <- counts %>%
      dplyr::filter(
        !is.na(value)
      )
  }
  # Calculate and round to integer percentages
  counts %>%
    dplyr::mutate(
      percent = (n / sum(n)) %>% round(digits)
    )
}

labelled_ns <- function(dataset, variable, weight, prompt) {
  # Extract the metadata from the labelled class
  counts <- base_ns(dataset, variable, weight)
  if (prompt) {
    counts <- counts %>%
      dplyr::mutate(
        prompt = labelled::var_label(value)
      )
  }
  counts <- counts %>%
    dplyr::mutate(
      label = labelled::to_factor(value) %>% as.character,
      value = value %>% as.character
    )
  return(counts)
}

unlabelled_ns <- function(dataset, variable, weight, prompt) {
  counts <- base_ns(dataset, variable, weight) %>%
    dplyr::mutate(
      label = value %>% as.character,
      value = value %>% as.character
    )
  if (prompt) {
    counts <- counts %>%
      dplyr::mutate(
        prompt = ''
      )
  }
  return(counts)
}

base_ns <- function(dataset, variable, weight) {
  dataset %>%
    # When wt is NULL, it runs unweighted counts
    dplyr::count(!!variable, wt = !!weight) %>%
    dplyr::rename(value = !!variable) %>%
    dplyr::mutate(
      variable = dplyr::quo_name(variable)
    )
}

derive_var_name <- function(variable_names) {
  # Just chops off the final _1 or _x2. We can make this more sophisticated
  # later if this isn't working right for us.
  stringr::str_replace(variable_names, '_[x\\d]+$', '')
}
