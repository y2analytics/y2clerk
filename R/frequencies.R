
##### Public functions #####

#' Run a frequency for a single variable.
#'
#' @param dataset A dataframe.
#' @param variable The unquoted name of a variable in the dataframe.
#' @param include_nas Boolean, whether or not to include NAs in the tabulation.
#' @param weight The unquoted name of a weighting variable in the dataframe.
#' @return A dataframe with the variable name, prompt, values, labels, counts,
#' and percents.
#' @examples
#' df <- data.frame(
#'   a = c(1, 2, 2, 3, 4, 2, NA),
#'   weights = c(0.9, 0.9, 1.1, 1.1, 1, 1, 1)
#' )
#'
#' freq(df, a)
#' freq(df, a, include_nas = FALSE)
#' freq(df, a, weight = weights)
#' @export
freq <- function(dataset, variable, include_nas = TRUE, weight = NULL) {
  variable <- dplyr::enquo(variable)
  weight <- dplyr::enquo(weight)
  ns(dataset, variable, weight) %>%
    percents(include_nas)
}

#' Run frequencies for multiple variables.
#'
#' @param dataset A dataframe.
#' @param variables The unquoted name of a set of variables in the dataframe.
#' @param include_nas Boolean, whether or not to include NAs in the tabulation.
#' @param weight The unquoted name of a weighting variable in the dataframe.
#' @return A dataframe with the variable names, prompts, values, labels, counts,
#' and percents.
#' @examples
#' df <- data.frame(
#'   a = c(1, 2, 2, 3, 4, 2, NA),
#'   b = c(1, 2, 2, 3, 4, 1, NA),
#'   weights = c(0.9, 0.9, 1.1, 1.1, 1, 1, 1)
#' )
#'
#' freq(df, a, b)
#' freq(df, a, b, include_nas = FALSE)
#' freq(df, a, b, weight = weights)
#' @export
freqs <- function(dataset, ..., include_nas = TRUE, weight = NULL) {
  weight = dplyr::enquo(weight)
  purrr::map_dfr(
    .x = dplyr::quos(...),
    .f = function(variable) {
      freq(dataset, !!variable, include_nas, !!weight)
    }
  )
}

##### Private functions #####

ns <- function(dataset, variable, weight) {
  counts <- if (class(dataset %>% dplyr::pull(!!variable)) == 'labelled') {
    # Metadata is better if the given variable has labels
    labelled_ns(dataset, variable, weight)
  } else {
    # Otherwise, use some sensible defaults
    unlabelled_ns(dataset, variable, weight)
  }
  # Reorder because Scotty is OCD
  counts %>%
    dplyr::select(
      variable,
      prompt,
      value,
      label,
      n
    )
}

percents <- function(counts, include_nas) {
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
      percent = (n / sum(n)) %>% round(2)
    )
}

labelled_ns <- function(dataset, variable, weight) {
  # Extract the metadata from the labelled class
  base_ns(dataset, variable, weight) %>%
    dplyr::mutate(
      label = labelled::to_factor(value) %>% as.character,
      prompt = labelled::var_label(value),
      value = value %>% as.character
    )
}

unlabelled_ns <- function(dataset, variable, weight) {
  base_ns(dataset, variable, weight) %>%
    dplyr::mutate(
      label = value %>% as.character,
      prompt = '',
      value = value %>% as.character
    )
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
