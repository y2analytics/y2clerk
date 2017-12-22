##### Public functions #####

#' Run a mean in frequency form for a single variable.
#'
#' @param dataset A dataframe.
#' @param variable The unquoted name of a variable in the dataframe.
#' @param wt The unquoted name of a weighting variable in the dataframe.
#' @param prompt Boolean, whether or not to include the prompt in the dataframe.
#' @return A dataframe with the variable name, prompt, values, labels, counts,
#' and means.
#' @examples
#' df <- data.frame(
#'   a = c(1, 2, 2, 3, 4, 2, NA),
#'   weights = c(0.9, 0.9, 1.1, 1.1, 1, 1, 1)
#' )
#'
#' freq_mean(df, a)
#' freq_mean(df, a, wt = weights)
#' @export
freq_mean <- function(dataset, variable, nas = TRUE, wt = NULL, prompt = F) {
  variable <- dplyr::enquo(variable)
  weight <- dplyr::enquo(wt)
  n <- dataset %>%
    dplyr::filter(
      !is.na(!!variable)
    ) %>%
    dplyr::pull(!!variable) %>%
    length
  if (is.null(wt)) {
    mean <- dataset %>%
      dplyr::filter(
        !is.na(!!variable)
      ) %>%
      dplyr::summarize(
        mean = mean(!!variable)
      ) %>%
      dplyr::pull(mean)
  } else {
    mean <- dataset %>%
      dplyr::filter(
        !is.na(!!variable)
      ) %>%
      dplyr::summarize(
        mean = weighted.mean(!!variable, !!weight)
      ) %>%
      dplyr::pull(mean)
  }
  if (prompt) {
    data.frame(
      variable = dplyr::quo_name(variable),
      prompt = labelled::var_label(variable),
      value = '',
      label = '',
      n = n,
      percent = mean
    )
  } else {
    data.frame(
      variable = dplyr::quo_name(variable),
      value = '',
      label = '',
      n = n,
      percent = mean
    )
  }
}
