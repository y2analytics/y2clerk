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
#' frequencies <-
#' tibble::tribble(
#' ~qstreamer_viewers,  ~qyoutube_subscribers,  ~qyoutube_views,  ~qpodcast_subscribe,  ~qpodcast_downloads,
#' "500",                   "400",             "600",                 "200",                   "6",
#' "450",                  "1200",           "15000",                 "850",                 "950",
#' "25",                   "133",             "300",                    NA,                    NA,
#' NA,                     "3",             "200",                  "86",                  "67",
#' "8",                     "6",              "13",                   "8",                   "7",
#' "500",                  "6000",         "2000000",               "10000",                "5000",
#' "3500",                 "10000",           "40000",                    NA,                    NA,
#' "159",                  "3852",             "822",                    NA,                    NA,
#' NA,                      NA,                "50",                    NA,                    NA,
#' NA,                  "2000",           "30000",                "3000",              "100000",
#' NA,                   "500",            "2000",                 "500",                 "100",
#' "50",                   "100",             "400",                    NA,                    NA
#' ) %>%
#' mutate_all(as.numeric)
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
