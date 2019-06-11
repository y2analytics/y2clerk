
rm(list = ls())

library(tidyverse)
library(rlang)


# test data ---------------------------------------------------------------

set.seed(99)

frequencies <-
  tibble::tribble(
  ~qstreamer_viewers, ~qyoutube_subscribers, ~qyoutube_views, ~qpodcast_subscribe, ~qpodcast_downloads,
    "500",                   "400",             "600",                 "200",               "6",
    "450",                  "1200",           "15000",                 "850",                 "950",
    "25",                   "133",             "300",                    NA,                    NA,
    NA,                     "3",             "200",                  "86",                  "67",
    "8",                     "6",              "13",                   "8",                   "7",
    "500",                  "6000",         "2000000",               "10000",                "5000",
    "3500",                 "10000",           "40000",                    NA,                    NA,
    "159",                  "3852",             "822",                    NA,                    NA,
    NA,                      NA,                "50",                    NA,                    NA,
    NA,                  "2000",           "30000",                "3000",              "100000",
    NA,                   "500",            "2000",                 "500",                 "100",
    "50",                   "100",             "400",                    NA,                    NA
  ) %>%
  mutate_at(vars(starts_with("q")), as.numeric) %>%
  mutate(gender = sample(c("M","F"), 12, replace = T),
         weightvals = rnorm(12, mean = 1, sd = 0.1))


# building: helper functions ----------------------------------------------

get_means <- function(dataset, variable, nas, wt, prompt, digits) {

  # "failing fast"
  # 1) if there are NAs in the data, you should use nas = F
  if(nas) {
    count_nas <- dataset %>%
      filter(is.na(!!variable)) %>%
      nrow()
    if(count_nas != 0) stop('NAs present in variable(s); to proceed, set nas = F')
  }

  # 2) can't take mean of categorical variable
  check_class <- dataset %>%
    dplyr::summarise_all(class) %>%
    select(!!variable) %>%
    pull()
  if(check_class %in% c("character", "factor")) stop("Can't take mean of non-numeric variable")

  if(is.null(wt)){
    wt <- dplyr::enquo(wt)
  }

  # wt = NULL
  if(quo_is_null(wt)) {
    mean_df <- dataset %>%
      filter(!is.na(!!variable)) %>%
      summarise(n = length(!!variable),
                mean = base::mean(!!variable)
      )
  }
  # wt exists
  else {
    mean_df <- dataset %>%
      filter(!is.na(!!variable)) %>%
      summarise(n = sum(!!wt),
                mean = stats::weighted.mean(!!variable, !!wt)
      )
  }

  # get group column names
  grouping_vars <- c("")
  if (is.grouped_df(dataset)) {
    grouping_vars <- dplyr::group_vars(dataset)
  }

  mean_df <- mean_df %>%
    mutate(variable = dplyr::quo_name(variable),
           prompt = '',
           value = '',
           label = '',
           stat = 'mean',
           result = mean %>% round(digits)) %>%
    select(one_of(grouping_vars), variable, prompt, value, label, stat, n, result)

  # not built out
  if (prompt) {
    mean_df <- mean_df %>%
      dplyr::mutate(
        prompt = labelled::var_label(value)
      )
  }


  if(!quo_is_null(wt)) {
    mean_df <- mean_df %>%
      filter(variable != quo_name(wt))
  }

  return(mean_df)
}
frequencies %>%
  get_means(variable = quo(qyoutube_views), wt = quo(weightvals), digits = 2, prompt = F, nas = F) %>% print()

column_quos <- function(dataset) {
  col_names <- dataset %>% colnames()
  if (is.grouped_df(dataset)) {
    # Exclude grouping variables since they cannot be counted independent of groups.
    grouping_vars <- dplyr::group_vars(dataset)
    col_names <- setdiff(col_names, grouping_vars)
  }
  col_syms <- col_names %>% dplyr::syms()
  col_quos <- purrr::map(col_syms, dplyr::quo)
  return(col_quos)
}

freq_var <- function(dataset, variable, stat, nas, wt, prompt, digits) {
  variable <- dplyr::enquo(variable)
  weight <- dplyr::enquo(wt)

  if (stat == 'percent') {
    base <- ns(dataset, variable, weight, prompt)
    freq_result <- base %>%
      percents(nas, digits = digits)
  }

  else if(stat == 'mean') {
    freq_result <- get_means(dataset, variable, nas, weight, prompt, digits)
  }

  return(freq_result)
}

# building into freqs() ---------------------------------------------------



freqs <- function(dataset, ..., stat = 'percent', nas = TRUE, wt = NULL, prompt = F, digits = 2) {
  weight = dplyr::enquo(wt)
  variables = dplyr::quos(...)

  # If no variables are specified in the function call,
  # assume the user wants to run a frequency on all columns.
  if (!length(variables)) {
    variables <- column_quos(dataset)
  }

  suppressWarnings(
    purrr::map_dfr(
    .x = variables,
    .f = function(variable) {
      freq_var(dataset, !!variable, stat, nas, !!weight, prompt, digits)
    }
  ))
}


# testing -----------------------------------------------------------------



frequencies %>%
  select(starts_with("q"), weightvals) %>%
  freqs(stat = 'mean', nas = F, wt = weightvals)

frequencies %>%
  select(starts_with("q"), weightvals) %>%
  freqs(stat = 'mean', nas = F, wt = weightvals)

frequencies %>%
  freqs(stat = 'mean', nas = F)

frequencies %>%
  freqs(stat = 'mean', nas = T)

frequencies %>%
  group_by(gender) %>%
  freqs(stat = 'mean', nas = F)

frequencies %>%
  group_by(gender) %>%
  freqs(stat = 'mean', nas = F, wt = weightvals)

