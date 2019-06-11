
library(tidyverse)
library(rlang)


# test data ---------------------------------------------------------------

set.seed(99)

frequencies <-
  tibble::tribble(
    ~qstreamer_viewers,  ~qyoutube_subscribers,  ~qyoutube_views,  ~qpodcast_subscribe,  ~qpodcast_downloads,
    "500",                   "400",             "600",                 "200",                   "6",
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
  mutate_all(as.numeric) %>%
  mutate(weights = rnorm(12, mean = 1, sd = 0.1))

frequencies %>%
  freq_mean(qstreamer_viewers)



############################################## workspace: freq_mean() --------------------------------------------------

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


############################################# workspace: freqs() ------------------------------------------------------

freqs <- function(dataset, ..., stat = 'percent', nas = TRUE, wt = NULL, prompt = F, digits = 2) {
  weight = dplyr::enquo(wt)
  variables = dplyr::quos(...)
  if (!length(variables)) {
    # If no variables are specified in the function call,
    # assume the user wants to run a frequency on all columns.
    variables <- column_quos(dataset)
  }
  purrr::map_dfr(
    .x = variables,
    .f = function(variable) {
      freq_var(dataset, !!variable, stat, nas, !!weight, prompt, digits)
    }
  )
}
# Create a redundant function for convenience/backwards compatibility.
freq <- freqs

##### Private functions #####

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

freq_var <- function(dataset, variable, stat = 'percent', nas = TRUE, wt = NULL, prompt = F, digits = 2) {
  variable <- dplyr::enquo(variable)
  weight <- dplyr::enquo(wt)
  base <- ns(dataset, variable, weight, prompt)
  if (stat == 'percent') {
    freq_result <- base %>%
      percents(nas, digits = digits)
  }
  return(freq_result)
}

ns <- function(dataset, variable, weight, prompt) {
  counts <- if (class(dataset %>% dplyr::pull(!!variable)) %in% c('labelled','haven_labelled','haven_labelled_spss')) {
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
      stat = 'percent',
      result = (n / sum(n)) %>% round(digits)
    )
}

means <- function(values, include_nas, digits) {
  # Filter out NAs if requested
  if (! include_nas) {
    values <- values %>%
      dplyr::filter(
        !is.na(value)
      )
  }
  # Calculate and round to integer percentages
  values %>%
    dplyr::mutate(
      stat = 'percent',
      result = (n / sum(n)) %>% round(digits)
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
    dplyr::count(!!variable, wt = !!weight, .drop = F) %>%
    dplyr::rename(value = !!variable) %>%
    dplyr::mutate(
      variable = dplyr::quo_name(variable)
    )
}

