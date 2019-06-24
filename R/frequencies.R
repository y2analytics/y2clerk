
##### Public functions #####

#' Run frequencies for multiple variables.
#'
#' @param dataset A dataframe.
#' @param ... The unquoted names of a set of variables in the dataframe. If nothing
#' is specified, the function runs a frequency on every column in given dataset.
#' @param stat Character, stat to run. Currently accepts 'percent,' 'mean,' and 'quantile' (default: 'percent').
#' @param pr Double, for use when stat = 'quantile.' Stands for percentile rank, which is a quantile relative to a 100-point scale. Returns median unless otherwise specified. As an example, pr = 60 will return a real number such that 60% of values are lower than that number. pr = 0 and pr = 100 are special cases which will return the minimum and maximum in the data set. Input should be a real number x such that 0<=x<=100. (default: 50)
#' @param nas Boolean, whether or not to include NAs in the tabulation (default: T).
#' @param wt The unquoted name of a weighting variable in the dataframe (default: NULL).
#' @param prompt Boolean, whether or not to include the prompt in the dataframe (default: F).
#' @param digits Integer, number of significant digits for rounding (default: 2).
#' @return A dataframe with the variable names, prompts, values, labels, counts,
#' stats, and resulting calculations.
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
#' freq(df, stat = 'mean', nas = F)
#' freq(df, stat = 'mean', nas = F, wt = weights)
#' freq(df %>% group_by(a), b, stat = 'mean', nas = F, wt = weights)
#' @export

freqs <- freq <- function(dataset, ..., stat = 'percent', pr = 50, nas = TRUE, wt = NULL, prompt = F, digits = 2) {
  weight = dplyr::enquo(wt)
  variables = dplyr::quos(...)

  # If no variables are specified in the function call,
  # assume the user wants to run a frequency on all columns.
  if (!length(variables)) {
    variables <- column_quos(dataset)
  }

  # suppressWarnings added for trivial bind rows warning
  suppressWarnings(
    purrr::map_dfr(
      .x = variables,
      .f = function(variable) {
        freq_var(dataset, !!variable, stat, pr, nas, !!weight, prompt, digits)
      }
    )
  )
}

##### Private functions #####

calculate_from_cont_var <- function(dataset, variable, stat, pr, wt) {
  # (if wt = NULL) change class so logical test can be performed in all cases:
  if (is.null(wt)) {
    wt <- dplyr::enquo(wt)
  }

  if (stat == 'mean') {
    # 1) wt = NULL
    if (rlang::quo_is_null(wt)) {
      out_df <- dataset %>%
        dplyr::filter(!is.na(!!variable)) %>%
        dplyr::summarise(n = base::length(!!variable),
                         result = base::mean(!!variable)
        )
    }
    # 2) wt exists in dataset
    else {
      out_df <- dataset %>%
        dplyr::filter(!is.na(!!variable)) %>%
        dplyr::summarise(n = base::sum(!!wt),
                         result = stats::weighted.mean(!!variable, !!wt)
        )
    }
    # 3) [not built] wt is non-null, not in dataset (might be good to just build a verbose error)
  }
  if (stat == 'quantile') {

    # check quantile input (accept integers only)
    # if(pr %% 1 != 0) stop("quantile argument input is not an integer")

    # 1) wt = NULL
    if (rlang::quo_is_null(wt)) {
      out_df <- dataset %>%
        dplyr::filter(!is.na(!!variable)) %>%
        dplyr::summarise(n = base::length(!!variable),
                         result = stats::quantile(x = !!variable,
                                                  probs = pr / 100)
        )
    }
    # 2) wt exists in dataset
    else {
      out_df <- dataset %>%
        dplyr::filter(!is.na(!!variable)) %>%
        dplyr::summarise(n = base::length(!!variable),
                         result = reldist::wtd.quantile(!!variable,
                                                        q = pr / 100,
                                                        weight = !!wt)
        )
    }
    # 3) [not built] wt is non-null, not in dataset (might be good to just build a verbose error)
  }
  return(out_df)
}

get_quant <- function(dataset, variable, stat, pr, nas, wt, prompt, digits) {

  # "failing fast"

  # 1) if there are NAs in the data, you should use nas = F
  if (nas) {
    count_nas <- dataset %>%
      dplyr::filter(is.na(!!variable)) %>%
      base::nrow()
    if (count_nas != 0) stop('NAs present in variable(s); to proceed, set nas = F')
  }

  # 2) can't take mean of categorical variable
  # * probably need to build more robust way of validating class

  check_class <- dataset %>%
    dplyr::select(!!variable) %>%
    remove_labels() %>%
    dplyr::pull() %>%
    base::class()

  # collapse for examples like c("ordered" "factor")
  check_class <- str_c(check_class, collapse = " ")

  # if not one of these types, stop
  if (! (check_class %in% c("numeric", "integer")) ) stop("Can't take mean of non-numeric variable")


  # 3) stop if value labels exist

  check_labels <- dataset %>%
    dplyr::select(!!variable) %>%
    labelled::val_labels() %>%
    tibble::deframe() %>%
    base::is.null()

  if (! check_labels) stop("Value labels exist; consider converting values to labels or using stat = 'percent'")

  out_df <- calculate_from_cont_var(dataset, variable, stat, pr, wt)

  # get group column names to later add (if they exist/as necessary)
  grouping_vars <- c("")
  if (dplyr::is.grouped_df(dataset)) {
    grouping_vars <- dplyr::group_vars(dataset)
  }

  # produce dataframe to output
  # * what should value and label display here? not as relevant as for freqs(stat='percent') ?

  # make copy, maybe fix issues? i think it's mixing up the stat variable and the stat fx argument
  statistic <- stat
  rm(stat)

  out_df <-
    out_df %>%
    dplyr::mutate(variable = dplyr::quo_name(variable),
                  value = '',
                  label = '',
                  # different labels depending on input
                  stat = case_when(
                    statistic == 'mean' ~ 'mean',
                    statistic == 'quantile' & pr == 0 ~ 'quantile - min',
                    statistic == 'quantile' & pr == 50 ~ 'quantile - median',
                    statistic == 'quantile' & pr == 100 ~ 'quantile - max',
                    statistic == 'quantile' & !(pr %in% c(0,50,100)) ~ str_c('quantile - ', pr, '%'),
                    TRUE ~ 'error'
                  ),
                  # add 'weighted' to stat column if relevant
                  stat = case_when(
                    !rlang::quo_is_null(wt) & statistic == 'mean' ~ str_c(stat, ' - weighted'),
                    !rlang::quo_is_null(wt) & statistic == 'quantile' & (0 < pr) & (pr < 100) ~ str_c(stat, ' - weighted'),
                    TRUE ~ stat
                  ),
                  result = base::round(result,
                                       digits)) %>%
    dplyr::select(tidyselect::one_of(grouping_vars),
                  variable,
                  value,
                  label,
                  n,
                  stat,
                  result) %>%
    tibble::as_tibble()

  # fill out prompt column if specified
  if (prompt) {

    prompt_text <- dataset %>%
      dplyr::ungroup() %>%
      dplyr::select(!!variable) %>%
      labelled::var_label() %>%
      tibble::deframe()

    # when prompt = T but there is no variable label, output ""
    if (is.null(prompt_text)) {
      prompt_text <- ""
    }

    out_df <- out_df %>%
      dplyr::mutate(
        prompt = prompt_text
      ) %>%
      dplyr::select(tidyselect::one_of(grouping_vars),
                    variable,
                    prompt,
                    value,
                    label,
                    n,
                    stat,
                    result)
  }

  # if weights are used, remove weight column rows from output
  if (!rlang::quo_is_null(wt)) {
    out_df <- out_df %>%
      dplyr::filter(variable != rlang::quo_name(wt))
  }

  return(out_df)
}

column_quos <- function(dataset) {
  col_names <- dataset %>% colnames()
  if (dplyr::is.grouped_df(dataset)) {
    # Exclude grouping variables since they cannot be counted independent of groups.
    grouping_vars <- dplyr::group_vars(dataset)
    col_names <- base::setdiff(col_names, grouping_vars)
  }
  col_syms <- col_names %>% dplyr::syms()
  col_quos <- purrr::map(col_syms, dplyr::quo)
  return(col_quos)
}

freq_var <- function(dataset, variable, stat = 'percent', pr = 50, nas = TRUE, wt = NULL, prompt = F, digits = 2) {
  variable <- dplyr::enquo(variable)
  weight <- dplyr::enquo(wt)

  # check stat argument input
  if(!(stat %in% c('mean','quantile'))) stop('"stat" argument must receive either "mean" or "quantile"')

  if (stat == 'percent') {
    base <- ns(dataset, variable, weight, prompt)
    freq_result <- base %>%
      percents(nas, digits = digits)
  }

  else if(stat %in% c('mean', 'quantile')) {
    freq_result <- get_quant(dataset, variable, stat, pr, nas, weight, prompt, digits)
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
