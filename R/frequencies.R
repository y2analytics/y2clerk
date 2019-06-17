
##### Public functions #####

#' Run frequencies for multiple variables.
#'
#' @param dataset A dataframe.
#' @param ... The unquoted names of a set of variables in the dataframe. If nothing
#' is specified, the function runs a frequency on every column in given dataset.
#' @param stat Character, stat to run. Currently only 'percent' and 'mean' work (default: 'percent').
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

freqs <- freq <- function(dataset, ..., stat = 'percent', nas = TRUE, wt = NULL, prompt = F, digits = 2) {
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
        freq_var(dataset, !!variable, stat, nas, !!weight, prompt, digits)
      }
    )
  )
}

##### Private functions #####

get_means <- function(dataset, variable, nas, wt, prompt, digits) {

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


  check_labels <- dataset %>%
    dplyr::select(!!variable) %>%
    labelled::val_labels() %>%
    tibble::deframe() %>%
    base::is.null()

  if (! check_labels) stop("Value labels exist; consider converting values to labels or using stat = 'percent'")


  # (if wt = NULL) change class so logical test can be performed in all cases:
  if (is.null(wt)) {
    wt <- dplyr::enquo(wt)
  }

  # 1) wt = NULL
  if (rlang::quo_is_null(wt)) {
    mean_df <- dataset %>%
      dplyr::filter(!is.na(!!variable)) %>%
      dplyr::summarise(n = base::length(!!variable),
                       mean = base::mean(!!variable)
      )
  }
  # 2) wt exists in dataset
  else {
    mean_df <- dataset %>%
      dplyr::filter(!is.na(!!variable)) %>%
      dplyr::summarise(n = base::sum(!!wt),
                       mean = stats::weighted.mean(!!variable, !!wt)
      )
  }

  # 3) [not built] wt is non-null, not in dataset

  # get group column names to later add (if they exist/as necessary)
  grouping_vars <- c("")
  if (dplyr::is.grouped_df(dataset)) {
    grouping_vars <- dplyr::group_vars(dataset)
  }

  # produce means
  # * what should value and label display here? not as relevant as for freqs(stat='percent') ?
  mean_df <- mean_df %>%
    dplyr::mutate(variable = dplyr::quo_name(variable),
                  value = '',
                  label = '',
                  stat = 'mean',
                  result = mean %>% base::round(digits)) %>%
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

    mean_df <- mean_df %>%
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
    mean_df <- mean_df %>%
      dplyr::filter(variable != rlang::quo_name(wt))
  }

  return(mean_df)
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

freq_var <- function(dataset, variable, stat = 'percent', nas = TRUE, wt = NULL, prompt = F, digits = 2) {
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
