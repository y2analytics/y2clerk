
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

  suppressWarnings(
    purrr::map_dfr(
      .x = variables,
      .f = function(variable) {
        freq_var(dataset, !!variable, stat, nas, !!weight, prompt, digits)
      }
    )
  )
}

# Create a redundant function for convenience/backwards compatibility.

##### Private functions #####

get_means <- function(dataset, variable, nas, wt, prompt, digits) {

  # "failing fast"
  # 1) if there are NAs in the data, you should use nas = F
  if(nas) {
    count_nas <- dataset %>%
      dplyr::filter(is.na(!!variable)) %>%
      base::row()
    if(count_nas != 0) stop('NAs present in variable(s); to proceed, set nas = F')
  }

  # 2) can't take mean of categorical variable
  check_class <- dataset %>%
    dplyr::select(!!variable) %>%
    dplyr::summarise_all(class) %>%
    dplyr::pull()
  if(check_class %in% c("character", "factor")) stop("Can't take mean of non-numeric variable")

  if(is.null(wt)){
    wt <- dplyr::enquo(wt)
  }

  # wt = NULL
  if(rlang::quo_is_null(wt)) {
    mean_df <- dataset %>%
      dplyr::filter(!is.na(!!variable)) %>%
      dplyr::summarise(n = base::length(!!variable),
                       mean = base::mean(!!variable)
      )
  }
  # wt exists
  else {
    mean_df <- dataset %>%
      dplyr::ungroup %>%
      dplyr::filter(!is.na(!!variable)) %>%
      dplyr::summarise(n = base::sum(!!wt),
                       mean = stats::weighted.mean(!!variable, !!wt)
      )
  }

  # get group column names
  grouping_vars <- c("")
  if (dplyr::is.grouped_df(dataset)) {
    grouping_vars <- dplyr::group_vars(dataset)
  }

  mean_df <- mean_df %>%
    dplyr::mutate(variable = dplyr::quo_name(variable),
                  prompt = '',
                  value = '',
                  label = '',
                  stat = 'mean',
                  result = mean %>% base::round(digits)) %>%
    dplyr::select(tidyselect::one_of(grouping_vars), variable, prompt, value, label, stat, n, result) %>%
    tibble::as_tibble()

  # not built out
  if (prompt) {

    prompt_text <- dataset %>%
      dplyr::select(!!variable) %>%
      labelled::var_label() %>%
      tibble::deframe()

    mean_df <- mean_df %>%
      dplyr::mutate(
        prompt = prompt_text
      )
  }

  if(!rlang::quo_is_null(wt)) {
    mean_df <- mean_df %>%
      dplyr::filter(variable != quo_name(wt))
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
