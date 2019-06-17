##### Public functions #####

#' Run frequencies for multiple variables.
#'
#' @param dataset A dataframe.
#' @param ... The unquoted names of a set of variables in the dataframe. If nothing
#' is specified, the function runs a frequency on every column in given dataset.
#' @param stat Character, stat to run. Currently only 'percent' works (default: 'percent').
#' @param nas Boolean, whether or not to include NAs in the tabulation (default: T).
#' @param wt The unquoted name of a weighting variable in the dataframe (default: NULL).
#' @param prompt Boolean, whether or not to include the prompt in the dataframe (default: F).
#' @param digits Integer, number of significant digits for rounding (default: 2).
#' @param nas_group Boolean, whether or not to include NA values for the grouping variable in the tabulation (default: T).
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
#' @export
freqs <- function(dataset, ..., stat = 'percent', nas = TRUE, wt = NULL, prompt = F, digits = 2, nas_group = TRUE) {
  dataset <- group_factor(dataset, nas_group)
  weight = dplyr::enquo(wt)
  variables = dplyr::quos(...)
  if (!length(variables)) {
    # If no variables are specified in the function call,
    # assume the user wants to run a frequency on all columns.
    variables <- column_quos(dataset, !!weight)
  }
  frequencies <- purrr::map_dfr(
    .x = variables,
    .f = function(variable) {
      freq_var(dataset, !!variable, stat, nas, !!weight, prompt, digits)
    }
  )
  frequencies <- group_rename(frequencies)
}
# Create a redundant function for convenience/backwards compatibility.
freq <- freqs

##### Private functions #####

group_factor <- function(dataset, nas_group){
  grouping_vars <- dplyr::group_vars(dataset)
  if(length(grouping_vars) > 1){ #if there are 2+ grouping vars
    dataset <- group2_factors(dataset, grouping_vars, nas_group)
   } else if(length(grouping_vars) == 1){ #1 grouping var
     dataset <- group1_factor(dataset, grouping_vars, nas_group)
   } else{ # no grouping vars
     dataset <- dataset
   }
}

group2_factors <- function(dataset, grouping_vars, nas_group) {
    group_flag <- dplyr::group_vars(dataset)[1] %>% as.symbol()
    group_flag2 <- dplyr::group_vars(dataset)[2] %>% as.symbol()
    if(nas_group == FALSE){
    dataset <- dataset %>%
      dplyr::filter(
        !is.na(!!group_flag),
        !is.na(!!group_flag2)
      )
    }
    dataset <- dataset %>%
      dplyr::ungroup() %>%
      dplyr::mutate_at(
        dplyr::vars(
          grouping_vars
        ),
        list(~forcats::as_factor(.))
      ) %>%
      dplyr::group_by(
        !!group_flag,
        !!group_flag2
      )
    }

group1_factor <- function(dataset, grouping_vars, nas_group){
    group_flag <- dplyr::group_vars(dataset)[1] %>% as.symbol()
    if(nas_group == FALSE){
      dataset <- dataset %>%
        dplyr::filter(
          !is.na(!!group_flag)
        )
    }
      dataset <- dataset %>%
      dplyr::ungroup() %>%
      dplyr::mutate_at(
        dplyr::vars(
          grouping_vars
        ),
        list(~forcats::as_factor(.))
      ) %>%
      dplyr::group_by(
        !!group_flag
      )
}

column_quos <- function(dataset, wt) {
  col_names <- dataset %>% colnames()
  if (dplyr::is.grouped_df(dataset)) {
    # Exclude grouping variables since they cannot be counted independent of groups.
    grouping_vars <- dplyr::group_vars(dataset)
    col_names <- setdiff(col_names, grouping_vars)
  }
    # Exclude weighting varaible from freqs in select
    weight_name <- dplyr::enquo(wt) %>% as.character()
    weight_name2 <- weight_name[2]
    col_names <- setdiff(col_names, weight_name2)

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

group_rename <- function(dataset){
  if(names(dataset)[1] != 'variable'){
    if(names(dataset)[2] != 'variable'){ #if there are 2 grouping vars
      dataset <- dataset %>%
        dplyr::rename(
          group_var = names(dataset)[1],
          group_var2 = names(dataset)[2]
          )
    }else{ #if there is 1 grouping var
    dataset <- dataset %>%
      dplyr::rename(group_var = names(dataset)[1])
    }
    #NOT GROUPED
  } else{
    dataset <- dataset
  }
}


