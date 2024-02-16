# multi_freqs() --------------------------------------------------------------------

#' Run frequencies for multiple select variables
#'
#' Filters out rows that are completely NULL values (if respondent did not answer question) then runs freqs
#'
#' @param dataset A dataframe.
#' @param ... The unquoted names of a set of variables in the dataset referring to variable "stems". If nothing
#' is specified, the function runs a frequency on every column in given dataset.
#' @param remove_nas Boolean, after freqs is run (which always includes NAs), whether or not to filter out counts of NA value (default: TRUE).
#' @param wt The unquoted name of a weighting variable in the dataset (default: NULL).
#' @param prompt Boolean, whether or not to include the prompt in the dataset (default: FALSE).
#' @param digits Integer, number of significant digits for rounding (default: 2).
#' @param nas_group Boolean, whether or not to include NA values for the grouping variable in the tabulation (default: TRUE).
#' @param factor_group Boolean, whether or not to convert the grouping variable to a factor and use its labels instead of its underlying numeric values (default: FALSE)
#' @param unweighted_ns Boolean, whether the 'n' column in the freqs table should be UNweighted while results ARE weighted. This argument can only be used if a wt variable is used. If no weight variable is used, the 'n' column will always be unweighted (default: FALSE).
#' @param show_missing_levels Boolean, whether to keep response levels with no data (default: TRUE)
#' @return A dataframe with the variable names, prompts, values, labels, counts,
#' stats, and resulting calculations.
#' @examples
#'
#' df <- data.frame(
#'   a = c(1, 2, 3, 1, 2, 3, 1),
#'   Q1_1 = c(1, NA, 1, 1, NA, 1, NA),
#'   Q1_2 = c(1, 1, NA, 1, NA, 1, NA),
#'   Q1_3 = c(NA, 1, 1, NA, 4, 1, NA),
#'   weights = c(0.9, 0.9, 1.1, 1.1, 1, 1, 1)
#' ) %>%
#'   tidyr::as_tibble()
#'
#'
#' # All 3 methods below give the same output
#' multi_freqs(df, Q1_1)
#' df %>% multi_freqs(Q1_1)
#' df %>%
#'   dplyr::select(dplyr::starts_with("Q1")) %>%
#'   multi_freqs()
#'
#'
#' # Grouped examples with weights (both have same outputs)
#' df %>%
#'   dplyr::group_by(a) %>%
#'   multi_freqs(Q1_1, wt = weights)
#' df %>%
#'   dplyr::group_by(a) %>%
#'   dplyr::select(starts_with("Q1"), weights) %>%
#'   multi_freqs(wt = weights)
#'
#' @export

multi_freqs <- function(
  dataset,
  ... ,
  remove_nas = TRUE,
  wt = NULL,
  prompt = FALSE,
  digits = 2,
  nas_group = TRUE,
  factor_group = FALSE,
  unweighted_ns = FALSE,
  show_missing_levels = TRUE
) {

  # Creates an empty list to be populated with frequencies data frames
  datalist <- list()
  
  pattern <- dataset %>%
    dplyr::ungroup() %>%
    dplyr::select(...) %>%
    names() %>%
    stringr::str_remove(
      '_[0-9]+$'
    ) %>%
    stringr::str_remove(
      '_[0-9]+_TEXT$'
    ) %>%
    unique()

  # If no variables are specified, assume user wants to run function on entire dataset
  if (length(pattern) == 0 & dplyr::is_grouped_df(dataset) == FALSE) {
    
    pattern <- dataset %>%
      dplyr::select(-{{ wt }}) %>%
      names() %>%
      stringr::str_remove(
        '_[0-9]+$'
      ) %>%
      stringr::str_remove(
        '_[0-9]+_TEXT$'
      ) %>%
      unique()

  }

  # Same as above for grouped, length == 0 dataset
  if (length(pattern) == 0 & dplyr::is_grouped_df(dataset) == TRUE) {
    
    pattern <- dataset %>%
      dplyr::ungroup() %>%
      dplyr::select(
          -{{ wt }},
          -tidyselect::all_of(dplyr::group_vars(dataset))
      ) %>%
      names() %>%
      stringr::str_remove(
        '_[0-9]+$'
      ) %>%
      stringr::str_remove(
        '_[0-9]+_TEXT$'
      ) %>%
      unique()

  }

  # Creating a filtered frequencies dataframe for each stem
  for (i in pattern) {

    # Warning Section
    type_check <- dataset %>%
      dplyr::ungroup() %>%
      dplyr::select(
        dplyr::matches(stringr::str_c(i, '_[0-9]'))
      )

    # Throw warning if stem is character variable
    if (is.character(type_check[, 1])) {
      warning('Text variable stem detected -- please ensure this is intentional')
    }

    # Throw warning if stem is single select variable
    if (nrow(freqs(type_check %>% dplyr::select(1), nas = FALSE)) > 1){
      warning('Single select variable stem detected -- please ensure this is intentional')
    }

    data <- dataset %>%
      # dataset selects all columns that start with the string or the ith element in the string list
      dplyr::select(
        dplyr::matches(stringr::str_c(i, '_[0-9]')),
        # "_TEXT" question is always removed
        -dplyr::ends_with('_TEXT'),
        # weight is selected if specified
        {{ wt }}
      ) %>%
      # Following lines filter out rows where none of the questions have been answered
      dplyr::mutate(ns = rowSums(
        dplyr::across(
          .cols = dplyr::starts_with(i),
          .fns = ~ifelse(
            is.na(.x),
            FALSE,
            TRUE
          )
        )
      )) %>%
      dplyr::filter(
        ns > 0
      ) %>%
      dplyr::select(
        -ns
      ) %>%
      # Original freqs operation
      freqs(
        nas = TRUE,
        wt = {{ wt }},
        prompt = prompt,
        digits = digits,
        nas_group = nas_group,
        factor_group = factor_group,
        unweighted_ns = unweighted_ns,
        show_missing_levels = show_missing_levels
      )

    if (remove_nas == TRUE) {
      
      data <- data %>%
        dplyr::filter(
          !is.na(.data$value)
        )
    
    }

    # Adds stem freqs to datalist
    datalist[[i]] <- data
    
    message(
      stringr::str_c(
        'Variable stem "',
        i,
        '" successfully freq\'d'
      )
    )

  }

  # Combine
  frequencies <- dplyr::bind_rows(datalist)

  # Returns full data frame
  return(frequencies)

}
