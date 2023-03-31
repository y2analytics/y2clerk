# Public function ---------------------------------------------------------
### cross_freqs

#' Run crosstabs in R
#'
#' Create a frequencies table with multiple distinct grouping variables/banners
#'
#' @param dataset A dataframe.
#' @param group_vars Accepts a character vector of variable names.
#' The variables by which you want to subset your freqeuncies.
#' In a traditional crosstab, these would be the banner variables.
#' @param ... The unquoted names of a set of variables in the dataset. If nothing
#' is specified, the function runs a frequency on every column in given dataset.
#' @param stat Character, stat to run. Currently accepts 'percent,' 'mean,' 'median,' 'min,' 'max,' 'quantile,' and 'summary' (default: 'percent').
#' @param percentile Double, for use when stat = 'quantile.' Input should be a real number x such that 0 <= x <= 100. Stands for percentile rank, which is a quantile relative to a 100-point scale. (default:NULL)
#' @param nas Boolean, whether or not to include NAs in the tabulation (default: TRUE).
#' @param wt The unquoted name of a weighting variable in the dataset (default: NULL).
#' @param prompt Boolean, whether or not to include the prompt in the dataset (default: FALSE).
#' @param digits Integer, number of significant digits for rounding (default: 2).
#' @param nas_group Boolean, whether or not to include NA values for the grouping variable in the tabulation (default: TRUE).
#' @param factor_group Boolean, whether or not to convert the grouping variable to a factor and use its labels instead of its underlying numeric values (default: FALSE)
#' @param wide Boolean, whether the dataframe should be one long dataframe (FALSE) or a wide and nested dataframe, nested on the group_vars (TRUE) (default: FALSE)
#' @param exclude_groups Boolean, argument only applies if group_vars are also included as freqs vars - group_vars are included as freqs vars if using select() to run cross_freqs on all variables in the dataset. FALSE will INclude group_vars as freqs vars. TRUE will EXclude group_vars from also being freqs vars (default: FALSE)
#' @param include_overall Boolean, whether to include the overall frequency levels for variables (default = FALSE)
#' @return A dataframe with the variable names, prompts, values, labels, counts,
#' stats, and resulting calculations, split out by subgroups (group_vars).
#' @export
#' @examples
#' GROUP_VARS <-
#'   mtcars %>%
#'   dplyr::select(
#'     am,
#'     vs
#'   ) %>%
#'   names()
#'
#' GROUP_VARS <- c("am", "vs")
#'
#' mtcars %>% cross_freqs(
#'   group_vars = GROUP_VARS,
#'   gear,
#'   carb
#' )
#'

cross_freqs <-
  function(
    dataset,
    group_vars,
    ...,
    stat = c("percent", "mean", "median", "min", "max", "quantile", "summary"),
    percentile = NULL,
    nas = TRUE,
    wt = NULL,
    prompt = FALSE,
    digits = 2,
    nas_group = TRUE,
    factor_group = FALSE,
    wide = FALSE,
    exclude_groups = FALSE,
    include_overall = FALSE
  ) {

    # custom error messages
    stat <- rlang::arg_match(stat)
    cross_error_messages(dataset, group_vars)

    if(include_overall == TRUE) {
      dataset$Overall <- '1'
      group_vars <- c('Overall', group_vars)
    }

    # start for loop: run freqs for each level in group_vars
    for (i in 1:length(group_vars)) {
      group_symbol <- rlang::sym(group_vars[i])
      if (i == 1) {
        results_raw <-
          dataset %>%
          dplyr::group_by({{ group_symbol }}) %>%
          freqs(
            ...,
            stat = stat,
            percentile = percentile,
            nas = nas,
            wt = {{ wt }},
            prompt = prompt,
            digits = digits,
            nas_group = nas_group,
            factor_group = factor_group
          ) %>%
          dplyr::mutate(
            group_var = forcats::as_factor(.data$group_var),
            group_var_name = group_vars[i]
          )
      } else {
        results_raw <-
          results_raw %>%
          dplyr::bind_rows(
            dataset %>%
              dplyr::group_by({{ group_symbol }}) %>%
              freqs(
                ...,
                stat = stat,
                percentile = percentile,
                nas = nas,
                wt = {{wt}},
                prompt = prompt,
                digits = digits,
                nas_group = nas_group,
                factor_group = factor_group
              ) %>%
              dplyr::mutate(
                group_var = forcats::as_factor(.data$group_var),
                group_var_name = group_vars[i]
              )
          )
      }
    } # end of for loop

    # exclude_groups (cut group_vars from variable if TRUE)
    results_raw <- exclude_groups_fun(results_raw, group_vars, exclude_groups)

    # Run long or wide
    if (wide == FALSE){
      output <-
        results_raw %>%
        dplyr::select(
          'group_var_name',
          tidyselect::everything()
          ) %>%
        dplyr::ungroup()
    } else {
      output_unnamed <-
        results_raw %>%
        pivot_nest() %>%
        dplyr::ungroup()

      output <-
        output_unnamed %>%
        dplyr::mutate(
          results = purrr::set_names(
            .data$results,
            output_unnamed$group_var_name
            )
          )
    }
    return(output)
  }



# Private functions -------------------------------------------------------

### cross_error_messages
cross_error_messages <- function(dataset, group_vars) {
  # Not found b/c not a string
  tryCatch(
    exists(group_vars),
    error = function(err) {
      msg <- conditionMessage(err)
      if (grepl("object '.*' not found", msg)) {
        stop('group_vars should be a character vector of variable names. Try formatting like c("var1", "var2") instead of c(var1, var2) or quos(var1, var2)')
      }
    }
  )

  # Not a vector
  if (is.vector(group_vars) == FALSE) {
    stop('group_vars should be a character vector of variable names. Try formatting like c("var1", "var2") instead of c(var1, var2) or quos(var1, var2)')
  }
}



### exclude_groups
exclude_groups_fun <- function(results_raw, group_vars, exclude_groups) {
  if (exclude_groups == FALSE) {
    results_raw <- results_raw
  } else {
    results_raw <- results_raw  %>%
      dplyr::mutate(
        filter_out = as.numeric(.data$variable %in% group_vars) # 1 if matching
      ) %>%
      dplyr::filter(.data$filter_out == 0) %>%
      dplyr::select(-'filter_out')
  }
}



### pivot_nest
pivot_nest <-
  function(
    dataset
  ) {
    dataset %>%
      dplyr::select(
        'group_var_name',
        'group_var',
        'variable',
        'label',
        'result'
      )  %>%
      dplyr::ungroup() %>%
      dplyr::nest_by(
        .data$group_var_name
      ) %>%
      dplyr::mutate(
        results = list(
          tidyr::pivot_wider(
            .data$data,
            values_from = 'result',
            names_from = 'group_var')
        )
      ) %>%
      dplyr::select(
        'group_var_name',
        'results'
      )
  }


