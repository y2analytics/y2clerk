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
#' @param pr Double, for use when stat = 'quantile.' Input should be a real number x such that 0 <= x <= 100. Stands for percentile rank, which is a quantile relative to a 100-point scale. (default:NULL)
#' @param nas Boolean, whether or not to include NAs in the tabulation (default: TRUE).
#' @param wt The unquoted name of a weighting variable in the dataset (default: NULL).
#' @param prompt Boolean, whether or not to include the prompt in the dataset (default: FALSE).
#' @param digits Integer, number of significant digits for rounding (default: 2).
#' @param nas_group Boolean, whether or not to include NA values for the grouping variable in the tabulation (default: TRUE).
#' @param factor_group Boolean, whether or not to convert the grouping variable to a factor and use its labels instead of its underlying numeric values (default: FALSE)
#' @param wide Boolean, whether the dataframe should be one long dataframe (FALSE) or a wide and nested dataframe, nested on the group_vars (TRUE) (default: FALSE)
#' @return A dataframe with the variable names, prompts, values, labels, counts,
#' stats, and resulting calculations, split out by subgroups (group_vars).
#' @export
#' @examples
#' \dontrun{
#' GROUP_VARS <-
#'   mtcars %>%
#'   dplyr::select(
#'     am,
#'     vs
#'   ) %>%
#'   names()
#' }
#' GROUP_VARS <- c("am", "vs")
#'
#' cross_freqs(
#'   mtcars,
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
    stat = "percent",
    pr = NULL,
    nas = TRUE,
    wt = NULL,
    prompt = FALSE,
    digits = 2,
    nas_group = TRUE,
    factor_group = FALSE,
    wide = FALSE
  ) {

    for (i in 1:length(group_vars)) {
      group_symbol <- rlang::sym(group_vars[i])
      if(i == 1) {
        results_raw <-
          dataset %>%
          dplyr::group_by({{ group_symbol }}) %>%
          freqs(
            ...,
            stat = stat,
            pr = pr,
            nas = nas,
            wt = {{ wt }},
            prompt = prompt,
            digits = digits,
            nas_group = nas_group,
            factor_group = factor_group
          ) %>%
          dplyr::mutate(
            group_var = factor(.data$group_var),
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
                pr = pr,
                nas = nas,
                wt = {{wt}},
                prompt = prompt,
                digits = digits,
                nas_group = nas_group,
                factor_group = factor_group
              ) %>%
              dplyr::mutate(
                group_var = factor(.data$group_var),
                group_var_name = group_vars[i]
              )
          )
      }
    }

    if(wide == FALSE){
      output <-
        results_raw %>%
        dplyr::select(
          .data$group_var_name,
          tidyselect::everything()
          ) %>%
        dplyr::ungroup()
    } else {
      output_unnamed <-
        results_raw %>%
        dplyr::select(
          .data$group_var_name,
          tidyselect::everything()
          ) %>%
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

    output
  }




# Private functions -------------------------------------------------------

### pivot_nest
pivot_nest <-
  function(
    dataset
  ) {

    dataset %>%
      dplyr::select(
        .data$group_var_name,
        .data$group_var,
        .data$variable,
        .data$label,
        .data$result
      )  %>%
      dplyr::ungroup() %>%
      dplyr::nest_by(
        .data$group_var_name
      ) %>%
      dplyr::mutate(
        results = list(
          tidyr::pivot_wider(
            dataset,
            values_from = .data$result,
            names_from = .data$group_var)
        )
      ) %>%
      dplyr::select(
        .data$group_var_name,
        .data$results
      )

  }




