# sig_test_y2() ----------------------------------------------------------------

#' Significance testing
#'
#' Akin to crosstabs, run significance testing on a grouped frequencies
#'
#' @param frequencies A frequencies table created using the freqs() function in y2clerk
#' @param dataset (default: 'responses') The original data frame that the frequencies table came from
#' @param banner_var This will be the banner variables for cross tabs. Same as the grouping variable from the freqs() function.
#' @param wt The weight variable used in the frequencies function, if applicable
#' @param layout (default: 'tall') 'tall' formats the output to look like a basic grouped freqs table. 'wide' formats the output to look like the result from Q-formatted cross tabs
#' @return A table that matches the output of cross tabs, showing significance differences between different groups for any input variables
#' stats, cross tabs
#' @examples
#'
#' # Example Data
#'
#' df <- data.frame(
#'   V1 = c(
#'     rep('Agree', 100),
#'     rep('Neither', 100),
#'     rep('Disagree', 100),
#'     rep('Agree', 200),
#'     rep('Neither', 50),
#'     rep('Disagree', 50),
#'     rep('Agree', 50),
#'     rep('Neither', 125),
#'     rep('Disagree', 125)
#'   ),
#'   group = c(
#'     rep('Group 1', 300),
#'     rep('Group 2', 300),
#'     rep('Group 3', 300)
#'   ),
#'   weight = c(
#'     rnorm(900, 1, 0.25)
#'   )
#' )
#'
#' # Frequencies
#'
#' frequencies <- df %>%
#'   dplyr::group_by(group) %>%
#'   freqs(V1)
#'
#' # Frequencies with significance tests
#'
#' tested_freqs <- frequencies %>%
#'   sig_test_y2(dataset = df,
#'               banner_var = group)
#'
#' # Tested frequencies in crosstab format
#'
#' tested_freqs <- frequencies %>%
#'   sig_test_y2(dataset = df,
#'               banner_var = group,
#'               layout = 'wide')
#'
#' # Weighted tested frequencies
#'
#' tested_freqs <- df %>%
#'   dplyr::group_by(group) %>%
#'   freqs(V1,
#'         wt = weight) %>%
#'   sig_test_y2(dataset = df,
#'               banner_var = group,
#'               wt = weight)
#'
#' # Weighted tested frequencies in crosstab format
#'
#' tested_freqs <- df %>%
#'   dplyr::group_by(group) %>%
#'   freqs(V1,
#'         wt = weight) %>%
#'   sig_test_y2(dataset = df,
#'               banner_var = group,
#'               wt = weight,
#'               layout = 'wide')
#'
#' @export

sig_test_y2 <- function(
    frequencies,
    dataset,
    banner_var,
    wt = NULL,
    layout = c('tall', 'wide')
) {

  ## Error for labelled double group_var
  if(haven::is.labelled(frequencies$group_var) == TRUE) {

    stop(
      stringr::str_c(
        'Banner variable "',
        deparse(substitute(banner_var)),
        '" is a labelled double; please set "factor_group" equal to TRUE in freqs() for this variable'
      )
    )

  }

  ## Create logical for if there are weights
  weight_null <- dplyr::enquo(wt)
  weight_exists <- !rlang::quo_is_null(weight_null)

  ## Test matching arguments
  layout <- rlang::arg_match(layout)

  ## Getting iterables
  # Define variable name for responses reference
  var_name <- frequencies %>%
    dplyr::ungroup() %>%
    dplyr::distinct(.data$variable) %>%
    dplyr::pull()

  # Define group_var levels from frequencies object
  group_levels <- frequencies %>%
    dplyr::count(.data$group_var) %>%
    dplyr::pull(.data$group_var) %>%
    as.character()

  # Define value levels from frequencies object
  if (is.factor(dataset[[var_name]]) == TRUE ||
      is.character(dataset[[var_name]]) == TRUE) {

    # (fix for factor or character vars in freqs)
    value_levels <- frequencies %>%
      dplyr::ungroup() %>%
      dplyr::count(.data$label) %>%
      dplyr::pull(.data$label)

  } else if (is.na(as.numeric(frequencies$value)[1]) == FALSE) {

    # (fix for character type number values in freqs)
    value_levels <- frequencies %>%
      dplyr::ungroup() %>%
      dplyr::count(.data$value) %>%
      dplyr::pull(.data$value) %>%
      as.numeric()

  } else {

    value_levels <- frequencies %>%
      dplyr::ungroup() %>%
      dplyr::count(.data$value) %>%
      dplyr::pull(.data$value)

  }

  # Letter references
  sig_codes <- data.frame(
    group_levels = group_levels,
    reference = letters[1:length(group_levels)]
  )

  # New column for test results
  tested_freqs <- frequencies %>%
    dplyr::mutate(sig = '')

  ## Populate

  # Iterate through each value level (comparing ACROSS groups WITHIN values)
  for (i in value_levels) {

    # Process updates
    message(
      stringr::str_c(
        'Adding grouped pairwise significance tests for response "',
        frequencies %>%
          dplyr::ungroup() %>%
          dplyr::filter(
            .data$value == i |
              .data$label == i
          ) %>%
          dplyr::distinct(.data$label) %>%
          dplyr::pull(.data$label),
        '" for group_var "',
        deparse(substitute(banner_var)),
        '"'
      )
    )

    # All group_level pairwise combinations
    for (j in group_levels) {
      for (k in group_levels) {

        # Only runs test for pairings of un-alike groups
        if (j != k) {

          # Proportions check

          Px <- frequencies %>%
            dplyr::filter(
              .data$group_var == j,
              .data$value == i
            ) %>%
            dplyr::pull(.data$result)

          Py <- frequencies %>%
            dplyr::filter(
              .data$group_var == k,
              .data$value == i
            ) %>%
            dplyr::pull(.data$result)

          # In cases where no one in group answered

          if (rlang::is_empty(Px)) {

            Px <- 0

          }

          if (rlang::is_empty(Py)) {

            Py <- 0

          }

          # Only for cases where Px is greater than Py

          if (Px > Py) {

            # Unweighted

            if (weight_exists == FALSE) {

              # Set up testing data

              test_data <- dataset %>%
                dplyr::select(
                  test_var = tidyselect::all_of(var_name),
                  group = {{ banner_var }}
                ) %>%
                dplyr::mutate(
                  group = as.character(haven::as_factor(.data$group)),
                  test_var = dplyr::case_when(
                    test_var == i ~ 1,
                    is.na(test_var) ~ NA_real_,
                    TRUE ~ 0
                  )
                ) %>%
                dplyr::filter(
                  !is.na(.data$test_var),
                  .data$group == j |
                    .data$group == k
                ) %>%
                dplyr::mutate(id = dplyr::row_number())

              # Create survey design object

              surv_object <- survey::svydesign(
                id =~ id,
                data = test_data
              )

            }

            # Weighted

            if (weight_exists == TRUE) {

              # Set up testing data

              test_data <- dataset %>%
                dplyr::select(
                  test_var = tidyselect::all_of(var_name),
                  group = {{ banner_var }},
                  weight = {{ wt }}
                ) %>%
                dplyr::mutate(
                  group = as.character(haven::as_factor(.data$group)),
                  test_var = dplyr::case_when(
                    test_var == i ~ 1,
                    is.na(test_var) ~ NA_real_,
                    TRUE ~ 0
                  )
                ) %>%
                dplyr::filter(
                  !is.na(.data$test_var),
                  .data$group == j |
                    .data$group == k
                ) %>%
                dplyr::mutate(id = dplyr::row_number())

              # Create survey design object

              surv_object <- survey::svydesign(
                id =~ id,
                weights =~ weight,
                data = test_data
              )

            }

            # Get test results

            p_value <- survey::svychisq(
              ~test_var + group,
              surv_object
            ) %>%
              purrr::pluck('p.value')

            # Get legend code

            group_letter <- sig_codes %>%
              dplyr::filter(group_levels == k) %>%
              dplyr::pull(.data$reference)

            # FDR correction (default; used in Q crosstabs)

            p_value <- stats::p.adjust(
              p_value,
              method = 'fdr',
              n = choose(length(group_levels), 2)
            )

            code_result <- dplyr::case_when(
              # < 0.05 is lower-case code; < 0.001 is upper-case
              dplyr::between(p_value, 0.001, 0.05) ~ group_letter,
              p_value < 0.001 ~ toupper(group_letter),
              TRUE ~ ''
            )

            # Skip for any Px < Py

          } else {

            code_result = ''

          }

          # Skip if j == k

        } else {

          code_result = ''

        }

        if (is.factor(dataset[[var_name]]) == TRUE ||
            is.character(dataset[[var_name]]) == TRUE) {

          ## Merge
          tested_freqs <- tested_freqs %>%
            dplyr::mutate(
              sig = ifelse(
                # For the group_var and value just tested against,
                .data$group_var == j &
                  .data$label == i,
                # Add the appropriate legend code to the sig column
                stringr::str_c(.data$sig, code_result),
                # Keep it the same for all else
                .data$sig
              )
            )

        } else {

          ## Merge
          tested_freqs <- tested_freqs %>%
            dplyr::mutate(
              sig = ifelse(
                # For the group_var and value just tested against,
                .data$group_var == j &
                  .data$value == i,
                # Add the appropriate legend code to the sig column
                stringr::str_c(.data$sig, code_result),
                # Keep it the same for all else
                .data$sig
              )
            )

        }

      }
    }

  }

  ## Final appending group level references
  tested_freqs <- tested_freqs %>%
    dplyr::left_join(
      sig_codes %>%
        dplyr::mutate(
          reference = stringr::str_c(
            '[',
            toupper(.data$reference),
            ']'
          )
        ),
      by = c('group_var' = 'group_levels')) %>%
    dplyr::mutate(
      group_var = stringr::str_c(
        .data$group_var,
        ' ',
        .data$reference
      ),
      group_var = haven::as_factor(.data$group_var),
      group_var = forcats::fct_relevel(
        .data$group_var,
        stringr::str_c(
          group_levels,
          ' [',
          toupper(sig_codes$reference),
          ']')
      )
    ) %>%
    dplyr::select(-'reference')

  ## Layout options
  # Normal Freqs Layout (tall)

  if (layout == 'tall') {

    return(tested_freqs)

  }

  # Crosstab Layout (wide)

  if (layout == 'wide') {

    # Get wide percentages
    xtab_results <- tested_freqs %>%
      tidyr::pivot_wider(
        .data$label,
        names_from = 'group_var',
        values_from = 'result'
      ) %>%
      dplyr::select(
        'label',
        stringr::str_c(
          group_levels,
          ' [',
          toupper(sig_codes$reference),
          ']'
        )
      ) %>%
      dplyr::mutate_all(~replace(., is.na(.), 0))

    # Get wide sig codes
    xtab_codes <- tested_freqs %>%
      tidyr::pivot_wider(
        .data$label,
        names_from = 'group_var',
        values_from = 'sig'
      ) %>%
      dplyr::select(
        'label',
        stringr::str_c(
          group_levels,
          ' [',
          toupper(sig_codes$reference),
          ']'
        )
      ) %>%
      dplyr::mutate_all(~replace(., is.na(.), ''))

    # Combine

    xtab <- data.frame()

    for (i in 1:nrow(xtab_results)) {

      xtab <- rbind(xtab,
                    xtab_results[i, ],
                    xtab_codes[i, ])

    }

    return(xtab)

  }

}
