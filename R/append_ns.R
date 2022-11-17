# append_ns() --------------------------------------------------------------------

#' Tastefully combine "label" and "n" columns
#'
#' Uses the output from the y2clerk freqs() function and adds (n = ...) to the end of each label, based off the column "n". Useful for charts that require ns for all levels. this is a random comment
#'
#' @param dataset A dataframe
#' @param append_to (default: label). The variable to which you wish to add (n = ...)
#' @param by_group_var (default: FALSE). If FALSE, takes ns by row. If TRUE, takes group ns by group_var levels. Set to TRUE if making a stacked bar chart.
#' @param newline (default: FALSE). If FALSE, (n = ...) comes after a space " ". If TRUE, it comes after a hard return
#' @importFrom data.table :=
#' @examples
#'
#' # by_group_var = FALSE
#' ToothGrowth %>%
#'   freqs(supp) %>%
#'   append_ns()
#'
#' ToothGrowth %>%
#'   dplyr::group_by(supp) %>%
#'   freqs(dose) %>%
#'   append_ns(newline = TRUE)
#'
#' # by_group_var = TRUE
#' ToothGrowth %>%
#'   dplyr::group_by(supp) %>%
#'   freqs(dose) %>%
#'   append_ns(
#'     append_to = group_var,
#'     by_group_var = TRUE
#'   )
#' @export

append_ns <-
  function(
    dataset,
    append_to = label,
    by_group_var = FALSE,
    newline = FALSE
  ) {
    label <- NULL
    character_split <- ' '
    if (newline == TRUE) {
      character_split <- '\n'
    }

    if (by_group_var == FALSE) {
    dataset %>%
      dplyr::mutate(
        '{{append_to}}' := stringr::str_c(
          {{ append_to }},
          character_split,
          '(n = ',
          as.character(.data$n),
          ')'
          )
      )
    } else { # by_group_var == TRUE
      dataset %>%
        dplyr::mutate(
          n_group = sum(.data$n),
          '{{append_to}}' := stringr::str_c(
            {{ append_to }},
            character_split,
            '(n = ',
            as.character(.data$n_group),
            ')'
          )
        ) %>%
        dplyr::select(-.data$n_group)
    }
  }


