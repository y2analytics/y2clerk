# Public function ---------------------------------------------------------
### pivot_freqs

#' Widen a frequencies table
#'
#' Given a grouped frequencies table, pivot_freqs will create new columns for each label level in the frequencies
#'
#' @param dataset A grouped frequencies table as produced by y2clerk::freqs()
#' @param columns_var DEFAULT = label; If label, the frequencies will be pivoted so a new column will be created for each unique level of label.
#' Can also be set to group_var to pivot the other way and create new columns for each unique level of group_var
#' @return A wide tibble of frequencies with one row for each group (by default)
#' @export
#' @examples
#'   frequencies <- forcats::gss_cat %>%
#'     dplyr::group_by(year) %>%
#'       y2clerk::freqs(marital) %>%
#'       pivot_freqs()

pivot_freqs <-
  function(
    dataset,
    columns_var = label
  ) {

    label <- NULL
    pivot_errors(dataset)

    dataset %>%
      dplyr::select(
        .data$group_var,
        .data$label,
        .data$result
      ) %>%
      tidyr::pivot_wider(
        names_from = {{ columns_var }},
        values_from = .data$result,
        values_fill = 0
      )
  }



# Private functions -------------------------------------------------------

pivot_errors <- function(dataset) {

  if (
    !'label' %in% names(dataset) |
    !'result' %in% names(dataset)
  ) {
    stop('Input data must contain a "label" column and a "result" column. Ensure you are passing the output from a freqs() call.')
  }

  if (unique(dataset$label)[1] == '' &
      length(unique(dataset$label)) == 1) {
    stop('Your frequencies label column is blank. Please provide labels on which to pivot.')
  }

  if (
    sum(names(dataset) == 'group_var') != 1
  ) {
    stop('Your frequencies does not contain a group_var. It must have a group_var to pivot correctly.')
  }

}
