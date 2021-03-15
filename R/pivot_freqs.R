# Public function ---------------------------------------------------------
### pivot_freqs

#' Widen a frequencies table
#'
#' Given a grouped frequencies table, pivot_freqs will create new columns for each label level in the frequencies
#'
#' @param dataset A grouped frequencies table as produced by y2clerk::freqs()
#'
#' @return A wide tibble of frequencies with one row for each group
#' @export
#' @examples
#'   frequencies <- ToothGrowth %>%
#'     dplyr::group_by(supp) %>%
#'       y2clerk::freqs(dose) %>%
#'       pivot_freqs()

pivot_freqs <-
  function(dataset) {

    pivot_errors(dataset)

    dataset %>%
      dplyr::select(
        .data$group_var,
        .data$label,
        .data$result
        ) %>%
      tidyr::pivot_wider(
        names_from = .data$label,
        values_from = .data$result
        )
  }



# Private functions -------------------------------------------------------

pivot_errors <- function(dataset) {

  if (unique(dataset$label)[1] == "" &
      length(unique(dataset$label)) == 1) {
    stop("Your frequencies label column is blank. Please provide labels on which to pivot")
  }

}

