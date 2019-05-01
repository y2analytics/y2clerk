#' Auto change those pesky "Other please specify"s into "Other"
#'
#' Takes a dataframe (frequencies) and replaces the usual variations of "Other please specify" into Other. Converts all "None of the above" variations into "None of the above". Also removes all extra text in parantheses. Does this for both the 'label' and 'variable' vars.
#' @param dataset The name of the data frame that the mscharts pulls from, automatically included if function is piped in after running freqs. You almost never need any arguments in this function.
#' @keywords other none extra
#' @export
#' @examples
#' frequencies %>% other_rm()
#

other_rm <- function(
  dataset
) {
  #Remove for 'label' var
  dataset <- dataset %>%
    mutate(
      label = as.character(label),
      label = dplyr::case_when(
        stringr::str_detect(label, regex('please specify', ignore_case = T)) == T ~ 'Other',
        stringr::str_detect(label, regex('none of the', ignore_case = T)) == T ~ 'None of the above',
        label == 'None' ~ 'None of the above',
        T ~ label
      ),
      label = str_remove_all(label, ' \\(.*')
    )
  #Remove for 'variable' var
  dataset <- dataset %>%
    mutate(
      variable = as.character(variable),
      variable = dplyr::case_when(
        stringr::str_detect(variable, regex('please specify', ignore_case = T)) == T ~ 'Other',
        stringr::str_detect(variable, regex('none of the', ignore_case = T)) == T ~ 'None of the above',
        variable == 'None' ~ 'None of the above',
        T ~ variable
      ),
      variable = str_remove_all(variable, ' \\(.*')
    )
}


