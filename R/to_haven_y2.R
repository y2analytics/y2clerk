# to_haven_y2() --------------------------------------------------------------------

#' Convert a variable to haven labelled
#'
#' Convert a character or factor vector into a dbl labelled (haven labelled) vector. Useful for when you need to either extract labels or attach underlying numbers to each label
#'
#' @param variable The vector or variable within a data frame that you wish to convert to haven labelled
#' @examples
#'
#' test <- tibble::tibble(
#'   color_factor = c('Blue', 'Blue', 'Red', 'Yellow') %>%
#'     forcats::as_factor()
#' )
#'
#' test$color <- to_haven_y2(test$color_factor)
#' test <- test %>%
#'   dplyr::mutate(
#'     color = to_haven_y2(color_factor)
#'   )
#' @export

to_haven_y2 <- function(
    variable
) {

  # Errors
  if (class(variable)[1] == 'numeric') {
    variable_char <- deparse(substitute(variable))
    stop(stringr::str_c('to_haven_y2 cannot be used on numeric variable: ', variable_char))
  }
  if (class(variable)[1] == 'haven_labelled') {
    variable_char <- deparse(substitute(variable))
    stop(stringr::str_c(variable_char, ' is already a haven_labelled variable'))
  }

  # Functionality
  if (class(variable)[1] == 'character') {
    variable <- forcats::as_factor(variable)
  }
  var_numeric <- as.numeric(variable)
  var_character <- as.character(variable)
  var_numeric_unique <- var_numeric %>% unique()
  var_character_unique <- var_character %>% unique()

  matching_vector <- var_numeric_unique
  names(matching_vector) <- var_character_unique

  haven_vector <- haven::labelled(
    x = c(var_numeric),
    labels = c(matching_vector)
  )
}


