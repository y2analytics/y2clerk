# to_haven_y2() --------------------------------------------------------------------

#' Convert a variable to haven labelled
#'
#' Convert a character or factor vector into a labelled (haven labelled) vector. Useful for when you need to either extract labels or attach underlying numbers to each label.
#'
#' @param variable The vector you wish to convert to haven labelled
#' @param ... Reserved for future expansion. Must be empty.
#' @examples
#'
#' test <- tibble::tibble(
#'   color_vec = c('Blue', 'Blue', 'Red', 'Yellow'),
#'   color_factor = forcats::as_factor(color_vec)
#' )
#'
#' test$color <- to_haven_y2(test$color_factor)
#'
#' test <- test |>
#'   dplyr::mutate(
#'     color = to_haven_y2(color_vec)
#'   )
#' @export

to_haven_y2 <- function(variable, ...) {
  rlang::check_dots_empty()
  UseMethod("to_haven_y2")
}

#' @export
to_haven_y2.default <- function(variable, ...) {
  variable_char <- deparse(match.call()[[2]])
  cli::cli_abort(
    c(
      "x" = "{.fn to_haven_y2} cannot be used on {.cls {class(variable)[1]}} variable: {variable_char}"
    ),
    call = rlang::call2("to_haven_y2")
  )
}

#' @export
to_haven_y2.character <- function(variable, ...) {
  to_haven_y2(forcats::as_factor(variable))
}

#' @export
to_haven_y2.factor <- function(variable, ...) {
  var_levels <- levels(variable)
  matching_vector <- seq_along(var_levels)
  names(matching_vector) <- var_levels

  haven::labelled(
    x = as.integer(variable),
    labels = matching_vector
  )
}

#' @export
to_haven_y2.haven_labelled <- function(variable, ...) {
  variable
}
