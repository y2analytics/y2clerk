#' Title
#'
#' @param df A Tibble or object that can be coerced to a tibble
#' @param p A length 1+ vector of named prompts
#'
#' @returns A freq_y2 class (Subclass of a tibble)
#' @export
#'
#' @examples
as_freq_y2 <- function (df, p) {
  df <- tibble::as_tibble(df)

  #that p is a named character vector
  if (!is.character(p)) {
    stop("p must be a character vector")
  }

  #Make sure that p is named for every element of p
  if (any(is.null(names(p))) | any(names(p) == "")) {
    stop("Every element of p must be named")
  }

  attr(df, "prompts") <- p
  class(df) <- c("freq_y2", class(df))
  return(df)
}

#' @export
print.freq_y2 <- function(df) {
  p <- attr(df, "prompts")

  for (i in seq_along(p)) {
    cat(names(p)[i], ": ", p[i], "\n", sep="")
  }

  NextMethod()
}

