#' Title
#'
#' @param df A Tibble or object that can be coerced to a tibble
#' @param p A length 1+ vector of named prompts
#'
#' @returns A freq_y2 class (Subclass of a tibble)
as_freq_y2 <- function(df, p = NULL) {
  if ('freq_y2' %in% class(df)) {
    return(df)
  }

  if (!('tbl_df' %in% class(df))) {
    df <- tibble::as_tibble(df)
  }


  if (!is.null(p)) {
    #that p is a named character vector
    if (!is.character(p)) {
      stop("p must be a character vector")
    }

    #Make sure that p is named for every element of p
    if (any(is.null(names(p))) | any(names(p) == "")) {
      stop("Every element of p must be named")
    }

    attr(df, "prompts") <- p
  }

  class(df) <- c("freq_y2", class(df))
  return(df)
}


#' @exportS3Method dplyr::as_tibble
as_tibble.freq_y2 <- function(
  x,
  ...,
  .rows = NULL,
  .name_repair = c("check_unique", "unique", "universal", "minimal"),
  rownames = pkgconfig::get_config("tibble::rownames", NULL)
) {
  attr(x, 'prompts') <- NULL
  NextMethod()
}

#' @exportS3Method
as.data.frame.freq_y2 <- function(
  x,
  row.names = NULL,
  optional = FALSE,
  ...
) {
  attr(x, 'prompts') <- NULL
  NextMethod()
}


#' @exportS3Method dplyr::group_by
group_by.freq_y2 <- function(.data, ..., add = FALSE, .drop = dplyr::group_by_drop_default(.data)) {
  prompts <- attr(.data, "prompts")
  class(.data) <- setdiff(class(.data), "freq_y2")
  as_freq_y2(NextMethod(), p = prompts)
}










