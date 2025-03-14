#' Title
#'
#' @param df A Tibble or object that can be coerced to a tibble
#' @param p A length 1+ vector of named prompts
#'
#' @returns A freq_y2 class (Subclass of a tibble)
#' @export
as_freq_y2 <- function (df, p = NULL) {
  df <- tibble::as_tibble(df)

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





#' @export
print.freq_y2 <- function (x, n, ...) {
  create_env_in_global()
  NextMethod()
}





#' @exportS3Method pillar::tbl_sum
tbl_sum.freq_y2 <- function (x, ...)
{
  c(`A frequency tibble` = pillar::dim_desc(x))
}





#' @exportS3Method pillar::tbl_format_header
tbl_format_header.freq_y2 <- function(x, setup, ...) {
  named_header <- setup$tbl_sum
  focus <- attr(x, "pillar_focus")
  if (!is.null(focus)) {
    named_header <- c(named_header, `Focus columns` = pillar:::collapse(pillar:::tick_if_needed(focus)))
  }
  if (all(rlang::names2(named_header) == "")) {
    header <- named_header
  }
  else {
    header <- paste0(pillar::align(paste0(rlang::names2(named_header), ":"),
                           space = "\U00A0"), " ", named_header)
  }

  qs <- get_question_wordings(x, setup)

  pillar::style_subtle(c(qs, pillar:::format_comment(header, width = setup$width)))
}


get_question_wordings <- function(x, setup) {
  #Question Names
  p <- attr(x, "prompts")
  qs <- character()

  if (length(p) > 0) {
    max_iter <- min(3, length(p))

    for (i in seq_len(max_iter)) {
      qs[i] <- pillar::style_subtle(
        pillar:::format_comment(
          paste0(cli::style_underline(names(p)[i]), ": ", p[i], "\n", sep=""),
          width = setup$width
        )
      )
    }
    questions_left <- length(p) - max_iter
    #display a message that says: X more questions with labels
    if (questions_left > 0) {
      qs[max_iter + 1] <- pillar::style_subtle(
        pillar:::format_comment(
          paste0(cli::symbol$info, " ", questions_left, " more questions with labels\n", sep=""),
          width = setup$width
        )
      )
      qs[max_iter + 2] <- pillar::style_subtle(
        pillar:::format_comment(
          "\n",
          width = setup$width
        )
      )
    } else {
      qs[max_iter + 1] <- pillar::style_subtle(
        pillar:::format_comment(
          "\n",
          width = setup$width
        )
      )
    }



  }
  return(qs)
}


#' @exportS3Method pillar::tbl_format_footer
tbl_format_footer.freq_y2 <- function(x, setup, ...) {
  footer <- pillar:::format_footer(x, setup)
  footer_comment <- pillar:::wrap_footer_bullet(footer, setup)
  footer_advice <- format_footer_advice.freq_y2(x, setup)
  footer_advice_comment <- pillar:::wrap_footer_bullet(footer_advice,
                                                       setup, lines = 1, ellipsis = FALSE, bullet = cli::symbol$info)
  pillar::style_subtle(c(footer_comment, pillar:::format_comment(footer_advice_comment, width = setup$width)))
}





#' @exportS3Method pillar::format_footer_advice
format_footer_advice.freq_y2 <- function(x, setup) {
  if (!isTRUE(pillar:::pillar_options$advice())) {
    return()
  }
  if (setup$extra_cols_total > length(setup$extra_cols)) {
    cols <- "`colnames()` to see all variable names"
  }
  else {
    cols <- NULL
  }
  if (is.na(setup$rows_missing) || setup$rows_missing > 0) {

    .print_buffer$last_freq <- x

    rows <- cli::cli_text(cli::col_silver(
      "# {cli::symbol$info} Use `{.run [print(n = ...)](y2clerk::print_freq_inf())}` to see more rows"
      ))
    ###cli_text ends this function
  }
  else {
    rows <- NULL
  }
  advice <- pillar:::enum_collapse(c(rows, cols))
  if (length(advice) == 0) {
    return()
  }
  paste0("Use ", paste(advice, collapse = " "))
}




#' Print with n = Inf
#'
#' @description
#' This function prints the most recently displayed frequency tibble with print(n = Inf)
#'
#' @export
print_freq_inf <- function() {
  print(.print_buffer$last_freq, n = Inf)
}


#' @exportS3Method dplyr::group_by
group_by.freq_y2 <- function(.data, ..., .add = FALSE, .drop = dplyr:::group_by_drop_default(.data)) {
  p <- attr(.data, 'prompts')
  groups <- group_by_prepare(.data, ..., .add = .add, error_call = rlang:::current_env())
  grouped_df(groups$data, groups$group_names, .drop) |> as_freq_y2(p)
}


#' Create print buffer environment if it does not exist
create_env_in_global <- function() {
  if (!exists(".print_buffer", envir = .GlobalEnv)) {
    eval(quote(.GlobalEnv$.print_buffer <- new.env(parent = emptyenv())), envir = .GlobalEnv)
  }
}



#' @exportS3Method dplyr::as_tibble
as_tibble.freq_y2 <- function(x, ...,
                              .rows = NULL,
                              .name_repair = c("check_unique",
                                               "unique",
                                               "universal",
                                               "minimal"),
                              rownames = pkgconfig::get_config("tibble::rownames",
                                                               NULL)) {
  attr(x, 'prompts') <- NULL
  NextMethod()
}


