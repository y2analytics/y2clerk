#### verbatims_y2 ####
### Description
#' Create a dataframe of open-ended questions formatted for internbot, used to create appendices
#'
#' Formats open-ended questions with the proper columns for internbot: variable, prompt, label
#' @param dataset no default. Usually piped in from your main dataset
#' @param ... The names of the openended variables from your dataset you want to include in your new dataframe
#' @keywords openend open end frequencies freqs internbot appendix
#' @export
#' @examples
#' responses <- tibble(
#'   var1 = c(
#'     'I like to talk about dogs',
#'     'Dogs are cool but cats are aight too',
#'     'I prefer dogs over cats',
#'     "My dog's collars are always too tight",
#'     'One last sentence about dogs',
#'     'Cats collars are typically cooler than dogs'
#'   )
#' )
#'
#' responses %>% verbatims_y2(var1)


#### Public function ####
verbatims_y2 <- function(
  df,
  ...
){
  freq_flags <- dplyr::quos(...)

    frequencies <- purrr::map_dfr(
      .x = freq_flags,
      .f = function(freq_flag) {
        verbatims_y2_single(df, !!freq_flag)
      }
    )

  if("No label" %in% frequencies$prompt == TRUE){
    warning("You are working with variables that have no labeling. You may want to consider adding a prompt before continuing")
  }

  return(frequencies)

}


#### Private functions ####
taking_names <- function(dataset = responses) {
  labels <- sapply(dataset, function(x) attr(x, "label"))
  tibble::tibble(name = names(labels),
                 label = labels)
}

verbatims_y2_single <- function(
  df,
  freq_var
){
  freq_flag <- dplyr::enquo(freq_var)

  var_label_list <- taking_names(df) %>%
    dplyr::mutate(
      label = as.character(label)
    ) %>%
    dplyr::mutate(
      label = dplyr::case_when(
        label == "NULL" ~ "No label",
        label == "" ~ "No label",
        TRUE ~ label
      )
    ) %>%
    dplyr::select(
      label
    ) %>%
    unlist()

  labelled::var_label(df) <- var_label_list

  freq_df <- df %>%
    dplyr::select(
      !!freq_flag
    ) %>%
    tidyr::gather(
      variable,
      label
    )

  labels <- df %>%
    dplyr::select(
      !!freq_flag
    ) %>%
    dplyr::mutate_all(
      list(
        ~dplyr::case_when(
          is.na(.) ~ labelled::var_label(.),
          TRUE ~ labelled::var_label(.)
        )
      )
    ) %>%
    tidyr::gather(
      variable,
      prompt
    ) %>%
    dplyr::distinct(
      variable,
      .keep_all = T
    )

  dplyr::left_join(freq_df, labels, by = c("variable")) %>%
    dplyr::select(
      variable,
      prompt,
      label
    ) %>%
    dplyr::filter(
      label != ""
    )

}
