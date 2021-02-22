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
#' responses <- tibble::tibble(
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
#' verbatims_y2(responses, var1)


#### Public function ####
verbatims_y2 <- function(
  dataset,
  ...
){
  freq_flags <- dplyr::quos(...)

  # If no variables are specified in the function call,
  # assume the user wants to run a frequency on all columns.
  if(!length(freq_flags)) {
    freq_flags <- column_quos_verbatims(dataset)
  }

    frequencies <- purrr::map_dfr(
      .x = freq_flags,
      .f = function(freq_flag) {
        verbatims_y2_single(dataset, !!freq_flag)
      }
    )

  if("No label" %in% frequencies$prompt == TRUE){
    warning("You are working with variables that have no labeling. You may want to consider adding a prompt before continuing")
  }

  return(frequencies)

}


#### Private functions ####
taking_names <- function(dataset) {
  labels <- sapply(dataset, function(x) attr(x, "label"))
  tibble::tibble(
    name = names(labels),
    label = labels %>% as.character()
    )
}


column_quos_verbatims <- function(dataset) {
  col_names <- dataset %>% colnames()
  col_syms <- col_names %>% dplyr::syms()
  col_quos <- purrr::map(col_syms, dplyr::quo)
  class(col_quos) <- append(class(col_quos),"quosures", after = 0)
  return(col_quos)
}


verbatims_y2_single <- function(
  dataset,
  freq_var
){
  freq_flag <- dplyr::enquo(freq_var)
  freq_var_char <- rlang::quo_name(freq_flag) #convert quoed var into a string

  var_label_list <- taking_names(dataset) %>%
    dplyr::mutate(
      label = as.character(.data$label)
    ) %>%
    dplyr::mutate(
      label = dplyr::case_when(
        .data$label == "NULL" ~ "No label",
        .data$label == "" ~ "No label",
        TRUE ~ .data$label
      )
    ) %>%
    dplyr::select(
      .data$label
    ) %>%
    unlist()

  labelled::var_label(dataset) <- var_label_list

  freq_df <- dataset %>%
    dplyr::select(
      !!freq_flag
    ) %>%
    dplyr::mutate(variable = freq_var_char) %>%
    dplyr::select(
      .data$variable,
      label = !!freq_flag
      )

  labels <- dataset %>%
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
    )%>%
    dplyr::mutate(variable = freq_var_char) %>%
    dplyr::select(
      .data$variable,
      prompt = !!freq_flag
    ) %>%
    dplyr::distinct(
      .data$variable,
      .keep_all = T
    )

  dplyr::left_join(freq_df, labels, by = c("variable")) %>%
    dplyr::select(
      .data$variable,
      .data$prompt,
      .data$label
    ) %>%
    dplyr::filter(
      .data$label != ""
    )

}

