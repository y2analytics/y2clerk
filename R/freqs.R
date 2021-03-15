##### Public functions #####

#' Run frequencies for multiple variables
#'
#' @param dataset A dataframe.
#' @param ... The unquoted names of a set of variables in the dataset. If nothing
#' is specified, the function runs a frequency on every column in given dataset.
#' @param stat Character, stat to run. Currently accepts 'percent,' 'mean,' 'median,' 'min,' 'max,' 'quantile,' and 'summary' (default: 'percent').
#' @param pr Double, for use when stat = 'quantile.' Input should be a real number x such that 0 <= x <= 100. Stands for percentile rank, which is a quantile relative to a 100-point scale. (default:NULL)
#' @param nas Boolean, whether or not to include NAs in the tabulation (default: TRUE).
#' @param wt The unquoted name of a weighting variable in the dataset (default: NULL).
#' @param prompt Boolean, whether or not to include the prompt in the dataset (default: FALSE).
#' @param digits Integer, number of significant digits for rounding (default: 2).
#' @param nas_group Boolean, whether or not to include NA values for the grouping variable in the tabulation (default: TRUE).
#' @param factor_group Boolean, whether or not to convert the grouping variable to a factor and use its labels instead of its underlying numeric values (default: FALSE)
#' @return A dataframe with the variable names, prompts, values, labels, counts,
#' stats, and resulting calculations.
#' @importFrom rlang .data
#' @examples
#' df <- data.frame(
#'   a = c(1, 2, 2, 3, 4, 2, NA),
#'   b = c(1, 2, 2, 3, 4, 1, NA),
#'   weights = c(0.9, 0.9, 1.1, 1.1, 1, 1, 1)
#' )
#'
#' freqs(df, a, b)
#' freqs(df, a, b, wt = weights)
#' freq(df, stat = 'mean', nas = FALSE)
#' freq(df, stat = 'mean', nas = FALSE, wt = weights)
#' df %>%
#'   dplyr::group_by(a) %>%
#'   freqs(b, stat = 'mean', nas = FALSE, wt = weights)
#'
#' # Note that pr = 60 will return an estimate
#' # of the real number such that 60% of values
#' # are lower than that number
#'
#' # * note also that minimums and maximums are
#' # unaffected by weighting
#' freqs(df, a, stat = 'min', nas = FALSE)
#' freqs(df, a, stat = 'median', nas = FALSE)
#' freqs(df, a, stat = 'quantile', pr = 95, nas = FALSE)
#' freqs(df, a, stat = 'summary', nas = FALSE, wt = weights)
#' @export

freqs  <- function(
  dataset,
  ...,
  stat = c("percent", "mean", "median", "min", "max", "quantile", "summary"),
  pr = NULL,
  nas = TRUE,
  wt = NULL,
  prompt = FALSE,
  digits = 2,
  nas_group = TRUE,
  factor_group = FALSE
  ) {
  # options(warn=-1)
  stat <- rlang::arg_match(stat)

  if(factor_group == TRUE){dataset <- group_factor(dataset)}
  if(nas_group == FALSE){dataset <- remove_group_nas(dataset)}
  weight <- dplyr::enquo(wt)
  variables <- dplyr::quos(...)

  # If no variables are specified in the function call,
  # assume the user wants to run a frequency on all columns.
  if(!length(variables)) {
    variables <- column_quos(dataset, wt = !!weight)
  }

  # suppressWarnings added for trivial bind rows warning
  # i used messages instead of warnings to still communicate my own conditional output
  suppressWarnings(
    frequencies <- purrr::map_dfr(
      .x = variables,
      .f = function(variable) {
        freq_var(dataset, !!variable, stat, pr, nas, !!weight, prompt, digits)
      }
    )
  )
  frequencies <- group_rename(frequencies)

  return(frequencies)
  # options(warn=0)
}

#' @rdname freqs
#' @export
freq <- freqs

# Private functions -------------------------------------------------------

calculate_result_for_cont_var <- function(dataset, variable, stat, pr, wt) {

  # first: (if wt = NULL) change class so logical test can be performed in all cases:
  if(base::is.null(wt)) {
    wt <- dplyr::enquo(wt)
  }

  # next: separate, verbose specifications for mean and quantile when weight is provided/not provided
  # (these if-else structures are inefficient but I wanted to be really clear about what we want)

  if(stat == 'mean') {
    # 1) wt = NULL
    if(rlang::quo_is_null(wt)) {
      out_df <- dataset %>%
        # always filter nas because the function previously checked
        # to ensure nas = FALSE is set if necessary
        dplyr::filter(!is.na(!!variable)) %>%
        dplyr::summarise(n = base::length(!!variable),
                         result = base::mean(!!variable)
        )
    }
    # 2) wt exists in dataset
    else {
      out_df <- dataset %>%
        dplyr::filter(!is.na(!!variable)) %>%
        dplyr::summarise(n = base::sum(!!wt),
                         result = stats::weighted.mean(!!variable,
                                                       !!wt)
        )
    }
  }

  if(stat %in% c('quantile', 'median', 'min', 'max')) {

    if(stat == 'median'){
      pr <- 50
    } else if(stat == 'min') {
      pr <- 0
    } else if(stat == 'max') {
      pr <- 100
    }

    if(stat %in% c('min', 'max')) {
      # mins and maxes are never weighted, per our decision
      wt <- dplyr::quo(NULL)
    }

    # 1) wt = NULL
    if(rlang::quo_is_null(wt)) {
      out_df <- dataset %>%
        # always filter nas because the function previously checked
        # to ensure nas = FALSE is set if necessary
        dplyr::filter(!is.na(!!variable)) %>%
        dplyr::summarise(n = base::length(!!variable),
                         result = stats::quantile(x = !!variable,
                                                  probs = pr / 100)
        )
    }
    # 2) wt exists in dataset
    else {
      out_df <- dataset %>%
        dplyr::filter(!is.na(!!variable)) %>%
        dplyr::summarise(n = base::length(!!variable),
                         result = reldist::wtd.quantile(!!variable,
                                                        q = pr / 100,
                                                        weight = !!wt)
        )
    }
  }
  return(out_df)
}

validate_inputs <- function(dataset, variable, stat, pr, nas, wt, prompt, digits) {

  # "failing fast"

  # 0) validate percentile rank
  if(stat == 'quantile' & is.null(pr)) stop("No input given for pr (percentile rank)")


  if(stat == 'quantile' & !is.null(pr)) {
    if(pr < 0 | pr > 100) stop('Percentile rank should be between 0 and 100, inclusive')
  }

  if(stat == 'quantile' & !is.null(pr)) {
    if(pr < 1) rlang::inform('Remember that pr ranges between 0 and 100. pr = 0.5 returns the bottom half percentile, whereas pr = 50 returns the median.')
  }

  # 1) if there are NAs in the data, you should use nas = FALSE
  if(nas) {
    count_nas <- dataset %>%
      dplyr::filter(is.na(!!variable)) %>%
      base::nrow()
    if(count_nas != 0) stop('NAs present in variable(s); to proceed, set nas = F')
  }

  # 2) can't take mean of categorical variable
  check_class <- dataset %>%
    dplyr::select(!!variable) %>%
    labelled::remove_labels() %>%
    dplyr::pull() %>%
    base::class()

  # make length = 1: collapse c("ordered", "factor") ==> c("ordered factor") as necessary
  check_class <- stringr::str_c(check_class, collapse = " ")

  # if not one of these types, stop
  if(! (check_class %in% c("numeric", "integer")) ) stop("Can't take mean of non-numeric variable")


  # 3) stop if value labels exist
  check_labels <- dataset %>%
    dplyr::select(!!variable) %>%
    labelled::val_labels() %>%
    tibble::deframe() %>%
    base::is.null()
  if(! check_labels) stop("Value labels exist; consider converting values to labels or using stat = 'percent'")

  # 4) give reminder if pr input given when stat is not set to 'quantile'
  if(!(stat %in% c('quantile', 'summary'))) {
    if(!is.null(pr)) rlang::inform("Remember that the percentile rank argument impacts output only when stat = 'quantile'")
  }
}

get_output_for_cont_var <- function(dataset, variable, stat, pr, nas, wt, prompt, digits) {

  # validation & checks
  validate_inputs(dataset,
                  variable,
                  stat,
                  pr,
                  nas,
                  wt,
                  prompt,
                  digits)

  # get mean or quantile
  out_df <- calculate_result_for_cont_var(dataset,
                                          variable,
                                          stat,
                                          pr,
                                          wt)

  # get group column names to add later (if they exist/as necessary)
  grouping_vars <- c("")
  if(dplyr::is.grouped_df(dataset)) {
    grouping_vars <- dplyr::group_vars(dataset)
  }

  # produce dataframe to output

  # make copy of "stat". the stat variable in the output data frame and the
  # stat function argument don't play well together here.
  statistic <- stat
  rm(stat)
  # this is not a great fix imo but it's been a pretty resilient problem.
  # if possible, i would rename either the column or the argument, but
  # on the other hand, either of those would presumably be breaking changes

  # for convenience:
  if(is.null(pr)) {
    pr <- -99
  }

  out_df <- out_df %>%
    dplyr::mutate(variable = dplyr::quo_name(variable),
                  value = '',
                  label = '',
                  # different labels depending on input
                  stat = dplyr::case_when(
                    statistic == 'mean' ~ 'mean',
                    statistic == 'min' ~ 'min',
                    statistic == 'median' ~ 'median',
                    statistic == 'max' ~ 'max',
                    statistic == 'quantile' &
                      !(pr %in% c(0,50,100)) ~ stringr::str_c('q', pr),
                    statistic == 'quantile' & pr == 0 ~ 'min',
                    statistic == 'quantile' & pr == 50 ~ 'median',
                    statistic == 'quantile' & pr == 100 ~ 'max',
                    TRUE ~ 'error'
                  ),
                  # add 'weighted' string to existing value in stat column as relevant
                  stat = dplyr::case_when(
                    !rlang::quo_is_null(wt) & !(stat %in% c('min', 'max')) ~ stringr::str_c(stat, ' - weighted'),
                    TRUE ~ stat
                  ),
                  n = base::round(.data$n,
                                  digits),
                  result = base::round(.data$result,
                                       digits)) %>%
    dplyr::select(tidyselect::one_of(grouping_vars),
                  .data$variable,
                  .data$value,
                  .data$label,
                  .data$n,
                  .data$stat,
                  .data$result) %>%
    tibble::as_tibble()

  # fill out prompt column if specified
  if(prompt) {

    prompt_text <- dataset %>%
      dplyr::ungroup() %>%
      dplyr::select(!!variable) %>%
      labelled::var_label() %>%
      tibble::deframe()

    # when prompt = TRUE but there is no variable label, output ""
    if(is.null(prompt_text)) {
      prompt_text <- ""
    }

    # final column ordering
    out_df <- out_df %>%
      dplyr::mutate(
        prompt = prompt_text
      ) %>%
      dplyr::select(tidyselect::one_of(grouping_vars),
                    .data$variable,
                    .data$prompt,
                    .data$value,
                    .data$label,
                    .data$n,
                    .data$stat,
                    .data$result)
  }

  # if weights are used, remove weight column rows from output
  if(!rlang::quo_is_null(wt)) {
    out_df <- out_df %>%
      dplyr::filter(variable != rlang::quo_name(wt))
  }

  # for convenience:
  if(pr == -99) {
    pr <- NULL
  }

  return(out_df)
}

get_summary_output_for_cont_var <- function(dataset, variable, stat, pr, nas, wt, prompt, digits) {

  # add redundant reminder because  subsequent code overrides user inputs for stat & pr
  # [for other cases, this reminder is also present in validate_inputs()]
  if(!is.null(pr)) rlang::inform("Remember that the percentile rank argument impacts output only when stat = 'quantile'")

  out <- dplyr::bind_rows(
    get_output_for_cont_var(dataset, variable, stat = 'min', pr,            nas, wt, prompt, digits),
    get_output_for_cont_var(dataset, variable, stat = 'quantile', pr = 25,  nas, wt, prompt, digits),
    get_output_for_cont_var(dataset, variable, stat = 'median', pr,         nas, wt, prompt, digits),
    get_output_for_cont_var(dataset, variable, stat = 'mean', pr,           nas, wt, prompt, digits),
    get_output_for_cont_var(dataset, variable, stat = 'quantile', pr = 75,  nas, wt, prompt, digits),
    get_output_for_cont_var(dataset, variable, stat = 'max', pr,            nas, wt, prompt, digits)
  ) %>%
    dplyr::mutate(stat = forcats::fct_relevel(stat,
                                       c('min',
                                         'q25',
                                         'q25 - weighted',
                                         'median',
                                         'median - weighted',
                                         'mean',
                                         'mean - weighted',
                                         'q75',
                                         'q75 - weighted',
                                         'max')
    )
    )

  return(out)
}

group_factor <- function(dataset){
  grouping_vars <- dplyr::group_vars(dataset)
  if(length(grouping_vars) > 1){ #if there are 2+ grouping vars
    group_flag <- dplyr::group_vars(dataset)[1] %>% as.symbol()
    group_flag2 <- dplyr::group_vars(dataset)[2] %>% as.symbol()
    dataset <- dataset %>%
      dplyr::ungroup() %>%
      dplyr::mutate_at(
        dplyr::vars(
          grouping_vars
        ),
        list(~forcats::as_factor(.))
      ) %>%
      dplyr::group_by(
        !!group_flag,
        !!group_flag2
      )
    return(dataset)
    } else if(length(grouping_vars) == 1){ #1 grouping var
      group_flag <- dplyr::group_vars(dataset)[1] %>% as.symbol()
      dataset <- dataset %>%
        dplyr::ungroup() %>%
        dplyr::mutate_at(
          dplyr::vars(
            grouping_vars
          ),
          list(~forcats::as_factor(.))
        ) %>%
        dplyr::group_by(
          !!group_flag
        )
      return(dataset)
    } else{ # no grouping vars
      dataset <- dataset
    }
  return(dataset)
  }

remove_group_nas <- function(dataset){
  grouping_vars <- dplyr::group_vars(dataset)
  if(length(grouping_vars) > 1){ #if there are 2+ grouping vars
    group_flag <- dplyr::group_vars(dataset)[1] %>% as.symbol()
    group_flag2 <- dplyr::group_vars(dataset)[2] %>% as.symbol()
    dataset <- dataset %>%
      dplyr::filter(
        !is.na(!!group_flag),
        !is.na(!!group_flag2)
      )
    return(dataset)
  } else if(length(grouping_vars) == 1){ #1 grouping var
    group_flag <- dplyr::group_vars(dataset)[1] %>% as.symbol()
    dataset <- dataset %>%
      dplyr::filter(
        !is.na(!!group_flag)
      )
    return(dataset)
  } else{ # no grouping vars
    dataset <- dataset
  }
  return(dataset)
}

group_rename <- function(dataset){
  if(names(dataset)[1] != 'variable'){
    if(names(dataset)[2] != 'variable'){ #if there are 2 grouping vars
      dataset <- dataset %>%
        dplyr::rename(
          group_var = names(dataset)[1],
          group_var2 = names(dataset)[2]
        )
    }else{ #if there is 1 grouping var
      dataset <- dataset %>%
        dplyr::rename(group_var = names(dataset)[1])
    }
    #NOT GROUPED
  } else{
    dataset <- dataset
  }
  return(dataset)
}

freq_var <- function(dataset, variable, stat = 'percent', pr = 50, nas = TRUE, wt = NULL, prompt = FALSE, digits = 2) {
  variable <- dplyr::enquo(variable)
  wt <- dplyr::enquo(wt)

  # check stat argument input
  if(!(stat %in% c('percent','mean','quantile',
                   'summary','min','max','median'))) stop('"stat" argument must receive a value from c("percent", "mean", "quantile", "summary", "min", "median", "max")')

  if(stat == 'percent') {
    base <- ns(dataset, variable, wt, prompt)
    freq_result <- base %>%
      percents(nas, digits = digits)
  }

  else if(stat %in% c('mean', 'quantile', 'min', 'median', 'max')) {
    freq_result <- get_output_for_cont_var(dataset, variable, stat, pr, nas, wt, prompt, digits)
  }

  else if(stat == 'summary') {
    freq_result <- get_summary_output_for_cont_var(dataset, variable, stat, pr, nas, wt, prompt, digits)
  }

  return(freq_result)
}

column_quos <- function(dataset, wt) {
  col_names <- dataset %>% colnames()
  if (dplyr::is.grouped_df(dataset)) {
    # Exclude grouping variables since they cannot be counted independent of groups.
    grouping_vars <- dplyr::group_vars(dataset)
    col_names <- setdiff(col_names, grouping_vars)
  }
  # Exclude weighting variable from freqs in select
  weight_name <- rlang::enquo(wt) %>% rlang::as_label()
  col_names <- setdiff(col_names, weight_name)

  col_syms <- col_names %>% dplyr::syms()
  col_quos <- purrr::map(col_syms, dplyr::quo)
  class(col_quos) <- append(class(col_quos),"quosures", after = 0)
  return(col_quos)
}

ns <- function(dataset, variable, weight, prompt) {
  counts <- if(class(dataset %>% dplyr::pull(!!variable)) %in% c('labelled','haven_labelled','haven_labelled_spss')) {
    # Metadata is better if the given variable has labels
    labelled_ns(dataset, variable, weight, prompt)
  } else {
    # Otherwise, use some sensible defaults
    unlabelled_ns(dataset, variable, weight, prompt)
  }
  # Reorder because Scotty is OCD
  if(prompt) {
    counts %>%
      dplyr::select(
        .data$variable,
        .data$prompt,
        .data$value,
        .data$label,
        .data$n
      )
  } else {
    counts %>%
      dplyr::select(
        .data$variable,
        .data$value,
        .data$label,
        .data$n
      )
  }
}

percents <- function(counts, include_nas, digits) {
  # Filter out NAs if requested
  if(! include_nas) {
    counts <- counts %>%
      dplyr::filter(
        !is.na(.data$value)
      )
  }
  # Calculate and round to integer percentages
  counts %>%
    dplyr::mutate(
      stat = 'percent',
      result = (.data$n / sum(.data$n)) %>% round(digits)
    )
}

labelled_ns <- function(dataset, variable, weight, prompt) {
  # Extract the metadata from the labelled class
  counts <- base_ns(dataset, variable, weight)
  if(prompt) {
    counts <- counts %>%
      dplyr::mutate(
        prompt = labelled::var_label(.data$value)
      )
  }
  counts <- counts %>%
    dplyr::mutate(
      label = labelled::to_factor(.data$value) %>% as.character,
      value = .data$value %>% as.character
    )
  return(counts)
}

unlabelled_ns <- function(dataset, variable, weight, prompt) {
  counts <- base_ns(dataset, variable, weight) %>%
    dplyr::mutate(
      label = .data$value %>% as.character,
      value = .data$value %>% as.character
    )
  if(prompt) {
    counts <- counts %>%
      dplyr::mutate(
        prompt = ''
      )
  }
  return(counts)
}

base_ns <- function(dataset, variable, weight) {
  dataset %>%
    # When wt is NULL, it runs unweighted counts
    dplyr::count(!!variable, wt = !!weight, .drop = FALSE) %>%
    dplyr::rename(value = !!variable) %>%
    dplyr::mutate(
      variable = dplyr::quo_name(variable)
    )
}
