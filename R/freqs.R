##### Public functions #####

#' Run frequencies for multiple variables
#'
#' @param dataset A dataframe.
#' @param ... <tidy-select> One or more unquoted expressions separated by commas. Variable names can be used as if they were positions in the data frame, so expressions like x:y can be used to select a range of variables. If nothing
#' is specified, the function runs a frequency on every column in given dataset.
#' @param .by <tidy-select> Variables to group by for this operation only. Cannot be used when the dataset is already a grouped data frame.
#' @param stat Character, stat to run. Currently accepts 'percent,' 'mean,' 'median,' 'min,' 'max,' 'quantile,' and 'summary' (default: 'percent').
#' @param percentile Double, for use when stat = 'quantile.' Input should be a real number x such that 0 <= x <= 100. Stands for percentile rank, which is a quantile relative to a 100-point scale. (default:NULL)
#' @param nas Boolean, whether or not to include NAs in the tabulation (default: TRUE).
#' @param wt The unquoted name of a weighting variable in the dataset (default: NULL).
#' @param prompt Boolean, whether or not to include the prompt in the dataset (default: FALSE).
#' @param digits Integer, number of significant digits for rounding (default: 2).
#' @param nas_group Boolean, whether or not to include NA values for the grouping variable in the tabulation (default: TRUE).
#' @param factor_group Boolean, whether or not to convert the grouping variable to a factor and use its labels instead of its underlying numeric values (default: FALSE)
#' @param unweighted_ns Boolean, whether the 'n' column in the freqs table should be Unweighted while results ARE weighted. This argument can only be used if a wt variable is used. If no weight variable is used, the 'n' column will always be unweighted (default: FALSE).
#' @param show_missing_levels Boolean, whether to keep response levels with no data (default: TRUE)
#' @return A dataframe with the variable names, prompts, values, labels, counts,
#' stats, and resulting calculations.
#' @seealso [y2clerk-options] for setting `y2clerk.quantile_algorithm` globally.
#' @importFrom rlang .data
#' @examples
#' df <- data.frame(
#'   a = c(1, 2, 2, 3, 4, 2, NA),
#'   b = c(1, 2, 2, 3, 4, 1, NA),
#'   c = c("Red", "Red", "Blue", NA, NA, NA, "Yellow"),
#'   weights = c(0.9, 0.9, 1.1, 1.1, 1, 1, 1)
#' )
#'
#' freqs(df, a, b)
#' freqs(df, a, b, wt = weights)
#' freq(df, a:b)
#' freq(df, tidyselect::starts_with('a'), wt = weights)
#' freq(df, nas = FALSE)
#' freq(df, tidyselect::where(is.numeric), stat = 'mean', nas = FALSE, wt = weights)
#' df |>
#'   dplyr::group_by(a) |>
#'   freqs(b, nas = FALSE, wt = weights)
#' freqs(df, b, .by = a)
#'
#' # Note that percentile = 60 will return an estimate
#' # of the real number such that 60% of values
#' # are lower than that number
#'
#' # * note also that minimums and maximums are
#' # unaffected by weighting
#' freqs(df, a, stat = 'min', nas = FALSE)
#' freqs(df, a, stat = 'median', nas = FALSE)
#' freqs(df, a, stat = 'quantile', percentile = 95, nas = FALSE)
#' freqs(df, a, stat = 'summary', nas = FALSE, wt = weights)
#' @export

freqs <- function(
  dataset,
  ...,
  .by = NULL,
  stat = c("percent", "mean", "median", "min", "max", "quantile", "summary"),
  percentile = NULL,
  nas = TRUE,
  wt = NULL,
  prompt = FALSE,
  digits = 2,
  nas_group = TRUE,
  factor_group = FALSE,
  unweighted_ns = FALSE,
  show_missing_levels = TRUE
) {
  # options(warn = -1)
  stat <- rlang::arg_match(stat)
  check_data_frame2(dataset)
  rlang::check_bool(nas)
  rlang::check_bool(prompt)
  rlang::check_bool(nas_group)
  rlang::check_bool(factor_group)
  rlang::check_bool(unweighted_ns)
  rlang::check_bool(show_missing_levels)
  rlang::check_number_whole(digits, min = 0)

  # .by grouping: resolve tidy-selection and apply as grouping
  freqs_call <- rlang::current_env()
  by_quo <- rlang::enquo(.by)
  by_sel <- tryCatch(
    tidyselect::eval_select(by_quo, data = dataset),
    error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("doesn't exist", msg, fixed = TRUE)) {
        col_match <- gsub("`", "", regmatches(msg, regexpr("`[^`]+`", msg)))
        hints <- col_hint(col_match, colnames(dataset))
        hint_bullet <- if (length(hints) > 0L) {
          c("i" = cli::format_inline("Did you mean: {.val {hints}}?"))
        } else {
          character(0)
        }
        cli::cli_abort(
          c(
            "{.arg .by} column {.var {col_match}} not found in {.arg dataset}.",
            hint_bullet
          ),
          call = freqs_call
        )
      } else {
        cli::cli_abort(
          c("Invalid {.arg .by} selection.", "x" = msg),
          call = freqs_call
        )
      }
    }
  )
  if (length(by_sel) > 0) {
    if (dplyr::is.grouped_df(dataset)) {
      cli::cli_abort(
        c(
          "Cannot use {.arg .by} on an already-grouped data frame.",
          "i" = "Use {.code dplyr::group_by()} or {.arg .by}, not both.",
          "i" = "The dataset is currently grouped by: {.val {dplyr::group_vars(dataset)}}."
        )
      )
    }
    by_vars <- names(by_sel)
    dataset <- dplyr::group_by(
      dataset,
      dplyr::across(tidyselect::all_of(by_vars))
    )
  }

  # Create logical for if there are weights
  weight_quo <- dplyr::enquo(wt)
  weight_exists <- !rlang::quo_is_null(weight_quo)

  if (weight_exists) {
    numeric_names <- dataset |>
      dplyr::select(tidyselect::where(is.numeric)) |>
      colnames()
    check_col(
      "wt",
      rlang::as_label(weight_quo),
      dataset,
      hint_cols = numeric_names,
      keywords = "wt|weight"
    )
  }

  if (unweighted_ns == TRUE && weight_exists == FALSE) {
    cli::cli_abort(
      c(
        "{.arg unweighted_ns} is {.val TRUE} but no weight variable was provided.",
        "i" = "Supply a weighting column via {.arg wt}, or set {.code unweighted_ns = FALSE}."
      )
    )
  } else if (unweighted_ns == TRUE && weight_exists == TRUE) {
    frequencies <- freqs_wuw(
      dataset,
      ...,
      stat = stat,
      percentile = percentile,
      nas = nas,
      wt = {{ wt }},
      prompt = prompt,
      digits = digits,
      nas_group = nas_group,
      factor_group = factor_group,
      show_missing_levels = show_missing_levels
    )
  } else {
    frequencies <- freqs_original(
      dataset,
      ...,
      stat = stat,
      percentile = percentile,
      nas = nas,
      wt = {{ wt }},
      prompt = prompt,
      digits = digits,
      nas_group = nas_group,
      factor_group = factor_group,
      show_missing_levels = show_missing_levels
    )
  }

  #Attach question wordings
  if ('variable' %in% names(frequencies)) {
    vars <- unique(frequencies$variable)
    p <- character(length(vars))
    names(p) <- vars

    for (var in vars) {
      p[var] <- attr(dataset[[var]], "label", exact = TRUE) %||% ""
    }

    p <- p[p != ""]
  }
  if (length(by_sel) > 0) {
    frequencies <- dplyr::ungroup(frequencies)
  }
  return(as_freq_y2(frequencies, p))
}

#' @rdname freqs
#' @export
freq <- freqs

# Private functions -------------------------------------------------------
# Freqs weighted results unweighted ns function
freqs_wuw <- function(
  dataset,
  ...,
  stat,
  percentile,
  nas,
  wt,
  prompt,
  digits,
  nas_group,
  factor_group,
  show_missing_levels
) {
  # run weighted freqs
  freqs_weighted <-
    dataset |>
    freqs_original(
      ...,
      stat = stat,
      percentile = percentile,
      nas = nas,
      wt = {{ wt }},
      prompt = prompt,
      digits = digits,
      nas_group = nas_group,
      factor_group = factor_group,
      show_missing_levels = show_missing_levels
    ) |>
    dplyr::select(-'n')

  # run unweighted freqs, but only keep n
  freqs_unweighted <-
    dataset |>
    dplyr::select(-{{ wt }}) |>
    freqs_original(
      ...,
      stat = stat,
      percentile = percentile,
      nas = nas,
      wt = NULL,
      prompt = prompt,
      digits = digits,
      nas_group = nas_group,
      factor_group = factor_group,
      show_missing_levels = show_missing_levels
    ) |>
    dplyr::select('n')

  # Ungroup if freqs are grouped
  if (dplyr::is.grouped_df(freqs_unweighted)) {
    freqs_unweighted <- freqs_unweighted |>
      dplyr::ungroup() |>
      dplyr::select('n')
  }

  # bind freqs together
  frequencies <- dplyr::bind_cols(
    freqs_weighted,
    freqs_unweighted
  ) |>
    dplyr::relocate(
      'n',
      .after = "label"
    )
  return(frequencies)
}


# Try including original freqs function as sub function
freqs_original <- function(
  dataset,
  ...,
  stat = stat,
  percentile = percentile,
  nas = nas,
  wt = wt,
  prompt = prompt,
  digits = digits,
  nas_group = nas_group,
  factor_group = factor_group,
  show_missing_levels = show_missing_levels
) {
  if (factor_group == TRUE) {
    dataset <- group_factor(dataset)
  }
  if (nas_group == FALSE) {
    dataset <- remove_group_nas(dataset)
  }
  weight <- dplyr::enquo(wt)

  # Capture the user-facing call (freqs/freq) for clean error attribution.
  user_call <- rlang::caller_env()

  if (...length() == 0L) {
    # Nothing passed: select all columns (minus weight, minus group vars)
    col_names <- column_names(dataset, wt = !!weight)
  } else {
    # tidyselect resolution - catch "column doesn't exist" and rethrow cleanly
    col_names <- tryCatch(
      colnames(dataset)[tidyselect::eval_select(
        rlang::expr(c(...)),
        data = dataset
      )],
      error = function(e) {
        msg <- conditionMessage(e)
        body <- if (grepl("doesn't exist", msg, fixed = TRUE)) {
          lines <- strsplit(msg, "\n", fixed = TRUE)[[1]]
          found <- trimws(lines[grepl("doesn't exist", lines, fixed = TRUE)])
          purrr::set_names(found, rep("x", length(found)))
        } else {
          c("x" = msg)
        }
        cli::cli_abort(
          c("One or more columns not found in {.arg dataset}.", body),
          call = user_call
        )
      }
    )
    # Exclude weight variable if it was inadvertently included
    if (!rlang::quo_is_null(weight)) {
      col_names <- setdiff(col_names, rlang::as_label(weight))
    }
    # Exclude group vars (mirrors column_names() behaviour for empty ...)
    col_names <- setdiff(col_names, dplyr::group_vars(dataset))
  }

  if (stat != 'percent') {
    validate_inputs_all(
      dataset,
      col_names,
      stat = stat,
      percentile = percentile,
      nas = nas,
      wt = weight,
      prompt = prompt,
      digits = digits,
      call = user_call
    )
  }

  frequencies <- purrr::map_dfr(
    .x = col_names,
    .f = function(col_name) {
      freq_var(
        dataset,
        col_name,
        stat,
        percentile,
        nas,
        !!weight,
        prompt,
        digits,
        show_missing_levels,
        nas_group
      )
    }
  )
  frequencies <- group_rename(frequencies)

  return(frequencies)
}


calculate_result_for_cont_var <- function(
  dataset,
  variable,
  stat,
  percentile,
  wt
) {
  # first: (if wt = NULL) change class so logical test can be performed in all cases:
  if (base::is.null(wt)) {
    wt <- dplyr::enquo(wt)
  }

  # next: separate, verbose specifications for mean and quantile when weight is provided/not provided
  # (these if-else structures are inefficient but I wanted to be really clear about what we want)

  if (stat == 'mean') {
    # 1) wt = NULL
    if (rlang::quo_is_null(wt)) {
      out_df <- dataset |>
        # always filter nas because the function previously checked
        # to ensure nas = FALSE is set if necessary
        dplyr::filter(!is.na(!!variable)) |>
        dplyr::summarise(
          n = base::length(!!variable),
          result = base::mean(!!variable)
        )
    } else {
      # 2) wt exists in dataset
      out_df <- dataset |>
        dplyr::filter(!is.na(!!variable)) |>
        dplyr::summarise(
          n = base::sum(!!wt),
          result = stats::weighted.mean(!!variable, !!wt)
        )
    }
  }

  if (stat %in% c('quantile', 'median', 'min', 'max')) {
    if (stat == 'median') {
      percentile <- 50
    } else if (stat == 'min') {
      percentile <- 0
    } else if (stat == 'max') {
      percentile <- 100
    }

    if (stat %in% c('min', 'max')) {
      # mins and maxes are never weighted, per our decision
      wt <- dplyr::quo(NULL)
    }

    # 1) wt = NULL
    if (rlang::quo_is_null(wt)) {
      out_df <- dataset |>
        # always filter nas because the function previously checked
        # to ensure nas = FALSE is set if necessary
        dplyr::filter(!is.na(!!variable)) |>
        dplyr::summarise(
          n = base::length(!!variable),
          result = stats::quantile(x = !!variable, probs = percentile / 100)
        )
    } else {
      # 2) wt exists in dataset
      filtered_data <- dataset |>
        dplyr::filter(!is.na(!!variable))

      surv_design <- survey::svydesign(
        id = ~1,
        weights = stats::reformulate(rlang::as_name(wt)),
        data = filtered_data
      )

      quantile_algorithm <- getOption(
        'y2clerk.quantile_algorithm',
        default = "hf8"
      )

      q_result <- survey::svyquantile(
        x = stats::reformulate(rlang::as_name(variable)),
        design = surv_design,
        quantiles = percentile / 100,
        na.rm = TRUE,
        ci = FALSE,
        qrule = quantile_algorithm
      )

      out_df <- tibble::tibble(
        n = nrow(filtered_data),
        result = as.numeric(stats::coef(q_result))
      )
    }
  }
  return(out_df)
}

# Checks a single column and returns a named list of violation strings, or
# NULL if the column is clean. Violations are keyed by type so that
# validate_inputs_all() can group related problems across columns.
# Inform-only conditions (percentile scale, ignored percentile) are emitted
# immediately since they are not errors and need no aggregation.
validate_inputs <- function(
  dataset,
  variable,
  stat,
  percentile,
  nas,
  wt,
  prompt,
  digits
) {
  col_name <- rlang::as_label(variable)

  violations <- list()

  # 1) can't take mean/quantile of a categorical variable
  check_class <- dataset |>
    dplyr::ungroup() |>
    dplyr::select(!!variable) |>
    labelled::remove_labels() |>
    dplyr::pull() |>
    base::class() |>
    stringr::str_c(collapse = " ")

  if (!(check_class %in% c("numeric", "integer"))) {
    violations[["not_numeric"]] <- cli::format_inline(
      "{.var {col_name}} has class {.cls {check_class}}"
    )
    # If non-numeric, remaining checks are meaningless - return early.
    return(violations)
  }

  # 2) value labels present - numeric summary would be misleading
  val_labs <- dataset |>
    dplyr::ungroup() |>
    dplyr::pull(!!variable) |>
    labelled::val_labels()

  if (!is.null(val_labs)) {
    lab_names <- names(val_labs)
    violations[["has_labels"]] <- cli::format_inline(
      "{.var {col_name}} has value labels: {.val {lab_names}}"
    )
    return(violations)
  }

  # 3a) percentile required when stat = 'quantile'
  if (stat == 'quantile' && is.null(percentile)) {
    violations[["no_percentile"]] <- cli::format_inline(
      "{.var {col_name}}: {.arg percentile} must be supplied when {.code stat = 'quantile'}"
    )
    return(violations)
  }

  # 3b) percentile out of range
  if (stat == 'quantile' && !is.null(percentile)) {
    if (percentile < 0 || percentile > 100) {
      violations[["percentile_range"]] <- cli::format_inline(
        "{.var {col_name}}: {.arg percentile} must be between {.val 0} and {.val 100}; you supplied {.val {percentile}}"
      )
    }
    # 3c) subtle scale gotcha - inform immediately (not an error)
    if (percentile < 1) {
      cli::cli_inform(c(
        "i" = "{.arg percentile} uses a 0-100 scale, not 0-1.",
        "i" = "{.code percentile = {percentile}} returns the bottom {percentile}% percentile. Did you mean {.code percentile = {percentile * 100}}?"
      ))
    }
  }

  # 3d) percentile supplied but ignored - inform immediately (not an error)
  if (!(stat %in% c('quantile', 'summary')) && !is.null(percentile)) {
    cli::cli_inform(c(
      "i" = "{.arg percentile} only affects output when {.code stat = 'quantile'}.",
      "i" = "Current {.arg stat} is {.val {stat}}, so {.arg percentile} ({.val {percentile}}) is ignored."
    ))
  }

  # 4) NAs present
  if (nas) {
    count_nas <- dataset |>
      dplyr::filter(is.na(!!variable)) |>
      base::nrow()
    if (count_nas > 0L) {
      violations[["has_nas"]] <- cli::format_inline(
        "{.var {col_name}} contains {count_nas} NA value{?s}"
      )
    }
  }

  if (length(violations) == 0L) NULL else violations
}

# Runs validate_inputs() over every column, collects all violations, then
# emits a single combined cli_abort() grouping related problems together.
# This ensures the user sees every problem at once rather than one at a time.
validate_inputs_all <- function(
  dataset,
  col_names,
  stat,
  percentile,
  nas,
  wt,
  prompt,
  digits,
  call = rlang::caller_env()
) {
  all_violations <- purrr::map(
    col_names,
    \(col_name) {
      validate_inputs(
        dataset,
        variable = rlang::sym(col_name),
        stat = stat,
        percentile = percentile,
        nas = nas,
        wt = wt,
        prompt = prompt,
        digits = digits
      )
    }
  ) |>
    purrr::set_names(col_names) |>
    purrr::compact()

  if (length(all_violations) == 0L) {
    return(invisible(NULL))
  }

  # Each entry: label (cli template, can reference n_vars/stat/percentile) +
  # optional hint shown after the per-column bullets for that violation type.
  violation_specs <- list(
    not_numeric = list(
      label = "Can't compute {.val {stat}} for {n_vars} non-numeric variable{?s}:",
      hint = "Convert the variable to numeric first with {.code as.numeric()}, or use {.code stat = 'percent'}."
    ),
    has_labels = list(
      label = "Value labels detected in {n_vars} variable{?s} - numeric summaries may be misleading:",
      hint = "Strip labels with {.fn labelled::remove_labels}, {.fn haven::as_factor}, or use {.code stat = 'percent'}."
    ),
    has_nas = list(
      label = "NAs present in {n_vars} variable{?s}:",
      hint = "Exclude NAs from the {.val {stat}} calculation with {.code nas = FALSE}."
    ),
    no_percentile = list(
      label = "{.arg percentile} is required when {.code stat = 'quantile'} but was not supplied ({n_vars} variable{?s} affected):",
      hint = "Add {.code percentile = <value>} where value is between 0 and 100."
    ),
    percentile_range = list(
      label = "{.arg percentile} = {.val {percentile}} is out of range - must be between 0 and 100:",
      hint = NULL
    )
  )

  bullets <- purrr::imap(
    violation_specs,
    \(spec, type) {
      cols_with_type <- purrr::keep(all_violations, \(v) type %in% names(v))
      if (length(cols_with_type) == 0L) {
        return(NULL)
      }

      n_vars <- length(cols_with_type)

      detail_bullets <- purrr::map_chr(cols_with_type, \(v) v[[type]]) |>
        purrr::set_names(rep("*", n_vars))

      hint_bullet <- if (!is.null(spec$hint)) {
        c("i" = cli::format_inline(spec$hint))
      } else {
        character(0)
      }

      c("!" = cli::format_inline(spec$label), detail_bullets, hint_bullet)
    }
  ) |>
    purrr::compact() |>
    purrr::reduce(c)

  cli::cli_abort(bullets, call = call)
}

get_output_for_cont_var <- function(
  dataset,
  variable,
  stat,
  percentile,
  nas,
  wt,
  prompt,
  digits
) {
  # get mean or quantile
  out_df <- calculate_result_for_cont_var(
    dataset,
    variable,
    stat,
    percentile,
    wt
  )

  # get group column names to add later (if they exist/as necessary)
  grouping_vars <- c(NULL)
  if (dplyr::is.grouped_df(dataset)) {
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
  if (is.null(percentile)) {
    percentile <- -99
  }

  out_df <- out_df |>
    dplyr::mutate(
      variable = dplyr::quo_name(variable),
      value = '',
      label = '',
      # different labels depending on input
      stat = dplyr::case_when(
        statistic == 'mean' ~ 'mean',
        statistic == 'min' ~ 'min',
        statistic == 'median' ~ 'median',
        statistic == 'max' ~ 'max',
        statistic == 'quantile' &
          !(percentile %in% c(0, 50, 100)) ~
          stringr::str_c('q', percentile),
        statistic == 'quantile' & percentile == 0 ~ 'min',
        statistic == 'quantile' & percentile == 50 ~ 'median',
        statistic == 'quantile' & percentile == 100 ~ 'max',
        TRUE ~ 'error'
      ),
      n = base::round(.data$n, digits),
      result = base::round(.data$result, digits)
    ) |>
    dplyr::select(
      tidyselect::all_of(
        c(
          grouping_vars,
          'variable',
          'value',
          'label',
          'n',
          'stat',
          'result'
        )
      )
    ) |>
    tibble::as_tibble()

  # fill out prompt column if specified
  if (prompt) {
    prompt_text <- dataset |>
      dplyr::ungroup() |>
      dplyr::select(!!variable) |>
      labelled::var_label() |>
      tibble::deframe()

    # when prompt = TRUE but there is no variable label, output ""
    if (is.null(prompt_text)) {
      prompt_text <- ""
    }

    # final column ordering
    out_df <- out_df |>
      dplyr::mutate(
        prompt = prompt_text
      ) |>
      dplyr::select(
        tidyselect::all_of(
          c(
            grouping_vars,
            'variable',
            'prompt',
            'value',
            'label',
            'n',
            'stat',
            'result'
          )
        )
      )
  }

  # if weights are used, remove weight column rows from output
  if (!rlang::quo_is_null(wt)) {
    out_df <- out_df |>
      dplyr::filter(variable != rlang::quo_name(wt))
  }

  # for convenience:
  if (percentile == -99) {
    percentile <- NULL
  }

  return(out_df)
}

get_summary_output_for_cont_var <- function(
  dataset,
  variable,
  stat,
  percentile,
  nas,
  wt,
  prompt,
  digits
) {
  # Remind user that percentile is ignored for stat = 'summary' (subsequent
  # code hard-codes the six summary quantiles).
  if (!is.null(percentile)) {
    cli::cli_inform(
      c(
        "i" = "{.arg percentile} only affects output when {.code stat = 'quantile'}.",
        "i" = "Current {.arg stat} is {.val summary}, so {.arg percentile} ({.val {percentile}}) is ignored."
      )
    )
  }

  out <- dplyr::bind_rows(
    get_output_for_cont_var(
      dataset,
      variable,
      stat = 'min',
      percentile,
      nas,
      wt,
      prompt,
      digits
    ),
    get_output_for_cont_var(
      dataset,
      variable,
      stat = 'quantile',
      percentile = 25,
      nas,
      wt,
      prompt,
      digits
    ),
    get_output_for_cont_var(
      dataset,
      variable,
      stat = 'median',
      percentile,
      nas,
      wt,
      prompt,
      digits
    ),
    get_output_for_cont_var(
      dataset,
      variable,
      stat = 'mean',
      percentile,
      nas,
      wt,
      prompt,
      digits
    ),
    get_output_for_cont_var(
      dataset,
      variable,
      stat = 'quantile',
      percentile = 75,
      nas,
      wt,
      prompt,
      digits
    ),
    get_output_for_cont_var(
      dataset,
      variable,
      stat = 'max',
      percentile,
      nas,
      wt,
      prompt,
      digits
    )
  ) |>
    dplyr::mutate(
      stat = forcats::fct_relevel(
        stat,
        c('min', 'q25', 'median', 'mean', 'q75', 'max')
      )
    )

  return(out)
}

group_factor <- function(dataset) {
  grouping_vars <- dplyr::group_vars(dataset)
  if (length(grouping_vars) > 0) {
    # 1 or more grouping vars
    group_flags <- list()
    for (grouping_var in grouping_vars) {
      group_flag <- grouping_var |> as.symbol()
      group_flags <- c(group_flags, group_flag)
    }
    dataset <- dataset |>
      dplyr::ungroup() |>
      dplyr::mutate(
        dplyr::across(
          .cols = tidyselect::all_of(grouping_vars),
          .fns = ~ forcats::as_factor(.x)
        )
      )
    for (group_flag in group_flags) {
      dataset <- dataset |>
        dplyr::group_by(
          !!group_flag,
          .add = TRUE
        )
    }
    return(dataset)
  } else {
    # Not grouped
    return(dataset)
  }
}

remove_group_nas <- function(dataset) {
  grouping_vars <- dplyr::group_vars(dataset)
  if (length(grouping_vars) > 0) {
    # 1 or more grouping vars
    group_flags <- list()
    for (grouping_var in grouping_vars) {
      group_flag <- grouping_var |> as.symbol()
      group_flags <- c(group_flags, group_flag)
    }
    for (group_flag in group_flags) {
      dataset <- dataset |>
        dplyr::filter(
          !is.na(!!group_flag)
        )
    }
    return(dataset)
  } else {
    # Not grouped
    return(dataset)
  }
}

group_rename <- function(dataset) {
  # Assumed, since non-percent calculations aren't grouped dataframes
  grouping_vars <- dataset |>
    dplyr::select(
      -(tidyselect::all_of('variable'):dplyr::last_col())
    ) |>
    names()

  if (length(grouping_vars) > 0) {
    # 1 or more grouping vars
    for (i in 1:length(grouping_vars)) {
      if (i == 1) {
        dataset <- dataset |>
          dplyr::rename(group_var = grouping_vars[i])
      } else {
        dataset <- dataset |>
          dplyr::rename(
            !!dplyr::sym(stringr::str_c('group_var', i)) := grouping_vars[i]
          )
      }
    }
    return(dataset)
  } else {
    # Not grouped
    return(dataset)
  }
}

freq_var <- function(
  dataset,
  col_name,
  stat = 'percent',
  percentile = 50,
  nas = TRUE,
  wt = NULL,
  prompt = FALSE,
  digits = 2,
  show_missing_levels = show_missing_levels,
  nas_group,
  call = rlang::caller_env()
) {
  variable <- rlang::sym(col_name)
  wt <- dplyr::enquo(wt)

  if (stat == 'percent') {
    base <- ns(dataset, variable, wt, prompt, show_missing_levels, nas_group)
    freq_result <- base |>
      percents(nas, digits = digits)
  } else if (stat %in% c('mean', 'quantile', 'min', 'median', 'max')) {
    freq_result <- get_output_for_cont_var(
      dataset,
      variable,
      stat,
      percentile,
      nas,
      wt,
      prompt,
      digits
    )
  } else if (stat == 'summary') {
    freq_result <- get_summary_output_for_cont_var(
      dataset,
      variable,
      stat,
      percentile,
      nas,
      wt,
      prompt,
      digits
    )
  }

  return(freq_result)
}

# Returns a character vector of similar column names using fuzzy string distance.
# keywords: optional regex pattern (e.g. "wt|weight") - any col whose name
#   matches is included as a hint even if it's outside the fuzzy threshold.
# Returns character(0) when nothing close enough is found.
col_hint <- function(input_name, col_names, keywords = NULL) {
  if (length(col_names) == 0L) {
    return(character(0))
  }

  distances <- utils::adist(input_name, col_names, ignore.case = TRUE)[1, ]
  names(distances) <- col_names

  # Allow edits up to ~35% of the typed name length.
  # For short names (<= 3 chars) cap at 1 to avoid spurious matches.
  threshold <- if (nchar(input_name) <= 3L) {
    1L
  } else {
    max(2L, floor(nchar(input_name) * 0.35))
  }
  fuzzy_hits <- col_names[distances <= threshold]

  if (length(fuzzy_hits) > 0L) {
    fuzzy_hits <- fuzzy_hits[order(distances[fuzzy_hits], -nchar(fuzzy_hits))]
  }

  keyword_only <- if (!is.null(keywords)) {
    kw_hits <- col_names[grepl(keywords, col_names, ignore.case = TRUE)]
    kw_only <- setdiff(kw_hits, fuzzy_hits)
    if (length(kw_only) > 0L) kw_only[order(-nchar(kw_only))] else character(0)
  } else {
    character(0)
  }

  c(fuzzy_hits, keyword_only)
}

# Checks that col_name exists in dataset, aborting with a hint if not.
# arg_label: the argument name shown to the user (e.g. "wt", ".by").
# hint_cols: the candidate column names to search for suggestions (defaults
#   to all columns; pass a filtered set, e.g. numeric-only, for wt).
# keywords: forwarded to col_hint for keyword-based hint matches.
check_col <- function(
  arg_label,
  col_name,
  dataset,
  hint_cols = colnames(dataset),
  keywords = NULL,
  call = rlang::caller_env()
) {
  if (col_name %in% colnames(dataset)) {
    return(invisible(NULL))
  }

  hints <- col_hint(col_name, hint_cols, keywords = keywords)
  hint_bullet <- if (length(hints) > 0L) {
    c("i" = cli::format_inline("Did you mean: {.val {hints}}?"))
  } else {
    character(0)
  }

  cli::cli_abort(
    c(
      "{.arg {arg_label}} column {.var {col_name}} not found in {.arg dataset}.",
      hint_bullet
    ),
    call = call
  )
}

column_names <- function(dataset, wt) {
  col_names <- dataset |> colnames()
  if (dplyr::is.grouped_df(dataset)) {
    # Exclude grouping variables since they cannot be counted independent of groups.
    grouping_vars <- dplyr::group_vars(dataset)
    col_names <- setdiff(col_names, grouping_vars)
  }
  # Exclude weighting variable from freqs in select
  weight_name <- rlang::enquo(wt) |> rlang::as_label()
  col_names <- setdiff(col_names, weight_name)
  return(col_names)
}

ns <- function(
  dataset,
  variable,
  weight,
  prompt,
  show_missing_levels,
  nas_group
) {
  is_labelled <- sum(
    class(dataset |> dplyr::ungroup() |> dplyr::pull(!!variable)) %in%
      c('labelled', 'haven_labelled', 'haven_labelled_spss')
  )
  counts <- if (is_labelled >= 1) {
    # Metadata is better if the given variable has labels
    labelled_ns(
      dataset,
      variable,
      weight,
      prompt,
      show_missing_levels,
      nas_group
    )
  } else {
    # Otherwise, use some sensible defaults
    unlabelled_ns(dataset, variable, weight, prompt)
  }
  # Reorder because Scotty is OCD
  # Explicitly include group vars so dplyr doesn't emit
  # "Adding missing grouping variables" when dataset is grouped.
  group_vars <- dplyr::group_vars(dataset)
  if (prompt) {
    counts |>
      dplyr::select(
        tidyselect::all_of(group_vars),
        'variable',
        'prompt',
        'value',
        'label',
        'n'
      )
  } else {
    counts |>
      dplyr::select(
        tidyselect::all_of(group_vars),
        'variable',
        'value',
        'label',
        'n'
      )
  }
}

percents <- function(counts, include_nas, digits) {
  # Filter out NAs if requested
  if (!include_nas) {
    counts <- counts |>
      dplyr::filter(
        !is.na(.data$value)
      )
  }
  # Calculate and round to integer percentages
  counts |>
    dplyr::mutate(
      stat = 'percent',
      result = (.data$n / sum(.data$n)) |> round(digits)
    )
}

labelled_ns <- function(
  dataset,
  variable,
  weight,
  prompt,
  show_missing_levels,
  nas_group
) {
  # Extract the metadata from the labelled class
  counts <- base_ns(dataset, variable, weight)
  if (prompt) {
    prompt_text <- counts |>
      dplyr::ungroup() |>
      dplyr::select('value') |>
      labelled::var_label() |>
      as.character()
  }
  counts <- counts |>
    dplyr::mutate(
      label = labelled::to_factor(.data$value) |> as.character(),
      value = .data$value |> as.character()
    )

  if (show_missing_levels == TRUE) {
    all_levels <- dataset |>
      dplyr::ungroup() |>
      dplyr::pull(!!variable) |>
      attributes() |>
      purrr::pluck('labels')
    all_levels_tibble <- tibble::tibble(
      value = as.numeric(all_levels) |> as.character(),
      label = names(all_levels) |> as.character(),
      variable = rlang::quo_name(variable)
    )

    if (dplyr::is.grouped_df(dataset)) {
      grouping_vars <- dplyr::group_vars(dataset)
      all_group_levels <- dataset |>
        dplyr::select(tidyselect::all_of(grouping_vars)) |>
        dplyr::distinct()
      all_levels_tibble <- dplyr::cross_join(
        all_group_levels,
        all_levels_tibble
      )
      counts <- counts |>
        dplyr::full_join(
          all_levels_tibble,
          by = c(grouping_vars, 'label', 'value', 'variable')
        ) |>
        dplyr::mutate(
          n = ifelse(is.na(.data$n), 0, .data$n)
        )
      if (nas_group == FALSE) {
        counts <- counts |>
          dplyr::filter_at(
            .vars = 1,
            ~ !is.na(.)
          )
      }
    } else {
      # If not grouped
      counts <- counts |>
        dplyr::full_join(
          all_levels_tibble,
          by = c('label', 'value', 'variable')
        ) |>
        dplyr::mutate(
          n = ifelse(is.na(.data$n), 0, .data$n)
        )
    }
    counts <- counts |> dplyr::arrange(.data$value)
  }

  if (prompt == TRUE) {
    counts$prompt <- prompt_text
  }

  return(counts)
}

unlabelled_ns <- function(dataset, variable, weight, prompt) {
  if (
    class(dataset |> dplyr::ungroup() |> dplyr::pull(!!variable))[1] == 'factor'
  ) {
    counts <- base_ns(dataset, variable, weight) |>
      dplyr::mutate(
        label = forcats::as_factor(.data$value) |> as.character(),
        value = forcats::as_factor(.data$value) |>
          as.numeric() |>
          as.character()
      )
  } else {
    counts <- base_ns(dataset, variable, weight) |>
      dplyr::mutate(
        label = .data$value |> as.character(),
        value = .data$value |> as.character()
      )
  }
  if (prompt) {
    counts <- counts |>
      dplyr::mutate(
        prompt = ''
      )
  }
  return(counts)
}

base_ns <- function(dataset, variable, weight) {
  dataset |>
    # When wt is NULL, it runs unweighted counts
    dplyr::count(!!variable, wt = !!weight, .drop = FALSE) |>
    dplyr::rename(value = !!variable) |>
    dplyr::mutate(
      variable = dplyr::quo_name(variable)
    )
}

check_data_frame2 <- function(dataset) {
  env <- rlang::caller_env()
  caller_call <- rlang::caller_call()

  tryCatch(
    rlang::check_data_frame(dataset, call = env),
    error = function(e) {
      if (grepl("must be used within a", conditionMessage(e), fixed = TRUE)) {
        cli::cli_abort(
          c(
            "x" = "dataset must not be NULL.",
            "i" = "Did you forget to supply a dataset?",
            make_missing_df_hint(caller_call)
          ),
          call = env
        )
      } else {
        stop(e)
      }
    }
  )
}

make_missing_df_hint <- function(caller_call) {
  if (is.null(caller_call)) {
    return(NULL)
  }

  hint_call <- as.call(c(
    caller_call[[1]],
    as.symbol("DATASET_NAME"),
    as.list(caller_call[-1])
  ))

  hint_str <- paste(deparse(hint_call), collapse = " ")
  if (nchar(hint_str) > 80L) {
    return(NULL)
  }

  c("i" = paste0("Try: {.code ", hint_str, "}"))
}