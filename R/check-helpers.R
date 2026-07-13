# Shared argument-validation helpers used by the external functions
# (e.g. freqs(), multi_freqs()). Each takes a `call` used for error
# attribution, defaulting to the calling function's environment so errors read
# as e.g. `Error in freqs()`.

# check_wt(): abort (with a numeric-column hint) when a weighting column is
# named via `wt` but absent from the dataset. No-op when `wt` is NULL.
check_wt <- function(dataset, wt_quo, call = rlang::caller_env()) {
  if (rlang::quo_is_null(wt_quo)) {
    return(invisible(NULL))
  }

  numeric_names <- dataset |>
    dplyr::select(tidyselect::where(is.numeric)) |>
    colnames()

  check_col(
    "wt",
    rlang::as_label(wt_quo),
    dataset,
    hint_cols = numeric_names,
    keywords = "wt|weight",
    call = call
  )
}

# check_unweighted_ns(): abort when `unweighted_ns = TRUE` but no weighting
# column was supplied (the argument only makes sense alongside weights).
check_unweighted_ns <- function(
  unweighted_ns,
  weight_exists,
  call = rlang::caller_env()
) {
  if (isTRUE(unweighted_ns) && !weight_exists) {
    cli::cli_abort(
      c(
        "{.arg unweighted_ns} is {.val TRUE} but no weight variable was provided.",
        "i" = "Supply a weighting column via {.arg wt}, or set {.code unweighted_ns = FALSE}."
      ),
      call = call
    )
  }
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
      } else if (grepl("not found", conditionMessage(e), fixed = TRUE)) {
        cli::cli_abort(
          c(
            "x" = "dataset {.val {dataset}} not found",
            "i" = "Please supply a valid dataset"
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
