# apply_by(): resolve a per-operation `.by` tidy-selection and apply it as
# grouping, mirroring dplyr's `.by`/`by` semantics. Shared by the external
# functions that accept a `.by` argument (e.g. freqs(), multi_freqs()).
#
# - Resolves `by_quo` against `dataset` with tidyselect.
# - Aborts with a column hint when a selected column does not exist.
# - Aborts when `.by` is combined with an already-grouped data frame.
# - Returns `dataset` grouped by the selected columns (or unchanged if `.by`
#   selected nothing).
#
# `call` controls error attribution and defaults to the calling function's
# environment, so errors read as e.g. `Error in freqs()`.
apply_by <- function(dataset, by_quo, call = rlang::caller_env()) {
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
          call = call
        )
      } else {
        cli::cli_abort(
          c("Invalid {.arg .by} selection.", "x" = msg),
          call = call
        )
      }
    }
  )

  if (length(by_sel) == 0) {
    return(dataset)
  }

  if (dplyr::is.grouped_df(dataset)) {
    cli::cli_abort(
      c(
        "Cannot use {.arg .by} on an already-grouped data frame.",
        "i" = "Use {.code dplyr::group_by()} or {.arg .by}, not both.",
        "i" = "The dataset is currently grouped by: {.val {dplyr::group_vars(dataset)}}."
      ),
      call = call
    )
  }

  dplyr::group_by(
    dataset,
    dplyr::across(tidyselect::all_of(names(by_sel)))
  )
}
