# Not a dataframe error - vectors

    Code
      freqs(df, a)
    Condition
      Error in `freqs()`:
      ! `dataset` must be a data frame, not a character vector.

# Not a dataframe error - matrix

    Code
      freqs(table, column_a)
    Condition
      Error in `freqs()`:
      ! `dataset` must be a data frame, not a double matrix.

# .by errors when grouping variable is not present in the data

    Code
      freq(mtcars, mpg, .by = ideology)
    Condition
      Error in `freq()`:
      ! `.by` column `ideology` not found in `dataset`.

# .by errors when data is already grouped

    Code
      freq(dplyr::group_by(mtcars, cyl), mpg, .by = vs)
    Condition
      Error in `freq()`:
      ! Cannot use `.by` on an already-grouped data frame.
      i Use `dplyr::group_by()` or `.by`, not both.
      i The dataset is currently grouped by: "cyl".

# Incorrect nas argument

    Code
      freqs(mtcars, cyl, nas = "True")
    Condition
      Error in `freqs()`:
      ! `nas` must be `TRUE` or `FALSE`, not the string "True".

# Incorrect wt argument

    Code
      freqs(mtcars, cyl, wt = "True")
    Condition
      Error in `freqs()`:
      ! `wt` column `"True"` not found in `dataset`.
      i Did you mean: "wt"?

# NAs present: throws error

    Code
      freqs(dplyr::select(responses, q1), stat = "mean")
    Condition
      Error in `freqs()`:
      ! NAs present in 1 variable:
      * `q1` contains 3 NA values
      i Exclude NAs from the "mean" calculation with `nas = FALSE`.

# factor variable input: throws error

    Code
      freqs(dplyr::select(responses, q2), stat = "mean")
    Condition
      Error in `freqs()`:
      ! Can't compute "mean" for 1 non-numeric variable:
      * `q2` has class <ordered factor>
      i Convert the variable to numeric first with `as.numeric()`, or use `stat = 'percent'`.

---

    Code
      freqs(dplyr::select(responses, q2), stat = "quantile")
    Condition
      Error in `freqs()`:
      ! Can't compute "quantile" for 1 non-numeric variable:
      * `q2` has class <ordered factor>
      i Convert the variable to numeric first with `as.numeric()`, or use `stat = 'percent'`.

# character variable input: throws error

    Code
      freqs(dplyr::select(responses, q3), stat = "mean")
    Condition
      Error in `freqs()`:
      ! Can't compute "mean" for 1 non-numeric variable:
      * `q3` has class <character>
      i Convert the variable to numeric first with `as.numeric()`, or use `stat = 'percent'`.

---

    Code
      freqs(dplyr::select(responses, q3), stat = "quantile")
    Condition
      Error in `freqs()`:
      ! Can't compute "quantile" for 1 non-numeric variable:
      * `q3` has class <character>
      i Convert the variable to numeric first with `as.numeric()`, or use `stat = 'percent'`.

# column with value labels input: throws error

    Code
      freqs(dplyr::select(responses, q4), stat = "mean")
    Condition
      Error in `freqs()`:
      ! Value labels detected in 1 variable - numeric summaries may be misleading:
      * `q4` has value labels: "Less than a year", "1-2 years", "3-4 years", "5-10 years", "10-20 years", "20-50 years", "50-100 years", and "More than 100 years"
      i Strip labels with `labelled::remove_labels()`, `haven::as_factor()`, or use `stat = 'percent'`.

---

    Code
      freqs(dplyr::select(responses, q4), stat = "quantile")
    Condition
      Error in `freqs()`:
      ! Value labels detected in 1 variable - numeric summaries may be misleading:
      * `q4` has value labels: "Less than a year", "1-2 years", "3-4 years", "5-10 years", "10-20 years", "20-50 years", "50-100 years", and "More than 100 years"
      i Strip labels with `labelled::remove_labels()`, `haven::as_factor()`, or use `stat = 'percent'`.

# stat argument only accepts percent, mean, quantile, or summary

    Code
      freqs(responses, q1, stat = "means", percentile = 75, nas = FALSE)
    Condition
      Error in `freqs()`:
      ! `stat` must be one of "percent", "mean", "median", "min", "max", "quantile", or "summary", not "means".
      i Did you mean "mean"?

# function stops when value labels exist

    Code
      freqs(responses, q4, stat = "mean", nas = FALSE)
    Condition
      Error in `freqs()`:
      ! Value labels detected in 1 variable - numeric summaries may be misleading:
      * `q4` has value labels: "Less than a year", "1-2 years", "3-4 years", "5-10 years", "10-20 years", "20-50 years", "50-100 years", and "More than 100 years"
      i Strip labels with `labelled::remove_labels()`, `haven::as_factor()`, or use `stat = 'percent'`.

# unweighted_ns = TRUE, but no wt variable

    Code
      freqs(responses, q4, unweighted_ns = TRUE)
    Condition
      Error in `freqs()`:
      ! `unweighted_ns` is "TRUE" but no weight variable was provided.
      i Supply a weighting column via `wt`, or set `unweighted_ns = FALSE`.

# bad input throws error

    Code
      freqs(dplyr::select(responses, q0), stat = "perc")
    Condition
      Error in `freqs()`:
      ! `stat` must be one of "percent", "mean", "median", "min", "max", "quantile", or "summary", not "perc".
      i Did you mean "percent"?

# NAs present, nas = T: throws error

    Code
      freqs(dplyr::select(responses, q1), stat = "quantile", percentile = 95)
    Condition
      Error in `freqs()`:
      ! NAs present in 1 variable:
      * `q1` contains 3 NA values
      i Exclude NAs from the "quantile" calculation with `nas = FALSE`.

# invalid input: groups errors together

    Code
      freqs(dplyr::select(responses, q2, q3), stat = "quantile")
    Condition
      Error in `freqs()`:
      ! Can't compute "quantile" for 2 non-numeric variables:
      * `q2` has class <ordered factor>
      * `q3` has class <character>
      i Convert the variable to numeric first with `as.numeric()`, or use `stat = 'percent'`.

# freqs returns empty if a tidyselect function is used to and does not match any columns

    Code
      freq(responses, stem("A"))
    Condition
      Warning:
      No columns matched selection.
    Output
      # A tibble: 0 x 0

