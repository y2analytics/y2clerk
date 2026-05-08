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

# .by errors when grouping variale is not present in the data

    Code
      freq(penguins, species, .by = ideology)
    Condition
      Error in `freq()`:
      ! `.by` column `ideology` not found in `dataset`.

# .by errors when data is already grouped

    Code
      freq(group_by(penguins, sex), species, .by = island)
    Condition
      Error in `group_by()`:
      ! could not find function "group_by"

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

# `freq()` prints question wordings

    Code
      print(test_freq1)
    Output
      # q1: % of males involved in agriculture
      # 
      # A frequency tibble: 20 x 6
         variable value label     n stat    result
         <chr>    <chr> <chr> <int> <chr>    <dbl>
       1 q1       17.6  17.6      1 percent   0.04
       2 q1       19.4  19.4      1 percent   0.04
       3 q1       26.8  26.8      1 percent   0.04
       4 q1       27.7  27.7      1 percent   0.04
       5 q1       35.3  35.3      1 percent   0.04
       6 q1       37.6  37.6      1 percent   0.04
       7 q1       38.4  38.4      1 percent   0.04
       8 q1       39.7  39.7      2 percent   0.08
       9 q1       43.5  43.5      1 percent   0.04
      10 q1       45.2  45.2      2 percent   0.08
      11 q1       49.5  49.5      1 percent   0.04
      12 q1       53.3  53.3      1 percent   0.04
      13 q1       58.1  58.1      2 percent   0.08
      14 q1       70.2  70.2      1 percent   0.04
      15 q1       71.2  71.2      1 percent   0.04
      16 q1       73    73        1 percent   0.04
      17 q1       75.9  75.9      1 percent   0.04
      18 q1       84.6  84.6      1 percent   0.04
      19 q1       84.9  84.9      1 percent   0.04
      20 q1       <NA>  <NA>      3 percent   0.12

# `freq()` prints only three question wordings

    Code
      print(test_freq)
    Output
      # q1: % of males involved in agriculture
      # q2: Orange tree ID
      # q3: Preferred fruit
      # i 1 more questions with labels
      # 
      # A frequency tibble: 48 x 6
         variable value label     n stat    result
         <chr>    <chr> <chr> <int> <chr>    <dbl>
       1 q1       17.6  17.6      1 percent   0.04
       2 q1       19.4  19.4      1 percent   0.04
       3 q1       26.8  26.8      1 percent   0.04
       4 q1       27.7  27.7      1 percent   0.04
       5 q1       35.3  35.3      1 percent   0.04
       6 q1       37.6  37.6      1 percent   0.04
       7 q1       38.4  38.4      1 percent   0.04
       8 q1       39.7  39.7      2 percent   0.08
       9 q1       43.5  43.5      1 percent   0.04
      10 q1       45.2  45.2      2 percent   0.08
      # i 38 more rows

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
      freqs(select(responses, q2), stat = "mean")
    Condition
      Error in `select()`:
      ! could not find function "select"

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

