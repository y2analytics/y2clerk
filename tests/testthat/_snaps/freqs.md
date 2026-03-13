# Not a dataframe error - vectors

    Code
      freqs(df, a)
    Condition
      Error in `map()`:
      i In index: 1.
      Caused by error in `UseMethod()`:
      ! no applicable method for 'pull' applied to an object of class "character"

# Not a dataframe error - matrix

    Code
      freqs(table, column_a)
    Condition
      Error in `map()`:
      i In index: 1.
      Caused by error in `UseMethod()`:
      ! no applicable method for 'pull' applied to an object of class "c('matrix', 'array', 'double', 'numeric')"

# Runs on variables, not integers

    Code
      freqs(mtcars, 10)
    Condition
      Error in `map()`:
      i In index: 1.
      Caused by error in `dplyr::rename()`:
      ! Can't rename columns that don't exist.
      i Location 10 doesn't exist.
      i There are only 2 columns.

# Incorrect nas argument

    Code
      freqs(mtcars, cyl, nas = "True")
    Condition
      Error in `map()`:
      i In index: 1.
      Caused by error in `!include_nas`:
      ! invalid argument type

# Incorrect wt argument

    Code
      freqs(mtcars, cyl, wt = "True")
    Condition
      Error in `map()`:
      i In index: 1.
      Caused by error in `dplyr::count()`:
      i In argument: `n = base::sum("True", na.rm = TRUE)`.
      i In group 1: `cyl = 4`.
      Caused by error in `base::sum()`:
      ! invalid 'type' (character) of argument

# NAs present, nas = T: throws error

    Code
      responses %>% dplyr::select(q1) %>% freqs(stat = "mean")
    Condition
      Error in `map()`:
      i In index: 1.
      Caused by error in `validate_inputs()`:
      ! NAs present in variable(s); to proceed, set nas = F

---

    Code
      responses %>% dplyr::select(q1) %>% freqs(stat = "quantile", percentile = 95)
    Condition
      Error in `map()`:
      i In index: 1.
      Caused by error in `validate_inputs()`:
      ! NAs present in variable(s); to proceed, set nas = F

# factor variable input: throws error

    Code
      responses %>% select(q2) %>% freqs(stat = "mean")
    Condition
      Error in `select()`:
      ! could not find function "select"

---

    Code
      responses %>% dplyr::select(q2) %>% freqs(stat = "quantile")
    Condition
      Error in `map()`:
      i In index: 1.
      Caused by error in `validate_inputs()`:
      ! No input given for percentile (percentile rank)

# character variable input: throws error

    Code
      responses %>% dplyr::select(q3) %>% freqs(stat = "mean")
    Condition
      Error in `map()`:
      i In index: 1.
      Caused by error in `validate_inputs()`:
      ! Can't take mean of non-numeric variable

---

    Code
      responses %>% dplyr::select(q3) %>% freqs(stat = "quantile")
    Condition
      Error in `map()`:
      i In index: 1.
      Caused by error in `validate_inputs()`:
      ! No input given for percentile (percentile rank)

# column with value labels input: throws error

    Code
      responses %>% dplyr::select(q4) %>% freqs(stat = "mean")
    Condition
      Error in `map()`:
      i In index: 1.
      Caused by error in `validate_inputs()`:
      ! Value labels exist; consider converting values to labels or using stat = 'percent'

---

    Code
      responses %>% dplyr::select(q4) %>% freqs(stat = "quantile")
    Condition
      Error in `map()`:
      i In index: 1.
      Caused by error in `validate_inputs()`:
      ! No input given for percentile (percentile rank)

# stat argument only accepts percent, mean, quantile, or summary

    Code
      responses %>% freqs(q1, stat = "means", percentile = 75, nas = FALSE)
    Condition
      Error in `freqs()`:
      ! `stat` must be one of "percent", "mean", "median", "min", "max", "quantile", or "summary", not "means".
      i Did you mean "mean"?

# function stops when value labels exist

    Code
      responses %>% freqs(q4, stat = "mean", nas = FALSE)
    Condition
      Error in `map()`:
      i In index: 1.
      Caused by error in `validate_inputs()`:
      ! Value labels exist; consider converting values to labels or using stat = 'percent'

# unweighted_ns = TRUE, but no wt variable

    Code
      responses %>% freqs(q4, unweighted_ns = TRUE)
    Condition
      Error in `freqs()`:
      ! If you use unweighted_ns = TRUE, you must specify a wt variable

# bad input throws error

    Code
      responses %>% dplyr::select(q0) %>% freqs(stat = "perc")
    Condition
      Error in `freqs()`:
      ! `stat` must be one of "percent", "mean", "median", "min", "max", "quantile", or "summary", not "perc".
      i Did you mean "percent"?

