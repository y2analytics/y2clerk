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

