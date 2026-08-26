# Run frequencies for multiple variables

Run frequencies for multiple variables

## Usage

``` r
freqs(
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
)

freq(
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
)
```

## Arguments

- dataset:

  A dataframe.

- ...:

  One or more unquoted expressions separated by commas. Variable names
  can be used as if they were positions in the data frame, so
  expressions like x:y can be used to select a range of variables. If
  nothing is specified, the function runs a frequency on every column in
  given dataset.

- .by:

  Variables to group by for this operation only. Cannot be used when the
  dataset is already a grouped data frame.

- stat:

  Character, stat to run. Currently accepts 'percent,' 'mean,' 'median,'
  'min,' 'max,' 'quantile,' and 'summary' (default: 'percent').

- percentile:

  Double, for use when stat = 'quantile.' Input should be a real number
  x such that 0 \<= x \<= 100. Stands for percentile rank, which is a
  quantile relative to a 100-point scale. (default:NULL)

- nas:

  Boolean, whether or not to include NAs in the tabulation (default:
  TRUE).

- wt:

  The unquoted name of a weighting variable in the dataset (default:
  NULL).

- prompt:

  Boolean, whether or not to include the prompt in the dataset (default:
  FALSE).

- digits:

  Integer, number of significant digits for rounding (default: 2).

- nas_group:

  Boolean, whether or not to include NA values for the grouping variable
  in the tabulation (default: TRUE).

- factor_group:

  Boolean, whether or not to convert the grouping variable to a factor
  and use its labels instead of its underlying numeric values (default:
  FALSE)

- unweighted_ns:

  Boolean, whether the 'n' column in the freqs table should be
  Unweighted while results ARE weighted. This argument can only be used
  if a wt variable is used. If no weight variable is used, the 'n'
  column will always be unweighted (default: FALSE).

- show_missing_levels:

  Boolean, whether to keep response levels with no data (default: TRUE)

## Value

A dataframe with the variable names, prompts, values, labels, counts,
stats, and resulting calculations.

## See also

[y2clerk-options](https://y2analytics.github.io/y2clerk/reference/y2clerk-options.md)
for setting `y2clerk.quantile_algorithm` globally.

## Examples

``` r
df <- data.frame(
  a = c(1, 2, 2, 3, 4, 2, NA),
  b = c(1, 2, 2, 3, 4, 1, NA),
  c = c("Red", "Red", "Blue", NA, NA, NA, "Yellow"),
  weights = c(0.9, 0.9, 1.1, 1.1, 1, 1, 1)
)

freqs(df, a, b)
#> # A tibble: 10 × 6
#>    variable value label     n stat    result
#>    <chr>    <chr> <chr> <int> <chr>    <dbl>
#>  1 a        1     1         1 percent   0.14
#>  2 a        2     2         3 percent   0.43
#>  3 a        3     3         1 percent   0.14
#>  4 a        4     4         1 percent   0.14
#>  5 a        NA    NA        1 percent   0.14
#>  6 b        1     1         2 percent   0.29
#>  7 b        2     2         2 percent   0.29
#>  8 b        3     3         1 percent   0.14
#>  9 b        4     4         1 percent   0.14
#> 10 b        NA    NA        1 percent   0.14
freqs(df, a, b, wt = weights)
#> # A tibble: 10 × 6
#>    variable value label     n stat    result
#>    <chr>    <chr> <chr> <dbl> <chr>    <dbl>
#>  1 a        1     1       0.9 percent   0.13
#>  2 a        2     2       3   percent   0.43
#>  3 a        3     3       1.1 percent   0.16
#>  4 a        4     4       1   percent   0.14
#>  5 a        NA    NA      1   percent   0.14
#>  6 b        1     1       1.9 percent   0.27
#>  7 b        2     2       2   percent   0.29
#>  8 b        3     3       1.1 percent   0.16
#>  9 b        4     4       1   percent   0.14
#> 10 b        NA    NA      1   percent   0.14
freq(df, a:b)
#> # A tibble: 10 × 6
#>    variable value label     n stat    result
#>    <chr>    <chr> <chr> <int> <chr>    <dbl>
#>  1 a        1     1         1 percent   0.14
#>  2 a        2     2         3 percent   0.43
#>  3 a        3     3         1 percent   0.14
#>  4 a        4     4         1 percent   0.14
#>  5 a        NA    NA        1 percent   0.14
#>  6 b        1     1         2 percent   0.29
#>  7 b        2     2         2 percent   0.29
#>  8 b        3     3         1 percent   0.14
#>  9 b        4     4         1 percent   0.14
#> 10 b        NA    NA        1 percent   0.14
freq(df, tidyselect::starts_with('a'), wt = weights)
#> # A tibble: 5 × 6
#>   variable value label     n stat    result
#>   <chr>    <chr> <chr> <dbl> <chr>    <dbl>
#> 1 a        1     1       0.9 percent   0.13
#> 2 a        2     2       3   percent   0.43
#> 3 a        3     3       1.1 percent   0.16
#> 4 a        4     4       1   percent   0.14
#> 5 a        NA    NA      1   percent   0.14
freq(df, nas = FALSE)
#> # A tibble: 14 × 6
#>    variable value  label      n stat    result
#>    <chr>    <chr>  <chr>  <int> <chr>    <dbl>
#>  1 a        1      1          1 percent   0.17
#>  2 a        2      2          3 percent   0.5 
#>  3 a        3      3          1 percent   0.17
#>  4 a        4      4          1 percent   0.17
#>  5 b        1      1          2 percent   0.33
#>  6 b        2      2          2 percent   0.33
#>  7 b        3      3          1 percent   0.17
#>  8 b        4      4          1 percent   0.17
#>  9 c        Blue   Blue       1 percent   0.25
#> 10 c        Red    Red        2 percent   0.5 
#> 11 c        Yellow Yellow     1 percent   0.25
#> 12 weights  0.9    0.9        2 percent   0.29
#> 13 weights  1      1          3 percent   0.43
#> 14 weights  1.1    1.1        2 percent   0.29
freq(df, tidyselect::where(is.numeric), stat = 'mean', nas = FALSE, wt = weights)
#> # A tibble: 2 × 6
#>   variable value label     n stat  result
#>   <chr>    <chr> <chr> <dbl> <chr>  <dbl>
#> 1 a        ""    ""        6 mean    2.37
#> 2 b        ""    ""        6 mean    2.2 
df |>
  dplyr::group_by(a) |>
  freqs(b, nas = FALSE, wt = weights)
#> # A tibble: 5 × 7
#> # Groups:   group_var [4]
#>   group_var variable value label     n stat    result
#>       <dbl> <chr>    <chr> <chr> <dbl> <chr>    <dbl>
#> 1         1 b        1     1       0.9 percent   1   
#> 2         2 b        1     1       1   percent   0.33
#> 3         2 b        2     2       2   percent   0.67
#> 4         3 b        3     3       1.1 percent   1   
#> 5         4 b        4     4       1   percent   1   
freqs(df, b, .by = a)
#> # A tibble: 6 × 7
#>   group_var variable value label     n stat    result
#>       <dbl> <chr>    <chr> <chr> <int> <chr>    <dbl>
#> 1         1 b        1     1         1 percent   1   
#> 2         2 b        1     1         1 percent   0.33
#> 3         2 b        2     2         2 percent   0.67
#> 4         3 b        3     3         1 percent   1   
#> 5         4 b        4     4         1 percent   1   
#> 6        NA b        NA    NA        1 percent   1   

# Note that percentile = 60 will return an estimate
# of the real number such that 60% of values
# are lower than that number

# * note also that minimums and maximums are
# unaffected by weighting
freqs(df, a, stat = 'min', nas = FALSE)
#> # A tibble: 1 × 6
#>   variable value label     n stat  result
#>   <chr>    <chr> <chr> <dbl> <chr>  <dbl>
#> 1 a        ""    ""        6 min        1
freqs(df, a, stat = 'median', nas = FALSE)
#> # A tibble: 1 × 6
#>   variable value label     n stat   result
#>   <chr>    <chr> <chr> <dbl> <chr>   <dbl>
#> 1 a        ""    ""        6 median      2
freqs(df, a, stat = 'quantile', percentile = 95, nas = FALSE)
#> # A tibble: 1 × 6
#>   variable value label     n stat  result
#>   <chr>    <chr> <chr> <dbl> <chr>  <dbl>
#> 1 a        ""    ""        6 q95     3.75
freqs(df, a, stat = 'summary', nas = FALSE, wt = weights)
#> # A tibble: 6 × 6
#>   variable value label     n stat   result
#>   <chr>    <chr> <chr> <dbl> <fct>   <dbl>
#> 1 a        ""    ""        6 min      1   
#> 2 a        ""    ""        6 q25      2   
#> 3 a        ""    ""        6 median   2   
#> 4 a        ""    ""        6 mean     2.37
#> 5 a        ""    ""        6 q75      3.11
#> 6 a        ""    ""        6 max      4   
```
