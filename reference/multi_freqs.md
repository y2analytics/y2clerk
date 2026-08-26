# Run frequencies for multiple-select variables

`multi_freqs()` runs
[`freqs()`](https://y2analytics.github.io/y2clerk/reference/freqs.md)
across one or more multiple-select ("select all that apply") question
*stems*. For each stem it selects the associated columns with the
[`stem()`](https://y2analytics.github.io/y2clerk/reference/stem.md)
tidyselect helper, drops respondents who answered none of them, then
runs
[`freqs()`](https://y2analytics.github.io/y2clerk/reference/freqs.md).

## Usage

``` r
multi_freqs(
  dataset,
  ...,
  .by = NULL,
  remove_nas = TRUE,
  wt = NULL,
  prompt = FALSE,
  digits = 2,
  nas_group = TRUE,
  factor_group = FALSE,
  unweighted_ns = FALSE,
  show_missing_levels = TRUE,
  separator = c("_", "r"),
  ignore.case = FALSE
)
```

## Arguments

- dataset:

  A dataframe.

- ...:

  Question stems to tabulate, given as bare symbols (`Q1`), strings
  (`"Q1"`), or a character vector wrapped in
  [`all_of()`](https://tidyselect.r-lib.org/reference/all_of.html) /
  [`any_of()`](https://tidyselect.r-lib.org/reference/all_of.html). If
  nothing is specified, the function runs on every stem in the dataset.

- .by:

  Variables to group by for this operation only. Cannot be used when the
  dataset is already a grouped data frame.

- remove_nas:

  Boolean, after freqs is run (which always includes NAs), whether or
  not to filter out counts of NA value (default: TRUE).

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
  UNweighted while results ARE weighted. This argument can only be used
  if a wt variable is used. If no weight variable is used, the 'n'
  column will always be unweighted (default: FALSE).

- show_missing_levels:

  Boolean, whether to keep response levels with no data (default: TRUE)

- separator:

  Character vector of separators allowed between the stem and its
  numeric suffix, passed through to
  [`stem()`](https://y2analytics.github.io/y2clerk/reference/stem.md)
  (default: `c("_", "r")`).

- ignore.case:

  Boolean, whether to match the stem case-insensitively, passed through
  to [`stem()`](https://y2analytics.github.io/y2clerk/reference/stem.md)
  (default: FALSE).

## Value

A dataframe with the variable names, prompts, values, labels, counts,
stats, and resulting calculations.

## Details

Pass the *stem* of each question, not an individual column. For a
question stored as `Q1_1`, `Q1_2`, `Q1_3`, pass `Q1`. Stems may be given
as bare symbols (`Q1`), strings (`"Q1"`), or spliced in from a character
vector with
[`tidyselect::all_of()`](https://tidyselect.r-lib.org/reference/all_of.html)
/
[`tidyselect::any_of()`](https://tidyselect.r-lib.org/reference/all_of.html).
If no stems are given, `multi_freqs()` runs on every stem in the
dataset.

Columns are matched with
[`stem()`](https://y2analytics.github.io/y2clerk/reference/stem.md), so
`_TEXT` / open-ended columns are excluded automatically. If you pass a
name that is itself a column in the dataset (e.g. `Q1_1`),
`multi_freqs()` warns: the modern interface expects the stem rather than
an exemplar column.

## See also

[`stem()`](https://y2analytics.github.io/y2clerk/reference/stem.md),
[`freqs()`](https://y2analytics.github.io/y2clerk/reference/freqs.md)

## Examples

``` r

df <- tibble::tibble(
  a = c(1, 2, 3, 1, 2, 3, 1),
  Q1_1 = c(1, NA, 1, 1, NA, 1, NA),
  Q1_2 = c(1, 1, NA, 1, NA, 1, NA),
  Q1_3 = c(NA, 1, 1, NA, 4, 1, NA),
  weights = c(0.9, 0.9, 1.1, 1.1, 1, 1, 1)
)


# Pass the stem, not an individual column. These give the same output:
multi_freqs(df, Q1)
#> Variable stem "Q1" successfully freq'd
#> # A tibble: 4 × 6
#>   variable value label     n stat    result
#>   <chr>    <chr> <chr> <int> <chr>    <dbl>
#> 1 Q1_1     1     1         4 percent   0.67
#> 2 Q1_2     1     1         4 percent   0.67
#> 3 Q1_3     1     1         3 percent   0.5 
#> 4 Q1_3     4     4         1 percent   0.17
df |> multi_freqs(Q1)
#> Variable stem "Q1" successfully freq'd
#> # A tibble: 4 × 6
#>   variable value label     n stat    result
#>   <chr>    <chr> <chr> <int> <chr>    <dbl>
#> 1 Q1_1     1     1         4 percent   0.67
#> 2 Q1_2     1     1         4 percent   0.67
#> 3 Q1_3     1     1         3 percent   0.5 
#> 4 Q1_3     4     4         1 percent   0.17
df |> multi_freqs("Q1")
#> Variable stem "Q1" successfully freq'd
#> # A tibble: 4 × 6
#>   variable value label     n stat    result
#>   <chr>    <chr> <chr> <int> <chr>    <dbl>
#> 1 Q1_1     1     1         4 percent   0.67
#> 2 Q1_2     1     1         4 percent   0.67
#> 3 Q1_3     1     1         3 percent   0.5 
#> 4 Q1_3     4     4         1 percent   0.17


# Splice stems in from a character vector
stems <- c("Q1")
df |> multi_freqs(tidyselect::all_of(stems))
#> Variable stem "Q1" successfully freq'd
#> # A tibble: 4 × 6
#>   variable value label     n stat    result
#>   <chr>    <chr> <chr> <int> <chr>    <dbl>
#> 1 Q1_1     1     1         4 percent   0.67
#> 2 Q1_2     1     1         4 percent   0.67
#> 3 Q1_3     1     1         3 percent   0.5 
#> 4 Q1_3     4     4         1 percent   0.17


# Grouped example with weights
df |>
  dplyr::group_by(a) |>
  multi_freqs(Q1, wt = weights)
#> Variable stem "Q1" successfully freq'd
#> # A tibble: 8 × 7
#> # Groups:   group_var [3]
#>   group_var variable value label     n stat    result
#>       <dbl> <chr>    <chr> <chr> <dbl> <chr>    <dbl>
#> 1         1 Q1_1     1     1       2   percent   1   
#> 2         3 Q1_1     1     1       2.1 percent   1   
#> 3         1 Q1_2     1     1       2   percent   1   
#> 4         2 Q1_2     1     1       0.9 percent   0.47
#> 5         3 Q1_2     1     1       1   percent   0.48
#> 6         2 Q1_3     1     1       0.9 percent   0.47
#> 7         2 Q1_3     4     4       1   percent   0.53
#> 8         3 Q1_3     1     1       2.1 percent   1   


# Group for this call only with .by
multi_freqs(df, Q1, .by = a)
#> Variable stem "Q1" successfully freq'd
#> # A tibble: 8 × 7
#> # Groups:   group_var [3]
#>   group_var variable value label     n stat    result
#>       <dbl> <chr>    <chr> <chr> <int> <chr>    <dbl>
#> 1         1 Q1_1     1     1         2 percent    1  
#> 2         3 Q1_1     1     1         2 percent    1  
#> 3         1 Q1_2     1     1         2 percent    1  
#> 4         2 Q1_2     1     1         1 percent    0.5
#> 5         3 Q1_2     1     1         1 percent    0.5
#> 6         2 Q1_3     1     1         1 percent    0.5
#> 7         2 Q1_3     4     4         1 percent    0.5
#> 8         3 Q1_3     1     1         2 percent    1  
```
