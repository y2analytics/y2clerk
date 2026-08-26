# Select columns by stem prefix followed by a numeric suffix

A tidyselect helper that matches columns whose names consist of a stem
immediately followed by one of the allowed separators and then a digit.
Useful for selecting multi-select variable sets (e.g. `Q1_1`, `Q1_2`,
...) without also grabbing unrelated columns that merely start with the
same stem.

## Usage

``` r
stem(stem, separator = c("_", "r"), ignore.case = FALSE)
```

## Arguments

- stem:

  A string giving the variable stem to match.

- separator:

  A character vector of separator strings allowed between the stem and
  the numeric suffix. Defaults to `c("_", "r")`. Use `""` to allow no
  separator (bare digits); in that case the column name must end with
  the digits (e.g. `Q11`, `Q12`).

- ignore.case:

  default: FALSE. Boolean indicating whether to ignore the case for the
  stem.

## Value

A tidyselect selection, suitable for use inside
[`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html),
[`dplyr::across()`](https://dplyr.tidyverse.org/reference/across.html),
[`y2clerk::freqs()`](https://y2analytics.github.io/y2clerk/reference/freqs.md),
etc.

## Details

`stem()` by default does not select column names who start with the
question stem and end with text (QuestionStem_oe or QuestionStem_TEXT).
This is intentional. If you need those columns as well, use a plain
[`tidyselect::starts_with()`](https://tidyselect.r-lib.org/reference/starts_with.html).
This function differs from
`dplyr::select(tidyselect::starts_with('QuestionStem'), -ends_with('Text'))`
in the fact that it does not select columns names who start with the
question stem, contain other information, then end with a digit.

## Examples

``` r
df <- data.frame(
  Q1_1 = 1, Q1_2 = 2, Q1_3 = 3,
  Q10_1 = 4, Q1r1 = 5, Q11 = 6, Q1_TEXT = "open end", other = 7
)

# Default (underscore or r): Q1_1, Q1_2, Q1_3, Q1r1
dplyr::select(df, stem("Q1"))
#>   Q1_1 Q1_2 Q1_3 Q1r1
#> 1    1    2    3    5

# Underscore only
dplyr::select(df, stem("Q1", separator = "_"))
#>   Q1_1 Q1_2 Q1_3
#> 1    1    2    3
```
