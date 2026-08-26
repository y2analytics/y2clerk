# Significance testing

Akin to crosstabs, run significance testing on a grouped frequencies

## Usage

``` r
sig_test_y2(
  frequencies,
  dataset,
  banner_var,
  wt = NULL,
  layout = c("tall", "wide"),
  correction = getOption("y2clerk.mcc_correction", default = "fdr")
)
```

## Arguments

- frequencies:

  A grouped frequencies table created using the freqs() function in
  y2clerk

- dataset:

  The original data frame that the frequencies table came from

- banner_var:

  This will be the banner variables for cross tabs. Must be the same as
  the grouping variable from the freqs() function.

- wt:

  The weight variable used in the frequencies function, if applicable

- layout:

  default: 'tall': 'tall' formats the output to look like a basic
  grouped freqs table. 'wide' formats the output to look like the result
  from Q-formatted cross tabs

- correction:

  (default: the value of `getOption('y2clerk.mcc_correction')`,
  `'fdr'`). An algorithm for Multiple Comparison Correction. One of the
  valid algorithms supported by
  [`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html): holm,
  hochberg, hommel, bonferroni, BH, BY, fdr, none.

## Value

A table that matches the output of cross tabs, showing significance
differences between different groups for any input variables stats,
cross tabs. Column comparison symbols: a, b, c... (p \<= 0.05), A, B,
C... (p \<= 0.001); No symbol: not significant at at least (p \<= 0.05)

## See also

[y2clerk-options](https://y2analytics.github.io/y2clerk/reference/y2clerk-options.md)
for setting `y2clerk.mcc_correction` globally.

## Examples

``` r

# Example Data

df <- data.frame(
  V1 = c(
    rep('Agree', 100),
    rep('Neither', 100),
    rep('Disagree', 100),
    rep('Agree', 200),
    rep('Neither', 50),
    rep('Disagree', 50),
    rep('Agree', 50),
    rep('Neither', 125),
    rep('Disagree', 125)
  ),
  group = c(
    rep('Group 1', 300),
    rep('Group 2', 300),
    rep('Group 3', 300)
  ),
  weight = c(
    rnorm(900, 1, 0.25)
  )
)

# Frequencies

frequencies <- df |>
  dplyr::group_by(group) |>
  freqs(V1)

# Frequencies with significance tests

tested_freqs <- frequencies |>
  sig_test_y2(dataset = df,
              banner_var = group)
#> Adding grouped pairwise significance tests for response "Agree" for group_var "group"
#> Adding grouped pairwise significance tests for response "Disagree" for group_var "group"
#> Adding grouped pairwise significance tests for response "Neither" for group_var "group"

# Tested frequencies in crosstab format

tested_freqs <- frequencies |>
  sig_test_y2(dataset = df,
              banner_var = group,
              layout = 'wide')
#> Adding grouped pairwise significance tests for response "Agree" for group_var "group"
#> Adding grouped pairwise significance tests for response "Disagree" for group_var "group"
#> Adding grouped pairwise significance tests for response "Neither" for group_var "group"

# Weighted tested frequencies

tested_freqs <- df |>
  dplyr::group_by(group) |>
  freqs(V1,
        wt = weight) |>
  sig_test_y2(dataset = df,
              banner_var = group,
              wt = weight)
#> Adding missing grouping variables: `group`
#> Adding grouped pairwise significance tests for response "Agree" for group_var "group"
#> Adding grouped pairwise significance tests for response "Disagree" for group_var "group"
#> Adding grouped pairwise significance tests for response "Neither" for group_var "group"

# Weighted tested frequencies in crosstab format

tested_freqs <- df |>
  dplyr::group_by(group) |>
  freqs(V1,
        wt = weight) |>
  sig_test_y2(dataset = df,
              banner_var = group,
              wt = weight,
              layout = 'wide')
#> Adding missing grouping variables: `group`
#> Adding grouped pairwise significance tests for response "Agree" for group_var "group"
#> Adding grouped pairwise significance tests for response "Disagree" for group_var "group"
#> Adding grouped pairwise significance tests for response "Neither" for group_var "group"
```
