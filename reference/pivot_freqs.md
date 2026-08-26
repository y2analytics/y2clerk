# Widen a frequencies table

Given a grouped frequencies table, pivot_freqs will create new columns
for each label level in the frequencies

## Usage

``` r
pivot_freqs(dataset, columns_var = label)
```

## Arguments

- dataset:

  A grouped frequencies table as produced by y2clerk::freqs()

- columns_var:

  DEFAULT = label; If label, the frequencies will be pivoted so a new
  column will be created for each unique level of label. Can also be set
  to group_var to pivot the other way and create new columns for each
  unique level of group_var

## Value

A wide tibble of frequencies with one row for each group (by default)

## Working with multiple variables and multi-select questions

[`freqs()`](https://y2analytics.github.io/y2clerk/reference/freqs.md)
and
[`multi_freqs()`](https://y2analytics.github.io/y2clerk/reference/multi_freqs.md)
can both return frequencies for more than one `variable` in a single
(optionally grouped) table – for example, running
[`freqs()`](https://y2analytics.github.io/y2clerk/reference/freqs.md) on
several columns at once, or running
[`multi_freqs()`](https://y2analytics.github.io/y2clerk/reference/multi_freqs.md)
on a "select all that apply" question stem made up of several checkbox
columns. `pivot_freqs()` detects this automatically by checking for a
`variable` column and handles it as follows:

- **Unique labels across variables** (the common multi-select case,
  where each checkbox item has its own distinct label, e.g.
  "Basketball", "Football", ...): `label` is used directly as the
  pivoted column name, exactly as with a single variable. The `variable`
  column is dropped since it adds no information beyond what `label`
  already conveys.

- **Colliding labels across variables** (e.g. several yes/no questions
  that all share the labels "Yes"/"No", or several Likert-scale
  questions that all share the same scale labels): using `label` alone
  as the column name would cause different questions' values to
  overwrite each other in the pivot. `pivot_freqs()` detects this by
  checking whether any `label` value appears under more than one
  `variable`, and if so builds compound column names of the form
  `variable_label` (e.g. `q_festivals_1_Yes`, `q_parades_1_Yes`)
  instead. When pivoting on `group_var` in this case, `variable` is kept
  as an additional id column (alongside `label`) so that rows for
  different questions remain distinguishable.

Label collision is only checked when more than one `variable` is
present; single-variable frequencies tables always pivot on the bare
`label` values.

## Examples

``` r
  frequencies <- forcats::gss_cat |>
    dplyr::group_by(year) |>
      y2clerk::freqs(marital) |>
      pivot_freqs()

  # Multi-select ("select all that apply") example: activity_1 and
  # activity_2 are separate checkbox columns (unchecked = NA), each with
  # its own distinct label, so pivot_freqs() uses those labels directly.
  df <- data.frame(
    group = c('A', 'A', 'B', 'B'),
    activity_1 = c(1, NA, 1, 1),
    activity_2 = c(NA, 1, NA, 1)
  ) |>
    labelled::set_value_labels(
      activity_1 = c('Basketball' = 1),
      activity_2 = c('Football' = 1)
    )

  df |>
    dplyr::group_by(group) |>
    multi_freqs(activity_1) |>
    pivot_freqs()
#> Adding missing grouping variables: `group`
#> Variable stem "activity" successfully freq'd
#> # A tibble: 2 × 3
#> # Groups:   group_var [2]
#>   group_var Basketball Football
#>   <chr>          <dbl>    <dbl>
#> 1 A                0.5      0.5
#> 2 B                1        0.5
```
