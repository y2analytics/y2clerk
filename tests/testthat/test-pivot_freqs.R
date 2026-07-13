### Column names
test_that("pivot_freqs works with Column names", {
  expect_snapshot(
    forcats::gss_cat |>
      dplyr::group_by(year) |>
      freqs(marital) |>
      pivot_freqs()
  )
})


test_that("pivot_freqs works with Column names with two group vars", {
  expect_snapshot(
    forcats::gss_cat |>
      dplyr::group_by(year) |>
      freqs(race) |>
      pivot_freqs()
  )
})


test_that("pivot_freqs works on haven labelled data", {
  expect_no_error(
    forcats::gss_cat |>
      dplyr::mutate(rincome = to_haven_y2(rincome)) |>
      freq(rincome, .by = marital) |>
      pivot_freqs()
  )

  expect_no_error(
    forcats::gss_cat |>
      dplyr::mutate(rincome = to_haven_y2(rincome)) |>
      freq(marital, .by = rincome) |>
      pivot_freqs()
  )
})

### Group_var levels
test_that("pivot_freqs returns a row for each group_var level", {
  frequencies_pivoted <- forcats::gss_cat |>
    dplyr::group_by(year) |>
    freqs(marital) |>
    pivot_freqs()

  nrows <- length(frequencies_pivoted$group_var)
  names_rows <- as.character(frequencies_pivoted$group_var)

  expect_equal(nrows, 8)
  expect_equal(
    names_rows,
    c('2000', '2002', '2004', '2006', '2008', '2010', '2012', '2014')
  )
})


test_that("pivot_freqs can pivot on group_var", {
  expect_snapshot(
    forcats::gss_cat |>
      dplyr::group_by(year) |>
      freqs(marital) |>
      pivot_freqs(group_var)
  )
})


### Errors
test_that("pivot_freqs errors on blank label column", {
  expect_snapshot(
    error = TRUE,
    forcats::gss_cat |>
      freqs(age, stat = 'mean', nas = FALSE) |>
      pivot_freqs()
  )
})

test_that("pivot_freqs errors on missing group_var", {
  expect_snapshot(
    error = TRUE,
    forcats::gss_cat |>
      freqs(marital) |>
      pivot_freqs()
  )
})

test_that("pivot_freqs errors on missing label or result column", {
  expect_snapshot(
    error = TRUE,
    forcats::gss_cat |>
      pivot_freqs()
  )
})

### Multi-select ("select all that apply") questions ---------------------------

# responses2's m_activity_* columns are a genuine multi-select ("select all
# that apply") stem, each item carrying its own distinct value label
# (Basketball, Football, ...), so multi_freqs() naturally produces labels
# that are unique across variables.
test_that("pivot_freqs: multi-select stem with unique labels pivots on label", {
  frequencies <- responses2 |>
    dplyr::group_by(gender) |>
    multi_freqs(m_activity)

  expect_snapshot(frequencies |> pivot_freqs())
})

test_that("pivot_freqs: multi-select stem with unique labels pivots on group_var", {
  frequencies <- responses2 |>
    dplyr::group_by(gender) |>
    multi_freqs(m_activity)

  expect_snapshot(frequencies |> pivot_freqs(group_var))
})

# responses_multi_select has two multi-select stems (q_festivals, q_parades)
# whose items all share the same 'Yes'/'No' value labels, so multi_freqs()
# produces a `label` column that collides across `variable`s.
test_that("pivot_freqs: multi-select stems with colliding labels get variable-prefixed column names", {
  frequencies <- responses_multi_select |>
    dplyr::group_by(group_var) |>
    multi_freqs(q_festivals, q_parades)

  expect_snapshot(frequencies |> pivot_freqs())
})

test_that("pivot_freqs: multi-select stems with colliding labels keep variable as id column on group_var pivot", {
  frequencies <- responses_multi_select |>
    dplyr::group_by(group_var) |>
    multi_freqs(q_festivals, q_parades)

  expect_snapshot(frequencies |> pivot_freqs(group_var))
})
