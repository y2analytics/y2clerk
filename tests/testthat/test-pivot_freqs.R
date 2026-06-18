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
      mutate(rincome = to_haven_y2(rincome)) |>
      freq(rincome, .by = marital) |>
      pivot_freqs()
  )

  expect_no_error(
    forcats::gss_cat |>
      mutate(rincome = to_haven_y2(rincome)) |>
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


### columns_var - pivot other way
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

### Multi-variable freqs -------------------------------------------------------

test_that("pivot_freqs: colliding labels produces variable_label column names", {
  result <- multi_collide_freqs() |> pivot_freqs()

  expect_true(all(c("q_festivals_Yes", "q_festivals_No", "q_parades_Yes", "q_parades_No") %in% names(result)))
  # bare label values must not leak through as column names
  expect_false(any(c("Yes", "No") %in% names(result)))
})

test_that("pivot_freqs: colliding labels pivot group_var keeps variable as id column", {
  result <- multi_collide_freqs() |> pivot_freqs(group_var)

  expect_true("variable" %in% names(result))
  expect_true("label" %in% names(result))
  expect_false("group_var" %in% names(result))
})

test_that("pivot_freqs: unique labels uses label directly as column names", {
  result <- multi_unique_freqs() |> pivot_freqs()

  expect_true(all(c("Festivals", "No to Festivals", "Parades", "No to Parades") %in% names(result)))
  # no variable prefix should be added when labels are already distinct
  expect_false(any(grepl("^q_", names(result))))
})

test_that("pivot_freqs: unique labels pivot group_var excludes variable id column", {
  result <- multi_unique_freqs() |> pivot_freqs(group_var)

  expect_false("variable" %in% names(result))
  expect_true("label" %in% names(result))
    expect_false("group_var" %in% names(result))
})

