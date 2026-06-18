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

expect_snapshot(forcats::gss_cat |>
    dplyr::group_by(year) |>
    freqs(race) |>
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
  expect_error(
    error = TRUE,
    forcats::gss_cat |>
      freqs(marital) |>
      pivot_freqs()
  )
})

test_that("pivot_freqs errors on missing label or result column", {
  expect_error(
    error = TRUE,
    forcats::gss_cat |>
      pivot_freqs()
  )
})
