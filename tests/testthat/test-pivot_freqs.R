# setup -------------------------------------------------------------------

### Packages
library(testthat)
library(dplyr)
library(y2clerk)
library(forcats)
library(stringr)

### Data
ToothGrowth
names(ToothGrowth)


# Tests -------------------------------------------------------------------
### Context
context("Tests on pivot_freqs")


### Column names
test_that("Column names", {
  frequencies_base <- ToothGrowth %>%
    group_by(supp) %>%
    freqs(dose)
  frequencies_base

  frequencies_pivoted <- frequencies_base %>% pivot_freqs()
  frequencies_pivoted

  names_actual <- frequencies_pivoted  %>% names()
  names_expected <- c(
    'group_var',
    '0.5',
    '1',
    '2'
    )
  expect_equal(names_actual, names_expected)
})


test_that("Each row is a group_var level", {
  frequencies_pivoted <- ToothGrowth %>%
    group_by(supp) %>%
    freqs(dose) %>%
    pivot_freqs()
  frequencies_pivoted
  nrows <- length(frequencies_pivoted$group_var)
  names_rows <- levels(frequencies_pivoted$group_var)

  expect_equal(nrows, 2)
  expect_equal(names_rows, c('OJ', 'VC'))
})


### columns_var - pivot other way
test_that("Pivot on group_var", {
  frequencies_pivoted <- ToothGrowth %>%
    group_by(supp) %>%
    freqs(dose) %>%
    pivot_freqs(group_var)
  frequencies_pivoted
  nrows <- nrow(frequencies_pivoted)
  names_cols <- names(frequencies_pivoted)

  expect_equal(nrows, 3)
  expect_equal(names_cols, c('label', 'OJ', 'VC'))
})


### Errors
test_that("pivot_freqs error testing", {
  expect_error(
    frequencies <- ToothGrowth %>%
      freqs(len, stat = 'mean') %>%
      pivot_freqs(),
    'Your frequencies label column is blank. Please provide labels on which to pivot',
    fixed = TRUE
  )
})

test_that("pivot_freqs error testing", {
  expect_error(
    frequencies <- ToothGrowth %>%
      freqs(supp) %>%
      pivot_freqs(),
    'Your frequencies does not contain a group_var. It must have a group_var to pivot correctly',
    fixed = TRUE
  )
})
