# setup -------------------------------------------------------------------

### Packages
library(testthat)
library(dplyr)
library(y2clerk)
library(forcats)

### Data
mtcars
names(mtcars)
GROUP_VARS <-
  mtcars %>%
  select(
    am,
    vs
  ) %>%
  names()

# Create names vector
GROUP_VARS1 <- 'am'

GROUP_VARS2 <-
  mtcars %>%
  select(
    am,
    vs
  ) %>%
  names()



# Tests -------------------------------------------------------------------
### Context
context("Tests on cross_freqs")

### Column names
test_that("Column names", {
  frequencies <- mtcars %>%
    cross_freqs(
      group_vars = GROUP_VARS1,
      gear
    )
  frequencies
  names_actual <- frequencies  %>% names()
  names_expected <- c(
    'group_var_name',
    'group_var',
    'variable',
    'value',
    'label',
    'n',
    'stat',
    'result'
    )
  expect_equal(names_actual, names_expected)
})



### Multiple freqs vars
test_that("Multiple freqs vars", {
  frequencies <- mtcars %>%
    cross_freqs(
      group_vars = GROUP_VARS1,
      gear,
      vs
    )
  frequencies
  label_levels <- unique(frequencies$label)
  variable_levels <- unique(frequencies$variable)
  expect_equal(label_levels, c("3", "4", "5", "0", "1"))
  expect_equal(variable_levels, c("gear", "vs"))
})



### Multiple grouping vars
test_that("Multiple grouping vars", {
  frequencies <- mtcars %>%
    cross_freqs(
      group_vars = GROUP_VARS2,
      gear
    )
  frequencies
  grouping_vars <- unique(frequencies$group_var_name)
  grouping_levels <- unique(frequencies$group_var) %>% as.character()
  expect_equal(grouping_vars, c("am", "vs"))
  expect_equal(grouping_levels, c("0", "1"))
})



### wide = TRUE
test_that("wide = TRUE", {
  frequencies <- mtcars %>%
    cross_freqs(
      group_vars = GROUP_VARS2,
      gear,
      wide = TRUE
    )
  frequencies
  grouping_vars <- unique(frequencies$group_var_name)

  expect_equal(class(frequencies$results), "list")
  expect_equal(grouping_vars, GROUP_VARS2)
})



### Mixed class group vars
test_that("Mixed class group vars", {
  # Long
  frequencies <- mtcars %>%
    mutate(am = as_factor(am)) %>%
    cross_freqs(
      group_vars = GROUP_VARS2,
      gear,
      carb
    )
  frequencies
  grouping_vars <- unique(frequencies$group_var_name)
  grouping_levels <- unique(frequencies$group_var) %>% as.character()
  expect_equal(grouping_vars, c("am", "vs"))
  expect_equal(grouping_levels, c("0", "1"))

  # Wide
  frequencies <- mtcars %>%
    mutate(am = as_factor(am)) %>%
    cross_freqs(
      group_vars = GROUP_VARS2,
      gear,
      carb,
      wide = TRUE
    )
  grouping_vars <- unique(frequencies$group_var_name)
  expect_equal(class(frequencies$results), "list")
  expect_equal(grouping_vars, GROUP_VARS2)
})
