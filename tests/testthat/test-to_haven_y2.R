# setup -------------------------------------------------------------------
library(y2clerk)
library(dplyr)
library(forcats)
library(labelled)
library(testthat)


# test data ---------------------------------------------------------------
set.seed(100)

responses <- {
  data.frame(

    # continuous numeric, no variable label, no NA
    q0 = sample(
      x = datasets::swiss$Agriculture,
      size = 25,
      replace = TRUE),

    # continuous numeric, variable label, incl. NA
    q1 = sample(
      x = c(datasets::swiss$Agriculture, NA),
      size = 25,
      prob = c(rep(.8/47,47), 0.2),
      replace = TRUE),

    # factor (numbers), no value labels
    q2 = sample(
      x = datasets::Orange$Tree,
      size = 25,
      replace = TRUE),

    # character, no value labels
    q3 = sample(
      stringr::fruit,
      25,
      prob = 1/(1:80 * sum(1/(1:80))),
      replace = TRUE),

    # numeric values, discrete numeric value labels
    q4 = sample(
      1:8,
      25,
      replace = TRUE),

    # character values, discrete character value labels
    q5 = sample(
      letters[1:4],
      25,
      prob = c(0.4,0.3,0.2,0.1),
      replace = TRUE),

    # factor (strings), no value labels
    q6 = sample(
      stringr::fruit,
      25,
      prob = 1/(1:80 * sum(1/(1:80))),
      replace = TRUE) %>% as_factor(),

    # numeric weights
    w = rnorm(25, mean = 1, sd = 0.1)
  ) %>%
    labelled::set_value_labels(
      q4 = c(`Less than a year` = 1,
             `1-2 years` = 2,
             `3-4 years` = 3,
             `5-10 years` = 4,
             `10-20 years` = 5,
             `20-50 years` = 6,
             `50-100 years` = 7,
             `More than 100 years` = 8),
      q5 = c(
        `Very happy` = "a",
        `Somewhat happy` = "b",
        `Somewhat unhappy` = "c",
        `Very unhappy` = "d"
      )
    ) %>%
    labelled::set_variable_labels(
      q1 = "% of males involved in agriculture",
      q2 = "Orange tree ID",
      q3 = "Preferred fruit",
      q4 = "Duration",
      q5 = "Satisfaction",
      q6 = 'Preferred fruit (f)',
      w = "Weights"
    ) %>%
    dplyr::as_tibble()
}

glimpse(responses)

# Tests -------------------------------------------------------------


context("to_haven_y2")


test_that("to_haven_y2 error on haven_labelled vars", {
  expect_error(
    responses %>%
      mutate(
      q4_haven = to_haven_y2(q4),
    'q4 is already a haven_labelled variable',
    fixed = TRUE
  )
  )
})


test_that("to_haven_y2 error on numeric vars", {
  expect_error(
    responses %>%
      mutate(
        q0_haven = to_haven_y2(q0),
        'to_haven_y2 cannot be used on numeric variable: q0',
        fixed = TRUE
      )
  )
})


# For character and factor tests below, the tests check the following:
# 1) Is the resulting class haven_labelled with both a numeric and character?
# 2) Are the characters preserved across all instances?
# 3) Is the ordering consistent after conversion to haven_labelled?
test_that("to_haven_y2: character vars", {
  # Original
  responses %>% pull(q3)
  responses_haven <- responses %>%
    mutate(q3_haven = to_haven_y2(q3))
  q3_factor <- responses_haven %>%
    slice(1:3) %>%
    pull(q3_haven) %>%
    as_factor() %>%
    as.character()
  q3_num <- responses_haven %>%
    slice(1:3) %>%
    pull(q3_haven) %>%
    as.numeric()
  responses_haven %>% select(q3, q3_haven)

  expect_equal(class(responses_haven$q3_haven)[1], "haven_labelled")
  expect_equal(q3_num, c(1, 1, 2))
  expect_equal(q3_factor, c('banana', 'banana', 'bilberry'))
})


test_that("to_haven_y2: factor vars", {
  # Original
  responses %>% pull(q6)
  responses_haven <- responses %>%
    mutate(q6_haven = to_haven_y2(q6))
  q6_factor <- responses_haven %>%
    slice(1:4) %>%
    pull(q6_haven) %>%
    as_factor() %>%
    as.character()
  q6_num <- responses_haven %>%
    slice(1:4) %>%
    pull(q6_haven) %>%
    as.numeric()
  responses_haven %>% select(q6, q6_haven)

  expect_equal(class(responses_haven$q6_haven)[1], "haven_labelled")
  expect_equal(q6_num, c(1, 2, 3, 2))
  expect_equal(q6_factor, c('apricot', 'apple', 'boysenberry', 'apple'))
})

