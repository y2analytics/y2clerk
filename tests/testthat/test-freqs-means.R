

# setup -------------------------------------------------------------------

rm(list = ls())

library(y2clerk)
library(tidyverse)
library(labelled)

set.seed(123)

# test data ---------------------------------------------------------------

responses <-
  data.frame(

    # continuous numeric, no label, no NA
    q0 = sample(x = datasets::swiss$Agriculture, size = 25, replace = T),

    # continuous numeric, labelled, incl. NA
    q1 = sample(x = c(datasets::swiss$Agriculture, NA), size = 25, prob = c(rep(.95/47,47), 0.05), replace = T),

    # factor (numbers), no label
    q2 = sample(x = datasets::Orange$Tree, size = 25, replace = T),

    # character, no label
    q3 = sample(stringr::fruit, 25, prob = 1/(1:80 * sum(1/(1:80))), replace = T),

    # numeric values, discrete labels
    q4 = sample(1:8, 25, replace = T),

    # character values, discrete labels
    q5 = sample(letters[1:4], 25, prob = c(0.4,0.3,0.2,0.1), replace = T),

    # numeric weights
    w = rnorm(25, mean = 1, sd = 0.1)

  ) %>%
  set_value_labels(
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
  set_variable_labels(
    q1 = "% of males involved in agriculture",
    q2 = "Orange tree ID",
    q3 = "Preferred fruit",
    q4 = "Duration",
    q5 = "Satisfaction",
    w = "Weights"
  ) %>%

  as_tibble()

# basic_mean_no_nas <-
#   tribble(
#     ~variable, ~value, ~label, ~n, ~stat, ~result,
#     "q0",         "",     "",  25L, "mean",  55.2
#   )

# tests -------------------------------------------------------------------

context("check test data")

test_that("test data is correct", {
  expect_type(responses, "list")
  expect_equal(ncol(responses), 7)
  expect_equal(nrow(responses), 25)
})

context("evaluate mean of numeric variable (no NAs)")

test_that("no NAs present, nas = T", {
  expect_equivalent(responses %>%
                      select(q0) %>%
                      freqs(stat = "mean") %>%
                      select(result) %>%
                      pull(),

                    round(mean(responses$q0),2)
  )
})

test_that("NAs not present, nas = F", {
  expect_equivalent(responses %>%
                      select(q0) %>%
                      freqs(stat = "mean", nas = F) %>%
                      select(result) %>%
                      pull(),

                    round(mean(responses$q0),2)
  )
  expect_equivalent(responses %>%
                      select(q0) %>%
                      freqs(stat = "mean", nas = F) %>%
                      select(n) %>%
                      pull(),

                    nrow(responses[!is.na(responses$q0),])
  )
})

context("evaluate mean of numeric variable (NAs present, nas = T)")

test_that("nas argument set incorrectly results in error", {
  expect_error(responses %>%
                      select(q1) %>%
                      freqs(stat = "mean") %>%
                      select(result) %>%
                      pull()
  )
})

test_that("nas argument set correctly yields correct output", {
  expect_equal(responses %>%
                 select(q1) %>%
                 freqs(stat = "mean", nas = F) %>%
                 select(result) %>%
                 pull(),

               round(mean(responses$q1, na.rm = T), 2)
  )
  expect_equivalent(responses %>%
                      select(q1) %>%
                      freqs(stat = "mean", nas = F) %>%
                      select(n) %>%
                      pull(),

                    nrow(responses[!is.na(responses$q1),])
  )
})

context("factor variable results in error")

test_that(
  expect_error(
    responses %>%
      select(q2) %>%
      freqs(stat = 'mean')
  )
)
