

# setup -------------------------------------------------------------------

rm(list = ls())

library(y2clerk)
library(tidyverse)
library(labelled)

set.seed(1)

# test data ---------------------------------------------------------------

responses <- {
  data.frame(

    # continuous numeric, no variable label, no NA
    q0 = sample(x = datasets::swiss$Agriculture, size = 25, replace = T),

    # continuous numeric, variable label, incl. NA
    q1 = sample(x = c(datasets::swiss$Agriculture, NA), size = 25, prob = c(rep(.9/47,47), 0.1), replace = T),

    # factor (numbers), no value labels
    q2 = sample(x = datasets::Orange$Tree, size = 25, replace = T),

    # character, no value labels
    q3 = sample(stringr::fruit, 25, prob = 1/(1:80 * sum(1/(1:80))), replace = T),

    # numeric values, discrete value labels
    q4 = sample(1:8, 25, replace = T),

    # character values, discrete value labels
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

  as_tibble()}

# tests -------------------------------------------------------------------

context("check test data")

test_that("test data is correct", {
  expect_type(responses, "list")
  expect_equal(ncol(responses), 7)
  expect_equal(nrow(responses), 25)
})

#

context("check input mistakes")

test_that("stat != mean & stat != percentile", {
  expect_error(
    responses %>%
      select(q0) %>%
      y2clerk::freqs(stat = "perc")
  )
})

#

context("numeric variables")

test_that("NAs not present, nas = T: n & result are correct", {
  expect_equivalent(responses %>%
                      select(q0) %>%
                      y2clerk::freqs(stat = "quantile") %>%
                      select(result) %>%
                      pull(),

                    round(median(responses$q0),2)
  )

  expect_equivalent(responses %>%
                      select(q0) %>%
                      freqs(stat = "mean", nas = T) %>%
                      select(n) %>%
                      pull(),

                    nrow(responses[!is.na(responses$q0),])
  )
})

test_that("NAs not present, nas = F: n & result are correct", {
  expect_equivalent(responses %>%
                      select(q0) %>%
                      freqs(stat = "quantile", nas = F) %>%
                      select(result) %>%
                      pull(),

                    round(median(responses$q0),2)
  )
  expect_equivalent(responses %>%
                      select(q0) %>%
                      freqs(stat = "quantile", nas = F) %>%
                      select(n) %>%
                      pull(),

                    nrow(responses[!is.na(responses$q0),])
  )
})

test_that("NAs present, nas = T: throws error", {
  expect_error(responses %>%
                 select(q1) %>%
                 freqs(stat = "quantile")
  )
})

test_that("NAs present, nas = F: n & result are correct", {
  expect_equal(responses %>%
                 select(q1) %>%
                 freqs(stat = "quantile", nas = F) %>%
                 select(result) %>%
                 pull(),

               round(median(responses$q1, na.rm = T), 2)
  )
  expect_equivalent(responses %>%
                      select(q1) %>%
                      freqs(stat = "quantile", nas = F) %>%
                      select(n) %>%
                      pull(),

                    nrow(responses[!is.na(responses$q1),])
  )
})

#

context("factor variables")

test_that("factor variable input: throws error", {
  expect_error(
    responses %>%
      select(q2) %>%
      freqs(stat = 'quantile')
  )
})

#

context("character variables")

test_that("character variable input: throws error", {
  expect_error(
    responses %>%
      select(q3) %>%
      freqs(stat = 'quantile')
  )
})

#

context("value labels")

test_that("column with value labels input: throws error", {
  expect_error(
    responses %>%
      select(q4) %>%
      freqs(stat = 'quantile')
  )
})

test_that("column with value labels input: (potentially misleading) result is correct
          after labels are removed", {
  expect_equivalent(
    responses %>%
      mutate(q4 = as.numeric(q4)) %>%
      select(q4) %>%
      freqs(stat = 'quantile') %>%
      select(result) %>%
      pull(),
    median(as.numeric(responses$q4))
  )
})

test_that("column with value labels input: answer is correct after labels removed (even if potentially misleading)", {
  expect_equivalent(
    responses %>%
      select(q4) %>%
      remove_labels() %>%
      freqs(stat = 'quantile') %>%
      select(result) %>%
      pull(),
    median(as.numeric(responses$q4))
  )
})

#

context("weights")

test_that("using weights: equivalent to wtd.quantile() output", {
  expect_equal(
    responses %>%
      select(q1, w) %>%
      freqs(stat = 'quantile', nas = F, wt = w) %>%
      select(result) %>%
      pull(),

    reldist::wtd.quantile(x = responses$q1,
                          q = 0.5,
                          weight = responses$w,
                          na.rm = T) %>%
      round(2) %>%
      as.numeric()
  )
})

#

context("prompt")

test_that("using prompt: variable label is correctly output", {
  expect_equal(
    responses %>%
      select(q1) %>%
      freqs(stat = 'quantile', nas = F, prompt = T) %>%
      select(prompt) %>%
      pull(),

    responses %>%
      select(q1) %>%
      var_label() %>%
      deframe()

  )
})

#

context("digits")

test_that("using digits: output is precise to multiple decimal places", {
  expect_equal(
    responses %>%
      select(w) %>%
      freqs(stat = 'quantile', digits = 6, nas = F) %>%
      select(result) %>%
      pull(),

    responses %>%
      select(w) %>%
      pull() %>%
      median(na.rm = T) %>%
      round(digits = 6)
  )
})

#

context("minimums and maximums")

test_that("output from 'pr = 0' is equivalent to base::min()", {
  expect_equal(
    responses %>%
      freqs(q0, stat = 'quantile', pr = 0) %>%
      select(result) %>%
      pull(),

    min(responses$q0)
  )
})

test_that("output from 'pr = 0' is equivalent to base::min() when weights are provided", {
  expect_equal(
    responses %>%
      freqs(q0, stat = 'quantile', pr = 0, wt = w) %>%
      select(result) %>%
      pull(),

    min(responses$q0)
  )
})

test_that("output from 'pr = 100' is equivalent to base::max()", {
  expect_equal(
    responses %>%
      freqs(q0, stat = 'quantile', pr = 100) %>%
      select(result) %>%
      pull(),

    max(responses$q0)
  )
})

test_that("output from 'pr = 100' is equivalent to base::max() when weights are provided", {
  expect_equal(
    responses %>%
      freqs(q0, stat = 'quantile', pr = 100, wt = w) %>%
      select(result) %>%
      pull(),

    max(responses$q0)
  )
})

#

context("structure")

test_that("output from 'pr = 0' is equivalent to base::min()", {
  expect_equal(
    responses %>%
      freqs(q0, stat = 'quantile', pr = 0) %>%
      select(result) %>%
      pull(),

    min(responses$q0)
  )
})


