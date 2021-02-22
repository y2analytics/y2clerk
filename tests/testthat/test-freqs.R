#### freqs ####

library(testthat)
library(dplyr)
library(y2clerk)

context("Tests on frequencies functions")



### Incorrect parameter testing
#dataframes
test_that("Not a dataframe error - vectors", {
  df <- c('This', 'is', 'not', 'a', 'dataframe')
  a = c(1, 1, 2, 3, 1)
  expect_error(freq(df, a))
})
test_that("Not a dataframe error - matrix", {
  column_a <- c(1,1,1,1,2,2,3)
  column_b <- c(0.5, 1.2, 0.8, 0.5, 0.2, 0.1, 1)
  table <- rbind(column_a, column_b)
  expect_error(freq(table, column_a))
})
#variables
test_that("Runs on variables, not integers", {
  expect_error(freq(mtcars, 10))
})
#nas
test_that("Incorrect nas argument", {
  expect_error(freq(mtcars, cyl, nas = 'True'))
})
#weights
test_that("Incorrect wt argument", {
  expect_error(freq(mtcars, cyl, wt = 'True'))
})



### weights
test_that("Weights", {
  df <- data.frame(
    a = c(1, 2, 2, 3, 4, 2, NA),
    weights = c(0.9, 0.9, 1.1, 1.1, 1, 1, 1)
  )

  freqs_weighted <- freq(df, a, wt = weights)
  expect_equal(freqs_weighted$n[1], .9)
})



### nas
#label
test_that("nas - label", {
  df <- data.frame(
    a = c(1, 2, 2, 3, 4, 2, NA),
    weights = c(0.9, 0.9, 1.1, 1.1, 1, 1, 1)
  )

  yes_nas <- freq(df, a)
  no_nas <- freqs(df, a, nas = FALSE)

  expect_equal(nrow(yes_nas), 5)
  expect_equal(nrow(no_nas), 4)
})
#group
test_that("nas - group", {
  df <- data.frame(
    a = c(1, 2, 2, 3, 4, 2, NA),
    g = c(1, 1, 2, 2, 3, NA, 2)
  ) %>% dplyr::group_by(g)

  yes_nas <- df %>%
    dplyr::group_by(g) %>%
    freqs(a)
  no_nas <- df %>%
    dplyr::group_by(g) %>%
    freqs(a, nas_group = FALSE)
  group_factors <- df %>%
    dplyr::group_by(g) %>%
    freqs(a, factor_group = TRUE)

  expect_equal(nrow(yes_nas), 7)
  expect_equal(nrow(no_nas), 6)
  expect_equal(is.factor(group_factors$group_var), TRUE)
  expect_equal(is.factor(no_nas$group_var), FALSE)
  expect_equal(names(yes_nas)[1], 'group_var')
})



###Digits
test_that("Digits", {
  df <- data.frame(
    a = c(.1, .2, .3)
  )

  dig1 <- freq(df, a, digits = 1)
  dig2 <- freq(df, a)
  dig3 <- freq(df, a, digits = 3)

  expect_equal(dig1$result[1], .3)
  expect_equal(dig2$result[1], .33)
  expect_equal(dig3$result[1], .333)
})



###Differing classes of variables
#character column freq
test_that("character vars", {
  df <- data.frame(
    a = c('Character', '1', 'test')
  )
  frequencies <- freqs(df, a)
  expect_equal(is.data.frame(frequencies), TRUE)
})
#numeric column freq
test_that("numeric vars", {
  df <- data.frame(
    a = c(1, 2, 3)
  )
  frequencies <- freqs(df, a)
  expect_equal(is.data.frame(frequencies), TRUE)
})
#factored/labelled column freq
test_that("factor vars with missing values", {
  df <- data.frame(
    a = c(1, 2, 3)
  )
  labelled::val_label(df$a, 1) <- 'Yes'
  labelled::val_label(df$a, 2) <- 'No'
  labelled::val_label(df$a, 3) <- 'Idk'
  labelled::val_label(df$a, 4) <- 'Do I show up?'
  df$a <- forcats::as_factor(df$a)

  frequencies <- freqs(df, a)
  expect_equal(nrow(frequencies), 4)
})

###Select function
#filter groups
test_that("filter out groups from vars", {
  df <- data.frame(
    a = c(1, 1, 3, 4, 5),
    b = c(1, 1, 1, 2, 2),
    c = c(2, 3, 4, 5, 6)
  )
  frequencies <- df %>%
    dplyr::select(a, b) %>%
    dplyr::group_by(b) %>%
    freqs()

  expect_equal(nrow(frequencies), 4)
})
#filter weights
test_that("filter out weights from vars", {
  df <- data.frame(
    a = c(1, 1, 3, 4, 5),
    b = c(1, 1, 1, 2, 2),
    c = c(2, 3, 4, 5, 6)
  )
  frequencies <- df %>%
    dplyr::select(a, b) %>%
    freqs(wt = b)

  expect_equal(nrow(frequencies), 4)
})
