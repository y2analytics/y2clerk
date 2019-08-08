context("Tests on frequencies functions")

### freq tests ------------

# parameter testing
test_that("freq function: Dataframe/Tibble parameter warning test class character", {
  df <- c('This', 'is', 'not', 'a', 'dataframe')
  a = c(1, 1, 2, 3, 1)
  expect_error(freq(df, a), "no applicable method for 'pull' applied to an object of class \"character\"")
  })

test_that("freq function: Dataframe/Tibble parameter warning test class matrix/double/integer", {
  column_a <- c(1,1,1,1,2,2,3)
  column_b <- c(0.5, 1.2, 0.8, 0.5, 0.2, 0.1, 1)
  table <- rbind(column_a, column_b)
  expect_error(freq(table, column_a))
})

test_that("freq function: Variable parameter warning test", {
  expect_error(freq(mtcars, 10), "`value` = 10 must be a symbol or a string, not a double vector")
})

test_that("freq function: NA parameter warning test", {
  expect_error(freq(mtcars, cyl, nas = 'True'), "invalid argument type")
})

test_that("freq function: WT parameter warning test", {
  expect_error(freq(mtcars, cyl, wt = 'True'))
})

# expected input freq
test_that("freq function: expected non-weighted input", {
  df <- data.frame(
    a = c(1, 2, 2, 3, 4, 2, NA),
    weights = c(0.9, 0.9, 1.1, 1.1, 1, 1, 1)
  )

  freq(df, a)
})

# expected weighted freq
test_that("freq function: expected weighted input", {
  df <- data.frame(
    a = c(1, 2, 2, 3, 4, 2, NA),
    weights = c(0.9, 0.9, 1.1, 1.1, 1, 1, 1)
  )

  freq(df, a, wt = weights)
})

# character column freq
# labelled column freq
# numeric column freq
# factor column freq


### freqs tests ===========

# parameter testing
test_that("freqs function: Dataframe/Tibble parameter warning test class character", {
  df <- c('This', 'is', 'not', 'a', 'dataframe')
  a = c(1, 1, 2, 3, 1)
  b = c(0, 7, 1, 2, 8)
  expect_error(freqs(df, a, b), "no applicable method for 'pull' applied to an object of class \"character\"")
  })

test_that("freqs function: Dataframe/Tibble parameter warning test class matrix/double/integer", {
  column_a <- c(1,1,1,1,2,2,3)
  column_b <- c(0.5, 1.2, 0.8, 0.5, 0.2, 0.1, 1)
  table <- rbind(column_a, column_b)
  expect_error(freqs(table, column_a, column_b))
})

test_that("freqs function: Variable parameter warning test", {
  expect_error(freqs(mtcars, 'Not a column'))
})

test_that("freqs function: NA parameter warning test", {
  expect_error(freqs(mtcars, cyl, disp, hp, nas = 'True'), "invalid argument type")
})

test_that("freqs function: WT parameter warning test", {
  expect_error(freqs(mtcars, cyl, disp, hp, wt = 'weights'))
})

# expected input freq
test_that("freqs function: expected non-weighted input", {
  df <- data.frame(
    a = c(1, 2, 2, 3, 4, 2, NA),
    b = c(1, 2, 2, 3, 4, 1, NA),
    weights = c(0.9, 0.9, 1.1, 1.1, 1, 1, 1)
  )
  freqs(df, a, b)
})

# expected weighted freq
test_that("freqs function: expected weighted input", {
  df <- data.frame(
    a = c(1, 2, 2, 3, 4, 2, NA),
    b = c(1, 2, 2, 3, 4, 1, NA),
    weights = c(0.9, 0.9, 1.1, 1.1, 1, 1, 1)
  )
  freqs(df, a, b, wt = weights)
})

# character column freq
# labelled column freq
# numeric column freq
# factor column freq

