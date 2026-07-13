test_that("informational messages are emitted by default", {
  df <- data.frame(x = c(1, 2, 3))

  expect_snapshot(
    invisible(freqs(df, x, stat = "mean", percentile = 50))
  )
})

test_that("y2clerk.quiet = TRUE suppresses informational messages", {
  withr::local_options(y2clerk.quiet = TRUE)
  df <- data.frame(x = c(1, 2, 3))

  expect_snapshot(
    invisible(freqs(df, x, stat = "mean", percentile = 50))
  )
})

test_that("is_quiet() reflects the y2clerk.quiet option", {
  withr::local_options(y2clerk.quiet = NULL)
  expect_false(is_quiet())

  withr::local_options(y2clerk.quiet = TRUE)
  expect_true(is_quiet())

  withr::local_options(y2clerk.quiet = FALSE)
  expect_false(is_quiet())
})
