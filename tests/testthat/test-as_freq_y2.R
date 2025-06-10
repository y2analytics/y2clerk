test_that("`as_freq_y2()` returns original object if already of class freq_y2", {
  # Create a freq_y2 object
  df <- tibble::tibble(x = 1:3, y = letters[1:3])
  class(df) <- c("freq_y2", class(df))

  # Test that the function returns the original object
  result <- as_freq_y2(df)
  expect_identical(result, df)
  expect_s3_class(result, "freq_y2")
})

test_that("`as_freq_y2()` converts a data frame to a freq_y2 object", {
  # Create a regular data frame
  df <- data.frame(x = 1:3, y = letters[1:3])

  # Test conversion
  result <- as_freq_y2(df)
  expect_s3_class(result, "freq_y2")
  # Test that the classes stack and do not replace
  expect_true("tbl_df" %in% class(result))
  expect_true("tbl" %in% class(result))
  expect_true("data.frame" %in% class(result))
})

test_that("`as_freq_y2()` converts a tibble to a freq_y2 object", {
  # Create a tibble
  df <- tibble::tibble(x = 1:3, y = letters[1:3])

  # Test conversion
  result <- as_freq_y2(df)
  expect_s3_class(result, "freq_y2")

  # Test that it did not change anything else about the tibble
  class(result) <- setdiff(class(result), 'freq_y2')
  expect_identical(result, df)
})

test_that("`as_freq_y2()` adds prompt attribute when valid named vector is provided", {
  df <- tibble::tibble(x = 1:3, y = letters[1:3])
  p <- c(prompt1 = "This is prompt 1", prompt2 = "This is prompt 2")

  result <- as_freq_y2(df, p)

  expect_s3_class(result, "freq_y2")
  expect_identical(attr(result, "prompts"), p)
})

test_that("`as_freq_y2()` errors when p is not a named character vector", {
  df <- tibble::tibble(x = 1:3, y = letters[1:3])
  p_numeric <- c(prompt1 = 1, prompt2 = 2)
  p_list <- list(prompt1 = "This is prompt 1", prompt2 = "This is prompt 2")
  p_unnamed <- c("This is prompt 1", "This is prompt 2")

  expect_error(as_freq_y2(df, p_numeric), "p must be a character vector")
  expect_error(as_freq_y2(df, df), "p must be a character vector")
  expect_error(as_freq_y2(df, p_list), "p must be a character vector")
  expect_error(as_freq_y2(df, p_unnamed), "Every element of p must be named")
})


test_that("`as_freq_y2()` correctly handles NULL p parameter", {
  df <- tibble::tibble(x = 1:3, y = letters[1:3])

  result <- as_freq_y2(df, NULL)

  expect_s3_class(result, "freq_y2")
  expect_null(attr(result, "prompts"))
})


test_that("`as_tibble.freq_y2()` removes class and prompt but leaves other data untouched", {
  # Create a freq_y2 object
  df1 <- tibble::tibble(x = 1:3, y = letters[1:3])
  df2 <- df1
  class(df2) <- c("freq_y2", class(df2))
  attr(df2, "prompts") <- c(prompt1 = "Test prompt")

  # Test conversion to tibble
  result <- dplyr::as_tibble(df2)

  # Class
  expect_false("freq_y2" %in% class(result))
  expect_true("tbl_df" %in% class(result))

  # Prompts
  expect_null(attr(result, "prompts"))

  # Data
  expect_identical(result, df1)
})

test_that("`as.data.frame.freq_y2()` removes class and prompt but leaves other data untouched", {
  # Create a freq_y2 object
  df1 <- data.frame(x = 1:3, y = letters[1:3])
  df2 <- df1
  class(df2) <- c("freq_y2", class(df2))
  attr(df2, "prompts") <- c(prompt1 = "Test prompt")

  # Test conversion to tibble
  result <- as.data.frame(df2)

  # Class
  expect_false("freq_y2" %in% class(result))
  expect_true("data.frame" %in% class(result))

  # Prompts
  expect_null(attr(result, "prompts"))

  # Data
  expect_identical(result, df1)
})
