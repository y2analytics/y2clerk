
test_that("single string var, no errors", {
  df_labelled <- tibble::tibble(
    var1 = c(
      'I like to talk about dogs',
      'Dogs are cool but cats are aight too',
      'I prefer dogs over cats',
      "My dog's collars are always too tight",
      'One last sentence about dogs',
      'Cats collars are typically cooler than dogs'
    )
  )
  labelled::var_label(df_labelled$var1) <- 'My prompt'

  frequencies <- verbatims_y2(df_labelled, var1)
  expect_equal(class(frequencies), c("tbl_df", "tbl", "data.frame"))
})

test_that("creates a data frame", {
  df_labelled <- tibble::tibble(
    var1 = c(
      'I like to talk about dogs',
      'Dogs are cool but cats are aight too',
      'I prefer dogs over cats',
      "My dog's collars are always too tight",
      'One last sentence about dogs',
      'Cats collars are typically cooler than dogs'
    )
  )
  labelled::var_label(df_labelled$var1) <- 'My prompt'

  frequencies <- verbatims_y2(df_labelled, var1)
  expect_error(frequencies, NA)
})


### No labels
test_that("variable with no label - give a warning", {
  df_nolab <- tibble::tibble(
    var1 = c(
      'I like to talk about dogs',
      'Dogs are cool but cats are aight too',
      'I prefer dogs over cats',
      "My dog's collars are always too tight",
      'One last sentence about dogs',
      'Cats collars are typically cooler than dogs'
    )
  )

  expect_warning(
    verbatims_y2(df_nolab, var1),
    "You are working with variables that have no labeling. You may want to consider adding a prompt before continuing"
    )
})


### Empty and NA strings
test_that("empty strings ('')", {
  df_empty_strings <- tibble::tibble(
    var1 = c(
      'I like to talk about dogs',
      'Dogs are cool but cats are aight too',
      'I prefer dogs over cats',
      "My dog's collars are always too tight",
      'One last sentence about dogs',
      'Cats collars are typically cooler than dogs'
    )
  ) %>%
    dplyr::add_row(var1 = c("", ""))
  labelled::var_label(df_empty_strings$var1) <- 'My prompt'

  frequencies <- verbatims_y2(df_empty_strings, var1)
  length_freqs <- dplyr::count(frequencies) %>% as.numeric()

  expect_error(frequencies, NA)
  expect_equal(length_freqs, 6)
})

test_that("NA strings", {
  df_na_strings <- tibble::tibble(
    var1 = c(
      'I like to talk about dogs',
      'Dogs are cool but cats are aight too',
      'I prefer dogs over cats',
      "My dog's collars are always too tight",
      'One last sentence about dogs',
      'Cats collars are typically cooler than dogs'
    )
  ) %>%
    dplyr::add_row(var1 = c(NA_character_, NA_character_))
  labelled::var_label(df_na_strings$var1) <- 'My prompt'

  frequencies <- verbatims_y2(df_na_strings, var1)
  length_freqs <- dplyr::count(frequencies) %>% as.numeric()

  expect_error(frequencies, NA)
  expect_equal(length_freqs, 6)
})


### Duplicates and large data frame
test_that("Duplicates not removed", {
  df_nolab <- tibble::tibble(
    var1 = c(
      'I like to talk about dogs',
      'Dogs are cool but cats are aight too',
      'I prefer dogs over cats',
      "My dog's collars are always too tight",
      'One last sentence about dogs',
      'Cats collars are typically cooler than dogs'
    )
  )
  df_duplicates <- dplyr::bind_rows(
    df_nolab,
    df_nolab
  )
  labelled::var_label(df_duplicates$var1) <- 'My prompt'

  frequencies <- verbatims_y2(df_duplicates, var1)
  length_freqs <- dplyr::count(frequencies) %>% as.numeric()

  expect_equal(length_freqs, 12)
})

test_that("Large data frame", {
  df_large <- tibble::tibble(
    var1 = rep("test test test test test test test test test test test", 100000)
  )
  labelled::var_label(df_large$var1) <- 'My prompt'

  frequencies <- verbatims_y2(df_large, var1)
  length_freqs <- dplyr::count(frequencies) %>% as.numeric()

  expect_equal(length_freqs, 100000)
})


### Special symbols
test_that("Large data frame", {
  df_special <- tibble::tibble(
    var1 = c(
      "line \n  breaks ",
      '!@#$%^&*()_+".,',
      "\U0001f600\U0001f601\U0001f602\U0001f603\U0001f604\U0001f605\U0001f606\U0001f607\U0001f608\U0001f609\U0001f60a\U0001f60b\U0001f60c\U0001f60d\U0001f60e\U0001f60f\U0001f610\U0001f611\U0001f612\U0001f613\U0001f614\U0001f615\U0001f616\U0001f617\U0001f618\U0001f619\U0001f61a\U0001f61b\U0001f61c\U0001f61d\U0001f61e\U0001f61f\U0001f620\U0001f621\U0001f622\U0001f623\U0001f624\U0001f625\U0001f626\U0001f627\U0001f628\U0001f629\U0001f62a\U0001f62b\U0001f62c\U0001f62d\U0001f62e\U0001f62f\U0001f630\U0001f631\U0001f632\U0001f633\U0001f634\U0001f635\U0001f636\U0001f637\U0001f638\U0001f639\U0001f63a\U0001f63b\U0001f63c\U0001f63d\U0001f63e\U0001f63f\U0001f640\U0001f641\U0001f642\U0001f643\U0001f644\U0001f645\U0001f646\U0001f647\U0001f648\U0001f649\U0001f64a\U0001f64b\U0001f64c\U0001f64d\U0001f64e\U0001f64f"
    )
  )
  labelled::var_label(df_special$var1) <- 'My prompt'

  frequencies <- verbatims_y2(df_special, var1)
  length_freqs <- dplyr::count(frequencies) %>% as.numeric()

  expect_error(frequencies, NA)
  expect_equal(length_freqs, 3)
})


### Select multiple vars
test_that("multiple vars", {
  df_labelled <- tibble::tibble(
    var1 = c(
      'I like to talk about dogs',
      'Dogs are cool but cats are aight too',
      'I prefer dogs over cats',
      "My dog's collars are always too tight",
      'One last sentence about dogs',
      'Cats collars are typically cooler than dogs'
    )
  )
  labelled::var_label(df_labelled$var1) <- 'My prompt'
  df_multiple <- df_labelled %>%
    dplyr::mutate(
      var2 = var1,
      var3 = var1
    )

  frequencies <- df_multiple %>% verbatims_y2(var1, var2, var3)
  length_freqs <- dplyr::count(frequencies) %>% as.numeric()

  expect_error(frequencies, NA)
  expect_equal(length_freqs, 18)
})

test_that("pipe vars", {
  df_labelled <- tibble::tibble(
    var1 = c(
      'I like to talk about dogs',
      'Dogs are cool but cats are aight too',
      'I prefer dogs over cats',
      "My dog's collars are always too tight",
      'One last sentence about dogs',
      'Cats collars are typically cooler than dogs'
    )
  )
  labelled::var_label(df_labelled$var1) <- 'My prompt'
  df_multiple <- df_labelled %>%
    dplyr::mutate(
      var2 = var1,
      var3 = var1
    )

  frequencies <- df_multiple %>%
    dplyr::select(dplyr::starts_with('var')) %>%
    verbatims_y2()
  length_freqs <- dplyr::count(frequencies) %>% as.numeric()

  expect_error(frequencies, NA)
  expect_equal(length_freqs, 18)
})

test_that("base_ns", {
  df_labelled <- tibble::tibble(
    var1 = c(
      'I like to talk about dogs',
      'Dogs are cool but cats are aight too',
      'NA',
      "My dog's collars are always too tight",
      '',
      'Cats collars are typically cooler than dogs'
    )
  )
  labelled::var_label(df_labelled$var1) <- 'My prompt'
  ns <- df_labelled %>%
    verbatims_y2(var1)

  expect_equal(names(ns)[4], 'base_ns')
  expect_equal(
    ns %>% dplyr::pull(base_ns),
    c(4, 4, 4, 4)
    )
})


test_that("empty variables", {
  df_labelled <- tibble::tibble(
    var1 = c('','',''),
    var2 = c(
      'I like to talk about dogs',
      'Dogs are cool but cats are aight too',
      "My dog's collars are always too tight"
    ),
    var3 = c('NA','NA','NA')
  )
  labelled::var_label(df_labelled$var1) <- 'My prompt'
  labelled::var_label(df_labelled$var2) <- 'My prompt 2'
  labelled::var_label(df_labelled$var3) <- 'My prompt 3'

  expect_error(
    verbatims_y2(df_labelled),
    regexp = NA
    )
  expect_equal(
    verbatims_y2(df_labelled) %>% dplyr::pull(variable),
    c('var2', 'var2', 'var2')
  )
})
