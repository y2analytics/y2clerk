df_stem <- data.frame(
  Q1_1 = 1,
  Q1_2 = 2,
  Q1_3 = 3,
  Q1_10 = 4,
  Q10_1 = 5,
  Q1r1 = 6,
  Q1r2 = 7,
  Q11 = 8,
  Q12 = 9,
  Q12_1 = 10,
  Q1_2_1 = 11,
  Q1_TEXT = "text",
  other = 12
)


# Separators ---------------------------------------------------------------

test_that("stem() - default separators match _ and r, not bare digits or wrong prefix", {
  result <- dplyr::select(df_stem, stem("Q1")) |> names()

  expect_true(all(
    c("Q1_1", "Q1_2", "Q1_3", "Q1_10", "Q1r1", "Q1r2") %in% result
  ))
  expect_false("Q10_1" %in% result)
  expect_false("Q11" %in% result)
  expect_false("Q12" %in% result)
  expect_false("Q1_TEXT" %in% result)
  expect_false("Q1_2_1" %in% result)
  expect_false("other" %in% result)
})

test_that("stem() - underscore separator only", {
  result <- dplyr::select(df_stem, stem("Q1", separator = "_")) |> names()

  expect_equal(result, c("Q1_1", "Q1_2", "Q1_3", "Q1_10"))
  expect_false("Q1r1" %in% result)
  expect_false("Q10_1" %in% result)
  expect_false("Q1_2_1" %in% result)
})

test_that("stem() - r separator only", {
  result <- dplyr::select(df_stem, stem("Q1", separator = "r")) |> names()

  expect_equal(result, c("Q1r1", "Q1r2"))
  expect_false("Q1_1" %in% result)
})

test_that("stem() - empty separator matches only exact stem+digits columns", {
  result <- dplyr::select(df_stem, stem("Q1", separator = "")) |> names()

  expect_equal(result, c("Q11", "Q12"))
  expect_false("Q10_1" %in% result)
  expect_false("Q1_1" %in% result)
  expect_false("Q1r1" %in% result)
})

test_that("stem() - all three separators combined", {
  result <- dplyr::select(df_stem, stem("Q1", separator = c("_", "r", ""))) |>
    names()

  expect_true(all(
    c("Q1_1", "Q1_2", "Q1_3", "Q1_10", "Q1r1", "Q1r2", "Q11", "Q12") %in% result
  ))
  expect_false("Q10_1" %in% result)
  expect_false("Q1_TEXT" %in% result)
  expect_false("other" %in% result)
})


# Edge cases ---------------------------------------------------------------

test_that("stem() - handles ignore.case correctly", {
  result <- dplyr::select(df_stem, stem("q1")) |> names()
  expect_length(result, 0)

  result2 <- dplyr::select(df_stem, stem("q1", ignore.case = TRUE)) |> names()
  expect_length(result2, 6)
})

test_that("stem() - no matches returns empty selection", {
  result <- dplyr::select(df_stem, stem("Z99")) |> names()
  expect_length(result, 0)
})

test_that("stem() - separator must be a character vector", {
  expect_snapshot(error = TRUE,
    dplyr::select(df_stem, stem("Q1", separator = 3)))
})
test_that("stem() - errors when stem is missing", {
  expect_snapshot(error = TRUE, dplyr::select(df_stem, stem(separator = 'a')))
})

test_that("stem() - stem errors correctly when not used in a selecting function", {
  expect_no_error(dplyr::select(df_stem, stem("Q1")))
  expect_no_error(freq(df_stem, stem("Q1")))
  
  expect_snapshot(error = TRUE, stem("Q1", separator = 'a'))
  
})

test_that("stem() - works inside dplyr::across", {
  result <- df_stem |>
    dplyr::mutate(
      n_selected = rowSums(dplyr::across(
        stem("Q1", separator = "_"),
        ~ !is.na(.x)
      ))
    ) |>
    dplyr::pull(n_selected)

  expect_equal(result, 4)
})
