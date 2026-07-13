# Overall functionality --------------------------------------------------------

test_that("multi_freqs - formatting", {
  test <- responses2 |> multi_freqs(m_activity)
  test_names <- responses2 |> multi_freqs(m_activity) |> names()

  expect_equal(class(test)[1], 'freq_y2')
  expect_equal(class(test)[2], 'tbl_df')
  expect_equal(
    test_names,
    c('variable', 'value', 'label', 'n', 'stat', 'result')
  )
})


test_that("multi_freqs - pulls all vars with stem", {
  test <- responses2 |> multi_freqs(m_activity)
  check_vars_pulled <- test |> dplyr::pull(variable)

  expect_equal(
    check_vars_pulled,
    c(
      'm_activity_1',
      'm_activity_2',
      'm_activity_3',
      'm_activity_10',
      'm_activity_21',
      'm_activity_22'
    )
  )
})


test_that("multi_freqs - ns and percentages", {
  test <- responses2 |> multi_freqs(m_activity)

  expected_n <- responses2 |>
    dplyr::count(m_activity_10) |>
    dplyr::filter(m_activity_10 == 1) |>
    dplyr::pull(n)
  total_n <- responses2 |>
    dplyr::filter(
      !is.na(m_activity_1) |
        !is.na(m_activity_2) |
        !is.na(m_activity_3) |
        !is.na(m_activity_10) |
        !is.na(m_activity_21)
    ) |>
    nrow()
  actual_n <- test |>
    dplyr::filter(label == 'Baseball') |>
    dplyr::pull(n)
  actual_p <- test |>
    dplyr::filter(label == 'Baseball') |>
    dplyr::pull(result)

  expect_equal(expected_n, actual_n)
  expect_equal(round(expected_n / total_n, 2), actual_p)
})


test_that("multi_freqs - grouped ns and percentages", {
  test <- responses2 |>
    dplyr::group_by(gender) |>
    multi_freqs(m_activity)

  expected_n <- responses2 |>
    dplyr::filter(gender == 'other') |>
    dplyr::count(m_activity_10) |>
    dplyr::filter(m_activity_10 == 1) |>
    dplyr::pull(n)
  total_n <- responses2 |>
    dplyr::filter(gender == 'other') |>
    dplyr::filter(
      !is.na(m_activity_1) |
        !is.na(m_activity_2) |
        !is.na(m_activity_3) |
        !is.na(m_activity_10) |
        !is.na(m_activity_21)
    ) |>
    nrow()
  actual_n <- test |>
    dplyr::filter(group_var == 'other' & label == 'Baseball') |>
    dplyr::pull(n)
  actual_p <- test |>
    dplyr::filter(group_var == 'other' & label == 'Baseball') |>
    dplyr::pull(result)

  expect_equal(expected_n, actual_n)
  expect_equal(round(expected_n / total_n, 2), actual_p)
})


# Stem interface ----------------------------------------------------------

test_that("multi_freqs - accepts symbol, string, all_of() and any_of()", {
  stems <- c("m_activity")

  by_symbol <- responses2 |> multi_freqs(m_activity)
  by_string <- responses2 |> multi_freqs("m_activity")
  by_all_of <- responses2 |> multi_freqs(tidyselect::all_of(stems))
  by_any_of <- responses2 |> multi_freqs(tidyselect::any_of(stems))

  expect_equal(by_symbol, by_string)
  expect_equal(by_symbol, by_all_of)
  expect_equal(by_symbol, by_any_of)
})


test_that("multi_freqs - runs multiple stems in one call", {
  df <- tibble::tibble(
    QA_1 = c(1, NA, 1),
    QA_2 = c(NA, 1, 1),
    QB_1 = c(1, 1, NA),
    QB_2 = c(NA, NA, 1)
  )

  test <- df |> multi_freqs(QA, QB)

  expect_setequal(
    unique(test$variable),
    c('QA_1', 'QA_2', 'QB_1', 'QB_2')
  )
})


test_that("multi_freqs - no stems runs on every stem in the dataset", {
  df <- tibble::tibble(
    Q1_1 = c(1, NA, 1),
    Q1_2 = c(NA, 1, 1)
  )

  test <- df |> multi_freqs()

  expect_setequal(unique(test$variable), c('Q1_1', 'Q1_2'))
})


test_that("multi_freqs - ignore.case argument controls stem matching", {
  matched <- responses2 |> multi_freqs("M_ACTIVITY", ignore.case = TRUE)
  unmatched <- responses2 |> multi_freqs("M_ACTIVITY")

  expect_true('m_activity_1' %in% matched$variable)
  expect_equal(nrow(unmatched), 0)
})


test_that("multi_freqs - separator argument controls which columns match", {
  df <- tibble::tibble(
    Q_1 = c(1, NA),
    Qr1 = c(1, 1),
    Qr2 = c(1, NA)
  )

  underscore <- df |> multi_freqs(Q, separator = "_")
  r_sep <- df |> multi_freqs(Q, separator = "r")

  expect_equal(unique(underscore$variable), 'Q_1')
  expect_setequal(unique(r_sep$variable), c('Qr1', 'Qr2'))
})


# Warnings ----------------------------------------------------------------

test_that("multi_freqs - warns and returns nothing when passed an actual variable", {
  expect_snapshot(x <- responses2 |> multi_freqs(m_activity_1))

  expect_equal(nrow(x), 0)
})


test_that("multi_freqs - warns on single-select and text stems", {
  single_warnings <- testthat::capture_warnings(
    responses2 |> multi_freqs(s_activity)
  )
  expect_true(any(grepl('Single select', single_warnings)))

  text_df <- tibble::tibble(
    Q1_1 = c('a', 'b'),
    Q1_2 = c('c', 'd')
  )
  text_warnings <- testthat::capture_warnings(text_df |> multi_freqs(Q1))
  expect_true(any(grepl('Text variable', text_warnings)))
})


# .by grouping ------------------------------------------------------------

test_that("multi_freqs - .by matches an equivalent group_by()", {
  by_dot <- responses2 |> multi_freqs(m_activity, .by = gender)
  by_group <- responses2 |>
    dplyr::group_by(gender) |>
    multi_freqs(m_activity)

  expect_equal(by_dot, by_group)
})


test_that("multi_freqs - .by errors when data is already grouped", {
  expect_snapshot(
    error = TRUE,
    responses2 |>
      dplyr::group_by(gender) |>
      multi_freqs(m_activity, .by = weights)
  )
})


test_that("multi_freqs - .by errors on an unknown column", {
  expect_snapshot(
    error = TRUE,
    responses2 |> multi_freqs(m_activity, .by = not_a_col)
  )
})


# Individual arguments ----------------------------------------------------

test_that("multi_freqs - remove_nas argument", {
  test_false <- responses2 |> multi_freqs(m_activity, remove_nas = FALSE)
  test_true <- responses2 |> multi_freqs(m_activity, remove_nas = TRUE)

  expect_equal(nrow(test_false), 12)
  expect_equal(nrow(test_true), 6)
})


test_that("multi_freqs - wt argument", {
  test <- responses2 |> multi_freqs(m_activity, wt = weights)

  expected_n <- responses2 |>
    dplyr::mutate(
      weighted_n = m_activity_3 * weights,
      sum_weighted_n = sum(weighted_n, na.rm = TRUE)
    ) |>
    dplyr::distinct(sum_weighted_n) |>
    dplyr::pull(sum_weighted_n)
  total_n <- responses2 |>
    dplyr::filter(
      !is.na(m_activity_1) |
        !is.na(m_activity_2) |
        !is.na(m_activity_3) |
        !is.na(m_activity_10) |
        !is.na(m_activity_21)
    ) |>
    dplyr::mutate(sum_weights = sum(weights)) |>
    dplyr::distinct(sum_weights) |>
    dplyr::pull(sum_weights)
  actual_n <- test |>
    dplyr::filter(label == 'Volleyball') |>
    dplyr::pull(n)
  actual_p <- test |>
    dplyr::filter(label == 'Volleyball') |>
    dplyr::pull(result)

  expect_equal(expected_n, actual_n)
  expect_equal(round(expected_n / total_n, 2), actual_p)
})


test_that("multi_freqs - prompt argument", {
  test <- responses2 |> multi_freqs(m_activity, prompt = TRUE)
  test_names <- test |> names()

  expect_equal(
    test_names,
    c('variable', 'prompt', 'value', 'label', 'n', 'stat', 'result')
  )
  expect_equal(
    test$prompt[3],
    'Which of the following activities have you done in the past month? Please select all that apply. - Volleyball'
  )
})


test_that("multi_freqs - digits argument", {
  test_3 <- responses2 |>
    multi_freqs(m_activity, digits = 3) |>
    dplyr::filter(label == 'Baseball') |>
    dplyr::pull(result)
  test_2 <- responses2 |>
    multi_freqs(m_activity) |>
    dplyr::filter(label == 'Baseball') |>
    dplyr::pull(result)
  test_1 <- responses2 |>
    multi_freqs(m_activity, digits = 1) |>
    dplyr::filter(label == 'Baseball') |>
    dplyr::pull(result)

  expect_equal(nchar(test_3), 5)
  expect_equal(nchar(test_2), 4)
  expect_equal(nchar(test_1), 3) # 3 characters because it includes the '0.' as 2 characters
})


test_that("multi_freqs - nas_group argument", {
  test <- responses2 |>
    dplyr::group_by(gender) |>
    multi_freqs(
      m_activity,
      nas_group = FALSE
    )

  # NAs showing up on the unchosen activity

  expect_length(unique(test$group_var), 3)
})


test_that("multi_freqs - factor_group argument", {
  test_factor_true <- responses2 |>
    dplyr::group_by(gender_labelled) |>
    multi_freqs(
      m_activity,
      factor_group = TRUE
    )
  test_factor_false <- responses2 |>
    dplyr::group_by(gender_labelled) |>
    multi_freqs(
      m_activity,
      factor_group = FALSE
    )

  expect_true(stringr::str_detect(test_factor_true$group_var, 'male')[1])
  expect_false(
    stringr::str_detect(test_factor_false$group_var, 'male')[1]
  )
})


test_that("multi_freqs - unweighted_ns argument", {
  test_n_standard <- responses2 |>
    multi_freqs(m_activity) |>
    dplyr::select(n)
  test_result_weighted <- responses2 |>
    multi_freqs(m_activity, wt = weights) |>
    dplyr::select(result)
  test_n_unweighted_ns <- responses2 |>
    multi_freqs(m_activity, wt = weights, unweighted_ns = TRUE) |>
    dplyr::select(n)
  test_result_unweighted_ns <- responses2 |>
    multi_freqs(m_activity, wt = weights, unweighted_ns = TRUE) |>
    dplyr::select(result)

  expect_equal(test_n_standard, test_n_unweighted_ns)
  expect_equal(test_result_weighted, test_result_unweighted_ns)
})


test_that("multi_freqs - show_missing_levels argument", {
  test_no_missing_levels <- responses2 |>
    multi_freqs(
      m_activity,
      show_missing_levels = FALSE
    )
  test_yes_missing_levels <- responses2 |>
    multi_freqs(
      m_activity,
      show_missing_levels = TRUE
    )
  sum_no_missing <-
    stringr::str_detect(
      test_no_missing_levels$label,
      'An unchosen activity'
    ) |>
    sum()
  sum_yes_missing <-
    stringr::str_detect(
      test_yes_missing_levels$label,
      'An unchosen activity'
    ) |>
    sum()

  expect_equal(sum_no_missing, 0)
  expect_equal(sum_yes_missing, 1)
})


test_that("multi_freqs - show_missing_levels argument, grouped", {
  no_missing <- responses2 |>
    dplyr::group_by(gender) |>
    multi_freqs(
      m_activity,
      nas_group = FALSE,
      show_missing_levels = FALSE
    )
  yes_missing <- responses2 |>
    dplyr::group_by(gender) |>
    multi_freqs(
      m_activity,
      nas_group = FALSE,
      show_missing_levels = TRUE
    )
  yes_missing_with_nas_group <- responses2 |>
    dplyr::group_by(gender) |>
    multi_freqs(
      m_activity,
      show_missing_levels = TRUE
    )
  sum_no_missing <-
    stringr::str_detect(no_missing$label, 'An unchosen activity') |>
    sum()
  sum_yes_missing <-
    stringr::str_detect(yes_missing$label, 'An unchosen activity') |>
    sum()
  sum_yes_missing_with_nas_group <-
    stringr::str_detect(
      yes_missing_with_nas_group$label,
      'An unchosen activity'
    ) |>
    sum()
  expect_equal(sum_no_missing, 0)
  expect_equal(sum_yes_missing, 3)
  expect_equal(sum_yes_missing_with_nas_group, 4)
})
