
# Setting the data --------------------------------------------------------
set.seed(532987)
responses <- data.frame(
  # character, no value labels
  gender = c(
    rep('male', 8,),
    rep('female', 12),
    rep('other', 4),
    rep(NA_character_, 1)
  ),
  gender_labelled = c(
    rep(1, 8,),
    rep(2, 12),
    rep(3, 4),
    rep(NA_real_, 1)
  ),
  # single select, not all options selected
  s_activity_1 = sample(
    1:5,
    25,
    prob = c(.4, .3, .2, .0, .1),
    replace = TRUE
  ),
  # multiple select
  m_activity_1 = sample(
    c(NA_real_, 1),
    25,
    prob = c(.9, .1),
    replace = TRUE
  ),
  m_activity_2 = sample(
    c(NA_real_, 1),
    25,
    prob = c(.6, .4),
    replace = TRUE
  ),
  m_activity_3 = sample(
    c(NA_real_, 1),
    25,
    prob = c(.8, .2),
    replace = TRUE
  ),
  m_activity_10 = sample(
    c(NA_real_, 1),
    25,
    prob = c(.5, .5),
    replace = TRUE
  ),
  m_activity_21 = sample(
    c(NA_real_, 1),
    25,
    prob = c(.1, .9),
    replace = TRUE
  ),
  m_activity_22 = NA_real_,
  # numeric weights
  weights = sample(
    c(.5, 1, 2, 4),
    25,
    prob = rep(.25, 4),
    replace = TRUE
  )
) %>%
  labelled::set_value_labels(
    s_activity_1 = c(
      'Basketball' = 1,
      'Football' = 2,
      'Volleyball' = 3,
      'Baseball' = 4,
      'Underwater Basket Weaving' = 5
    ),
    m_activity_1 = c('Basketball' = 1),
    m_activity_2 = c('Football' = 1),
    m_activity_3 = c('Volleyball' = 1),
    m_activity_10 = c('Baseball' = 1),
    m_activity_21 = c('Underwater Basket Weaving' = 1),
    m_activity_22 = c('An unchosen activity' = 1),
    gender_labelled = c(
      'male' = 1,
      'female' = 2,
      'other' = 3
    )
  ) %>%
  labelled::set_variable_labels(
    gender_labelled = "Which of the following best describes how you think of yourself?",
    s_activity_1 = "Which of the following is your preferred activity?",
    m_activity_1 = "Which of the following activities have you done in the past month? Please select all that apply. - Basketball",
    m_activity_2 = "Which of the following activities have you done in the past month? Please select all that apply. - Football",
    m_activity_3 = "Which of the following activities have you done in the past month? Please select all that apply. - Volleyball",
    m_activity_10 = "Which of the following activities have you done in the past month? Please select all that apply. - Baseball",
    m_activity_21 = "Which of the following activities have you done in the past month? Please select all that apply. - Underwater Basket Weaving",
    weights = "Weights"
  ) %>%
  dplyr::as_tibble()




# Overall functionality --------------------------------------------------------

test_that("multi_freqs - formatting", {
  test <- responses %>% multi_freqs(m_activity_1)
  test_names <- responses %>% multi_freqs(m_activity_1) %>% names()

  expect_equal(class(test)[1], 'tbl_df')
  expect_equal(test_names, c('variable', 'value', 'label', 'n', 'stat', 'result'))
})


test_that("multi_freqs - pulls all vars with stem", {
  test <- responses %>% multi_freqs(m_activity_1)
  check_vars_pulled <- test %>% dplyr::pull(variable)

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
  test <- responses %>% multi_freqs(m_activity_1)

  expected_n <- responses %>% dplyr::count(m_activity_10) %>%
    dplyr::filter(m_activity_10 == 1) %>%
    dplyr::pull(n)
  total_n <- responses %>%
    dplyr::filter(
      !is.na(m_activity_1) |
        !is.na(m_activity_2) |
        !is.na(m_activity_3) |
        !is.na(m_activity_10) |
        !is.na(m_activity_21)
    ) %>% nrow()
  actual_n <- test %>%
    dplyr::filter(label == 'Baseball') %>%
    dplyr::pull(n)
  actual_p <- test %>%
    dplyr::filter(label == 'Baseball') %>%
    dplyr::pull(result)

  expect_equal(expected_n, actual_n)
  expect_equal(round(expected_n / total_n, 2), actual_p)
})


test_that("multi_freqs - grouped ns and percentages", {
  test <- responses %>%
    dplyr::group_by(gender) %>%
    multi_freqs(m_activity_1)

  expected_n <- responses %>% dplyr::filter(gender == 'other') %>%
    dplyr::count(m_activity_10) %>%
    dplyr::filter(m_activity_10 == 1) %>%
    dplyr::pull(n)
  total_n <- responses %>%
    dplyr::filter(gender == 'other') %>%
    dplyr::filter(
      !is.na(m_activity_1) |
        !is.na(m_activity_2) |
        !is.na(m_activity_3) |
        !is.na(m_activity_10) |
        !is.na(m_activity_21)
    ) %>% nrow()
  actual_n <- test %>%
    dplyr::filter(group_var == 'other' & label == 'Baseball') %>%
    dplyr::pull(n)
  actual_p <- test %>%
    dplyr::filter(group_var == 'other' & label == 'Baseball') %>%
    dplyr::pull(result)

  expect_equal(expected_n, actual_n)
  expect_equal(round(expected_n / total_n, 2), actual_p)
})



# Individual arguments ----------------------------------------------------


test_that("multi_freqs - remove_nas argument", {
  test_false <- responses %>% multi_freqs(m_activity_1, remove_nas = FALSE)
  test_true <- responses %>% multi_freqs(m_activity_1, remove_nas = TRUE)

  expect_equal(nrow(test_false), 12)
  expect_equal(nrow(test_true), 6)
})


test_that("multi_freqs - wt argument", {
  test <- responses %>% multi_freqs(m_activity_1, wt = weights)

  expected_n <- responses %>%
    dplyr::mutate(
      weighted_n = m_activity_3 * weights,
      sum_weighted_n = sum(weighted_n, na.rm = TRUE)
    ) %>%
    dplyr::distinct(sum_weighted_n) %>%
    dplyr::pull(sum_weighted_n)
  total_n <- responses %>%
    dplyr::filter(
      !is.na(m_activity_1) |
        !is.na(m_activity_2) |
        !is.na(m_activity_3) |
        !is.na(m_activity_10) |
        !is.na(m_activity_21)
    ) %>%
    dplyr::mutate(sum_weights = sum(weights)) %>%
    dplyr::distinct(sum_weights) %>%
    dplyr::pull(sum_weights)
  actual_n <- test %>%
    dplyr::filter(label == 'Volleyball') %>%
    dplyr::pull(n)
  actual_p <- test %>%
    dplyr::filter(label == 'Volleyball') %>%
    dplyr::pull(result)

  expect_equal(expected_n, actual_n)
  expect_equal(round(expected_n / total_n, 2), actual_p)
})


test_that("multi_freqs - prompt argument", {
  test <- responses %>% multi_freqs(m_activity_1, prompt = TRUE)
  test_names <- test %>% names()

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
  test_3 <- responses %>% multi_freqs(m_activity_1, digits = 3) %>%
    dplyr::filter(label == 'Baseball') %>%
    dplyr::pull(result)
  test_2 <- responses %>% multi_freqs(m_activity_1) %>%
    dplyr::filter(label == 'Baseball') %>%
    dplyr::pull(result)
  test_1 <- responses %>% multi_freqs(m_activity_1, digits = 1) %>%
    dplyr::filter(label == 'Baseball') %>%
    dplyr::pull(result)

  expect_equal(nchar(test_3), 5)
  expect_equal(nchar(test_2), 4)
  expect_equal(nchar(test_1), 3) # 3 characters because it includes the '0.' as 2 characters
})


test_that("multi_freqs - nas_group argument", {
  test <- responses %>%
    dplyr::group_by(gender) %>%
    multi_freqs(
      m_activity_1,
      nas_group = FALSE
      )

  # NAs showing up on the unchosen activity

  expect_equal(length(unique(test$group_var)), 3)
})


test_that("multi_freqs - factor_group argument", {
  test_factor_true <- responses %>%
    dplyr::group_by(gender_labelled) %>%
    multi_freqs(
      m_activity_1,
      factor_group = TRUE
    )
  test_factor_false <- responses %>%
    dplyr::group_by(gender_labelled) %>%
    multi_freqs(
      m_activity_1,
      factor_group = FALSE
    )

  expect_equal(stringr::str_detect(test_factor_true$group_var, 'male')[1], TRUE)
  expect_equal(stringr::str_detect(test_factor_false$group_var, 'male')[1], FALSE)
})


test_that("multi_freqs - unweighted_ns argument", {
  test_n_standard <- responses %>%
    multi_freqs(m_activity_1) %>%
    dplyr::select(n)
  test_result_weighted <- responses %>%
    multi_freqs(m_activity_1, wt = weights) %>%
    dplyr::select(result)
  test_n_unweighted_ns <- responses %>%
    multi_freqs(m_activity_1, wt = weights, unweighted_ns = TRUE) %>%
    dplyr::select(n)
  test_result_unweighted_ns <- responses %>%
    multi_freqs(m_activity_1, wt = weights, unweighted_ns = TRUE) %>%
    dplyr::select(result)


  expect_equal(test_n_standard, test_n_unweighted_ns)
  expect_equal(test_result_weighted, test_result_unweighted_ns)
})


test_that("multi_freqs - show_missing_levels argument", {
  test_no_missing_levels <- responses %>%
    multi_freqs(
      m_activity_1,
      show_missing_levels = FALSE
      )
  test_yes_missing_levels <- responses %>%
    multi_freqs(
      m_activity_1,
      show_missing_levels = TRUE
    )
  sum_no_missing <-
    stringr::str_detect(test_no_missing_levels$label, 'An unchosen activity') %>%
    sum()
  sum_yes_missing <-
    stringr::str_detect(test_yes_missing_levels$label, 'An unchosen activity') %>%
    sum()

  expect_equal(sum_no_missing, 0)
  expect_equal(sum_yes_missing, 1)
})


test_that("multi_freqs - show_missing_levels argument, grouped", {
  no_missing <- responses %>%
    dplyr::group_by(gender) %>%
    multi_freqs(
      m_activity_1,
      nas_group = FALSE,
      show_missing_levels = FALSE
    )
  yes_missing <- responses %>%
    dplyr::group_by(gender) %>%
    multi_freqs(
      m_activity_1,
      nas_group = FALSE,
      show_missing_levels = TRUE
    )
  yes_missing_with_nas_group <- responses %>%
    dplyr::group_by(gender) %>%
    multi_freqs(
      m_activity_1,
      show_missing_levels = TRUE
    )
  sum_no_missing <-
    stringr::str_detect(no_missing$label, 'An unchosen activity') %>%
    sum()
  sum_yes_missing <-
    stringr::str_detect(yes_missing$label, 'An unchosen activity') %>%
    sum()
  sum_yes_missing_with_nas_group <-
    stringr::str_detect(yes_missing_with_nas_group$label, 'An unchosen activity') %>%
    sum()
  expect_equal(sum_no_missing, 0)
  expect_equal(sum_yes_missing, 3)
  expect_equal(sum_yes_missing_with_nas_group, 4)
})
