
# Data --------------------------------------------------------------------


# Tests -------------------------------------------------------------

test_that("to_haven_y2 error on haven_labelled vars", {
  expect_error(
    responses4 %>%
      dplyr::mutate(
      q4_haven = to_haven_y2(q4),
    'q4 is already a haven_labelled variable',
    fixed = TRUE
  )
  )
})


test_that("to_haven_y2 error on numeric vars", {
  expect_error(
    responses4 %>%
      dplyr::mutate(
        q0_haven = to_haven_y2(q0),
        'to_haven_y2 cannot be used on numeric variable: q0',
        fixed = TRUE
      )
  )
})


# For character and factor tests below, the tests check the following:
# 1) Is the resulting class haven_labelled with both a numeric and character?
# 2) Are the characters preserved across all instances?
# 3) Is the ordering consistent after conversion to haven_labelled?
test_that("to_haven_y2: character vars", {
  # Original
  responses4 %>% dplyr::pull(q3)
  responses4_haven <- responses4 %>%
    dplyr::mutate(q3_haven = to_haven_y2(q3))
  q3_factor <- responses4_haven %>%
    dplyr::slice(1:3) %>%
    dplyr::pull(q3_haven) %>%
    forcats::as_factor() %>%
    as.character()
  q3_num <- responses4_haven %>%
    dplyr::slice(1:3) %>%
    dplyr::pull(q3_haven) %>%
    as.numeric()
  responses4_haven %>% dplyr::select(q3, q3_haven)

  expect_equal(class(responses4_haven$q3_haven)[1], "haven_labelled")
  expect_equal(q3_num, c(1, 1, 2))
  expect_equal(q3_factor, c('banana', 'banana', 'bilberry'))
})


test_that("to_haven_y2: factor vars", {
  # Original
  responses4 %>% dplyr::pull(q6)
  responses4_haven <- responses4 %>%
    dplyr::mutate(q6_haven = to_haven_y2(q6))
  q6_factor <- responses4_haven %>%
    dplyr::slice(1:4) %>%
    dplyr::pull(q6_haven) %>%
    forcats::as_factor() %>%
    as.character()
  q6_num <- responses4_haven %>%
    dplyr::slice(1:4) %>%
    dplyr::pull(q6_haven) %>%
    as.numeric()
  responses4_haven %>% dplyr::select(q6, q6_haven)

  expect_equal(class(responses4_haven$q6_haven)[1], "haven_labelled")
  expect_equal(q6_num, c(1, 2, 3, 2))
  expect_equal(q6_factor, c('apricot', 'apple', 'boysenberry', 'apple'))
})

