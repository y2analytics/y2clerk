
### Column names
test_that("Column names", {
  GROUP_VARS1 <- 'am'

    frequencies <- mtcars %>%
    cross_freqs(
      group_vars = GROUP_VARS1,
      gear
    )
  frequencies
  names_actual <- frequencies  %>% names()
  names_expected <- c(
    'group_var_name',
    'group_var',
    'variable',
    'value',
    'label',
    'n',
    'stat',
    'result'
    )
  expect_equal(names_actual, names_expected)
})



### Multiple freqs vars
test_that("Multiple freqs vars", {
  GROUP_VARS1 <- 'am'

  frequencies <- mtcars %>%
    cross_freqs(
      group_vars = GROUP_VARS1,
      gear,
      vs
    )
  label_levels <- unique(frequencies$label)
  variable_levels <- unique(frequencies$variable)
  expect_equal(label_levels, c("3", "4", "5", "0", "1"))
  expect_equal(variable_levels, c("gear", "vs"))
})



### Multiple grouping vars
test_that("Multiple grouping vars", {
  GROUP_VARS2 <- mtcars %>% dplyr::select(am, vs) %>% names()

  frequencies <- mtcars %>%
    cross_freqs(
      group_vars = GROUP_VARS2,
      gear
    )
  grouping_vars <- unique(frequencies$group_var_name)
  grouping_levels <- unique(frequencies$group_var) %>% as.character()
  expect_equal(grouping_vars, c("am", "vs"))
  expect_equal(grouping_levels, c("0", "1"))
})



### wide = TRUE
test_that("wide = TRUE", {
  GROUP_VARS2 <- mtcars %>% dplyr::select(am, vs) %>% names()

  frequencies <- mtcars %>%
    cross_freqs(
      group_vars = GROUP_VARS2,
      gear,
      wide = TRUE
    )
  grouping_vars <- unique(frequencies$group_var_name)
  table1 <- frequencies[[2]][[1]]
  table2 <- frequencies[[2]][[2]]

  expect_equal(class(frequencies$results), "list") # is nested
  expect_equal(grouping_vars, GROUP_VARS2) # id columns contain all group_vars
  expect_equal(length(table1$variable), 3) # only am or vs

})



### Mixed class group vars
test_that("Mixed class group vars", {
  GROUP_VARS2 <- mtcars %>% dplyr::select(am, vs) %>% names()

  # Long
  frequencies <- mtcars %>%
    dplyr::mutate(am = forcats::as_factor(am)) %>%
    cross_freqs(
      group_vars = GROUP_VARS2,
      gear,
      carb
    )
  grouping_vars <- unique(frequencies$group_var_name)
  grouping_levels <- unique(frequencies$group_var) %>% as.character()
  expect_equal(grouping_vars, c("am", "vs"))
  expect_equal(grouping_levels, c("0", "1"))

  # Wide
  frequencies <- mtcars %>%
    dplyr::mutate(am = forcats::as_factor(am)) %>%
    cross_freqs(
      group_vars = GROUP_VARS2,
      gear,
      carb,
      wide = TRUE
    )
  grouping_vars <- unique(frequencies$group_var_name)
  expect_equal(class(frequencies$results), "list")
  expect_equal(grouping_vars, GROUP_VARS2)
})



### Error messages
test_that("Error messages", {

  expect_error(
    frequencies <- mtcars %>%
      cross_freqs(
        group_vars = dplyr::quos(am, vs),
        vs
      ),
    'group_vars should be a character vector of variable names. Try formatting like c("var1", "var2") instead of c(var1, var2) or quos(var1, var2)',
    fixed = TRUE
  )

  expect_error(
    frequencies <- mtcars %>%
      cross_freqs(
        group_vars = am,
        vs
      ),
    'group_vars should be a character vector of variable names. Try formatting like c("var1", "var2") instead of c(var1, var2) or quos(var1, var2)',
    fixed = TRUE
  )
})


### exclude_groups
test_that("exclude_groups, select() method", {
  GROUP_VARS2 <- mtcars %>% dplyr::select(am, vs) %>% names()

  # Long
  frequencies <- mtcars %>%
    dplyr::select(carb, gear, am, vs) %>%
    cross_freqs(
      group_vars = GROUP_VARS2, #am & vs
      exclude_groups = TRUE
    )
  grouping_vars <- unique(frequencies$variable)
  expect_equal(grouping_vars, c("carb", "gear"))

  # Wide
  frequencies <- mtcars %>%
    dplyr::select(carb, gear, am, vs) %>%
    cross_freqs(
      group_vars = GROUP_VARS2, #am & vs
      wide = TRUE,
      exclude_groups = TRUE
    )
  grouping_vars_am <- unique(frequencies[[2]][[1]]$variable)
  grouping_vars_vs <- unique(frequencies[[2]][[2]]$variable)
  expect_equal(grouping_vars_am, c("carb", "gear"))
  expect_equal(grouping_vars_vs, c("carb", "gear"))
})


### include_overall
test_that("include_overall", {
  GROUP_VARS2 <- mtcars %>% dplyr::select(am, vs) %>% names()

  # Long
  frequencies <- mtcars %>%
    dplyr::select(carb, gear, am, vs) %>%
    cross_freqs(
      group_vars = GROUP_VARS2, #am & vs
      exclude_groups = TRUE,
      include_overall = TRUE
    )
  grouping_vars <- unique(frequencies$group_var_name)
  group_sums <- frequencies %>%
    dplyr::group_by(group_var_name) %>%
    dplyr::mutate(sums = sum(n)) %>%
    dplyr::distinct(group_var_name, .keep_all = TRUE)
  expect_equal(grouping_vars, c("Overall", "am", "vs"))
  expect_equal(length(grouping_vars), 3)
  expect_equal(
    group_sums$sums[1],
    group_sums$sums[2],
    group_sums$sums[3]
  )

  # Wide
  frequencies <- mtcars %>%
    dplyr::select(carb, gear, am, vs) %>%
    cross_freqs(
      group_vars = GROUP_VARS2, #am & vs
      wide = TRUE,
      exclude_groups = TRUE,
      include_overall = TRUE
    )
  expect_equal(nrow(frequencies), 3)
  expect_equal(frequencies[[1]][1], 'Overall')
})

