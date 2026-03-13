# Overall ----------------------------------------------------------------

test_that('`sig` Column Created', {
  
  frequencies <- responses3 %>% 
    dplyr::group_by(group) %>% 
    freqs(V1) %>% 
    sig_test_y2(
      responses3,
      group
    )
  
  names_actual <- frequencies %>% 
    names()
  
  names_expected <- c(
    'group_var',
    'variable',
    'value',
    'label',
    'n',
    'stat',
    'result',
    'sig'
  )
  
  expect_equal(names_actual, names_expected)
  
})


test_that('Sig Tests Results Correctly Ordered', {
  
  frequencies <- responses3 %>% 
    dplyr::group_by(group) %>% 
    freqs(V1) %>% 
    sig_test_y2(
      responses3,
      group
    )
  
  sig_actual <- frequencies %>% 
    dplyr::pull(sig)
  
  sig_expected <- c(
    'C',
    'B',
    'B',
    'AC',
    '',
    '',
    '',
    'B',
    'B'
  )
  
  expect_equal(sig_actual, sig_expected)
  
})


test_that('Sig Test works on a grouped freqs object of multiple variables', {
  
  mod_df <- responses3 %>% 
    dplyr::mutate(
      V2 = c(
        rep('Agree', 100),
        rep('Neither', 100),
        rep('Disagree', 100),
        rep('Agree', 280),
        rep('Neither', 10),
        rep('Disagree', 10),
        rep('Agree', 60),
        rep('Neither', 60),
        rep('Disagree', 180)
      )
    ) %>% 
    dplyr::mutate(
      V2 = forcats::fct_relevel(
        V2,
        'Agree',
        'Neither',
        'Disagree'
      )
    )
  
  frequencies <- mod_df %>% 
    dplyr::group_by(group) %>% 
    freqs(V1,
          V2) %>% 
    sig_test_y2(
      mod_df,
      group
    )
  
  sig_actual <- frequencies %>% 
    dplyr::pull(sig)
  
  sig_expected <- c(
    'C',
    'B',
    'B',
    'AC',
    '',
    '',
    '',
    'B',
    'B',
    'C',
    'BC',
    'B',
    'AC',
    '',
    '',
    '',
    'B',
    'AB'
  )
  
  expect_equal(sig_actual, sig_expected)
  
})


test_that('Sig Test works on a grouped freqs object of multi select variables', {
  
  frequencies <- responses3 %>% 
    dplyr::group_by(group) %>% 
    multi_freqs(V2_1) %>% 
    sig_test_y2(
      responses3,
      group
    )
  
  sig_actual <- frequencies %>% 
    dplyr::pull(sig)
  
  sig_expected <- c(
    '',
    'A',
    'AB',
    'B',
    '',
    'AB',
    '',
    '',
    ''
  )
  
  expect_equal(sig_actual, sig_expected)
  
})


test_that('Group References Appended Correctly', {
  
  frequencies <- responses3 %>% 
    dplyr::group_by(group) %>% 
    freqs(V1) %>% 
    sig_test_y2(
      responses3,
      group
    )
  
  groups_actual <- frequencies %>% 
    dplyr::count(group_var) %>% 
    dplyr::pull(group_var) %>% 
    as.character()
  
  groups_expected <- c(
    'Group 1 [A]',
    'Group 2 [B]',
    'Group 3 [C]'
  )
  
  expect_equal(groups_actual, groups_expected)
  
})


test_that('Works on Numeric freqs var', {
  
  mod_df <- responses3 %>% 
    dplyr::mutate(
      V1 = dplyr::case_when(
        V1 == 'Agree' ~ 1,
        V1 == 'Neither' ~ 0,
        V1 == 'Disagree' ~ -1
      )
    )
  
  expect_snapshot(
    mod_df %>% 
      dplyr::group_by(group) %>% 
      freqs(V1) %>% 
      sig_test_y2(
        mod_df,
        group
      )
    
  )
  
})


test_that('Works on Character freqs var', {
  
  mod_df <- responses3 %>% 
    dplyr::mutate(
      V1 = as.character(V1)
    )
  
  expect_snapshot(
    
    mod_df %>% 
      dplyr::group_by(group) %>% 
      freqs(V1) %>% 
      sig_test_y2(
        mod_df,
        group
      )
    
  )
  
})


test_that('Works on Factor freqs var', {
  
  expect_snapshot(
    
    responses3 %>% 
      dplyr::group_by(group) %>% 
      freqs(V1) %>% 
      sig_test_y2(
        responses3,
        group
      )
    
  )
  
})


test_that('Works on labelled freqs var', {
  
  mod_df <- responses3 %>% 
    dplyr::mutate(
      V1 = dplyr::case_when(
        V1 == 'Agree' ~ 1,
        V1 == 'Disagree' ~ 2,
        V1 == 'Neither' ~ 3
      ),
      V1 = labelled::labelled(
        V1,
        labels = c(
          'Agree' = 1,
          'Disagree' = 2,
          'Neither' = 3
        )
      )
    )
  
  expect_snapshot(
    
    responses3 %>% 
      dplyr::group_by(group) %>% 
      freqs(V1) %>% 
      sig_test_y2(
        mod_df,
        group
      )
    
  )
  
})


test_that('Works on numeric group_var', {
  
  mod_df <- responses3 %>% 
    dplyr::mutate(
      group = dplyr::case_when(
        group == 'Group 1' ~ 1,
        group == 'Group 2' ~ 2,
        group == 'Group 3' ~ 3
      )
    )
  
  expect_snapshot(
    
    mod_df %>% 
      dplyr::group_by(group) %>% 
      freqs(V1) %>% 
      sig_test_y2(
        mod_df,
        group
      )
    
  )
  
})


test_that('Works on character group_var', {
  
  expect_snapshot(
    
    responses3 %>% 
      dplyr::group_by(group) %>% 
      freqs(V1) %>% 
      sig_test_y2(
        responses3,
        group
      )
    
  )
  
})


test_that('Works on factor group_var', {
  
  mod_df <- responses3 %>% 
    dplyr::mutate(
      group = haven::as_factor(group)
    )
  
  expect_snapshot(
    mod_df %>% 
      dplyr::group_by(group) %>% 
      freqs(V1) %>% 
      sig_test_y2(
        responses3,
        group
      )
    
  )
  
})


# Errors and Warnings ----------------------------------------------------


test_that('Haven labelled group_var error', {
  
  mod_df <- responses3 %>% 
    dplyr::mutate(
      group = dplyr::case_when(
        group == 'Group 1' ~ 1,
        group == 'Group 2' ~ 2,
        group == 'Group 3' ~ 3
      ),
      group = labelled::labelled(
        group,
        labels = c(
          'Group 1' = 1,
          'Group 2' = 2,
          'Group 3' = 3
        )
      )
    )
  
  expect_snapshot(error = TRUE, 
    frequencies <- mod_df %>% 
      dplyr::group_by(group) %>% 
      freqs(V1) %>% 
      sig_test_y2(
        mod_df,
        group
      )
  )
  
})


test_that('Missing dataset', {
  
  expect_snapshot(error = TRUE, 
    frequencies <- responses3 %>% 
      dplyr::group_by(group) %>% 
      freqs(V1) %>% 
      sig_test_y2(
        banner_var = group
      )
  )
  
})


test_that('Missing banner_var', {
  
  expect_snapshot(error = TRUE, 
    frequencies <- responses3 %>% 
      dplyr::group_by(group) %>% 
      freqs(V1) %>% 
      sig_test_y2(
        dataset = responses3
      )
  )
  
})

### End ------------------------------------------------------------------
