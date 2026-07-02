set.seed(100)
responses <- {
  data.frame(
    # continuous numeric, no variable label, no NA
    q0 = sample(
      x = datasets::swiss$Agriculture,
      size = 25,
      replace = TRUE
    ),
    # continuous numeric, variable label, incl. NA
    q1 = sample(
      x = c(datasets::swiss$Agriculture, NA),
      size = 25,
      prob = c(rep(.8 / 47, 47), 0.2),
      replace = TRUE
    ),
    # factor (numbers), no value labels
    q2 = sample(
      x = datasets::Orange$Tree,
      size = 25,
      replace = TRUE
    ),
    # character, no value labels
    q3 = sample(
      stringr::fruit,
      25,
      prob = 1 / (1:80 * sum(1 / (1:80))),
      replace = TRUE
    ),
    # numeric values, discrete numeric value labels
    q4 = sample(
      1:8,
      25,
      replace = TRUE
    ),
    # character values, discrete character value labels
    q5 = sample(
      letters[1:4],
      25,
      prob = c(0.4, 0.3, 0.2, 0.1),
      replace = TRUE
    ),
    # character, no value labels
    gender_labelled = c(
      rep(1, 12),
      rep(2, 12),
      rep(3, 0),
      rep(NA_real_, 1)
    ),
    # groups
    group_var1 = sample(
      c('group 1', 'group 2', NA_character_),
      25,
      prob = c(.8, .15, .05),
      replace = TRUE
    ),
    # numeric weights
    w = rnorm(25, mean = 1, sd = 0.1)
  ) %>%
    labelled::set_value_labels(
      q4 = c(
        `Less than a year` = 1,
        `1-2 years` = 2,
        `3-4 years` = 3,
        `5-10 years` = 4,
        `10-20 years` = 5,
        `20-50 years` = 6,
        `50-100 years` = 7,
        `More than 100 years` = 8
      ),
      q5 = c(
        `Very happy` = "a",
        `Somewhat happy` = "b",
        `Somewhat unhappy` = "c",
        `Very unhappy` = "d"
      ),
      gender_labelled = c(
        'male' = 1,
        'female' = 2,
        'other' = 3
      )
    ) %>%
    labelled::set_variable_labels(
      q1 = "% of males involved in agriculture",
      q2 = "Orange tree ID",
      q3 = "Preferred fruit",
      q4 = "Duration",
      q5 = "Satisfaction",
      w = "Weights",
      gender_labelled = 'gender'
    ) %>%
    dplyr::as_tibble()
}


set.seed(532987)
responses2 <- data.frame(
  # character, no value labels
  gender = c(
    rep('male', 8, ),
    rep('female', 12),
    rep('other', 4),
    rep(NA_character_, 1)
  ),
  gender_labelled = c(
    rep(1, 8, ),
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


# Small hand-built "select all that apply" style dataset: two question
# stems (q_festivals, q_parades), each with two checkbox items. Every item
# shares the same 'Yes'/'No' value labels, so running multi_freqs() across
# both stems produces a `label` column that collides across `variable`s --
# used to test pivot_freqs()'s variable+label "compound name" disambiguation
# on genuine multi-select data.
responses_multi_select <- data.frame(
  group_var = rep(c('Group 1', 'Group 2'), each = 6),
  q_festivals_1 = c(1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0),
  q_festivals_2 = c(0, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 0),
  q_parades_1 = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1),
  q_parades_2 = c(0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1)
) %>%
  labelled::set_value_labels(
    q_festivals_1 = c('No' = 0, 'Yes' = 1),
    q_festivals_2 = c('No' = 0, 'Yes' = 1),
    q_parades_1 = c('No' = 0, 'Yes' = 1),
    q_parades_2 = c('No' = 0, 'Yes' = 1)
  ) %>%
  dplyr::as_tibble()


set.seed(1)

responses3 <- data.frame(
  V1 = c(
    rep('Agree', 100),
    rep('Neither', 100),
    rep('Disagree', 100),
    rep('Agree', 200),
    rep('Neither', 50),
    rep('Disagree', 50),
    rep('Agree', 50),
    rep('Neither', 125),
    rep('Disagree', 125)
  ),
  V2_1 = c(
    rep(1, 100),
    rep(NA, 200),
    rep(1, 200),
    rep(NA, 100),
    rep(1, 250),
    rep(NA, 50)
  ),
  V2_2 = c(
    rep(1, 180),
    rep(NA, 120),
    rep(1, 100),
    rep(NA, 200),
    rep(1, 250),
    rep(NA, 50)
  ),
  V2_3 = c(
    rep(1, 200),
    rep(NA, 100),
    rep(1, 250),
    rep(NA, 50),
    rep(1, 250),
    rep(NA, 50)
  ),
  group = c(
    rep('Group 1', 300),
    rep('Group 2', 300),
    rep('Group 3', 300)
  ),
  weight = c(
    rnorm(900, 1, 0.25)
  )
) %>%
  dplyr::mutate(
    V1 = forcats::fct_relevel(
      V1,
      'Agree',
      'Neither',
      'Disagree'
    ),
    V2_1 = labelled::labelled(
      V2_1,
      labels = c(
        'Choice A' = 1
      )
    ),
    V2_2 = labelled::labelled(
      V2_2,
      labels = c(
        'Choice B' = 1
      )
    ),
    V2_3 = labelled::labelled(
      V2_3,
      labels = c(
        'Choice C' = 1
      )
    )
  )


set.seed(100)
responses4 <- {
  data.frame(
    # continuous numeric, no variable label, no NA
    q0 = sample(
      x = datasets::swiss$Agriculture,
      size = 25,
      replace = TRUE
    ),

    # continuous numeric, variable label, incl. NA
    q1 = sample(
      x = c(datasets::swiss$Agriculture, NA),
      size = 25,
      prob = c(rep(.8 / 47, 47), 0.2),
      replace = TRUE
    ),

    # factor (numbers), no value labels
    q2 = sample(
      x = datasets::Orange$Tree,
      size = 25,
      replace = TRUE
    ),

    # character, no value labels
    q3 = sample(
      stringr::fruit,
      25,
      prob = 1 / (1:80 * sum(1 / (1:80))),
      replace = TRUE
    ),

    # numeric values, discrete numeric value labels
    q4 = sample(
      1:8,
      25,
      replace = TRUE
    ),

    # character values, discrete character value labels
    q5 = sample(
      letters[1:4],
      25,
      prob = c(0.4, 0.3, 0.2, 0.1),
      replace = TRUE
    ),

    # factor (strings), no value labels
    q6 = sample(
      stringr::fruit,
      25,
      prob = 1 / (1:80 * sum(1 / (1:80))),
      replace = TRUE
    ) %>%
      forcats::as_factor(),

    # numeric weights
    w = rnorm(25, mean = 1, sd = 0.1)
  ) %>%
    labelled::set_value_labels(
      q4 = c(
        `Less than a year` = 1,
        `1-2 years` = 2,
        `3-4 years` = 3,
        `5-10 years` = 4,
        `10-20 years` = 5,
        `20-50 years` = 6,
        `50-100 years` = 7,
        `More than 100 years` = 8
      ),
      q5 = c(
        `Very happy` = "a",
        `Somewhat happy` = "b",
        `Somewhat unhappy` = "c",
        `Very unhappy` = "d"
      )
    ) %>%
    labelled::set_variable_labels(
      q1 = "% of males involved in agriculture",
      q2 = "Orange tree ID",
      q3 = "Preferred fruit",
      q4 = "Duration",
      q5 = "Satisfaction",
      q6 = 'Preferred fruit (f)',
      w = "Weights"
    ) %>%
    dplyr::as_tibble()
}
