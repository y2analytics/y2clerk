# Haven labelled group_var error

    Code
      frequencies <- mod_df %>% dplyr::group_by(group) %>% freqs(V1) %>% sig_test_y2(
        mod_df, group)
    Message
      Adding missing grouping variables: `group`
    Condition
      Error in `sig_test_y2()`:
      ! Banner variable "group" is a labelled double; please set "factor_group" equal to TRUE in freqs() for this variable

# Missing dataset

    Code
      frequencies <- responses3 %>% dplyr::group_by(group) %>% freqs(V1) %>%
        sig_test_y2(banner_var = group)
    Condition
      Error in `sig_test_y2()`:
      ! argument "dataset" is missing, with no default

# Missing banner_var

    Code
      frequencies <- responses3 %>% dplyr::group_by(group) %>% freqs(V1) %>%
        sig_test_y2(dataset = responses3)
    Condition
      Error in `sig_test_y2()`:
      ! argument "banner_var" is missing, with no default

