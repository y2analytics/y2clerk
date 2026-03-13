# Works on Numeric freqs var

    Code
      mod_df %>% dplyr::group_by(group) %>% freqs(V1) %>% sig_test_y2(mod_df, group)
    Message
      Adding missing grouping variables: `group`
      Adding grouped pairwise significance tests for response "-1" for group_var "group"
      Adding grouped pairwise significance tests for response "0" for group_var "group"
      Adding grouped pairwise significance tests for response "1" for group_var "group"
    Output
      # A tibble: 9 x 8
      # Groups:   group_var [3]
        group_var   variable value label     n stat    result sig  
        <fct>       <chr>    <chr> <chr> <int> <chr>    <dbl> <chr>
      1 Group 1 [A] V1       -1    -1      100 percent   0.33 "B"  
      2 Group 1 [A] V1       0     0       100 percent   0.33 "B"  
      3 Group 1 [A] V1       1     1       100 percent   0.33 "C"  
      4 Group 2 [B] V1       -1    -1       50 percent   0.17 ""   
      5 Group 2 [B] V1       0     0        50 percent   0.17 ""   
      6 Group 2 [B] V1       1     1       200 percent   0.67 "AC" 
      7 Group 3 [C] V1       -1    -1      125 percent   0.42 "B"  
      8 Group 3 [C] V1       0     0       125 percent   0.42 "B"  
      9 Group 3 [C] V1       1     1        50 percent   0.17 ""   

# Works on Character freqs var

    Code
      mod_df %>% dplyr::group_by(group) %>% freqs(V1) %>% sig_test_y2(mod_df, group)
    Message
      Adding missing grouping variables: `group`
      Adding grouped pairwise significance tests for response "Agree" for group_var "group"
      Adding grouped pairwise significance tests for response "Disagree" for group_var "group"
      Adding grouped pairwise significance tests for response "Neither" for group_var "group"
    Output
      # A tibble: 9 x 8
      # Groups:   group_var [3]
        group_var   variable value    label        n stat    result sig  
        <fct>       <chr>    <chr>    <chr>    <int> <chr>    <dbl> <chr>
      1 Group 1 [A] V1       Agree    Agree      100 percent   0.33 "C"  
      2 Group 1 [A] V1       Disagree Disagree   100 percent   0.33 "B"  
      3 Group 1 [A] V1       Neither  Neither    100 percent   0.33 "B"  
      4 Group 2 [B] V1       Agree    Agree      200 percent   0.67 "AC" 
      5 Group 2 [B] V1       Disagree Disagree    50 percent   0.17 ""   
      6 Group 2 [B] V1       Neither  Neither     50 percent   0.17 ""   
      7 Group 3 [C] V1       Agree    Agree       50 percent   0.17 ""   
      8 Group 3 [C] V1       Disagree Disagree   125 percent   0.42 "B"  
      9 Group 3 [C] V1       Neither  Neither    125 percent   0.42 "B"  

# Works on Factor freqs var

    Code
      responses3 %>% dplyr::group_by(group) %>% freqs(V1) %>% sig_test_y2(responses3,
        group)
    Message
      Adding missing grouping variables: `group`
      Adding grouped pairwise significance tests for response "Agree" for group_var "group"
      Adding grouped pairwise significance tests for response "Disagree" for group_var "group"
      Adding grouped pairwise significance tests for response "Neither" for group_var "group"
    Output
      # A tibble: 9 x 8
      # Groups:   group_var [3]
        group_var   variable value label        n stat    result sig  
        <fct>       <chr>    <chr> <chr>    <int> <chr>    <dbl> <chr>
      1 Group 1 [A] V1       1     Agree      100 percent   0.33 "C"  
      2 Group 1 [A] V1       2     Neither    100 percent   0.33 "B"  
      3 Group 1 [A] V1       3     Disagree   100 percent   0.33 "B"  
      4 Group 2 [B] V1       1     Agree      200 percent   0.67 "AC" 
      5 Group 2 [B] V1       2     Neither     50 percent   0.17 ""   
      6 Group 2 [B] V1       3     Disagree    50 percent   0.17 ""   
      7 Group 3 [C] V1       1     Agree       50 percent   0.17 ""   
      8 Group 3 [C] V1       2     Neither    125 percent   0.42 "B"  
      9 Group 3 [C] V1       3     Disagree   125 percent   0.42 "B"  

# Works on labelled freqs var

    Code
      responses3 %>% dplyr::group_by(group) %>% freqs(V1) %>% sig_test_y2(mod_df,
        group)
    Message
      Adding missing grouping variables: `group`
      Adding grouped pairwise significance tests for response "Agree" for group_var "group"
      Adding grouped pairwise significance tests for response "Neither" for group_var "group"
      Adding grouped pairwise significance tests for response "Disagree" for group_var "group"
    Output
      # A tibble: 9 x 8
      # Groups:   group_var [3]
        group_var   variable value label        n stat    result sig  
        <fct>       <chr>    <chr> <chr>    <int> <chr>    <dbl> <chr>
      1 Group 1 [A] V1       1     Agree      100 percent   0.33 "C"  
      2 Group 1 [A] V1       2     Neither    100 percent   0.33 "B"  
      3 Group 1 [A] V1       3     Disagree   100 percent   0.33 "B"  
      4 Group 2 [B] V1       1     Agree      200 percent   0.67 "AC" 
      5 Group 2 [B] V1       2     Neither     50 percent   0.17 ""   
      6 Group 2 [B] V1       3     Disagree    50 percent   0.17 ""   
      7 Group 3 [C] V1       1     Agree       50 percent   0.17 ""   
      8 Group 3 [C] V1       2     Neither    125 percent   0.42 "B"  
      9 Group 3 [C] V1       3     Disagree   125 percent   0.42 "B"  

# Works on numeric group_var

    Code
      mod_df %>% dplyr::group_by(group) %>% freqs(V1) %>% sig_test_y2(mod_df, group)
    Message
      Adding missing grouping variables: `group`
      Adding grouped pairwise significance tests for response "Agree" for group_var "group"
      Adding grouped pairwise significance tests for response "Disagree" for group_var "group"
      Adding grouped pairwise significance tests for response "Neither" for group_var "group"
    Output
      # A tibble: 9 x 8
      # Groups:   group_var [3]
        group_var variable value label        n stat    result sig  
        <fct>     <chr>    <chr> <chr>    <int> <chr>    <dbl> <chr>
      1 1 [A]     V1       1     Agree      100 percent   0.33 "C"  
      2 1 [A]     V1       2     Neither    100 percent   0.33 "B"  
      3 1 [A]     V1       3     Disagree   100 percent   0.33 "B"  
      4 2 [B]     V1       1     Agree      200 percent   0.67 "AC" 
      5 2 [B]     V1       2     Neither     50 percent   0.17 ""   
      6 2 [B]     V1       3     Disagree    50 percent   0.17 ""   
      7 3 [C]     V1       1     Agree       50 percent   0.17 ""   
      8 3 [C]     V1       2     Neither    125 percent   0.42 "B"  
      9 3 [C]     V1       3     Disagree   125 percent   0.42 "B"  

# Works on character group_var

    Code
      responses3 %>% dplyr::group_by(group) %>% freqs(V1) %>% sig_test_y2(responses3,
        group)
    Message
      Adding missing grouping variables: `group`
      Adding grouped pairwise significance tests for response "Agree" for group_var "group"
      Adding grouped pairwise significance tests for response "Disagree" for group_var "group"
      Adding grouped pairwise significance tests for response "Neither" for group_var "group"
    Output
      # A tibble: 9 x 8
      # Groups:   group_var [3]
        group_var   variable value label        n stat    result sig  
        <fct>       <chr>    <chr> <chr>    <int> <chr>    <dbl> <chr>
      1 Group 1 [A] V1       1     Agree      100 percent   0.33 "C"  
      2 Group 1 [A] V1       2     Neither    100 percent   0.33 "B"  
      3 Group 1 [A] V1       3     Disagree   100 percent   0.33 "B"  
      4 Group 2 [B] V1       1     Agree      200 percent   0.67 "AC" 
      5 Group 2 [B] V1       2     Neither     50 percent   0.17 ""   
      6 Group 2 [B] V1       3     Disagree    50 percent   0.17 ""   
      7 Group 3 [C] V1       1     Agree       50 percent   0.17 ""   
      8 Group 3 [C] V1       2     Neither    125 percent   0.42 "B"  
      9 Group 3 [C] V1       3     Disagree   125 percent   0.42 "B"  

# Works on factor group_var

    Code
      mod_df %>% dplyr::group_by(group) %>% freqs(V1) %>% sig_test_y2(responses3,
        group)
    Message
      Adding missing grouping variables: `group`
      Adding grouped pairwise significance tests for response "Agree" for group_var "group"
      Adding grouped pairwise significance tests for response "Disagree" for group_var "group"
      Adding grouped pairwise significance tests for response "Neither" for group_var "group"
    Output
      # A tibble: 9 x 8
      # Groups:   group_var [3]
        group_var   variable value label        n stat    result sig  
        <fct>       <chr>    <chr> <chr>    <int> <chr>    <dbl> <chr>
      1 Group 1 [A] V1       1     Agree      100 percent   0.33 "C"  
      2 Group 1 [A] V1       2     Neither    100 percent   0.33 "B"  
      3 Group 1 [A] V1       3     Disagree   100 percent   0.33 "B"  
      4 Group 2 [B] V1       1     Agree      200 percent   0.67 "AC" 
      5 Group 2 [B] V1       2     Neither     50 percent   0.17 ""   
      6 Group 2 [B] V1       3     Disagree    50 percent   0.17 ""   
      7 Group 3 [C] V1       1     Agree       50 percent   0.17 ""   
      8 Group 3 [C] V1       2     Neither    125 percent   0.42 "B"  
      9 Group 3 [C] V1       3     Disagree   125 percent   0.42 "B"  

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

