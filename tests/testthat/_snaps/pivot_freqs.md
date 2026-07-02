# pivot_freqs works with Column names

    Code
      pivot_freqs(freqs(dplyr::group_by(forcats::gss_cat, year), marital))
    Output
      # A tibble: 8 x 7
      # Groups:   group_var [8]
        group_var `No answer` `Never married` Separated Divorced Widowed Married
            <int>       <dbl>           <dbl>     <dbl>    <dbl>   <dbl>   <dbl>
      1      2000           0            0.25      0.04     0.16    0.1     0.45
      2      2002           0            0.26      0.03     0.16    0.09    0.46
      3      2004           0            0.22      0.03     0.15    0.07    0.53
      4      2006           0            0.24      0.03     0.16    0.08    0.48
      5      2008           0            0.26      0.03     0.14    0.08    0.48
      6      2010           0            0.28      0.03     0.17    0.09    0.44
      7      2012           0            0.27      0.03     0.16    0.08    0.46
      8      2014           0            0.27      0.03     0.16    0.08    0.46

# pivot_freqs works with Column names with two group vars

    Code
      pivot_freqs(freqs(dplyr::group_by(forcats::gss_cat, year), race))
    Output
      # A tibble: 8 x 5
      # Groups:   group_var [8]
        group_var Other Black White `Not applicable`
            <int> <dbl> <dbl> <dbl>            <dbl>
      1      2000  0.06  0.15  0.79                0
      2      2002  0.06  0.15  0.79                0
      3      2004  0.07  0.13  0.79                0
      4      2006  0.13  0.14  0.73                0
      5      2008  0.09  0.14  0.77                0
      6      2010  0.09  0.15  0.76                0
      7      2012  0.1   0.15  0.75                0
      8      2014  0.1   0.15  0.74                0

# pivot_freqs can pivot on group_var

    Code
      pivot_freqs(freqs(dplyr::group_by(forcats::gss_cat, year), marital), group_var)
    Output
      # A tibble: 6 x 9
        label         `2000` `2002` `2004` `2006` `2008` `2010` `2012` `2014`
        <chr>          <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
      1 No answer       0      0      0      0      0      0      0      0   
      2 Never married   0.25   0.26   0.22   0.24   0.26   0.28   0.27   0.27
      3 Separated       0.04   0.03   0.03   0.03   0.03   0.03   0.03   0.03
      4 Divorced        0.16   0.16   0.15   0.16   0.14   0.17   0.16   0.16
      5 Widowed         0.1    0.09   0.07   0.08   0.08   0.09   0.08   0.08
      6 Married         0.45   0.46   0.53   0.48   0.48   0.44   0.46   0.46

# pivot_freqs errors on blank label column

    Code
      pivot_freqs(freqs(forcats::gss_cat, age, stat = "mean", nas = FALSE))
    Condition
      Error in `pivot_freqs()`:
      x Your frequencies label column is blank. Please provide unique labels on which to pivot.

# pivot_freqs errors on missing group_var

    Code
      pivot_freqs(freqs(forcats::gss_cat, marital))
    Condition
      Error in `pivot_errors()`:
      x Your frequencies does not contain a `group_var`.
      i Supply a `group_var` to pivot correctly.

# pivot_freqs errors on missing label or result column

    Code
      pivot_freqs(forcats::gss_cat)
    Condition
      Error in `pivot_freqs()`:
      x Input data must contain a `label` column.
      i Ensure you are passing the output from a `freqs()` call.

# pivot_freqs: multi-select stem with unique labels pivots on label

    Code
      pivot_freqs(frequencies)
    Output
      # A tibble: 4 x 7
      # Groups:   group_var [4]
        group_var Basketball Football Volleyball Baseball `Underwater Basket Weaving`
        <chr>          <dbl>    <dbl>      <dbl>    <dbl>                       <dbl>
      1 male            0.14     0.43       0.14     0.71                        0.86
      2 female          0        0.55       0.27     0.27                        1   
      3 other           0        0          0        0.75                        1   
      4 <NA>            0        0          0        0                           1   
      # i 1 more variable: `An unchosen activity` <dbl>

# pivot_freqs: multi-select stem with unique labels pivots on group_var

    Code
      pivot_freqs(frequencies, group_var)
    Output
      # A tibble: 6 x 5
        label                      male female other  `NA`
        <chr>                     <dbl>  <dbl> <dbl> <dbl>
      1 Basketball                 0.14   0     0        0
      2 Football                   0.43   0.55  0        0
      3 Volleyball                 0.14   0.27  0        0
      4 Baseball                   0.71   0.27  0.75     0
      5 Underwater Basket Weaving  0.86   1     1        1
      6 An unchosen activity       0      0     0        0

# pivot_freqs: multi-select stems with colliding labels get variable-prefixed column names

    Code
      pivot_freqs(frequencies)
    Output
      # A tibble: 2 x 9
      # Groups:   group_var [2]
        group_var q_festivals_1_No q_festivals_1_Yes q_festivals_2_No
        <chr>                <dbl>             <dbl>            <dbl>
      1 Group 1                0.5               0.5             0.67
      2 Group 2                0.5               0.5             0.33
      # i 5 more variables: q_festivals_2_Yes <dbl>, q_parades_1_No <dbl>,
      #   q_parades_1_Yes <dbl>, q_parades_2_No <dbl>, q_parades_2_Yes <dbl>

# pivot_freqs: multi-select stems with colliding labels keep variable as id column on group_var pivot

    Code
      pivot_freqs(frequencies, group_var)
    Output
      # A tibble: 8 x 4
        variable      label `Group 1` `Group 2`
        <chr>         <chr>     <dbl>     <dbl>
      1 q_festivals_1 No         0.5       0.5 
      2 q_festivals_1 Yes        0.5       0.5 
      3 q_festivals_2 No         0.67      0.33
      4 q_festivals_2 Yes        0.33      0.67
      5 q_parades_1   No         0.33      0.5 
      6 q_parades_1   Yes        0.67      0.5 
      7 q_parades_2   No         0.5       0.33
      8 q_parades_2   Yes        0.5       0.67

