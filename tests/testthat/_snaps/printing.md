# freqs() prints a basic frequency tibble with the haven label data up top when applicable

    Code
      print(with_label)
    Output
      # q1: % of males involved in agriculture
      # 
      # A frequency tibble: 20 x 6
         variable value label     n stat    result
         <chr>    <chr> <chr> <int> <chr>    <dbl>
       1 q1       17.6  17.6      1 percent   0.04
       2 q1       19.4  19.4      1 percent   0.04
       3 q1       26.8  26.8      1 percent   0.04
       4 q1       27.7  27.7      1 percent   0.04
       5 q1       35.3  35.3      1 percent   0.04
       6 q1       37.6  37.6      1 percent   0.04
       7 q1       38.4  38.4      1 percent   0.04
       8 q1       39.7  39.7      2 percent   0.08
       9 q1       43.5  43.5      1 percent   0.04
      10 q1       45.2  45.2      2 percent   0.08
      11 q1       49.5  49.5      1 percent   0.04
      12 q1       53.3  53.3      1 percent   0.04
      13 q1       58.1  58.1      2 percent   0.08
      14 q1       70.2  70.2      1 percent   0.04
      15 q1       71.2  71.2      1 percent   0.04
      16 q1       73    73        1 percent   0.04
      17 q1       75.9  75.9      1 percent   0.04
      18 q1       84.6  84.6      1 percent   0.04
      19 q1       84.9  84.9      1 percent   0.04
      20 q1       <NA>  <NA>      3 percent   0.12

---

    Code
      print(without_label)
    Output
      # A frequency tibble: 4 x 6
        variable value label     n stat    result
        <chr>    <chr> <chr> <int> <chr>    <dbl>
      1 q5       a     a         8 percent   0.32
      2 q5       b     b         9 percent   0.36
      3 q5       c     c         4 percent   0.16
      4 q5       d     d         4 percent   0.16

# freqs() print shows truncation footer for long output

    Code
      print(test_freq)
    Output
      # A frequency tibble: 22 x 6
         variable value label     n stat    result
         <chr>    <chr> <chr> <int> <chr>    <dbl>
       1 hp       52    52        1 percent   0.03
       2 hp       62    62        1 percent   0.03
       3 hp       65    65        1 percent   0.03
       4 hp       66    66        2 percent   0.06
       5 hp       91    91        1 percent   0.03
       6 hp       93    93        1 percent   0.03
       7 hp       95    95        1 percent   0.03
       8 hp       97    97        1 percent   0.03
       9 hp       105   105       1 percent   0.03
      10 hp       109   109       1 percent   0.03
      # i 12 more rows

# freqs() print shows Groups in header when grouped

    Code
      print(test_freq)
    Output
      # A frequency tibble: 8 x 7
      # Groups:             group_var [3]
        group_var variable value label     n stat    result
            <dbl> <chr>    <chr> <chr> <int> <chr>    <dbl>
      1         4 gear     3     3         1 percent   0.09
      2         4 gear     4     4         8 percent   0.73
      3         4 gear     5     5         2 percent   0.18
      4         6 gear     3     3         2 percent   0.29
      5         6 gear     4     4         4 percent   0.57
      6         6 gear     5     5         1 percent   0.14
      7         8 gear     3     3        12 percent   0.86
      8         8 gear     5     5         2 percent   0.14

