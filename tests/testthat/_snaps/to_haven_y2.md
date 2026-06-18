# to_haven_y2 errors on numeric vars

    Code
      dplyr::mutate(responses4, q0_haven = to_haven_y2(q0))
    Condition
      Error in `dplyr::mutate()`:
      i In argument: `q0_haven = to_haven_y2(q0)`.
      Caused by error in `to_haven_y2()`:
      x `to_haven_y2()` cannot be used on <numeric> variable: q0

# to_haven_y2 works on haven_labelled vars

    Code
      dplyr::mutate(responses4, q4_haven = to_haven_y2(q4))
    Output
      # A tibble: 25 x 9
            q0    q1 q2    q3           q4                q5      q6        w q4_haven
         <dbl> <dbl> <ord> <chr>        <int+lbl>         <chr+l> <fct> <dbl> <int+lb>
       1  45.2  NA   4     banana       7 [50-100 years]  b [Som~ apri~ 1.02  7 [50-1~
       2  63.1  45.2 1     banana       4 [5-10 years]    b [Som~ apple 1.03  4 [5-10~
       3  71.2  17.6 1     bilberry     2 [1-2 years]     b [Som~ boys~ 0.939 2 [1-2 ~
       4  60.7  75.9 2     blueberry    5 [10-20 years]   c [Som~ apple 0.857 5 [10-2~
       5  18.7  38.4 3     avocado      7 [50-100 years]  c [Som~ eggp~ 0.967 7 [50-1~
       6  50.9  58.1 2     apple        6 [20-50 years]   a [Ver~ bana~ 1.01  6 [20-5~
       7  55.1  73   2     ugli fruit   8 [More than 100~ d [Ver~ pome~ 1.10  8 [More~
       8  35.3  84.6 4     pomelo       7 [50-100 years]  b [Som~ nut   0.974 7 [50-1~
       9  36.5  37.6 1     blood orange 1 [Less than a y~ b [Som~ goos~ 0.970 1 [Less~
      10  35.3  39.7 4     apple        4 [5-10 years]    b [Som~ avoc~ 1.16  4 [5-10~
      # i 15 more rows

