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
      dplyr::mutate(responses4, q4_haven = to_haven_y2(q4), q5_haven = to_haven_y2(q5))
    Output
      # A tibble: 25 x 10
            q0    q1 q2    q3           q4       q5      q6        w q4_haven q5_haven
         <dbl> <dbl> <ord> <chr>        <int+lb> <chr+l> <fct> <dbl> <int+lb> <chr+lb>
       1  45.2  NA   4     banana       7 [50-1~ b [Som~ apri~ 1.02  7 [50-1~ b [Some~
       2  63.1  45.2 1     banana       4 [5-10~ b [Som~ apple 1.03  4 [5-10~ b [Some~
       3  71.2  17.6 1     bilberry     2 [1-2 ~ b [Som~ boys~ 0.939 2 [1-2 ~ b [Some~
       4  60.7  75.9 2     blueberry    5 [10-2~ c [Som~ apple 0.857 5 [10-2~ c [Some~
       5  18.7  38.4 3     avocado      7 [50-1~ c [Som~ eggp~ 0.967 7 [50-1~ c [Some~
       6  50.9  58.1 2     apple        6 [20-5~ a [Ver~ bana~ 1.01  6 [20-5~ a [Very~
       7  55.1  73   2     ugli fruit   8 [More~ d [Ver~ pome~ 1.10  8 [More~ d [Very~
       8  35.3  84.6 4     pomelo       7 [50-1~ b [Som~ nut   0.974 7 [50-1~ b [Some~
       9  36.5  37.6 1     blood orange 1 [Less~ b [Som~ goos~ 0.970 1 [Less~ b [Some~
      10  35.3  39.7 4     apple        4 [5-10~ b [Som~ avoc~ 1.16  4 [5-10~ b [Some~
      # i 15 more rows

