# variable with no label - give a warning

    Code
      verbatims_y2(df_nolab, var1)
    Condition
      Warning in `verbatims_y2()`:
      You are working with variables that have no labeling. You may want to consider adding a prompt before continuing
    Output
      # A tibble: 6 x 4
        variable prompt   label                                       base_ns
        <chr>    <chr>    <chr>                                         <int>
      1 var1     No label I like to talk about dogs                         6
      2 var1     No label Dogs are cool but cats are aight too              6
      3 var1     No label I prefer dogs over cats                           6
      4 var1     No label My dog's collars are always too tight             6
      5 var1     No label One last sentence about dogs                      6
      6 var1     No label Cats collars are typically cooler than dogs       6

