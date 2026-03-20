# creates a data frame

    Code
      frequencies
    Output
      # A tibble: 6 x 4
        variable prompt    label                                       base_ns
        <chr>    <chr>     <chr>                                         <int>
      1 var1     My prompt I like to talk about dogs                         6
      2 var1     My prompt Dogs are cool but cats are aight too              6
      3 var1     My prompt I prefer dogs over cats                           6
      4 var1     My prompt My dog's collars are always too tight             6
      5 var1     My prompt One last sentence about dogs                      6
      6 var1     My prompt Cats collars are typically cooler than dogs       6

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

# empty strings ('')

    Code
      frequencies
    Output
      # A tibble: 6 x 4
        variable prompt    label                                       base_ns
        <chr>    <chr>     <chr>                                         <int>
      1 var1     My prompt I like to talk about dogs                         6
      2 var1     My prompt Dogs are cool but cats are aight too              6
      3 var1     My prompt I prefer dogs over cats                           6
      4 var1     My prompt My dog's collars are always too tight             6
      5 var1     My prompt One last sentence about dogs                      6
      6 var1     My prompt Cats collars are typically cooler than dogs       6

# NA strings

    Code
      frequencies
    Output
      # A tibble: 6 x 4
        variable prompt    label                                       base_ns
        <chr>    <chr>     <chr>                                         <int>
      1 var1     My prompt I like to talk about dogs                         6
      2 var1     My prompt Dogs are cool but cats are aight too              6
      3 var1     My prompt I prefer dogs over cats                           6
      4 var1     My prompt My dog's collars are always too tight             6
      5 var1     My prompt One last sentence about dogs                      6
      6 var1     My prompt Cats collars are typically cooler than dogs       6

# Large data frame

    Code
      frequencies
    Output
      # A tibble: 3 x 4
        variable prompt    label                                               base_ns
        <chr>    <chr>     <chr>                                                 <int>
      1 var1     My prompt "line \n  breaks "                                        3
      2 var1     My prompt "!@#$%^&*()_+\".,"                                        3
      3 var1     My prompt "😀😁😂😃😄😅😆😇😈😉😊😋😌😍😎😏😐😑😒😓😔😕😖😗😘😙😚😛😜😝😞😟😠😡😢😣😤😥😦😧😨😩😪😫😬😭😮😯😰~       3

# multiple vars

    Code
      frequencies
    Output
      # A tibble: 18 x 4
         variable prompt    label                                       base_ns
         <chr>    <chr>     <chr>                                         <int>
       1 var1     My prompt I like to talk about dogs                         6
       2 var1     My prompt Dogs are cool but cats are aight too              6
       3 var1     My prompt I prefer dogs over cats                           6
       4 var1     My prompt My dog's collars are always too tight             6
       5 var1     My prompt One last sentence about dogs                      6
       6 var1     My prompt Cats collars are typically cooler than dogs       6
       7 var2     My prompt I like to talk about dogs                         6
       8 var2     My prompt Dogs are cool but cats are aight too              6
       9 var2     My prompt I prefer dogs over cats                           6
      10 var2     My prompt My dog's collars are always too tight             6
      11 var2     My prompt One last sentence about dogs                      6
      12 var2     My prompt Cats collars are typically cooler than dogs       6
      13 var3     My prompt I like to talk about dogs                         6
      14 var3     My prompt Dogs are cool but cats are aight too              6
      15 var3     My prompt I prefer dogs over cats                           6
      16 var3     My prompt My dog's collars are always too tight             6
      17 var3     My prompt One last sentence about dogs                      6
      18 var3     My prompt Cats collars are typically cooler than dogs       6

# pipe vars

    Code
      frequencies
    Output
      # A tibble: 18 x 4
         variable prompt    label                                       base_ns
         <chr>    <chr>     <chr>                                         <int>
       1 var1     My prompt I like to talk about dogs                         6
       2 var1     My prompt Dogs are cool but cats are aight too              6
       3 var1     My prompt I prefer dogs over cats                           6
       4 var1     My prompt My dog's collars are always too tight             6
       5 var1     My prompt One last sentence about dogs                      6
       6 var1     My prompt Cats collars are typically cooler than dogs       6
       7 var2     My prompt I like to talk about dogs                         6
       8 var2     My prompt Dogs are cool but cats are aight too              6
       9 var2     My prompt I prefer dogs over cats                           6
      10 var2     My prompt My dog's collars are always too tight             6
      11 var2     My prompt One last sentence about dogs                      6
      12 var2     My prompt Cats collars are typically cooler than dogs       6
      13 var3     My prompt I like to talk about dogs                         6
      14 var3     My prompt Dogs are cool but cats are aight too              6
      15 var3     My prompt I prefer dogs over cats                           6
      16 var3     My prompt My dog's collars are always too tight             6
      17 var3     My prompt One last sentence about dogs                      6
      18 var3     My prompt Cats collars are typically cooler than dogs       6

# empty variables

    Code
      verbatims_y2(df_labelled)
    Output
      # A tibble: 3 x 4
        variable prompt      label                                 base_ns
        <chr>    <chr>       <chr>                                   <int>
      1 var2     My prompt 2 I like to talk about dogs                   3
      2 var2     My prompt 2 Dogs are cool but cats are aight too        3
      3 var2     My prompt 2 My dog's collars are always too tight       3

