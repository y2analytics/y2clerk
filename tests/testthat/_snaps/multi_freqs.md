# multi_freqs - warns and returns nothing when passed an actual variable

    Code
      x <- multi_freqs(responses2, m_activity_1)
    Condition
      Warning:
      "m_activity_1" exists in the dataset
      i `multi_freqs()` now works on stems instead of an example variable
      i Did you mean `multi_freqs(data, m_activity)`?
      ! As-is, "m_activity_1" will match nothing.

---

    Code
      tibble::as_tibble(multi_freqs(dplyr::mutate(responses2, m_activity_1_1 = m_activity_10),
      m_activity_1))
    Condition
      Warning:
      "m_activity_1" exists in the dataset
      i `multi_freqs()` now works on stems instead of an example variable
      i Did you mean `multi_freqs(data, m_activity)`?
      ! As-is, "m_activity_1" will match: "m_activity_1_1".
    Message
      Variable stem "m_activity_1" successfully freq'd
    Output
      # A tibble: 1 x 6
        variable       value label        n stat    result
        <chr>          <chr> <chr>    <dbl> <chr>    <dbl>
      1 m_activity_1_1 1     Baseball    11 percent      1

# multi_freqs - warns on variables with multiple response options

    Code
      tibble::as_tibble(multi_freqs(responses2, s_activity))
    Condition
      Warning in `multi_freqs()`:
      ! Matrix question detected
      i Question "s_activity_1" contains 5 response options
      i Please make sure this is intentional
    Message
      Variable stem "s_activity" successfully freq'd
    Output
      # A tibble: 5 x 6
        variable     value label                         n stat    result
        <chr>        <chr> <chr>                     <dbl> <chr>    <dbl>
      1 s_activity_1 1     Basketball                   11 percent   0.44
      2 s_activity_1 2     Football                      8 percent   0.32
      3 s_activity_1 3     Volleyball                    3 percent   0.12
      4 s_activity_1 4     Baseball                      0 percent   0   
      5 s_activity_1 5     Underwater Basket Weaving     3 percent   0.12

# multi_freqs - .by errors when data is already grouped

    Code
      multi_freqs(dplyr::group_by(responses2, gender), m_activity, .by = weights)
    Condition
      Error in `multi_freqs()`:
      ! Cannot use `.by` on an already-grouped data frame.
      i Use `dplyr::group_by()` or `.by`, not both.
      i The dataset is currently grouped by: "gender".

# multi_freqs - .by errors on an unknown column

    Code
      multi_freqs(responses2, m_activity, .by = not_a_col)
    Condition
      Error in `multi_freqs()`:
      ! `.by` column `not_a_col` not found in `dataset`.

# multi_freqs - errors on a non-data-frame dataset

    Code
      multi_freqs(1:10, m_activity)
    Condition
      Error in `multi_freqs()`:
      ! `dataset` must be a data frame, not an integer vector.

# multi_freqs - errors on non-boolean flags

    Code
      multi_freqs(responses2, m_activity, remove_nas = "yes")
    Condition
      Error in `multi_freqs()`:
      ! `remove_nas` must be `TRUE` or `FALSE`, not the string "yes".

# multi_freqs - errors on invalid digits

    Code
      multi_freqs(responses2, m_activity, digits = -1)
    Condition
      Error in `multi_freqs()`:
      ! `digits` must be a whole number larger than or equal to 0, not the number -1.

# multi_freqs - errors on invalid separator

    Code
      multi_freqs(responses2, m_activity, separator = 1)
    Condition
      Error in `multi_freqs()`:
      ! `separator` must be a single string, not the number 1.

# multi_freqs - errors when wt column is not found

    Code
      multi_freqs(responses2, m_activity, wt = not_a_weight)
    Condition
      Error in `multi_freqs()`:
      ! `wt` column `not_a_weight` not found in `dataset`.
      i Did you mean: "weights"?

# multi_freqs - errors when unweighted_ns is TRUE without a weight

    Code
      multi_freqs(responses2, m_activity, unweighted_ns = TRUE)
    Condition
      Error in `multi_freqs()`:
      ! `unweighted_ns` is "TRUE" but no weight variable was provided.
      i Supply a weighting column via `wt`, or set `unweighted_ns = FALSE`.

