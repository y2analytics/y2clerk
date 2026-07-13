# multi_freqs - warns and returns nothing when passed an actual variable

    Code
      x <- multi_freqs(responses2, m_activity_1)
    Condition
      Warning:
      "m_activity_1" appears to be an actual variable in the dataset, not a stem.
      i `multi_freqs()` now selects columns with `stem()`; pass the stem instead, e.g. `multi_freqs(data, m_activity)`.
      ! Passed to `stem()` as-is, "m_activity_1" will match nothing.

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

