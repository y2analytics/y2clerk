# multi_freqs - warns and returns nothing when passed an actual variable

    Code
      x <- multi_freqs(responses2, m_activity_1)
    Condition
      Warning:
      "m_activity_1" appears to be an actual variable in the dataset, not a stem.
      i `multi_freqs()` now selects columns with `stem()`; pass the stem instead, e.g. `multi_freqs(data, m_activity)`.
      ! Passed to `stem()` as-is, "m_activity_1" will match nothing.

