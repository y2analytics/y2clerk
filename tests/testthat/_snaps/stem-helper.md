# stem() - separator must be a character vector

    Code
      dplyr::select(df_stem, stem("Q1", separator = 3))
    Condition
      Error in `dplyr::select()`:
      i In argument: `stem("Q1", separator = 3)`.
      Caused by error in `stem()`:
      ! `separator` must be a character vector.

# stem() - errors when stem is missing

    Code
      dplyr::select(df_stem, stem(separator = "a"))
    Condition
      Error in `dplyr::select()`:
      i In argument: `stem(separator = "a")`.
      Caused by error in `stem()`:
      ! argument "stem" is missing, with no default

# stem() - stem errors correctly when not used in a selecting function

    Code
      stem("Q1", separator = "a")
    Condition
      Error:
      ! `stem()` must be used within a *selecting* function.
      i See <https://tidyselect.r-lib.org/reference/faq-selection-context.html> for mroe details.

