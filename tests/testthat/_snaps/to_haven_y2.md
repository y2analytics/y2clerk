# to_haven_y2 error on haven_labelled vars

    Code
      responses4 %>% dplyr::mutate(q4_haven = to_haven_y2(q4),
      "q4 is already a haven_labelled variable", fixed = TRUE)
    Condition
      Error in `dplyr::mutate()`:
      i In argument: `q4_haven = to_haven_y2(q4)`.
      Caused by error in `to_haven_y2()`:
      ! q4 is already a haven_labelled variable

# to_haven_y2 error on numeric vars

    Code
      responses4 %>% dplyr::mutate(q0_haven = to_haven_y2(q0),
      "to_haven_y2 cannot be used on numeric variable: q0", fixed = TRUE)
    Condition
      Error in `dplyr::mutate()`:
      i In argument: `q0_haven = to_haven_y2(q0)`.
      Caused by error in `to_haven_y2()`:
      ! to_haven_y2 cannot be used on numeric variable: q0

