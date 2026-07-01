test_that("freqs() prints a basic frequency tibble with the haven label data up top when applicable", {
  with_label <- responses |> freqs(q1)
  without_label <- responses |> dplyr::mutate(q5 = as.character(q5)) |> freqs(q5)
  expect_snapshot(print(with_label))
  expect_snapshot(print(without_label))
})

test_that("freqs() print shows truncation footer for long output", {
  test_freq <- mtcars |> freqs(hp)
  expect_snapshot(print(test_freq))
})

test_that("freqs() print shows Groups in header when grouped", {
  test_freq <- mtcars |>
    dplyr::group_by(cyl) |>
    freqs(gear)
  expect_snapshot(print(test_freq))
})
