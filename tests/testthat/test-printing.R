test_that("`create_env_in_global()` creates .print_buffer environment if it doesn't exist", {
  # Ensure .print_buffer doesn't exist before testing
  if (exists(".print_buffer", envir = .GlobalEnv)) {
    rm(".print_buffer", envir = .GlobalEnv)
  }

  # Call the function
  create_env_in_global()

  # Check if .print_buffer was created
  expect_true(exists(".print_buffer", envir = .GlobalEnv))
  expect_true(is.environment(.GlobalEnv$.print_buffer))

  # Check that the parent is emptyenv()
  expect_identical(parent.env(.GlobalEnv$.print_buffer), emptyenv())

  # Clean up
  rm(".print_buffer", envir = .GlobalEnv)
})

