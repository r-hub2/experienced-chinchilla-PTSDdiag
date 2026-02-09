test_that("rename_ptsd_columns works correctly", {
  # Create test data
  test_data <- data.frame(
    matrix(sample(0:4, 20 * 10, replace = TRUE),
           nrow = 10,
           ncol = 20)
  )

  # Tests renaming
  renamed_data <- rename_ptsd_columns(test_data)
  expect_equal(colnames(renamed_data), paste0("symptom_", 1:20))

  # Tests if dimensions are preserved
  expect_equal(dim(renamed_data), dim(test_data))

  # Tests if data values are preserved
  expect_equal(renamed_data[1,1], test_data[1,1])

  # Test with already correctly named columns
  already_named <- test_data
  colnames(already_named) <- paste0("symptom_", 1:20)
  renamed_again <- rename_ptsd_columns(already_named)
  expect_equal(colnames(renamed_again), paste0("symptom_", 1:20))
})
