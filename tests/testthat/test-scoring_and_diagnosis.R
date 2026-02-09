test_that("calculate_ptsd_total works correctly", {
  # Create test data
  test_data <- data.frame(
    matrix(sample(0:4, 20 * 10, replace = TRUE),
           nrow = 10,
           ncol = 20)
  )

  # Rename columns first
  renamed_data <- rename_ptsd_columns(test_data)

  # Calculate totals
  data_with_total <- calculate_ptsd_total(renamed_data)
  # Test total column added
  expect_true("total" %in% colnames(data_with_total))

  # Test total calculation
  expected_total <- rowSums(renamed_data)
  expect_equal(data_with_total$total, expected_total)

  # Test range of totals
  expect_true(all(data_with_total$total >= 0))
  expect_true(all(data_with_total$total <= 80))
})

test_that("create_ptsd_diagnosis_nonbinarized works correctly", {
  # Test case 1: All criteria met
  test_data <- data.frame(matrix(3, nrow = 1, ncol = 20))  # All symptoms rated 3 (above threshold)
  colnames(test_data) <- paste0("symptom_", 1:20)
  result <- create_ptsd_diagnosis_nonbinarized(test_data)
  expect_true(result$PTSD_Diagnosis)

  # Test case 2: Cluster 1 not met
  test_data[1, 1:5] <- 1  # Set all symptoms of cluster 1 below threshold
  result <- create_ptsd_diagnosis_nonbinarized(test_data)
  expect_false(result$PTSD_Diagnosis)

  # Test case 3: Cluster 2 not met
  test_data <- data.frame(matrix(3, nrow = 1, ncol = 20))
  colnames(test_data) <- paste0("symptom_", 1:20)
  test_data[1, 6:7] <- 1  # Set all symptoms of cluster 2 below threshold
  result <- create_ptsd_diagnosis_nonbinarized(test_data)
  expect_false(result$PTSD_Diagnosis)

  # Test case 4: Cluster 3 not met
  test_data <- data.frame(matrix(3, nrow = 1, ncol = 20))
  colnames(test_data) <- paste0("symptom_", 1:20)
  test_data[1, 8:14] <- 1  # Set all symptoms of cluster 3 below threshold
  result <- create_ptsd_diagnosis_nonbinarized(test_data)
  expect_false(result$PTSD_Diagnosis)

  # Test case 5: Cluster 4 not met
  test_data <- data.frame(matrix(3, nrow = 1, ncol = 20))
  colnames(test_data) <- paste0("symptom_", 1:20)
  test_data[1, 15:20] <- 1  # Set all symptoms of cluster 4 below threshold
  result <- create_ptsd_diagnosis_nonbinarized(test_data)
  expect_false(result$PTSD_Diagnosis)

  # Test case 6: Multiple rows
  test_data <- data.frame(matrix(3, nrow = 3, ncol = 20))
  colnames(test_data) <- paste0("symptom_", 1:20)
  # Row 1: All criteria met
  # Row 2: Cluster 1 not met
  test_data[2, 1:5] <- 1
  # Row 3: Cluster 3 not met
  test_data[3, 8:14] <- 1

  result <- create_ptsd_diagnosis_nonbinarized(test_data)
  expect_equal(result$PTSD_Diagnosis, c(TRUE, FALSE, FALSE))

  # Test case 7: Edge cases for multiple criteria
  test_data <- data.frame(matrix(1, nrow = 4, ncol = 20))  # Start with all below threshold
  colnames(test_data) <- paste0("symptom_", 1:20)

  # Row 1: Exactly minimum criteria met
  test_data[1, 1] <- 3      # One cluster 1 symptom
  test_data[1, 6] <- 3      # One cluster 2 symptom
  test_data[1, 8:9] <- 3    # Two cluster 3 symptoms
  test_data[1, 15:16] <- 3  # Two cluster 4 symptoms

  # Row 2: Just below minimum criteria (only one cluster 3 symptom)
  test_data[2, 1] <- 3      # One cluster 1 symptom
  test_data[2, 6] <- 3      # One cluster 2 symptom
  test_data[2, 8] <- 3      # Only one cluster 3 symptom
  test_data[2, 15:16] <- 3  # Two cluster 4 symptoms

  # Row 3: Just below minimum criteria (only one cluster 4 symptom)
  test_data[3, 1] <- 3      # One cluster 1 symptom
  test_data[3, 6] <- 3      # One cluster 2 symptom
  test_data[3, 8:9] <- 3    # Two cluster 3 symptoms
  test_data[3, 15] <- 3     # Only one cluster 4 symptom

  # Row 4: All symptoms at threshold (2)
  test_data[4, ] <- 2

  result <- create_ptsd_diagnosis_nonbinarized(test_data)
  expect_equal(result$PTSD_Diagnosis, c(TRUE, FALSE, FALSE, TRUE))
})



test_that("summarize_ptsd works correctly", {
  # Create test data with known values
  test_data <- data.frame(
    total = c(30, 40, 50, 60),
    PTSD_Diagnosis = c(TRUE, FALSE, TRUE, TRUE)
  )

  results <- summarize_ptsd(test_data)

  # Test summary calculations
  expect_equal(results$mean_total, mean(test_data$total))
  expect_equal(results$n_diagnosed, sum(test_data$PTSD_Diagnosis))
  expect_equal(results$sd_total, sd(test_data$total))
})
