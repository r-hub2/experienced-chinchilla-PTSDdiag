test_that("binarize_data works correctly", {
  # Create test data with all 20 PCL-5 items
  test_data <- data.frame(matrix(rep(0:4,4), nrow = 5, ncol = 20))
  colnames(test_data) <- paste0("symptom_", 1:20)

  # Test actual binarization
  result <- binarize_data(test_data)

  # Test dimensions preserved
  expect_equal(dim(result), dim(test_data))

  # Test column names preserved
  expect_equal(colnames(result), colnames(test_data))

  # Test values correctly binarized
  expect_true(all(result[test_data < 2] == 0))
  expect_true(all(result[test_data >= 2] == 1))

  # Test only 0s and 1s in result
  expect_true(all(result == 0 | result == 1))

  # Test specific pattern for first column
  expected_col1 <- c(0, 0, 1, 1, 1)  # Based on input 0,1,2,3,4
  expect_equal(result$symptom_1, expected_col1)
})


test_that("create_ptsd_diagnosis_binarized works correctly", {
  # Test case 1: All criteria met
  test_data <- data.frame(matrix(3, nrow = 1, ncol = 20))  # All symptoms rated 3 (above threshold)
  colnames(test_data) <- paste0("symptom_", 1:20)
  result <- create_ptsd_diagnosis_binarized(test_data)
  expect_true(result$PTSD_orig)

  # Test case 2: Cluster 1 not met
  test_data[1, 1:5] <- 1  # Set all symptoms of cluster 1 below threshold
  result <- create_ptsd_diagnosis_binarized(test_data)
  expect_false(result$PTSD_orig)

  # Test case 3: Cluster 2 not met
  test_data <- data.frame(matrix(3, nrow = 1, ncol = 20))
  colnames(test_data) <- paste0("symptom_", 1:20)
  test_data[1, 6:7] <- 1  # Set all symptoms of cluster 2 below threshold
  result <- create_ptsd_diagnosis_binarized(test_data)
  expect_false(result$PTSD_orig)

  # Test case 4: Cluster 3 not met
  test_data <- data.frame(matrix(3, nrow = 1, ncol = 20))
  colnames(test_data) <- paste0("symptom_", 1:20)
  test_data[1, 8:14] <- 1  # Set all symptoms of cluster 3 below threshold
  result <- create_ptsd_diagnosis_binarized(test_data)
  expect_false(result$PTSD_orig)

  # Test case 5: Cluster 4 not met
  test_data <- data.frame(matrix(3, nrow = 1, ncol = 20))
  colnames(test_data) <- paste0("symptom_", 1:20)
  test_data[1, 15:20] <- 1  # Set all symptoms of cluster 4 below threshold
  result <- create_ptsd_diagnosis_binarized(test_data)
  expect_false(result$PTSD_orig)

  # Test case 6: Multiple rows
  test_data <- data.frame(matrix(3, nrow = 3, ncol = 20))
  colnames(test_data) <- paste0("symptom_", 1:20)
  # Row 1: All criteria met
  # Row 2: Cluster 1 not met
  test_data[2, 1:5] <- 1
  # Row 3: Cluster 3 not met
  test_data[3, 8:14] <- 1

  result <- create_ptsd_diagnosis_binarized(test_data)
  expect_equal(result$PTSD_orig, c(TRUE, FALSE, FALSE))

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

  result <- create_ptsd_diagnosis_binarized(test_data)
  expect_equal(result$PTSD_orig, c(TRUE, FALSE, FALSE, TRUE))
})

test_that("create_ptsd_diagnosis_binarized and create_ptsd_diagnosis_nonbinarized show same diagnosis", {
  # Test case 1: Regular pattern data
  test_data <- data.frame(matrix(rep(0:4, 4), nrow = 5, ncol = 20))
  colnames(test_data) <- paste0("symptom_", 1:20)

  results_diagnosis_binarized <- create_ptsd_diagnosis_binarized(test_data)
  results_diagnosis_nonbinarized <- create_ptsd_diagnosis_nonbinarized(test_data)

  expect_equal(results_diagnosis_binarized$PTSD_orig,
               results_diagnosis_nonbinarized$PTSD_Diagnosis)

  # Test case 2: Edge case data with threshold values
  edge_data <- data.frame(matrix(2, nrow = 5, ncol = 20))  # All values at threshold
  colnames(edge_data) <- paste0("symptom_", 1:20)

  results_edge_binarized <- create_ptsd_diagnosis_binarized(edge_data)
  results_edge_nonbinarized <- create_ptsd_diagnosis_nonbinarized(edge_data)

  expect_equal(results_edge_binarized$PTSD_orig,
               results_edge_nonbinarized$PTSD_Diagnosis)
})

test_that("create_readable_summary works correctly", {
  # Create test summary stats
  test_stats <- data.frame(
    column = c("PTSD_orig", "Test1"),
    diagnosed = c(50, 45),
    non_diagnosed = c(50, 55),
    diagnosed_percent = c(50, 45),
    non_diagnosed_percent = c(50, 55),
    newly_diagnosed = c(0, 5),
    newly_nondiagnosed = c(0, 10),
    true_positive = c(50, 40),
    true_negative = c(50, 45),
    true_cases = c(100, 85),
    false_cases = c(0, 15),
    sensitivity = c(1, 0.8),
    specificity = c(1, 0.9),
    ppv = c(1, 0.89),
    npv = c(1, 0.82)
  )

  results <- create_readable_summary(test_stats)

  # Check column names
  expected_columns <- c("Scenario", "Total Diagnosed", "Total Non-Diagnosed",
                        "True Positive", "True Negative", "Newly Diagnosed",
                        "Newly Non-Diagnosed", "True Cases", "False Cases",
                        "Sensitivity", "Specificity", "PPV", "NPV")
  expect_equal(colnames(results), expected_columns)

  # Test formatting
  expect_equal(results$`Total Diagnosed`[1], "50 (50%)")
  expect_equal(results$Sensitivity[2], 0.8)
})

test_that("summarize_ptsd_changes works correctly", {
  # Test case 1: Perfect agreement
  perfect_data <- data.frame(
    PTSD_orig = c(TRUE, TRUE, FALSE, FALSE),
    PTSD_alt = c(TRUE, TRUE, FALSE, FALSE)
  )
  perfect_result <- summarize_ptsd_changes(perfect_data)

  expect_equal(perfect_result$sensitivity[perfect_result$column == "PTSD_alt"], 1)
  expect_equal(perfect_result$specificity[perfect_result$column == "PTSD_alt"], 1)
  expect_equal(perfect_result$ppv[perfect_result$column == "PTSD_alt"], 1)
  expect_equal(perfect_result$npv[perfect_result$column == "PTSD_alt"], 1)
  expect_equal(perfect_result$newly_diagnosed[perfect_result$column == "PTSD_alt"], 0)
  expect_equal(perfect_result$newly_nondiagnosed[perfect_result$column == "PTSD_alt"], 0)

  # Test case 2: Complete disagreement
  opposite_data <- data.frame(
    PTSD_orig = c(TRUE, TRUE, FALSE, FALSE),
    PTSD_alt = c(FALSE, FALSE, TRUE, TRUE)
  )
  opposite_result <- summarize_ptsd_changes(opposite_data)

  expect_equal(opposite_result$sensitivity[opposite_result$column == "PTSD_alt"], 0)
  expect_equal(opposite_result$specificity[opposite_result$column == "PTSD_alt"], 0)
  expect_equal(opposite_result$newly_diagnosed[opposite_result$column == "PTSD_alt"], 2)
  expect_equal(opposite_result$newly_nondiagnosed[opposite_result$column == "PTSD_alt"], 2)

  # Test case 3: Partial agreement
  partial_data <- data.frame(
    PTSD_orig = c(TRUE, TRUE, FALSE, FALSE),
    PTSD_alt1 = c(TRUE, FALSE, FALSE, FALSE),  # 50% sensitivity, 100% specificity
    PTSD_alt2 = c(TRUE, TRUE, TRUE, FALSE)     # 100% sensitivity, 50% specificity
  )
  partial_result <- summarize_ptsd_changes(partial_data)

  # Check alt1 metrics
  alt1_row <- partial_result[partial_result$column == "PTSD_alt1", ]
  expect_equal(alt1_row$sensitivity, 0.5)
  expect_equal(alt1_row$specificity, 1)
  expect_equal(alt1_row$newly_diagnosed, 0)
  expect_equal(alt1_row$newly_nondiagnosed, 1)

  # Check alt2 metrics
  alt2_row <- partial_result[partial_result$column == "PTSD_alt2", ]
  expect_equal(alt2_row$sensitivity, 1)
  expect_equal(alt2_row$specificity, 0.5)
  expect_equal(alt2_row$newly_diagnosed, 1)
  expect_equal(alt2_row$newly_nondiagnosed, 0)

  # Test case 4: Edge case - all positive
  all_pos_data <- data.frame(
    PTSD_orig = c(TRUE, TRUE, TRUE),
    PTSD_alt = c(TRUE, TRUE, TRUE)
  )
  all_pos_result <- summarize_ptsd_changes(all_pos_data)
  expect_equal(all_pos_result$diagnosed_percent[all_pos_result$column == "PTSD_alt"], 100)
  expect_equal(all_pos_result$non_diagnosed_percent[all_pos_result$column == "PTSD_alt"], 0)

  # Test case 5: Edge case - all negative
  all_neg_data <- data.frame(
    PTSD_orig = c(FALSE, FALSE, FALSE),
    PTSD_alt = c(FALSE, FALSE, FALSE)
  )
  all_neg_result <- summarize_ptsd_changes(all_neg_data)
  expect_equal(all_neg_result$diagnosed_percent[all_neg_result$column == "PTSD_alt"], 0)
  expect_equal(all_neg_result$non_diagnosed_percent[all_neg_result$column == "PTSD_alt"], 100)

  # Test case 6: Multiple alternative criteria
  multi_data <- data.frame(
    PTSD_orig = c(TRUE, TRUE, FALSE, FALSE),
    PTSD_alt1 = c(TRUE, FALSE, FALSE, FALSE),
    PTSD_alt2 = c(TRUE, TRUE, TRUE, FALSE),
    PTSD_alt3 = c(FALSE, FALSE, FALSE, TRUE)
  )
  multi_result <- summarize_ptsd_changes(multi_data)
  expect_equal(nrow(multi_result), 4)  # One row for each criterion
  expect_true(all(c("PTSD_orig", "PTSD_alt1", "PTSD_alt2", "PTSD_alt3") %in% multi_result$column))
})
