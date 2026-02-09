test_that("holdout_validation works correctly", {
  # Create test data
  set.seed(42)
  test_data <- data.frame(
    matrix(sample(0:4, 20 * 100, replace = TRUE),
           nrow = 100,
           ncol = 20)
  )
  colnames(test_data) <- paste0("symptom_", 1:20)
  
  # Test basic functionality
  results <- holdout_validation(test_data, train_ratio = 0.7, seed = 42)
  
  # Check structure of results
  expect_type(results, "list")
  expect_equal(names(results), c("without_clusters", "with_clusters"))
  
  # Check without_clusters results
  expect_type(results$without_clusters, "list")
  expect_equal(names(results$without_clusters), 
               c("best_combinations", "test_results", "summary"))
  expect_equal(length(results$without_clusters$best_combinations), 3)
  expect_true(all(sapply(results$without_clusters$best_combinations, length) == 6))
  
  # Check with_clusters results
  expect_type(results$with_clusters, "list")
  expect_equal(names(results$with_clusters), 
               c("best_combinations", "test_results", "summary"))
  expect_equal(length(results$with_clusters$best_combinations), 3)
  expect_true(all(sapply(results$with_clusters$best_combinations, length) == 6))
  
  # Check test_results structure
  expect_true("PTSD_orig" %in% names(results$without_clusters$test_results))
  expect_true("PTSD_orig" %in% names(results$with_clusters$test_results))
  
  # Check that test results have correct number of rows (30% of data)
  expected_test_rows <- floor((1 - 0.7) * nrow(test_data))
  expect_equal(nrow(results$without_clusters$test_results), expected_test_rows)
  expect_equal(nrow(results$with_clusters$test_results), expected_test_rows)
  
  # Check that combinations from with_clusters have cluster representation
  for (combination in results$with_clusters$best_combinations) {
    has_cluster1 <- any(combination %in% 1:5)
    has_cluster2 <- any(combination %in% 6:7)
    has_cluster3 <- any(combination %in% 8:14)
    has_cluster4 <- any(combination %in% 15:20)
    expect_true(has_cluster1 && has_cluster2 && has_cluster3 && has_cluster4)
  }
})

test_that("holdout_validation handles different train_ratio values", {
  # Create test data
  test_data <- data.frame(
    matrix(sample(0:4, 20 * 50, replace = TRUE),
           nrow = 50,
           ncol = 20)
  )
  colnames(test_data) <- paste0("symptom_", 1:20)
  
  # Test with different train ratios
  results_50 <- holdout_validation(test_data, train_ratio = 0.5, seed = 123)
  results_80 <- holdout_validation(test_data, train_ratio = 0.8, seed = 123)
  
  # Check that test set sizes are correct
  expect_equal(nrow(results_50$without_clusters$test_results), 25)
  expect_equal(nrow(results_80$without_clusters$test_results), 10)
})

test_that("holdout_validation handles different score_by options", {
  # Create test data
  test_data <- data.frame(
    matrix(sample(0:4, 20 * 50, replace = TRUE),
           nrow = 50,
           ncol = 20)
  )
  colnames(test_data) <- paste0("symptom_", 1:20)
  
  # Test with different scoring methods
  results_false <- holdout_validation(test_data, score_by = "false_cases", seed = 123)
  results_newly <- holdout_validation(test_data, score_by = "newly_nondiagnosed", seed = 123)
  
  # Both should return valid results
  expect_type(results_false, "list")
  expect_type(results_newly, "list")
  
  # Results might differ based on optimization criterion
  # but structure should be the same
  expect_equal(names(results_false), names(results_newly))
})

test_that("holdout_validation validates input correctly", {
  # Create test data
  test_data <- data.frame(
    matrix(sample(0:4, 20 * 50, replace = TRUE),
           nrow = 50,
           ncol = 20)
  )
  colnames(test_data) <- paste0("symptom_", 1:20)
  
  # Test invalid train_ratio
  expect_error(
    holdout_validation(test_data, train_ratio = 0),
    "train_ratio must be between 0 and 1"
  )
  expect_error(
    holdout_validation(test_data, train_ratio = 1),
    "train_ratio must be between 0 and 1"
  )
  expect_error(
    holdout_validation(test_data, train_ratio = 1.5),
    "train_ratio must be between 0 and 1"
  )
  
  # Test invalid score_by
  expect_error(
    holdout_validation(test_data, score_by = "invalid"),
    "score_by must be one of"
  )
  
  # Test with wrong number of columns
  wrong_cols <- test_data[, 1:15]
  expect_error(
    holdout_validation(wrong_cols),
    "Data must contain exactly 20 columns"
  )
  
  # Test with wrong column names
  wrong_names <- test_data
  colnames(wrong_names) <- paste0("col_", 1:20)
  expect_error(
    holdout_validation(wrong_names),
    "Data must contain columns named 'symptom_1' through 'symptom_20'"
  )
})

test_that("cross_validation works correctly", {
  # Create test data
  set.seed(42)
  test_data <- data.frame(
    matrix(sample(0:4, 20 * 100, replace = TRUE),
           nrow = 100,
           ncol = 20)
  )
  colnames(test_data) <- paste0("symptom_", 1:20)
  
  # Test basic functionality with k=3 for speed
  results <- cross_validation(test_data, k = 3, seed = 42)
  
  # Check structure of results
  expect_type(results, "list")
  expect_equal(names(results), c("without_clusters", "with_clusters"))
  
  # Check without_clusters results
  expect_type(results$without_clusters, "list")
  expect_equal(names(results$without_clusters), 
               c("fold_results", "summary_by_fold", "combinations_summary"))
  expect_equal(length(results$without_clusters$fold_results), 3)  # k=3 folds
  
  # Check with_clusters results
  expect_type(results$with_clusters, "list")
  expect_equal(names(results$with_clusters), 
               c("fold_results", "summary_by_fold", "combinations_summary"))
  expect_equal(length(results$with_clusters$fold_results), 3)  # k=3 folds
  
  # Check that each fold has results
  for (i in 1:3) {
    expect_true("PTSD_orig" %in% names(results$without_clusters$fold_results[[i]]))
    expect_true("PTSD_orig" %in% names(results$with_clusters$fold_results[[i]]))
  }
})

test_that("cross_validation handles different k values", {
  # Create test data
  test_data <- data.frame(
    matrix(sample(0:4, 20 * 50, replace = TRUE),
           nrow = 50,
           ncol = 20)
  )
  colnames(test_data) <- paste0("symptom_", 1:20)
  
  # Test with different k values
  results_2 <- cross_validation(test_data, k = 2, seed = 123)
  results_5 <- cross_validation(test_data, k = 5, seed = 123)
  
  # Check that number of folds is correct
  expect_equal(length(results_2$without_clusters$fold_results), 2)
  expect_equal(length(results_5$without_clusters$fold_results), 5)
  
  # Each fold should have approximately equal size
  fold_sizes_2 <- sapply(results_2$without_clusters$fold_results, nrow)
  fold_sizes_5 <- sapply(results_5$without_clusters$fold_results, nrow)
  
  # Allow for small differences due to rounding
  expect_true(max(fold_sizes_2) - min(fold_sizes_2) <= 1)
  expect_true(max(fold_sizes_5) - min(fold_sizes_5) <= 1)
})

test_that("cross_validation validates input correctly", {
  # Create test data
  test_data <- data.frame(
    matrix(sample(0:4, 20 * 50, replace = TRUE),
           nrow = 50,
           ncol = 20)
  )
  colnames(test_data) <- paste0("symptom_", 1:20)
  
  # Test invalid k values
  expect_error(
    cross_validation(test_data, k = 1),
    "k must be between 2 and the number of rows"
  )
  expect_error(
    cross_validation(test_data, k = 51),
    "k must be between 2 and the number of rows"
  )
  
  # Test invalid score_by
  expect_error(
    cross_validation(test_data, score_by = "invalid"),
    "score_by must be one of"
  )
  
  # Test with wrong number of columns
  wrong_cols <- test_data[, 1:15]
  expect_error(
    cross_validation(wrong_cols),
    "Data must contain exactly 20 columns"
  )
  
  # Test with non-dataframe input
  expect_error(
    cross_validation(as.matrix(test_data)),
    "Input must be a dataframe"
  )
})

test_that("cross_validation combinations_summary works correctly", {
  # Create test data with patterns that might repeat across folds
  set.seed(123)
  test_data <- data.frame(
    matrix(sample(0:4, 20 * 100, replace = TRUE, 
                  prob = c(0.3, 0.3, 0.2, 0.1, 0.1)),  # Skewed distribution
           nrow = 100,
           ncol = 20)
  )
  colnames(test_data) <- paste0("symptom_", 1:20)
  
  # Run cross-validation
  results <- cross_validation(test_data, k = 5, seed = 123)
  
  # Check combinations_summary structure if it exists (can be NULL)
  if (!is.null(results$without_clusters$combinations_summary)) {
    # If combinations repeat, check the summary structure
    summary_data <- results$without_clusters$combinations_summary$x$data
    expect_true("Splits_Appeared" %in% names(summary_data))
    expect_true("Sensitivity" %in% names(summary_data))
    expect_true("Specificity" %in% names(summary_data))
    expect_true("PPV" %in% names(summary_data))
    expect_true("NPV" %in% names(summary_data))
    
    # Splits_Appeared should be > 1 for all rows
    expect_true(all(summary_data$Splits_Appeared > 1))
  }
  
  # Same for with_clusters
  if (!is.null(results$with_clusters$combinations_summary)) {
    summary_data <- results$with_clusters$combinations_summary$x$data
    expect_true("Splits_Appeared" %in% names(summary_data))
    expect_true(all(summary_data$Splits_Appeared > 1))
  }
})

test_that("holdout_validation produces consistent results with same seed", {
  # Create test data
  test_data <- data.frame(
    matrix(sample(0:4, 20 * 50, replace = TRUE),
           nrow = 50,
           ncol = 20)
  )
  colnames(test_data) <- paste0("symptom_", 1:20)
  
  # Run twice with same seed
  results1 <- holdout_validation(test_data, seed = 42)
  results2 <- holdout_validation(test_data, seed = 42)
  
  # Results should be identical
  expect_equal(results1$without_clusters$best_combinations, 
               results2$without_clusters$best_combinations)
  expect_equal(results1$with_clusters$best_combinations, 
               results2$with_clusters$best_combinations)
  
  # Run with different seed
  results3 <- holdout_validation(test_data, seed = 123)
  
  # Results might differ (though not guaranteed for small datasets)
  # At least the structure should be the same
  expect_equal(names(results1), names(results3))
})

test_that("cross_validation produces consistent results with same seed", {
  # Create test data
  test_data <- data.frame(
    matrix(sample(0:4, 20 * 50, replace = TRUE),
           nrow = 50,
           ncol = 20)
  )
  colnames(test_data) <- paste0("symptom_", 1:20)
  
  # Run twice with same seed
  results1 <- cross_validation(test_data, k = 3, seed = 42)
  results2 <- cross_validation(test_data, k = 3, seed = 42)
  
  # Check that fold assignments are the same
  for (i in 1:3) {
    expect_equal(nrow(results1$without_clusters$fold_results[[i]]),
                 nrow(results2$without_clusters$fold_results[[i]]))
  }
})
