test_that("analyze_best_six_symptoms_four_required works correctly", {
  # Create small test dataset
  test_data <- data.frame(
    matrix(sample(0:4, 20 * 50, replace = TRUE),
           nrow = 50,
           ncol = 20)
  )
  colnames(test_data) <- paste0("symptom_", 1:20)

  # Test basic functionality
  results <- analyze_best_six_symptoms_four_required(test_data, score_by = "false_cases")

  # Check structure of results
  expect_type(results, "list")
  expect_equal(length(results$best_symptoms), 3)
  expect_true(all(sapply(results$best_symptoms, length) == 6))

  # Check that symptoms are valid
  expect_true(all(unlist(results$best_symptoms) >= 1))
  expect_true(all(unlist(results$best_symptoms) <= 20))

  # Check invalid scoring method
  expect_error(
    analyze_best_six_symptoms_four_required(test_data, "invalid_method"),
    ("score_by must be one of: false_cases, newly_nondiagnosed")
  )
})

test_that("analyze_best_six_symptoms_four_required_clusters works correctly", {
  # Create small test dataset
  test_data <- data.frame(
    matrix(sample(0:4, 20 * 50, replace = TRUE),
           nrow = 50,
           ncol = 20)
  )
  colnames(test_data) <- paste0("symptom_", 1:20)

  # Test basic functionality
  results <- analyze_best_six_symptoms_four_required_clusters(
    test_data,
    score_by = "false_cases"
  )

  # Check structure of results
  expect_type(results, "list")
  expect_equal(length(results$best_symptoms), 3)
  expect_true(all(sapply(results$best_symptoms, length) == 6))

  # Check that each combination has representation from all clusters
  for (combination in results$best_symptoms) {
    # Check if combination includes symptoms from each cluster
    has_cluster1 <- any(combination %in% 1:5)
    has_cluster2 <- any(combination %in% 6:7)
    has_cluster3 <- any(combination %in% 8:14)
    has_cluster4 <- any(combination %in% 15:20)

    expect_true(has_cluster1 && has_cluster2 && has_cluster3 && has_cluster4)
  }

  # Check invalid scoring method
  expect_error(
    analyze_best_six_symptoms_four_required_clusters(test_data, "invalid_method"),
    ("score_by must be one of: false_cases, newly_nondiagnosed")
  )
})
