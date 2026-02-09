# Suppress R CMD check NOTEs for dplyr non-standard evaluation
utils::globalVariables(c(
  "true_positive", "newly_nondiagnosed", "true_negative", "newly_diagnosed",
  "sensitivity", "specificity", "ppv", "npv", "across", "Split",
  "Total Diagnosed", "Total Non-Diagnosed", "Scenario",
  "Total_Diagnosed_N", "Total_Diagnosed_Pct",
  "Total_Non_Diagnosed_N", "Total_Non_Diagnosed_Pct",
  "True Positive", "True Negative", "Newly Diagnosed", "Newly Non-Diagnosed",
  "True Cases", "False Cases",
  "True_Positive", "Newly_Non_Diagnosed", "True_Negative", "Newly_Diagnosed",
  "Splits_Appeared"
))

#' Perform holdout validation for PTSD diagnostic models
#'
#' @description
#' Validates PTSD diagnostic models using a train-test split approach (holdout validation).
#' Trains the model on a portion of the data and evaluates performance on the held-out test set.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Splits data into training (70%) and test (30%) sets
#'   \item Finds optimal symptom combinations on training data
#'   \item Evaluates these combinations on test data
#'   \item Compares results to original DSM-5 diagnoses
#' }
#'
#' Two models are evaluated:
#' \itemize{
#'   \item Model without cluster representation: Any 4 of 6 symptoms
#'   \item Model with cluster representation: 4 of 6 symptoms with at least one from each cluster
#' }
#'
#' @param data A dataframe containing exactly 20 columns with PCL-5 item scores
#'   (output of rename_ptsd_columns). Each symptom should be scored on a 0-4 scale.
#' @param train_ratio Numeric between 0 and 1 indicating proportion of data for training
#'   (default: 0.7 for 70/30 split)
#' @param score_by Character string specifying optimization criterion:
#'   \itemize{
#'     \item "false_cases": Minimize total misclassifications
#'     \item "newly_nondiagnosed": Minimize false negatives only (default)
#'   }
#' @param seed Integer for random number generation reproducibility (default: 123)
#'
#' @returns A list containing:
#' \itemize{
#'   \item without_clusters: Results for model without cluster representation
#'     \itemize{
#'       \item best_combinations: The 3 best six-symptom combinations from training
#'       \item test_results: Diagnostic comparison on test data
#'       \item summary: Formatted summary statistics
#'     }
#'   \item with_clusters: Results for model with cluster representation
#'     \itemize{
#'       \item best_combinations: The 3 best six-symptom combinations from training
#'       \item test_results: Diagnostic comparison on test data
#'       \item summary: Formatted summary statistics
#'     }
#' }
#'
#' @export
#'
#'
#' @examples
#' # Create sample data
#' set.seed(42)
#' sample_data <- data.frame(
#'   matrix(sample(0:4, 20 * 200, replace = TRUE),
#'          nrow = 200,
#'          ncol = 20)
#' )
#' colnames(sample_data) <- paste0("symptom_", 1:20)
#'
#' \donttest{
#' # Perform holdout validation
#' validation_results <- holdout_validation(sample_data, train_ratio = 0.7)
#'
#' # Access results
#' validation_results$without_clusters$summary
#' validation_results$with_clusters$summary
#' }
#'
holdout_validation <- function(data, train_ratio = 0.7, score_by = "newly_nondiagnosed", seed = 123) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("Input must be a dataframe")
  }
  
  if (ncol(data) != 20) {
    stop("Data must contain exactly 20 columns (one for each PCL-5 item)")
  }
  
  expected_cols <- paste0("symptom_", 1:20)
  if (!all(expected_cols %in% colnames(data))) {
    stop("Data must contain columns named 'symptom_1' through 'symptom_20'")
  }
  
  if (train_ratio <= 0 || train_ratio >= 1) {
    stop("train_ratio must be between 0 and 1")
  }
  
  valid_scoring <- c("false_cases", "newly_nondiagnosed")
  if (!score_by %in% valid_scoring) {
    stop("score_by must be one of: ", paste(valid_scoring, collapse = ", "))
  }
  
  # Set seed for reproducibility
  set.seed(seed)
  
  # Split data
  train_index <- sample(seq_len(nrow(data)), size = train_ratio * nrow(data))
  train_data <- data[train_index, ]
  test_data <- data[-train_index, ]
  
  # Determine original PTSD diagnosis in test data
  test_data_with_diagnosis <- create_ptsd_diagnosis_nonbinarized(test_data)
  
  # Model without cluster representation
  train_results_without <- analyze_best_six_symptoms_four_required(train_data, score_by = score_by)
  top_combos_without <- train_results_without$best_symptoms
  
  # Apply to test data
  test_diagnoses_without <- lapply(top_combos_without, function(symptoms) {
    binarized_test <- binarize_data(test_data)
    subset_data <- binarized_test[, paste0("symptom_", symptoms)]
    rowSums(subset_data) >= 4
  })
  
  # Create comparison dataframe
  comparison_df_without <- data.frame(
    PTSD_orig = test_data_with_diagnosis$PTSD_Diagnosis,
    vapply(1:3, function(i) test_diagnoses_without[[i]], logical(nrow(test_data)))
  )
  names(comparison_df_without)[2:4] <- vapply(1:3, function(i) {
    paste0("symptom_", paste(top_combos_without[[i]], collapse = "_"))
  }, character(1))

  # Model with cluster representation
  train_results_with <- analyze_best_six_symptoms_four_required_clusters(train_data, score_by = score_by)
  top_combos_with <- train_results_with$best_symptoms
  
  # Apply to test data with cluster checking
  test_diagnoses_with <- lapply(top_combos_with, function(symptoms) {
    binarized_test <- as.matrix(binarize_data(test_data))
    
    # Define clusters
    clusters <- list(
      cluster1 = 1:5,
      cluster2 = 6:7,
      cluster3 = 8:14,
      cluster4 = 15:20
    )
    
    # Create lookup array
    cluster_lookup <- integer(20)
    for(j in seq_along(clusters)) {
      cluster_lookup[clusters[[j]]] <- j
    }
    
    # Check cluster representation
    check_cluster_representation <- function(symp) {
      length(unique(cluster_lookup[symp])) == 4
    }
    
    # Get diagnosis
    subset_data <- binarized_test[, symptoms, drop = FALSE]
    symptom_counts <- rowSums(subset_data)
    sufficient_rows <- which(symptom_counts >= 4)
    result <- logical(nrow(test_data))
    
    if (length(sufficient_rows) > 0) {
      for (i in sufficient_rows) {
        present_symptoms <- symptoms[subset_data[i, ] == 1]
        if (length(present_symptoms) >= 4) {
          result[i] <- check_cluster_representation(present_symptoms)
        }
      }
    }
    return(result)
  })
  
  # Create comparison dataframe
  comparison_df_with <- data.frame(
    PTSD_orig = test_data_with_diagnosis$PTSD_Diagnosis,
    vapply(1:3, function(i) test_diagnoses_with[[i]], logical(nrow(test_data)))
  )
  names(comparison_df_with)[2:4] <- vapply(1:3, function(i) {
    paste0("symptom_", paste(top_combos_with[[i]], collapse = "_"))
  }, character(1))

  # Return results
  summary_without <- create_readable_summary(summarize_ptsd_changes(comparison_df_without))
  summary_with <- create_readable_summary(summarize_ptsd_changes(comparison_df_with))
  if (requireNamespace("DT", quietly = TRUE)) {
    summary_without <- DT::datatable(summary_without, options = list(scrollX = TRUE))
    summary_with <- DT::datatable(summary_with, options = list(scrollX = TRUE))
  }

  list(
    without_clusters = list(
      best_combinations = top_combos_without,
      test_results = comparison_df_without,
      summary = summary_without
    ),
    with_clusters = list(
      best_combinations = top_combos_with,
      test_results = comparison_df_with,
      summary = summary_with
    )
  )
}


#' Perform k-fold cross-validation for PTSD diagnostic models
#'
#' @description
#' Validates PTSD diagnostic models using k-fold cross-validation to assess
#' generalization performance and identify stable symptom combinations.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Splits data into k folds
#'   \item For each fold, trains on k-1 folds and tests on the held-out fold
#'   \item Identifies symptom combinations that appear across multiple folds
#'   \item Calculates average performance metrics for repeated combinations
#' }
#'
#' Two models are evaluated:
#' \itemize{
#'   \item Model without cluster representation: Any 4 of 6 symptoms
#'   \item Model with cluster representation: 4 of 6 symptoms with at least one from each cluster
#' }
#'
#' @param data A dataframe containing exactly 20 columns with PCL-5 item scores
#'   (output of rename_ptsd_columns). Each symptom should be scored on a 0-4 scale.
#' @param k Number of folds for cross-validation (default: 5)
#' @param score_by Character string specifying optimization criterion:
#'   \itemize{
#'     \item "false_cases": Minimize total misclassifications
#'     \item "newly_nondiagnosed": Minimize false negatives only (default)
#'   }
#' @param seed Integer for random number generation reproducibility (default: 123)
#'
#' @returns A list containing:
#' \itemize{
#'   \item without_clusters: Results for model without cluster representation
#'     \itemize{
#'       \item fold_results: List of diagnostic comparisons for each fold
#'       \item summary_by_fold: Detailed results for each fold
#'       \item combinations_summary: Average performance for combinations appearing
#'         in multiple folds (NULL if no combinations repeat)
#'     }
#'   \item with_clusters: Results for model with cluster representation
#'     \itemize{
#'       \item fold_results: List of diagnostic comparisons for each fold
#'       \item summary_by_fold: Detailed results for each fold
#'       \item combinations_summary: Average performance for combinations appearing
#'         in multiple folds (NULL if no combinations repeat)
#'     }
#' }
#'
#' @export
#'
#' @importFrom modelr crossv_kfold
#' @importFrom dplyr bind_rows group_by summarise mutate select everything across filter n
#'
#' @examples
#' # Create sample data
#' set.seed(42)
#' sample_data <- data.frame(
#'   matrix(sample(0:4, 20 * 200, replace = TRUE),
#'          nrow = 200,
#'          ncol = 20)
#' )
#' colnames(sample_data) <- paste0("symptom_", 1:20)
#'
#' \donttest{
#' # Perform 5-fold cross-validation
#' cv_results <- cross_validation(sample_data, k = 5)
#'
#' # View summary for each fold
#' cv_results$without_clusters$summary_by_fold
#'
#' # View combinations that appeared multiple times
#' cv_results$without_clusters$combinations_summary
#' }
#'
cross_validation <- function(data, k = 5, score_by = "newly_nondiagnosed", seed = 123) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("Input must be a dataframe")
  }
  
  if (ncol(data) != 20) {
    stop("Data must contain exactly 20 columns (one for each PCL-5 item)")
  }
  
  expected_cols <- paste0("symptom_", 1:20)
  if (!all(expected_cols %in% colnames(data))) {
    stop("Data must contain columns named 'symptom_1' through 'symptom_20'")
  }
  
  if (k < 2 || k > nrow(data)) {
    stop("k must be between 2 and the number of rows in the data")
  }
  
  valid_scoring <- c("false_cases", "newly_nondiagnosed")
  if (!score_by %in% valid_scoring) {
    stop("score_by must be one of: ", paste(valid_scoring, collapse = ", "))
  }
  
  # Set seed for reproducibility
  set.seed(seed)
  
  # Create cross-validation folds
  cv_splits <- modelr::crossv_kfold(data, k = k)
  
  # Initialize result lists
  cv_results_without <- list()
  cv_results_with <- list()
  
  # Helper function to summarize CV splits
  summarize_cv_splits <- function(cv_list) {
    summaries <- lapply(seq_along(cv_list), function(i) {
      fold_data <- cv_list[[i]]
      summary_stats <- summarize_ptsd_changes(fold_data)
      
      # Compute diagnostic metrics and avoid division by zero
      summary_stats <- summary_stats %>%
        dplyr::mutate(
          Sensitivity = ifelse((true_positive + newly_nondiagnosed) == 0, NA,
                               round(true_positive / (true_positive + newly_nondiagnosed), 4)), 
          Specificity = ifelse((true_negative + newly_diagnosed) == 0, NA,
                               round(true_negative / (true_negative + newly_diagnosed), 4)),   
          PPV         = ifelse((true_positive + newly_diagnosed) == 0, NA,
                               round(true_positive / (true_positive + newly_diagnosed), 4)), 
          NPV         = ifelse((true_negative + newly_nondiagnosed) == 0, NA,
                               round(true_negative / (true_negative + newly_nondiagnosed), 4)) 
        ) %>%
        
        # Replace NA values with 0 to prevent errors in summary display
        dplyr::mutate(
          across(c(sensitivity, specificity, ppv, npv),
                 ~ ifelse(is.na(.), 0, .))
        )
      
      # Convert to a readable summary table
      readable <- create_readable_summary(summary_stats)
      readable$Split <- paste0("Split ", i)
      return(readable)
    })
    
    # Combine all splits into a single dataframe
    final_summary <- dplyr::bind_rows(summaries)
    final_summary <- dplyr::select(final_summary, Split, dplyr::everything())
    
    return(final_summary)
  }
  
  # Helper function to summarize average performance for repeated symptom combinations across splits
  summarize_combinations_across_splits <- function(cv_summary) {
    combo_summary <- cv_summary %>%
      dplyr::mutate(
        Total_Diagnosed_N = as.numeric(gsub(" \\(.*\\)", "", `Total Diagnosed`)),
        Total_Diagnosed_Pct = as.numeric(gsub(".*\\((.*)%\\)", "\\1", `Total Diagnosed`)),
        Total_Non_Diagnosed_N = as.numeric(gsub(" \\(.*\\)", "", `Total Non-Diagnosed`)),
        Total_Non_Diagnosed_Pct = as.numeric(gsub(".*\\((.*)%\\)", "\\1", `Total Non-Diagnosed`))
      ) %>%
      dplyr::group_by(Scenario) %>%
      dplyr::summarise(
        Splits_Appeared = dplyr::n(),
        Total_Diagnosed = paste0(
          round(mean(Total_Diagnosed_N), 2),
          " (", round(mean(Total_Diagnosed_Pct), 2), "%)"
        ),
        Total_Non_Diagnosed = paste0(
          round(mean(Total_Non_Diagnosed_N), 2),
          " (", round(mean(Total_Non_Diagnosed_Pct), 2), "%)"
        ),
        True_Positive = round(mean(`True Positive`), 2),
        True_Negative = round(mean(`True Negative`), 2),
        Newly_Diagnosed = round(mean(`Newly Diagnosed`), 2),
        Newly_Non_Diagnosed = round(mean(`Newly Non-Diagnosed`), 2),
        True_Cases = round(mean(`True Cases`), 2),
        False_Cases = round(mean(`False Cases`), 2),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        Sensitivity = round(True_Positive / (True_Positive + Newly_Non_Diagnosed), 4),
        Specificity = round(True_Negative / (True_Negative + Newly_Diagnosed), 4),
        PPV = round(True_Positive / (True_Positive + Newly_Diagnosed), 4),
        NPV = round(True_Negative / (True_Negative + Newly_Non_Diagnosed), 4)
      )
    
    multiple_appearance <- combo_summary %>% dplyr::filter(Splits_Appeared > 1)
    
    if (nrow(multiple_appearance) == 0) {
      return(NULL)
    } else {
      return(multiple_appearance)
    }
  }
  
  # Loop for cross-validation
  for (i in seq_len(k)) {
    # Extract training and test data
    train_data <- as.data.frame(cv_splits$train[[i]])
    test_data <- as.data.frame(cv_splits$test[[i]])
    
    # Determine original PTSD diagnosis in test data
    test_data_with_diagnosis <- create_ptsd_diagnosis_nonbinarized(test_data)
    
    # Model without cluster representation
    train_results_without <- analyze_best_six_symptoms_four_required(train_data, score_by = score_by)
    top_combos_without <- train_results_without$best_symptoms
    
    # Apply to test data
    diagnoses_without <- lapply(top_combos_without, function(symptoms) {
      binarized_test <- binarize_data(test_data)
      subset_data <- binarized_test[, paste0("symptom_", symptoms)]
      rowSums(subset_data) >= 4
    })
    
    # Create comparison dataframe
    comparison_df_without <- data.frame(
      PTSD_orig = test_data_with_diagnosis$PTSD_Diagnosis,
      vapply(1:3, function(i) diagnoses_without[[i]], logical(nrow(test_data)))
    )
    names(comparison_df_without)[2:4] <- vapply(top_combos_without, function(symptoms) {
      paste0("symptom_", paste(symptoms, collapse = "_"))
    }, character(1))

    cv_results_without[[i]] <- comparison_df_without
    
    # Model with cluster representation
    train_results_with <- analyze_best_six_symptoms_four_required_clusters(train_data, score_by = score_by)
    top_combos_with <- train_results_with$best_symptoms
    
    # Apply to test data with cluster checking
    diagnoses_with <- lapply(top_combos_with, function(symptoms) {
      binarized_test <- as.matrix(binarize_data(test_data))
      
      # Define clusters
      clusters <- list(
        cluster1 = 1:5,
        cluster2 = 6:7,
        cluster3 = 8:14,
        cluster4 = 15:20
      )
      
      # Create lookup array
      cluster_lookup <- integer(20)
      for(j in seq_along(clusters)) {
        cluster_lookup[clusters[[j]]] <- j
      }
      
      # Check cluster representation
      check_cluster_representation <- function(symp) {
        length(unique(cluster_lookup[symp])) == 4
      }
      
      # Get diagnosis
      subset_data <- binarized_test[, symptoms, drop = FALSE]
      symptom_counts <- rowSums(subset_data)
      sufficient_rows <- which(symptom_counts >= 4)
      result <- logical(nrow(test_data))
      
      if (length(sufficient_rows) > 0) {
        for (idx in sufficient_rows) {
          present_symptoms <- symptoms[subset_data[idx, ] == 1]
          if (length(present_symptoms) >= 4) {
            result[idx] <- check_cluster_representation(present_symptoms)
          }
        }
      }
      return(result)
    })
    
    # Create comparison dataframe
    comparison_df_with <- data.frame(
      PTSD_orig = test_data_with_diagnosis$PTSD_Diagnosis,
      vapply(1:3, function(i) diagnoses_with[[i]], logical(nrow(test_data)))
    )
    names(comparison_df_with)[2:4] <- vapply(top_combos_with, function(symptoms) {
      paste0("symptom_", paste(symptoms, collapse = "_"))
    }, character(1))

    cv_results_with[[i]] <- comparison_df_with
  }
  
  # Apply summary functions
  cv_summary_without <- summarize_cv_splits(cv_results_without)
  cv_summary_with <- summarize_cv_splits(cv_results_with)
  
  # Apply combination summary functions
  combo_summary_without <- summarize_combinations_across_splits(cv_summary_without)
  combo_summary_with <- summarize_combinations_across_splits(cv_summary_with)
  
  # Format results (use DT if available)
  wrap_dt <- function(x) {
    if (!is.null(x) && requireNamespace("DT", quietly = TRUE)) {
      DT::datatable(x, options = list(scrollX = TRUE))
    } else {
      x
    }
  }

  list(
    without_clusters = list(
      fold_results = cv_results_without,
      summary_by_fold = wrap_dt(cv_summary_without),
      combinations_summary = wrap_dt(combo_summary_without)
    ),
    with_clusters = list(
      fold_results = cv_results_with,
      summary_by_fold = wrap_dt(cv_summary_with),
      combinations_summary = wrap_dt(combo_summary_with)
    )
  )
}
