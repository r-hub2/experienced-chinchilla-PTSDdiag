#' Find optimal non-hierarchical six-symptom combinations for PTSD diagnosis
#'
#' @description
#' Identifies the three best six-symptom combinations for PTSD diagnosis
#' where any four symptoms must be present, regardless of their cluster membership.
#' This function implements a simplified diagnostic approach compared to the full
#' DSM-5 criteria.
#'
#' @details
#' The function:
#'
#' \enumerate{
#'  \item Tests all possible combinations of 6 symptoms from the 20 PCL-5 items
#'  \item Requires 4 symptoms to be present (>=2 on original 0-4 scale) for diagnosis
#'  \item Identifies the three combinations that best match the original DSM-5 diagnosis
#' }
#'
#' Optimization can be based on either:
#'
#' \itemize{
#' \item Minimizing false cases (both false positives and false negatives)
#' \item Minimizing only false negatives (newly non-diagnosed cases)
#'}
#'
#' The symptom clusters in PCL-5 are:
#'
#' \itemize{
#' \item Items 1-5: Intrusion symptoms (Criterion B)
#' \item Items 6-7: Avoidance symptoms (Criterion C)
#' \item Items 8-14: Negative alterations in cognitions and mood (Criterion D)
#' \item Items 15-20: Alterations in arousal and reactivity (Criterion E)
#'}
#'
#' @param data A dataframe containing exactly 20 columns with PCL-5 item scores
#'   (output of rename_ptsd_columns). Each symptom should be scored on a 0-4
#'   scale where:
#'
#' \itemize{
#'   \item 0 = Not at all
#'   \item 1 = A little bit
#'   \item 2 = Moderately
#'   \item 3 = Quite a bit
#'   \item 4 = Extremely
#'}
#'
#' @param score_by Character string specifying optimization criterion:
#'
#' \itemize{
#'   \item "false_cases": Minimize total misclassifications
#'   \item "newly_nondiagnosed": Minimize false negatives only
#'}
#'
#' @returns A list containing:
#'
#' \itemize{
#'   \item best_symptoms: List of three vectors, each containing six symptom numbers
#'     representing the best combinations found
#'   \item diagnosis_comparison: Dataframe comparing original DSM-5 diagnosis with
#'     diagnoses based on the three best combinations
#'   \item summary: Interactive datatable (DT) showing diagnostic accuracy metrics
#'     for each combination
#'}
#'
#' @export
#'
#' @importFrom utils combn
#'
#' @examples
#' # Create example data
#' ptsd_data <- data.frame(matrix(sample(0:4, 200, replace=TRUE), ncol=20))
#' names(ptsd_data) <- paste0("symptom_", 1:20)
#'
#' \donttest{
#' # Find best combinations minimizing false cases
#' results <- analyze_best_six_symptoms_four_required(ptsd_data, score_by = "false_cases")
#'
#' # Get symptom numbers
#' results$best_symptoms
#'
#' # View raw comparison data
#' results$diagnosis_comparison
#'
#' # View summary statistics
#' results$summary
#' }
#'
analyze_best_six_symptoms_four_required <- function(data, score_by = "false_cases") {
  # Validate input is a dataframe
  if (!is.data.frame(data)) {
    stop("Input must be a dataframe")
  }

  # Validate number of columns
  if (ncol(data) != 20) {
    stop("Data must contain exactly 20 columns (one for each PCL-5 item)")
  }

  # Validate column names
  expected_cols <- paste0("symptom_", 1:20)
  if (!all(expected_cols %in% colnames(data))) {
    stop("Data must contain columns named 'symptom_1' through 'symptom_20'")
  }

  # Check for total column (warning)
  if ("total" %in% colnames(data)) {
    warning("'total' column detected. This function should only be used with raw symptom scores.")
  }

  # Validate data type
  if (!all(vapply(data[expected_cols], is.numeric, logical(1)))) {
    stop("All symptom columns must contain numeric values")
  }

  # Check for missing values
  if (any(is.na(data[expected_cols]))) {
    stop("Data contains missing values (NA)")
  }

  # Validate value ranges and check for integers
  invalid_values <- !all(vapply(data[expected_cols], function(x)
    all(x >= 0 & x <= 4 & x == floor(x)), logical(1)))
  if (invalid_values) {
    stop("All symptom values must be integers between 0 and 4")
  }

  # Validate scoring method
  valid_scoring <- c("false_cases", "newly_nondiagnosed")
  if (!score_by %in% valid_scoring) {
    stop("score_by must be one of: ", paste(valid_scoring, collapse = ", "))
  }

  # Validate minimum number of rows
  if (nrow(data) < 1) {
    stop("Data must contain at least one row")
  }

  # Get baseline results and binarize data
  baseline_results <- create_ptsd_diagnosis_binarized(data)$PTSD_orig
  binarized_data <- binarize_data(data)

  # Helper function to find best combinations
  find_best_combinations <- function(combinations, binarized_data, baseline_results, score_by, get_diagnosis_fn) {
    top_combinations <- list(
      first = list(combination = NULL, score = -Inf, diagnoses = NULL),
      second = list(combination = NULL, score = -Inf, diagnoses = NULL),
      third = list(combination = NULL, score = -Inf, diagnoses = NULL)
    )

    for(combination in combinations) {
      current_diagnoses <- get_diagnosis_fn(binarized_data, combination)

      newly_diagnosed <- sum(!baseline_results & current_diagnoses)
      newly_nondiagnosed <- sum(baseline_results & !current_diagnoses)

      score <- if(score_by == "false_cases") {
        -(newly_diagnosed + newly_nondiagnosed)
      } else {
        -newly_nondiagnosed
      }

      if(score > top_combinations$first$score) {
        top_combinations$third <- top_combinations$second
        top_combinations$second <- top_combinations$first
        top_combinations$first <- list(
          combination = combination,
          score = score,
          diagnoses = current_diagnoses
        )
      } else if(score > top_combinations$second$score) {
        top_combinations$third <- top_combinations$second
        top_combinations$second <- list(
          combination = combination,
          score = score,
          diagnoses = current_diagnoses
        )
      } else if(score > top_combinations$third$score) {
        top_combinations$third <- list(
          combination = combination,
          score = score,
          diagnoses = current_diagnoses
        )
      }
    }

    return(top_combinations)
  }

  # Helper function for diagnosis
  get_diagnosis <- function(data, symptoms) {
    subset_data <- data[, paste0("symptom_", symptoms)]
    return(rowSums(subset_data) >= 4)  # At least 4 symptoms must be present
  }

  # Generate all possible combinations of 6 symptoms and find best ones
  all_symptoms <- 1:20
  combinations <- utils::combn(all_symptoms, 6, simplify = FALSE)

  top_combinations <- find_best_combinations(combinations, binarized_data, baseline_results, score_by, get_diagnosis)

  # Create comparison dataframe
  comparison_df <- data.frame(
    PTSD_orig = baseline_results,
    vapply(1:3, function(i) top_combinations[[i]]$diagnoses, logical(nrow(data)))
  )
  names(comparison_df)[2:4] <- vapply(1:3, function(i) {
    paste0("symptom_", paste(top_combinations[[i]]$combination, collapse = "_"))
  }, character(1))

  summary_table <- create_readable_summary(summarize_ptsd_changes(comparison_df))
  if (requireNamespace("DT", quietly = TRUE)) {
    summary_table <- DT::datatable(summary_table, options = list(scrollX = TRUE))
  }

  return(list(
    best_symptoms = lapply(1:3, function(i) top_combinations[[i]]$combination),
    diagnosis_comparison = comparison_df,
    summary = summary_table
  ))
}



#' Find optimal hierarchical six-symptom combinations for PTSD diagnosis
#'
#' @description
#' Identifies the three best six-symptom combinations for PTSD diagnosis
#' where four symptoms must be present and must include at least one symptom from
#' each DSM-5 criterion cluster. This approach maintains the hierarchical structure
#' of PTSD diagnosis while reducing the total number of required symptoms.
#'
#' @details
#' The function:
#'
#' \enumerate{
#' \item Generates valid combinations ensuring representation from all clusters
#' \item Requires 4 symptoms to be present (>=2 on original 0-4 scale) for diagnosis
#' \item Validates that present symptoms include at least one from each cluster
#' \item Identifies the three combinations that best match the original DSM-5 diagnosis
#'}
#'
#' DSM-5 PTSD symptom clusters:
#'
#' \itemize{
#' \item Cluster 1 (B) - Intrusion: Items 1-5
#' \item Cluster 2 (C) - Avoidance: Items 6-7
#' \item Cluster 3 (D) - Negative alterations in cognitions and mood: Items 8-14
#' \item Cluster 4 (E) - Alterations in arousal and reactivity: Items 15-20
#'}
#'
#' Optimization can be based on either:
#'
#' \itemize{
#' \item Minimizing false cases (both false positives and false negatives)
#' \item Minimizing only false negatives (newly non-diagnosed cases)
#'}
#'
#' @param data A dataframe containing exactly 20 columns with PCL-5 item scores
#'   (output of rename_ptsd_columns). Each symptom should be scored on a 0-4
#'   scale where:
#'
#' \itemize{
#'   \item 0 = Not at all
#'   \item 1 = A little bit
#'   \item 2 = Moderately
#'   \item 3 = Quite a bit
#'   \item 4 = Extremely
#'}
#'
#' @param score_by Character string specifying optimization criterion:
#'
#' \itemize{
#'   \item "false_cases": Minimize total misclassifications
#'   \item "newly_nondiagnosed": Minimize false negatives only
#'}
#'
#' @returns A list containing:
#'
#' \itemize{
#'   \item best_symptoms: List of three vectors, each containing six symptom numbers
#'     representing the best combinations found
#'   \item diagnosis_comparison: Dataframe comparing original DSM-5 diagnosis with
#'     diagnoses based on the three best combinations
#'   \item summary: Interactive datatable (DT) showing diagnostic accuracy metrics
#'     for each combination
#'}
#'
#' @export
#'
#' @importFrom utils combn
#'
#' @examples
#' # Create example data
#' ptsd_data <- data.frame(matrix(sample(0:4, 200, replace=TRUE), ncol=20))
#' names(ptsd_data) <- paste0("symptom_", 1:20)
#'
#' \donttest{
#' # Find best hierarchical combinations minimizing false cases
#' results <- analyze_best_six_symptoms_four_required_clusters(ptsd_data, score_by = "false_cases")
#'
#' # Get symptom numbers
#' results$best_symptoms
#'
#' # View raw comparison data
#' results$diagnosis_comparison
#'
#' # View summary statistics
#' results$summary
#' }
#'
analyze_best_six_symptoms_four_required_clusters <- function(data, score_by = "false_cases") {
  # Validate input is a dataframe
  if (!is.data.frame(data)) {
    stop("Input must be a dataframe")
  }

  # Validate number of columns
  if (ncol(data) != 20) {
    stop("Data must contain exactly 20 columns (one for each PCL-5 item)")
  }

  # Validate column names
  expected_cols <- paste0("symptom_", 1:20)
  if (!all(expected_cols %in% colnames(data))) {
    stop("Data must contain columns named 'symptom_1' through 'symptom_20'")
  }

  # Check for total column (warning)
  if ("total" %in% colnames(data)) {
    warning("'total' column detected. This function should only be used with raw symptom scores.")
  }

  # Validate data type
  if (!all(vapply(data[expected_cols], is.numeric, logical(1)))) {
    stop("All symptom columns must contain numeric values")
  }

  # Check for missing values
  if (any(is.na(data[expected_cols]))) {
    stop("Data contains missing values (NA)")
  }

  # Validate value ranges and check for integers
  invalid_values <- !all(vapply(data[expected_cols], function(x)
    all(x >= 0 & x <= 4 & x == floor(x)), logical(1)))
  if (invalid_values) {
    stop("All symptom values must be integers between 0 and 4")
  }

  # Validate scoring method
  valid_scoring <- c("false_cases", "newly_nondiagnosed")
  if (!score_by %in% valid_scoring) {
    stop("score_by must be one of: ", paste(valid_scoring, collapse = ", "))
  }

  # Validate minimum number of rows
  if (nrow(data) < 1) {
    stop("Data must contain at least one row")
  }

  # Get baseline results and binarize data
  baseline_results <- create_ptsd_diagnosis_binarized(data)$PTSD_orig
  binarized_data <- as.matrix(binarize_data(data))

  # Define clusters
  clusters <- list(
    cluster1 = 1:5,
    cluster2 = 6:7,
    cluster3 = 8:14,
    cluster4 = 15:20
  )

  # Create lookup array for faster cluster membership checking
  cluster_lookup <- integer(20)
  for(i in seq_along(clusters)) {
    cluster_lookup[clusters[[i]]] <- i
  }

  # Fast cluster representation check using lookup
  check_cluster_representation <- function(symptoms) {
    length(unique(cluster_lookup[symptoms])) == 4
  }

  # Helper function to find best combination
  find_best_combinations <- function(combinations, binarized_data, baseline_results, score_by, get_diagnosis_fn) {
    top_combinations <- list(
      first = list(combination = NULL, score = -Inf, diagnoses = NULL),
      second = list(combination = NULL, score = -Inf, diagnoses = NULL),
      third = list(combination = NULL, score = -Inf, diagnoses = NULL)
    )

    for(combination in combinations) {
      current_diagnoses <- get_diagnosis_fn(binarized_data, combination)

      newly_diagnosed <- sum(!baseline_results & current_diagnoses)
      newly_nondiagnosed <- sum(baseline_results & !current_diagnoses)

      score <- if(score_by == "false_cases") {
        -(newly_diagnosed + newly_nondiagnosed)
      } else {
        -newly_nondiagnosed
      }

      if(score > top_combinations$first$score) {
        top_combinations$third <- top_combinations$second
        top_combinations$second <- top_combinations$first
        top_combinations$first <- list(
          combination = combination,
          score = score,
          diagnoses = current_diagnoses
        )
      } else if(score > top_combinations$second$score) {
        top_combinations$third <- top_combinations$second
        top_combinations$second <- list(
          combination = combination,
          score = score,
          diagnoses = current_diagnoses
        )
      } else if(score > top_combinations$third$score) {
        top_combinations$third <- list(
          combination = combination,
          score = score,
          diagnoses = current_diagnoses
        )
      }
    }

    return(top_combinations)
  }

  # Helper function for diagnosis
  get_diagnosis <- function(data, symptoms) {
    subset_data <- data[, symptoms, drop = FALSE]
    symptom_counts <- rowSums(subset_data)
    sufficient_rows <- which(symptom_counts >= 4)

    result <- logical(nrow(data))

    if(length(sufficient_rows) > 0) {
      for(i in sufficient_rows) {
        present_symptoms <- symptoms[subset_data[i,] == 1]
        if(length(present_symptoms) >= 4) {
          result[i] <- check_cluster_representation(present_symptoms)
        }
      }
    }

    return(result)
  }

  # Generate valid combinations efficiently
  valid_combinations <- vector("list", 1000)  # Pre-allocate
  combination_count <- 0

  for(s1 in clusters$cluster1) {
    for(s2 in clusters$cluster2) {
      for(s3 in clusters$cluster3) {
        for(s4 in clusters$cluster4) {
          base <- c(s1, s2, s3, s4)
          remaining <- setdiff(1:20, base)
          pairs <- utils::combn(remaining, 2, simplify = FALSE)

          for(pair in pairs) {
            combination_count <- combination_count + 1
            if(combination_count > length(valid_combinations)) {
              length(valid_combinations) <- length(valid_combinations) * 2
            }
            valid_combinations[[combination_count]] <- sort(c(base, pair))
          }
        }
      }
    }
  }

  valid_combinations <- valid_combinations[1:combination_count]
  valid_combinations <- unique(valid_combinations)

  # Find best combinations
  top_combinations <- find_best_combinations(
    valid_combinations,
    binarized_data,
    baseline_results,
    score_by,
    get_diagnosis
  )

  # Create comparison dataframe
  comparison_df <- data.frame(
    PTSD_orig = baseline_results,
    vapply(1:3, function(i) top_combinations[[i]]$diagnoses, logical(nrow(data)))
  )
  names(comparison_df)[2:4] <- vapply(1:3, function(i) {
    paste0("symptom_", paste(top_combinations[[i]]$combination, collapse = "_"))
  }, character(1))

  summary_table <- create_readable_summary(summarize_ptsd_changes(comparison_df))
  if (requireNamespace("DT", quietly = TRUE)) {
    summary_table <- DT::datatable(summary_table, options = list(scrollX = TRUE))
  }

  return(list(
    best_symptoms = lapply(1:3, function(i) top_combinations[[i]]$combination),
    diagnosis_comparison = comparison_df,
    summary = summary_table
  ))
}
