## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----Installation, eval=FALSE, include=TRUE-----------------------------------
#  # Install devtools if not already installed
#  if (!require("devtools")) install.packages("devtools")
#  
#  # Install PTSDdiag
#  library("devtools")
#  devtools::install_github("WeidmannL/PTSDdiag")

## ----Loading PTSDdiag, echo=TRUE, warning=FALSE-------------------------------
library("PTSDdiag")

## ----Loading other, echo=TRUE, warning=FALSE----------------------------------
library(psych)     # For reliability analysis

## ----Loading sample data, echo=TRUE-------------------------------------------
# Load the sample data
data("simulated_ptsd")

## ----Displaying sample data, echo=TRUE----------------------------------------
# Display first few rows
head(simulated_ptsd)

## ----Column names of input data, echo=TRUE------------------------------------
#  Example of potential input formats
names(simulated_ptsd)

## ----Renaming columns, echo=TRUE----------------------------------------------
# Rename columns to standard format (symptom_1 through symptom_20)
simulated_ptsd_renamed <- rename_ptsd_columns(simulated_ptsd)

# Show new names
names(simulated_ptsd_renamed)

## ----Basic Descriptive Statistics, echo=TRUE----------------------------------
# Step 1: Calculate total scores (range 0-80)
simulated_ptsd_total <- calculate_ptsd_total(simulated_ptsd_renamed)

# Step 2: Apply DSM-5 diagnostic criteria and determine PTSD diagnoses
simulated_ptsd_total_diagnosed <- create_ptsd_diagnosis_nonbinarized(simulated_ptsd_total)

# Step 3: Generate summary statistics
summary_stats <- summarize_ptsd(simulated_ptsd_total_diagnosed)
print(summary_stats)

## ----Reliability Analysis, echo=TRUE------------------------------------------
cronbach <- psych::alpha(subset(simulated_ptsd_total_diagnosed, select = (-total)))
print(cronbach$total)

## ----Optimal Hierarchical Symptom Combinations, echo=TRUE, eval=FALSE---------
#  # Find best combinations with hierarchical approach, minimizing false negatives
#  best_combinations_hierarchical <- analyze_best_six_symptoms_four_required_clusters(
#    simulated_ptsd_renamed,
#    score_by = "newly_nondiagnosed"
#  )

## ----Best Symptoms hierarchical, echo=TRUE, eval=FALSE------------------------
#  best_combinations_hierarchical$best_symptoms

## ----Comparison of diagnosis hierarchical, echo=TRUE, eval=FALSE--------------
#  # Shows true/false values for original vs. new criteria
#  head(best_combinations_hierarchical$diagnosis_comparison, 10)

## ----Summary table hierachical, echo=TRUE, eval=FALSE-------------------------
#  best_combinations_hierarchical$summary

## ----Optimal Non-hierarchical Symptom Combinations, echo=TRUE, eval=FALSE-----
#  # Find best combinations with non-hierarchical approach, minimizing false negatives
#  best_combinations_nonhierarchical <- analyze_best_six_symptoms_four_required(
#    simulated_ptsd_renamed,
#    score_by = "newly_nondiagnosed"
#  )

## ----Best Symptoms nonhierarchical, echo=TRUE, eval=FALSE---------------------
#  best_combinations_nonhierarchical$best_symptoms

## ----Comparison of diagnosis nonhierarchical, echo=TRUE, eval=FALSE-----------
#  # Shows true/false values for original vs. new criteria
#  head(best_combinations_nonhierarchical$diagnosis_comparison, 10)

## ----Summary table nonhierachical, echo=TRUE, eval=FALSE----------------------
#  best_combinations_nonhierarchical$summary

## ----Holdout Validation, echo=TRUE, eval=FALSE--------------------------------
#  # Perform holdout validation with 70/30 split
#  validation_results <- holdout_validation(
#    simulated_ptsd_renamed,
#    train_ratio = 0.7,
#    score_by = "newly_nondiagnosed",
#    seed = 123
#  )

## ----Holdout Results Non-Hierarchical, echo=TRUE, eval=FALSE------------------
#  # Best combinations identified on training data
#  validation_results$without_clusters$best_combinations
#  
#  # Performance summary on test data
#  validation_results$without_clusters$summary

## ----Holdout Results Hierarchical, echo=TRUE, eval=FALSE----------------------
#  # Best combinations identified on training data (with cluster representation)
#  validation_results$with_clusters$best_combinations
#  
#  # Performance summary on test data
#  validation_results$with_clusters$summary

## ----Cross Validation, echo=TRUE, eval=FALSE----------------------------------
#  # Perform 5-fold cross-validation
#  cv_results <- cross_validation(
#    simulated_ptsd_renamed,
#    k = 5,
#    score_by = "newly_nondiagnosed",
#    seed = 123
#  )

## ----CV Results by Fold for non-hierarchical model, echo=TRUE, eval=FALSE-----
#  # Summary statistics for each fold (non-hierarchical model)
#  cv_results$without_clusters$summary_by_fold

## ----CV Results by Fold for hierarchical model, echo=TRUE, eval=FALSE---------
#  # Summary statistics for each fold (hierarchical model)
#  cv_results$with_clusters$summary_by_fold

## ----CV Stable Combinations for non-hierarchical model, echo=TRUE, eval=FALSE----
#  # Check for combinations that appeared in multiple folds (non-hierarchical)
#  if (!is.null(cv_results$without_clusters$combinations_summary)) {
#    print("Stable combinations in non-hierarchical model:")
#    cv_results$without_clusters$combinations_summary
#  } else {
#    print("No combinations appeared in multiple folds for the non-hierarchical model")
#  }

## ----CV Stable Combinations for hierarchical model, echo=TRUE, eval=FALSE-----
#  # Check for combinations that appeared in multiple folds (hierarchical)
#  if (!is.null(cv_results$with_clusters$combinations_summary)) {
#    print("Stable combinations in hierarchical model:")
#    cv_results$with_clusters$combinations_summary
#  } else {
#    print("No combinations appeared in multiple folds for the hierarchical model")
#  }

## ----Compare Validation Methods, echo=TRUE, eval=FALSE------------------------
#  # Example: Compare sensitivity from both methods
#  # Note: Results will vary based on random splits
#  
#  # Holdout Validation sensitivity (first combination, non-hierarchical)
#  holdout_summary <- validation_results$without_clusters$summary$x$data
#  if (nrow(holdout_summary) > 1) {
#    holdout_sensitivity <- holdout_summary[2, "Sensitivity"]
#    print(paste("Holdout sensitivity for first combination:",
#                round(holdout_sensitivity, 3)))
#  }
#  
#  # Cross-Validation average sensitivity (if stable combinations exist)
#  if (!is.null(cv_results$without_clusters$combinations_summary)) {
#    cv_summary <- cv_results$without_clusters$combinations_summary$x$data
#    if (nrow(cv_summary) > 0) {
#      cv_sensitivity <- cv_summary[1, "Sensitivity"]
#      print(paste("Cross-validation average sensitivity:",
#                  round(cv_sensitivity, 3)))
#    }
#  }

