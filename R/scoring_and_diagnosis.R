#' Calculate PTSD total score
#'
#' Calculates the total score from PCL-5 items
#'
#' @description
#' Calculates the total PCL-5 (PTSD Checklist for DSM-5) score by summing all
#' 20 symptom scores. The total score ranges from 0 to 80, with higher scores
#' indicating greater symptom severity.
#'
#' @param data A dataframe containing standardized PCL-5 item scores (output of
#'   rename_ptsd_columns). Each symptom should be scored on a 0-4 scale where:
#'
#' \itemize{
#'   \item 0 = Not at all
#'   \item 1 = A little bit
#'   \item 2 = Moderately
#'   \item 3 = Quite a bit
#'   \item 4 = Extremely
#'}
#'
#' @returns A dataframe with all original columns plus an additional column "total"
#'   containing the sum of all 20 symptom scores (range: 0-80)
#'
#' @export
#'
#' @importFrom dplyr mutate select
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
#' # Create sample data
#' sample_data <- data.frame(
#'   matrix(sample(0:4, 20 * 10, replace = TRUE),
#'          nrow = 10,
#'          ncol = 20)
#' )
#' colnames(sample_data) <- paste0("symptom_", 1:20)
#'
#' # Calculate total scores
#' scores_with_total <- calculate_ptsd_total(sample_data)
#' print(scores_with_total$total)
#'
calculate_ptsd_total <- function(data) {
  # Validate column names
  expected_names <- paste0("symptom_", 1:20)
  if (!all(expected_names %in% colnames(data))) {
    stop("Data must contain columns named 'symptom_1' through 'symptom_20'")
  }

  # Validate data type and range
  if (!all(vapply(data[expected_names], is.numeric, logical(1)))) {
    stop("All symptom columns must contain numeric values")
  }

  invalid_values <- !all(vapply(data[expected_names], function(x)
    all(x >= 0 & x <= 4 & x == floor(x)), logical(1)))
  if (invalid_values) {
    stop("All symptom values must be integers between 0 and 4")
  }

  data %>%
    dplyr::mutate(total = rowSums(dplyr::select(data,paste0("symptom_", 1:20))))
}


#' Determine PTSD diagnosis based on DSM-5 criteria using non-binarized scores
#'
#' @description
#' Determines whether DSM-5 diagnostic criteria for PTSD are met based on PCL-5
#' item scores, using the original non-binarized values (0-4 scale).
#'
#' @details
#' The function applies the DSM-5 diagnostic criteria for PTSD:
#'
#' \itemize{
#' \item Criterion B (Intrusion): At least 1 symptom >= 2 from items 1-5
#' \item Criterion C (Avoidance): At least 1 symptom >= 2 from items 6-7
#' \item Criterion D (Negative alterations in cognitions and mood):
#'   At least 2 symptoms >= 2 from items 8-14
#' \item Criterion E (Alterations in arousal and reactivity):
#'   At least 2 symptoms >= 2 from items 15-20
#'}
#'
#' A symptom is considered present when rated 2 (Moderately) or higher.
#'
#' @param data A dataframe that can be either:
#'
#' \itemize{
#'   \item Output of rename_ptsd_columns(): 20 columns named symptom_1 to symptom_20
#'   \item Output of calculate_ptsd_total(): 21 columns including symptom_1 to
#'     symptom_20 plus a 'total' column
#'}
#'
#'   Each symptom should be scored on a 0-4 scale where:
#'
#' \itemize{
#'   \item 0 = Not at all
#'   \item 1 = A little bit
#'   \item 2 = Moderately
#'   \item 3 = Quite a bit
#'   \item 4 = Extremely
#'}
#'
#' @returns A dataframe with all original columns (including 'total' if present)
#'   plus an additional column "PTSD_Diagnosis" containing TRUE/FALSE values
#'   indicating whether DSM-5 diagnostic criteria are met
#'
#' @export
#'
#' @examples
#' # Example with output from rename_ptsd_columns
#' sample_data1 <- data.frame(
#'   matrix(sample(0:4, 20 * 10, replace = TRUE),
#'          nrow = 10,
#'          ncol = 20)
#' )
#' colnames(sample_data1) <- paste0("symptom_", 1:20)
#' diagnosed_data1 <- create_ptsd_diagnosis_nonbinarized(sample_data1)
#'
#' # Check diagnosis results
#' diagnosed_data1$PTSD_Diagnosis
#'
#' # Example with output from calculate_ptsd_total
#' sample_data2 <- calculate_ptsd_total(sample_data1)
#' diagnosed_data2 <- create_ptsd_diagnosis_nonbinarized(sample_data2)
#'
#' # Check diagnosis results
#' diagnosed_data2$PTSD_Diagnosis
#'
create_ptsd_diagnosis_nonbinarized <- function(data) {
  # Validate column names
  expected_cols <- paste0("symptom_", 1:20)
  if (!all(expected_cols %in% colnames(data))) {
    stop("Data must contain columns named 'symptom_1' through 'symptom_20'")
  }

  # Validate that columns are numeric
  symptom_cols <- data[, expected_cols]
  if (!all(vapply(symptom_cols, is.numeric, logical(1)))) {
    stop("All symptom columns must contain numeric values")
  }

  # Validate value range (0-4) and check for missing values
  invalid_values <- !all(vapply(symptom_cols, function(x)
    all(x >= 0 & x <= 4 & x == floor(x) & !is.na(x)), logical(1)))
  if (invalid_values) {
    stop("All symptom values must be integers between 0 and 4 with no missing values")
  }

  # Check if 'total' column exists and is numeric (since function can accept output from calculate_ptsd_total)
  if ("total" %in% colnames(data)) {
    if (!is.numeric(data$total)) {
      stop("If present, 'total' column must contain numeric values")
    }
  }

  criteria <- list(
    A = rowSums(data[, paste0("symptom_", 1:5)] >= 2) >= 1,
    B = rowSums(data[, paste0("symptom_", 6:7)] >= 2) >= 1,
    C = rowSums(data[, paste0("symptom_", 8:14)] >= 2) >= 2,
    D = rowSums(data[, paste0("symptom_", 15:20)] >= 2) >= 2
  )

  data$PTSD_Diagnosis <- Reduce(`&`, criteria)
  return(data)
}


#' Summarize PTSD scores and diagnoses
#'
#' @description
#' Creates a summary of PCL-5 total scores and PTSD diagnoses, including mean
#' total score, standard deviation, and number of positive diagnoses.
#'
#' @details
#' This function calculates key summary statistics for PCL-5 data:
#'
#' \itemize{
#' \item Mean total score (severity indicator)
#' \item Standard deviation of total scores (variability in severity)
#' \item Count of positive PTSD diagnoses (prevalence in the sample)
#'}
#'
#' @param data A dataframe containing at minimum:
#'
#' \itemize{
#'   \item A 'total' column with PCL-5 total scores (from calculate_ptsd_total)
#'   \item A 'PTSD_Diagnosis' column with TRUE/FALSE values (from
#'     determine_ptsd_diagnosis)
#'}
#'
#' @returns A dataframe with one row containing:
#'
#' \itemize{
#'   \item mean_total: Mean PCL-5 total score
#'   \item sd_total: Standard deviation of PCL-5 total scores
#'   \item n_diagnosed: Number of positive PTSD diagnoses
#'}
#'
#' @export
#'
#' @importFrom dplyr summarise
#' @importFrom stats sd
#' @importFrom magrittr %>%
#'
#' @examples
#' # Create sample data
#' sample_data <- data.frame(
#'   total = sample(0:80, 100, replace = TRUE),
#'   PTSD_Diagnosis = sample(c(TRUE, FALSE), 100, replace = TRUE)
#' )
#'
#' # Generate summary statistics
#' summary_stats <- summarize_ptsd(sample_data)
#' print(summary_stats)
#'
summarize_ptsd <- function(data) {
  # Check if required columns exist
  if (!all(c("total", "PTSD_Diagnosis") %in% colnames(data))) {
    stop("Data must contain both 'total' and 'PTSD_Diagnosis' columns")
  }

  # Validate total column
  if (!is.numeric(data$total)) {
    stop("'total' column must contain numeric values")
  }

  # Check for missing values in total
  if (any(is.na(data$total))) {
    stop("'total' column contains missing values")
  }

  # Validate total score range (should be 0-80 as each item is 0-4 and there are 20 items)
  if (any(data$total < 0 | data$total > 80)) {
    stop("'total' column contains invalid values (must be between 0 and 80)")
  }

  # Validate PTSD_Diagnosis column
  if (!is.logical(data$PTSD_Diagnosis)) {
    stop("'PTSD_Diagnosis' column must contain logical (TRUE/FALSE) values")
  }

  # Check for missing values in diagnosis
  if (any(is.na(data$PTSD_Diagnosis))) {
    stop("'PTSD_Diagnosis' column contains missing values")
  }

  data %>%
    dplyr::summarise(
      mean_total = mean(.data$total),
      sd_total = stats::sd(.data$total),
      n_diagnosed = sum(.data$PTSD_Diagnosis)
    )
}
