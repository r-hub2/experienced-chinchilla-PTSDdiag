#' Rename PTSD symptom (= PCL-5 item) columns
#'
#' @description
#' Standardizes column names in PCL-5 (PTSD Checklist for DSM-5) data by renaming
#' them to a consistent format (symptom_1 through symptom_20). This standardization
#' is essential for subsequent analyses using other functions in the package.
#'
#' @details
#' The function assumes the input data contains exactly 20 columns corresponding to
#' the 20 items of the PCL-5. The columns are renamed sequentially from symptom_1
#' to symptom_20, maintaining their original order. The PCL-5 items correspond to
#' different symptom clusters:
#'
#' \itemize{
#' \item symptom_1 to symptom_5: Intrusion symptoms (Criterion B)
#' \item symptom_6 to symptom_7: Avoidance symptoms (Criterion C)
#' \item symptom_8 to symptom_14: Negative alterations in cognitions and mood (Criterion D)
#' \item symptom_15 to symptom_20: Alterations in arousal and reactivity (Criterion E)
#'}
#'
#' @param data A dataframe containing exactly 20 columns, where each column
#'   represents a PCL-5 item score. The scores should be on a 0-4 scale where:
#'
#' \itemize{
#'   \item 0 = Not at all
#'   \item 1 = A little bit
#'   \item 2 = Moderately
#'   \item 3 = Quite a bit
#'   \item 4 = Extremely
#'}
#'
#' @returns A dataframe with the same data but renamed columns following the pattern
#'   'symptom_1' through 'symptom_20'
#'
#' @export
#'
#' @importFrom dplyr rename_with
#' @importFrom magrittr %>%
#'
#' @examples
#' # Example with a sample PCL-5 dataset
#' sample_data <- data.frame(
#'   matrix(sample(0:4, 20 * 10, replace = TRUE),
#'          nrow = 10,
#'          ncol = 20)
#' )
#' renamed_data <- rename_ptsd_columns(sample_data)
#' colnames(renamed_data)  # Shows new column names
#'
rename_ptsd_columns <- function(data) {
  # Validate number of columns
  if (ncol(data) != 20) {
    stop("Data must contain exactly 20 columns (one for each PCL-5 item)")
  }
  # Check for missing values
  if (any(is.na(data))) {
    stop("Data contains missing values (NA). All PCL-5 items must be rated")
  }

  # Validate data type and range
  if (!all(vapply(data, is.numeric, logical(1)))) {
    stop("All columns must contain numeric values")
  }

  invalid_values <- !all(vapply(data, function(x) all(x >= 0 & x <= 4 & x == floor(x)), logical(1)))
  if (invalid_values) {
    stop("All values must be integers between 0 and 4")
  }

  data %>%
      rename_with(~ paste0("symptom_", 1:20))
}
