#' @title get_measaurement_table_summary
#' @description This function takes an OMOP Concept ID in the Measurement domain returns a summary of the non-missing values stratified by standard_concept_name, measurement_type_concept_name, operator_concept_name,unit_concept_name, and visit_occurrence_concept_name.
#' @return A data frame with the following columns: standard_concept_name, measurement_type_concept_name, operator_concept_name, unit_concept_name, visit_occurrence_concept_name, and summary statistics.
#' @param query_result_path The path to the query result.
#' @export
#'
get_measurement_table_summary <- function(query_result_path) {
  # Read the measurement dataset
  dataset_measurement_df <- read_measurement_bq_export_from_workspace_bucket(query_result_path)
  # Get the summary of the non-missing values stratified by standard_concept_name, measurement_type_concept_name, operator_concept_name,unit_concept_name, and visit_occurrence_concept_name
  dataset_measurement_summary <- dataset_measurement_df %>%
    group_by(standard_concept_name, measurement_type_concept_name, operator_concept_name, unit_concept_name, visit_occurrence_concept_name) %>%
    summarise(
      count = n(),
      mean = mean(value_as_number, na.rm = TRUE),
      sd = sd(value_as_number, na.rm = TRUE),
      median = median(value_as_number, na.rm = TRUE),
      min = min(value_as_number, na.rm = TRUE),
      quantile_5 = quantile(value_as_number, 0.05, na.rm = TRUE),
      quantile_25 = quantile(value_as_number, 0.25, na.rm = TRUE),
      quantile_75 = quantile(value_as_number, 0.75, na.rm = TRUE),
      quantile_95 = quantile(value_as_number, 0.95, na.rm = TRUE),
      max = max(value_as_number, na.rm = TRUE)
    )
  return(dataset_measurement_summary)
}
