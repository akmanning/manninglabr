#' @title get_measaurement_table_summary
#' @description This function takes an OMOP Concept ID in the Measurement domain returns a summary of the non-missing values stratified by standard_concept_name, measurement_type_concept_name, operator_concept_name,unit_concept_name, and visit_occurrence_concept_name.
#' @return A data frame with the following columns: standard_concept_name, measurement_type_concept_name, operator_concept_name, unit_concept_name, visit_occurrence_concept_name, and summary statistics.
#' @param dataset_measurement_df Data frame as output from create_measurement_dataset_with_OMOP_CONCEPT_ID function
#' @param filter_unit_concept_name list of string values for filtering. example c("value1", "value2")
#' @param filter_operator_concept_name  list of string values for filtering. example c("value1", "value2")
#' @param filter_visit_occurrence_concept_name  list of string values for filtering. example c("value1", "value2")
#' @param filter_measurement_type_concept_name  list of string values for filtering. example c("value1", "value2")
#' @export
get_measurement_table_summary <- function(dataset_measurement_df,filter_unit_concept_name=NULL, filter_operator_concept_name=NULL, filter_visit_occurrence_concept_name=NULL, filter_measurement_type_concept_name=NULL) {
  dataset_measurement_df_tmp <- dataset_measurement_df

  if(length(filter_unit_concept_name)>0) {
  dataset_measurement_df_tmp <- dataset_measurement_df_tmp %>% filter(unit_concept_name %in% filter_unit_concept_name)
    }
  print(table("unit_concept_name"=dataset_measurement_df_tmp$unit_concept_name,useNA="always"))

  if(length(filter_operator_concept_name)>0) {
  dataset_measurement_df_tmp <- dataset_measurement_df_tmp %>% filter(operator_concept_name %in% filter_operator_concept_name)
    }
  print(table("operator_concept_name"=dataset_measurement_df_tmp$operator_concept_name,useNA="always"))


    if(length(filter_visit_occurrence_concept_name)>0) {
      print("Filtering by visit_occurrence_concept_name")
  dataset_measurement_df_tmp <- dataset_measurement_df_tmp %>% filter(visit_occurrence_concept_name %in% filter_visit_occurrence_concept_name)
    }
  print(table("visit_concept_name"=dataset_measurement_df_tmp$visit_occurrence_concept_name,useNA="always"))

    if(length(filter_measurement_type_concept_name)>0) {
  dataset_measurement_df_tmp <- dataset_measurement_df_tmp %>% filter(measurement_type_concept_name %in% filter_measurement_type_concept_name)
    }
    print(table("measurement_concept_name"=dataset_measurement_df_tmp$measurement_type_concept_name,useNA="always"))


  # Get the summary of the non-missing values stratified by standard_concept_name, measurement_type_concept_name, operator_concept_name,unit_concept_name, and visit_occurrence_concept_name
  dataset_measurement_summary <- dataset_measurement_df_tmp %>%
    group_by(unit_concept_name, operator_concept_name,
             visit_occurrence_concept_name,
             measurement_type_concept_name) %>%
    summarise(
      count = n(),
      missing = sum(is.na(value_as_number)),
      n.unique = length(unique(person_id)),
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
