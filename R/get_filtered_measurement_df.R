#' @title get_filtered_measurement_df
#' @description This function takes an data frame created by the create_measurement_dataset_with_OMOP_CONCEPT_ID function and applies the filters operator_concept_name, unit_concept_name, visit_occurrence_concept_name and measurement_type_concept_name
#' @param dataset_measurement_df Data frame as output from create_measurement_dataset_with_OMOP_CONCEPT_ID function
#' @param filter_unit_concept_name list of string values for filtering. example c("value1", "value2")
#' @param filter_operator_concept_name  list of string values for filtering. example c("value1", "value2")
#' @param filter_visit_occurrence_concept_name  list of string values for filtering. example c("value1", "value2")
#' @param filter_measurement_type_concept_name  list of string values for filtering. example c("value1", "value2")
#' @return A data frame with the filtered values
#' @export
get_filtered_measurement_df <- function(dataset_measurement_df,filter_unit_concept_name=NULL, filter_operator_concept_name=NULL, filter_visit_occurrence_concept_name=NULL, filter_measurement_type_concept_name=NULL) {
  dataset_measurement_df_tmp <- dataset_measurement_df

  if(length(filter_unit_concept_name)>0) {
    dataset_measurement_df_tmp <- dataset_measurement_df_tmp %>% filter(unit_concept_name %in% filter_unit_concept_name)
  }

  if(length(filter_operator_concept_name)>0) {
    dataset_measurement_df_tmp <- dataset_measurement_df_tmp %>% filter(operator_concept_name %in% filter_operator_concept_name)
  }

  if(length(filter_visit_occurrence_concept_name)>0) {
    dataset_measurement_df_tmp <- dataset_measurement_df_tmp %>% filter(visit_occurrence_concept_name %in% filter_visit_occurrence_concept_name)
  }

  if(length(filter_measurement_type_concept_name)>0) {
    dataset_measurement_df_tmp <- dataset_measurement_df_tmp %>% filter(measurement_type_concept_name %in% filter_measurement_type_concept_name)
  }

  return(dataset_measurement_df_tmp)
}
