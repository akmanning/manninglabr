#' @title Get person IDs with drug exposure
#' @description This function returns a vector of person IDs with drug exposure
#' @param query_result_path The path to the query result
#' @param exclude_inpatient Whether to exclude inpatient visits
#' @return A vector of person IDs with drug exposure
#' @export
get_person_ids_with_drug_exposure <- function(query_result_path, exclude_inpatient = FALSE) {
  dataset_condition_df <- read_drug_exposure_bq_export_from_workspace_bucket(query_result_path)

  if (exclude_inpatient) {
    inpatient_visits_text <- "Emergency Room Visit
Inpatient Visit
Emergency Room - Hospital
Emergency Room and Inpatient Visit
Emergency Room Visit
Hospital
Inpatient Hospital
Inpatient Visit
Urgent Care Facility
Emergency Room and Inpatient Visit
Emergency Room Visit
Hospital
Inpatient Hospital
Inpatient Visit
"
    inpatient_visit_occurrence_concept_name <- unique(stringr::str_split_1(inpatient_visits_text, pattern = "\n"))
    print(paste("Before filtering inpatient visits, there are", length(unique(dataset_condition_df$person_id)), "unique study IDs."))
    dataset_condition_df <- dataset_condition_df %>% dplyr::filter(!visit_occurrence_concept_name %in% inpatient_visit_occurrence_concept_name)
    print(paste("After filtering inpatient visits, there are", length(unique(dataset_condition_df$person_id)), "unique study IDs."))
  }
  return(unique(dataset_condition_df$person_id))
}
