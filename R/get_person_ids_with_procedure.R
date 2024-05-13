#' @title Get the IDs of the people in a procedure dataset
#' @description
#' This function returns the person_ids of the people in a procedure dataset.
#' @param query_result_path The path to the query result.
#' @return A vector of person_ids.
#' @export
get_person_ids_with_procedure <-  function(query_result_path) {
  dataset_procedure_df <- read_procedure_bq_export_from_workspace_bucket(query_result_path)
  if (nrow(dataset_procedure_df) == 0) {
    print("No data found. Please check your OMOP_CONCEPT_ID.")
  } else {
    print(paste("There are", nrow(dataset_procedure_df), "rows in the dataset."))
    print(paste("There are", length(unique(dataset_procedure_df$person_id)), "unique study IDs."))
  }
  return(unique(dataset_procedure_df$person_id))
}
