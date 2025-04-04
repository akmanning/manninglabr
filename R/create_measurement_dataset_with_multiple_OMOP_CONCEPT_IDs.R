#' @title Create measurement dataset with multiple OMOP_CONCEPT_IDs
#' @description
#' This function creates a dataset of measurement data with multiple OMOP_CONCEPT_IDs.
#' @param OMOPCONCEPTIDs The OMOP_CONCEPT_IDs of the measurements to be queried.
#' @param file_label The label for the file to be created.
#' @param query_sql The SQL query to be used to create the dataset. Default is measurement_query_sql.
#' @return A list containing the OMOPCONCEPTID, query_sql, query_result_path, and dataset_measurement_df.
#' @export
create_measurement_dataset_with_multiple_OMOP_CONCEPT_IDs <- function(OMOPCONCEPTIDs, file_label, query_sql = measurement_query_sql) {
  query_sql <- query_sql %>% stringr::str_replace_all("XYZ_OMOPCONCEPTID_XYZ", as.character(OMOPCONCEPTIDs))
  
  query_result_path <- file.path(
    Sys.getenv("WORKSPACE_BUCKET"),
    "bq_exports",
    paste0("measurement_OMOP_IDs_", file_label),
    paste0("measurement_OMOP_IDs_", file_label, "_*.csv")
  )
  
  message(stringr::str_glue(
    "The data will be written to {query_result_path}. Use this path when reading ",
    "the data into your notebooks in the future."
  ))
  
  if (system(paste0("gsutil ls ", query_result_path))) {
    print("Data set has not been previously created. Creating now.")
    bigrquery::bq_table_save(
      bigrquery::bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), query_sql,
                                  billing = Sys.getenv("GOOGLE_PROJECT")
      ),
      query_result_path,
      destination_format = "CSV"
    )
  } else {
    print("Data set has been previously created.")
  }
  
  dataset_measurement_df <- read_measurement_bq_export_from_workspace_bucket(query_result_path)
  
  return(list(OMOPCONCEPTID = file_label, query_sql = query_sql, query_result_path = query_result_path, dataset_measurement_df = dataset_measurement_df))
}
