#' @title Create a measurement dataset with OMOP_CONCEPT_ID
#' @description
#' This function takes an OMOP Concept ID in the Measurement domain and returns a dataset with the following columns: person_id, measurement_concept_id, measurement_date, measurement_datetime, measurement_type_concept_id, operator_concept_id, value_as_number, value_as_concept_id, unit_concept_id, range_low, range_high, and visit_occurrence_concept_id.
#' @param OMOPCONCEPTID The OMOP Concept ID of the measurement of interest.
#' @param query_sql = measurement_query_sql
#' @return A list containing the OMOPCONCEPTID, query_sql, and query_result_path.
#' @export

create_measurement_dataset_with_OMOP_CONCEPT_ID <- function(OMOPCONCEPTID, query_sql) {
  # Create a query to get the measurement dataset with OMOP_CONCEPT_ID
  query_sql <- query_sql %>%
    stringr::str_replace_all("XYZ_OMOPCONCEPTID_XYZ", as.character(OMOPCONCEPTID))

  # Define the query result path
  query_result_path <- file.path(
    Sys.getenv("WORKSPACE_BUCKET"),
    "bq_exports",
    "measurement_OMOP_ID_XYZ_OMOPCONCEPTID_XYZ",
    "measurement_OMOP_ID_XYZ_OMOPCONCEPTID_XYZ_*.csv"
  ) %>% stringr::str_replace_all("XYZ_OMOPCONCEPTID_XYZ", as.character(OMOPCONCEPTID))

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
    # Read the measurement dataset
  dataset_measurement_df <- read_measurement_bq_export_from_workspace_bucket(query_result_path)

  # Return a list containing the OMOP_CONCEPT_ID, query_sql, query_result_path and dataset_measurement_df
  return(list(OMOPCONCEPTID = OMOPCONCEPTID, query_sql = query_sql, query_result_path = query_result_path,dataset_measurement_df=dataset_measurement_df))
}
