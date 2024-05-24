#' create a dataset with drug exposure data for a given OMOP concept ID
#' @param OMOPCONCEPTID the OMOP concept ID for the drug exposure
#' @param query_sql the SQL query to use to create the dataset
#' @return a list with the OMOP concept ID, the query SQL, and the path to the query results
#' @export

create_drug_exposure_dataset_with_OMOP_CONCEPT_ID <- function(OMOPCONCEPTID, query_sql = drug_exposure_query_sql) {
  query_sql <- query_sql %>% stringr::str_replace_all("XYZ_OMOPCONCEPTID_XYZ", as.character(OMOPCONCEPTID))

  query_result_path <- file.path(
    Sys.getenv("WORKSPACE_BUCKET"),
    "bq_exports",
    "drug_exposure_OMOP_ID_XYZ_OMOPCONCEPTID_XYZ",
    "drug_exposure_OMOP_ID_XYZ_OMOPCONCEPTID_XYZ_*.csv"
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


  return(list(OMOPCONCEPTID = OMOPCONCEPTID, query_sql = query_sql, query_result_path = query_result_path))
}



utils::globalVariables(c("drug_exposure_query_sql", "condition_query_sql"))

