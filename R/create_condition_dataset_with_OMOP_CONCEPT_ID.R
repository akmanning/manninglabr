
#' @title Create condition dataset with OMOP_CONCEPT_ID
#' @description
#' This function creates a dataset of condition data with a specific OMOP_CONCEPT_ID.
#' @param OMOPCONCEPTID The OMOP_CONCEPT_ID of the condition to be queried.
#' @param query_sql The SQL query to be used to create the dataset.
#' @return A list containing the OMOPCONCEPTID, query_sql, and query_result_path.
#' @export
create_condition_dataset_with_OMOP_CONCEPT_ID <- function(OMOPCONCEPTID, query_sql = condition_query_sql) {
  query_sql <- query_sql %>%
    stringr::str_replace_all("XYZ_OMOPCONCEPTID_XYZ", as.character(OMOPCONCEPTID))

  query_result_path <- file.path(
    Sys.getenv("WORKSPACE_BUCKET"),
    "bq_exports",
    "condition_OMOP_ID_XYZ_OMOPCONCEPTID_XYZ",
    "condition_OMOP_ID_XYZ_OMOPCONCEPTID_XYZ_*.csv"
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

utils::globalVariables(c("condition_query_sql"))
