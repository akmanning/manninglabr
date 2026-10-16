#' @title Create condition occurrences dataset with OMOP_CONCEPT_ID
#' @description
#' Creates a dataset of condition occurrences for a given OMOP_CONCEPT_ID.
#' @param OMOPCONCEPTID The OMOP_CONCEPT_ID of the condition to be queried.
#' @param query_sql The SQL query used to create the dataset.
#' @param read_from_workspace_bucket The function that will read in the data frame given a query_result_path
#' @return A list containing OMOPCONCEPTID, query_sql, query_result_path,
#'   and the resulting data frame.
#' @export
create_condition_occurrences_dataset_with_OMOP_CONCEPT_ID <- function(
    OMOPCONCEPTID,
    query_sql,
    read_from_workspace_bucket) {

   query_sql <- query_sql %>%
    stringr::str_replace_all("XYZ_OMOPCONCEPTID_XYZ", as.character(OMOPCONCEPTID))

  query_result_path <- file.path(
    Sys.getenv("WORKSPACE_BUCKET"),
    "bq_exports",
    "condition_occurrences_OMOP_ID_XYZ_OMOPCONCEPTID_XYZ",
    "condition_occurrences_OMOP_ID_XYZ_OMOPCONCEPTID_XYZ_*.csv"
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
  dataset_condition_occurrences_df <- read_from_workspace_bucket(query_result_path)

  return(list(OMOPCONCEPTID = OMOPCONCEPTID, query_sql = query_sql, query_result_path = query_result_path, dataset_condition_occurrences_df=dataset_condition_occurrences_df))

}
