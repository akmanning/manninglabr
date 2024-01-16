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

# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {drug_48622269_path}` to copy these files
#       to the Jupyter disk.
read_drug_exposure_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(standard_concept_name = col_character(), standard_concept_code = col_character(), standard_vocabulary = col_character(), drug_type_concept_name = col_character(), stop_reason = col_character(), sig = col_character(), route_concept_name = col_character(), lot_number = col_character(), visit_occurrence_concept_name = col_character(), drug_source_value = col_character(), source_concept_name = col_character(), source_concept_code = col_character(), source_vocabulary = col_character(), route_source_value = col_character(), dose_unit_source_value = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}

utils::globalVariables(c("drug_exposure_query_sql", "drug_exposure_OMOP_ID_48622269_query_sql"))

