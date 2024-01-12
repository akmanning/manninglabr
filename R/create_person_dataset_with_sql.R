#' Create a person dataset
#' @param dataset_person_sql SQL query to create the dataset
#' @param label the label for the dataset
#' @return a list with the OMOP concept ID, the query SQL, and the path to the query results
#' @export
#'
create_person_dataset_with_sql <- function(dataset_person_sql, label) {
  # remove whitespace from the label
  label <- stringr::str_replace_all(label, pattern = " ", replacement = "_")

  query_result_path <- file.path(
    Sys.getenv("WORKSPACE_BUCKET"),
    "bq_exports",
    "person_df_XYZ_LABEL_XYZ",
    "person_df_XYZ_LABEL_XYZ_*.csv")
  query_result_path <- stringr::str_replace_all(query_result_path,"XYZ_LABEL_XYZ", label)

  message(stringr::str_glue(
    "The data will be written to {query_result_path}. Use this path when reading ",
    "the data into your notebooks in the future."
  ))

  if (system(paste0("gsutil ls ", query_result_path))) {
    print("Data set has not been previously created. Creating now.")

    bigrquery::bq_table_save(
      bigrquery::bq_dataset_query(Sys.getenv("WORKSPACE_CDR"),
        dataset_person_sql,
        billing = Sys.getenv("GOOGLE_PROJECT")
      ),
      query_result_path,
      destination_format = "CSV"
    )
  } else {
    print("Data set has been previously created.")
  }

  col_types <- readr::cols(
    gender = readr::col_character(),
    race = readr::col_character(),
    ethnicity = readr::col_character(),
    sex_at_birth = readr::col_character()
  )

  dataset_person_df <- dplyr::bind_rows(
    purrr::map(
      system2("gsutil", args = c("ls", query_result_path), stdout = TRUE, stderr = TRUE),
      function(csv) {
        message(stringr::str_glue("Loading {csv}."))
        chunk <- readr::read_csv(pipe(str_glue("gsutil cat {csv}")),
          col_types = col_types,
          show_col_types = FALSE
        )
        if (is.null(col_types)) {
          col_types <- spec(chunk)
        }
        chunk
      }
    )
  )

  dataset_person_df <- dplyr::mutate(dataset_person_df,age = lubridate::year(today()) - lubridate::year(date_of_birth))

    return(list(
    label = label,
    dataset_person_sql = dataset_person_sql,
    dataset_person_df = dataset_person_df,
    query_result_path = query_result_path
  ))
}
