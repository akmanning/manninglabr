#' Create a person dataset
#' @param dataset_person_sql SQL query to create the dataset
#' @param label the label for the dataset
#' @param mount_path The full file system path to the mounted workspace bucket
#' @param col_types Result of readr::cols() setting the column types of any desired columns. See example
#' @return a list with the OMOP concept ID, the query SQL, and the path to the query results
#' @export
#' @examples
#' \dontrun{res <- create_person_dataset_with_sql(
#' sql, "all participants with earliest visit",
#'  mount_path = "/home/jupyter/workspace/bucket-for-define-t2d-trait-harmonization"
#' )}
#' col_types <- readr::cols(gender = readr::col_character(), 
#'        race = readr::col_character(), ethnicity = readr::col_character(), 
#'        sex_at_birth = readr::col_character())
create_person_dataset_with_sql <- function(dataset_person_sql, label,
                                           mount_path = NULL,
                                           col_types = NULL) {
  label <- stringr::str_replace_all(label, " ", "_")

  # Same relative location under both the gs:// bucket and the mount
  rel_dir   <- file.path("bq_exports", paste0("person_df_", label))
  file_glob <- paste0("person_df_", label, "_*.csv")

  # BigQuery writes here (must be gs://)
  gcs_uri <- file.path(Sys.getenv("WORKSPACE_BUCKET"), rel_dir, file_glob)

  # We read here (mount if given, otherwise gsutil on the gs:// URI)
  local_glob <- if (!is.null(mount_path)) file.path(mount_path, rel_dir, file_glob)

  message(stringr::str_glue("The data will be written to {gcs_uri}"))

  existing <- if (!is.null(mount_path)) {
    Sys.glob(local_glob)                              # file.exists() can't expand "*"
  } else {
    suppressWarnings(system2("gsutil", c("ls", gcs_uri), stdout = TRUE, stderr = FALSE))
  }

  if (length(existing) == 0) {
    message("Data set has not been previously created. Creating now.")
    tb <- bigrquery::bq_dataset_query(
      Sys.getenv("WORKSPACE_CDR"),
      dataset_person_sql,
      billing = Sys.getenv("GOOGLE_PROJECT")
    )
    bigrquery::bq_table_save(tb, gcs_uri, destination_format = "CSV")

    existing <- if (!is.null(mount_path)) Sys.glob(local_glob) else
      system2("gsutil", c("ls", gcs_uri), stdout = TRUE)
  } else {
    message("Data set has been previously created.")
  }

  if (is.null(col_types)) {
    col_types <- col_types_from_query(dataset_person_sql)
  }

  read_chunk <- function(f) {
    message(stringr::str_glue("Loading {f}."))
    src <- if (!is.null(mount_path)) f else pipe(paste("gsutil cat", f))
    readr::read_csv(src, col_types = col_types, show_col_types = FALSE)
  }
  
  dataset_person_df <- dplyr::bind_rows(purrr::map(existing, read_chunk)) 

  list(label = label,
       dataset_person_sql = dataset_person_sql,
       dataset_person_df = dataset_person_df,
       query_result_path = if (!is.null(mount_path)) local_glob else gcs_uri)
}
