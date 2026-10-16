#' Dry run a person dataset SQL query
#'
#' Validates a query intended for [create_person_dataset_with_sql()] without
#' executing it, writing any data, or incurring query costs. BigQuery checks the
#' syntax, table and column names, and reports the number of bytes the query
#' would process.
#'
#' @param dataset_person_sql SQL query to validate
#' @return invisibly, a list with `valid` (logical), `bytes_processed` (numeric,
#'   `NA` if the query is invalid), and `message` (the BigQuery output)
#' @export
#'
dry_run_person_dataset_with_sql <- function(dataset_person_sql) {
  # WORKSPACE_CDR is "project.dataset"; the bq CLI expects "project:dataset"
  default_dataset <- sub("\\.", ":", Sys.getenv("WORKSPACE_CDR"))

  output <- suppressWarnings(system2(
    "bq",
    args = c(
      "query",
      "--use_legacy_sql=false",
      "--dry_run",
      paste0("--project_id=", Sys.getenv("GOOGLE_PROJECT")),
      paste0("--dataset_id=", default_dataset)
    ),
    input = dataset_person_sql,
    stdout = TRUE,
    stderr = TRUE
  ))
  valid <- is.null(attr(output, "status")) || attr(output, "status") == 0
  message_text <- paste(output, collapse = "\n")

  bytes_processed <- NA_real_
  if (valid) {
    bytes_processed <- as.numeric(
      stringr::str_match(message_text, "process (\\d+) bytes")[, 2]
    )
    message(stringr::str_glue(
      "Query is valid. It would process {format(bytes_processed, big.mark = ',')} ",
      "bytes ({round(bytes_processed / 1e9, 2)} GB)."
    ))
  } else {
    message("Query is NOT valid:\n", message_text)
  }

  invisible(list(
    valid = valid,
    bytes_processed = bytes_processed,
    message = message_text
  ))
}
