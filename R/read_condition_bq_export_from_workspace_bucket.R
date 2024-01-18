#' @title Read condition dataset from workspace bucket
#' @description
#' This function reads a condition dataset from the workspace bucket.
#' @param export_path The path to the query result.
#' @return A tibble of condition data.
#' @export
read_condition_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- readr::cols(standard_concept_name = readr::col_character(),
                           standard_concept_code = readr::col_character(),
                           standard_vocabulary = readr::col_character(),
                           condition_type_concept_name = readr::col_character(),
                           stop_reason = readr::col_character(),
                           visit_occurrence_concept_name = readr::col_character(),
                           condition_source_value = readr::col_character(),
                           source_concept_name = readr::col_character(),
                           source_concept_code = readr::col_character(),
                           source_vocabulary = readr::col_character(),
                           condition_status_source_value = readr::col_character(),
                           condition_status_concept_name = readr::col_character())
  dplyr::bind_rows(
    purrr::map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
               function(csv) {
                 message(stringr::str_glue('Loading {csv}.'))
                 chunk <- readr::read_csv(pipe(stringr::str_glue('gsutil cat {csv}')),
                                          col_types = col_types, show_col_types = FALSE)
                 if (is.null(col_types)) {
                   col_types <- stats::spec(chunk)
                 }
                 chunk
               }))
}
