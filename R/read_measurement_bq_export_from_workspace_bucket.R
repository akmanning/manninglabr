#' @title Read drug exposure dataset from workspace bucket
#' @description
#' This function reads a drug exposure dataset from the workspace bucket.
#' @param export_path The path to the query result.
#' @return A tibble of drug exposure data.
#' @export
read_measurement_bq_export_from_workspace_bucket <- function(export_path) {

  col_types <- readr::cols(standard_concept_name = readr::col_character(),
                    standard_concept_code = readr::col_character(),
                    standard_vocabulary = readr::col_character(),
                    measurement_type_concept_name = readr::col_character(),
                    operator_concept_name = readr::col_character(),
                    value_as_concept_name = readr::col_character(),
                    unit_concept_name = readr::col_character(),
                    visit_occurrence_concept_name = readr::col_character(),
                    measurement_source_value = readr::col_character(),
                    source_concept_name = readr::col_character(),
                    source_concept_code = readr::col_character(),
                    source_vocabulary = readr::col_character(),
                    unit_source_value = readr::col_character(),
                    value_source_value = readr::col_character())
  dplyr::bind_rows(
    purrr::map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(stringr::str_glue('Loading {csv}.'))
          chunk <- readr::read_csv(pipe(stringr::str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- readr::spec(chunk)
          }
          chunk
        }))
}
