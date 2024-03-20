#' @title Read observation domain BQ export from workspace bucket
#' @description Read observation domain BQ export from workspace bucket
#' @param export_path The path to the query result.
#' @return A tibble of drug exposure data.
#' @export
read_observation_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(standard_concept_name = col_character(), standard_concept_code = col_character(), standard_vocabulary = col_character(), observation_type_concept_name = col_character(), value_as_string = col_character(), value_as_concept_name = col_character(), qualifier_concept_name = col_character(), unit_concept_name = col_character(), visit_occurrence_concept_name = col_character(), observation_source_value = col_character(), source_concept_name = col_character(), source_concept_code = col_character(), source_vocabulary = col_character(), unit_source_value = col_character(), qualifier_source_value = col_character(), value_source_value = col_character())
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
