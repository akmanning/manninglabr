#' Direct translation of the R function read_bq_export_from_workspace_bucket from the All of Us Research Workbench.`
#' @param export_path The path to the export directory in the workspace bucket.
#' @return A data frame with the contents of the export.
#' This query represents dataset "All Participants" for domain "person" and was generated for All of Us Controlled Tier Dataset v7
#' @export
#' @examples
#' dataset_77466240_person_df <- read_bq_export_from_workspace_bucket(person_77466240_path)
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(gender = col_character(), race = col_character(), ethnicity = col_character(), sex_at_birth = col_character())
  bind_rows(
    map(
      system2("gsutil", args = c("ls", export_path), stdout = TRUE, stderr = TRUE),
      function(csv) {
        message(str_glue("Loading {csv}."))
        chunk <- read_csv(pipe(str_glue("gsutil cat {csv}")), col_types = col_types, show_col_types = FALSE)
        if (is.null(col_types)) {
          col_types <- spec(chunk)
        }
        chunk
      }
    )
  )
}
