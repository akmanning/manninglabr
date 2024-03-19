#' @title Create a measurement dataset with OMOP_CONCEPT_ID
#' @description
#' This function takes an OMOP Concept ID in the Measurement domain and returns a dataset with the following columns: person_id, measurement_concept_id, measurement_date, measurement_datetime, measurement_type_concept_id, operator_concept_id, value_as_number, value_as_concept_id, unit_concept_id, range_low, range_high, and visit_occurrence_concept_id.
#' @param OMOPCONCEPTID The OMOP Concept ID of the measurement of interest.
#' @param query_sql = measurement_query_sql
#' @return A list containing the OMOPCONCEPTID, query_sql, and query_result_path.
#' @export

create_measurement_dataset_with_OMOP_CONCEPT_ID <- function(OMOPCONCEPTID, query_sql = measurement_query_sql) {
  # Create a query to get the measurement dataset with OMOP_CONCEPT_ID
  query_sql <- query_sql %>%
    stringr::str_replace_all("XYZ_OMOPCONCEPTID_XYZ", as.character(OMOPCONCEPTID))

  # Define the query result path
  query_result_path <- file.path(
    Sys.getenv("WORKSPACE_BUCKET"),
    "bq_exports",
    "measurement_OMOP_ID_XYZ_OMOPCONCEPTID_XYZ",
    "measurement_OMOP_ID_XYZ_OMOPCONCEPTID_XYZ_*.csv"
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


  # Return a list containing the OMOP_CONCEPT_ID, query_sql, and query_result_path
  return(list(OMOPCONCEPTID, query_sql, query_result_path))
}

# # This query represents dataset "creatinine_mass-volume_in_serum_plasma_LOINC_2160-0_OMOP_3016723" for domain "measurement" and was generated for All of Us Controlled Tier Dataset v7
measurement_query_sql <- "
SELECT
measurement.person_id,
measurement.measurement_concept_id,
m_standard_concept.concept_name as standard_concept_name,
m_standard_concept.concept_code as standard_concept_code,
m_standard_concept.vocabulary_id as standard_vocabulary,
measurement.measurement_datetime,
measurement.measurement_type_concept_id,
m_type.concept_name as measurement_type_concept_name,
measurement.operator_concept_id,
m_operator.concept_name as operator_concept_name,
measurement.value_as_number,
measurement.value_as_concept_id,
m_value.concept_name as value_as_concept_name,
measurement.unit_concept_id,
m_unit.concept_name as unit_concept_name,
measurement.range_low,
measurement.range_high,
measurement.visit_occurrence_id,
m_visit.concept_name as visit_occurrence_concept_name,
measurement.measurement_source_value,
measurement.measurement_source_concept_id,
m_source_concept.concept_name as source_concept_name,
m_source_concept.concept_code as source_concept_code,
m_source_concept.vocabulary_id as source_vocabulary,
measurement.unit_source_value,
measurement.value_source_value
FROM
( SELECT
  *
    FROM
  `measurement` measurement
  WHERE
  (
    measurement_concept_id IN (
      SELECT
      DISTINCT c.concept_id
      FROM
      `cb_criteria` c
      JOIN
      (
        SELECT
        CAST(cr.id as string) AS id
        FROM
        `cb_criteria` cr
        WHERE
        concept_id IN (
          XYZ_OMOPCONCEPTID_XYZ
        )
        AND full_text LIKE '%_rank1]%'
      ) a
      ON (
        c.path LIKE CONCAT('%.',
                           a.id,
                           '.%')
        OR c.path LIKE CONCAT('%.',
                              a.id)
        OR c.path LIKE CONCAT(a.id,
                              '.%')
        OR c.path = a.id)
      WHERE
      is_standard = 1
      AND is_selectable = 1
    )
  )
) measurement
LEFT JOIN
`concept` m_standard_concept
ON measurement.measurement_concept_id = m_standard_concept.concept_id
LEFT JOIN
`concept` m_type
ON measurement.measurement_type_concept_id = m_type.concept_id
LEFT JOIN
`concept` m_operator
ON measurement.operator_concept_id = m_operator.concept_id
LEFT JOIN
`concept` m_value
ON measurement.value_as_concept_id = m_value.concept_id
LEFT JOIN
`concept` m_unit
ON measurement.unit_concept_id = m_unit.concept_id
LEFT JOIn
`visit_occurrence` v
ON measurement.visit_occurrence_id = v.visit_occurrence_id
LEFT JOIN
`concept` m_visit
ON v.visit_concept_id = m_visit.concept_id
LEFT JOIN
`concept` m_source_concept
ON measurement.measurement_source_concept_id = m_source_concept.concept_id"
