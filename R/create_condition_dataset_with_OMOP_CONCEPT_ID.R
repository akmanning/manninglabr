
#' @title Create condition dataset with OMOP_CONCEPT_ID
#' @description
#' This function creates a dataset of condition data with a specific OMOP_CONCEPT_ID.
#' @param OMOPCONCEPTID The OMOP_CONCEPT_ID of the condition to be queried.
#' @param query_sql The SQL query to be used to create the dataset.
#' @return A list containing the OMOPCONCEPTID, query_sql, and query_result_path.
#' @export
create_condition_dataset_with_OMOP_CONCEPT_ID <- function(OMOPCONCEPTID, query_sql = condition_query_sql) {
  query_sql <- query_sql %>%
    stringr::str_replace_all("XYZ_OMOPCONCEPTID_XYZ", as.character(OMOPCONCEPTID))

  query_result_path <- file.path(
    Sys.getenv("WORKSPACE_BUCKET"),
    "bq_exports",
    "condition_OMOP_ID_XYZ_OMOPCONCEPTID_XYZ",
    "condition_OMOP_ID_XYZ_OMOPCONCEPTID_XYZ_*.csv"
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





# This query represents dataset "t1d_cohort_example_disease_concept_set" for domain "condition" and was generated for All of Us Controlled Tier Dataset v7
# January 16, 2024
#' @export
condition_query_sql <- paste("
    SELECT
        c_occurrence.person_id,
        c_occurrence.condition_concept_id,
        c_standard_concept.concept_name as standard_concept_name,
        c_standard_concept.concept_code as standard_concept_code,
        c_standard_concept.vocabulary_id as standard_vocabulary,
        c_occurrence.condition_start_datetime,
        c_occurrence.condition_end_datetime,
        c_occurrence.condition_type_concept_id,
        c_type.concept_name as condition_type_concept_name,
        c_occurrence.stop_reason,
        c_occurrence.visit_occurrence_id,
        visit.concept_name as visit_occurrence_concept_name,
        c_occurrence.condition_source_value,
        c_occurrence.condition_source_concept_id,
        c_source_concept.concept_name as source_concept_name,
        c_source_concept.concept_code as source_concept_code,
        c_source_concept.vocabulary_id as source_vocabulary,
        c_occurrence.condition_status_source_value,
        c_occurrence.condition_status_concept_id,
        c_status.concept_name as condition_status_concept_name
    FROM
        ( SELECT
            *
        FROM
            `condition_occurrence` c_occurrence
        WHERE
            (
                condition_concept_id IN  (
                    SELECT
                        DISTINCT c.concept_id
                    FROM
                        `cb_criteria` c
                    JOIN
                        (
                            select
                                cast(cr.id as string) as id
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
                AND (
                    c_occurrence.PERSON_ID IN (
                        SELECT
                            distinct person_id
                        FROM
                            `cb_search_person` cb_search_person
                        WHERE
                            cb_search_person.person_id IN (
                                SELECT
                                    criteria.person_id
                                FROM
                                    (SELECT
                                        DISTINCT person_id,
                                        entry_date,
                                        concept_id
                                    FROM
                                        `cb_search_all_events`
                                    WHERE
                                        (
                                            concept_id IN (
                                                SELECT
                                                    DISTINCT c.concept_id
                                                FROM
                                                    `cb_criteria` c
                                                JOIN
                                                    (
                                                        select
                                                            cast(cr.id as string) as id
                                                        FROM
                                                            `cb_criteria` cr
                                                        WHERE
                                                            concept_id IN (XYZ_OMOPCONCEPTID_XYZ)
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
                                                    AND is_standard = 1
                                            )
                                        ) criteria
                                    ) ))
                        ) c_occurrence
                    LEFT JOIN
                        `concept` c_standard_concept
                            ON c_occurrence.condition_concept_id = c_standard_concept.concept_id
                    LEFT JOIN
                        `concept` c_type
                            ON c_occurrence.condition_type_concept_id = c_type.concept_id
                    LEFT JOIN
                        `visit_occurrence` v
                            ON c_occurrence.visit_occurrence_id = v.visit_occurrence_id
                    LEFT JOIN
                        `concept` visit
                            ON v.visit_concept_id = visit.concept_id
                    LEFT JOIN
                        `concept` c_source_concept
                            ON c_occurrence.condition_source_concept_id = c_source_concept.concept_id
                    LEFT JOIN
                        `concept` c_status
                            ON c_occurrence.condition_status_concept_id = c_status.concept_id", sep="")


