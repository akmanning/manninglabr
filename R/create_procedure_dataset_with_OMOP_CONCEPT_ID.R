#' @title Create procedure dataset with OMOP_CONCEPT_ID
#' @description
#' This function creates a dataset of procedure data with a specific OMOP_CONCEPT_ID.
#' @param OMOPCONCEPTID The OMOP_CONCEPT_ID of the procedure to be queried.
#' @param query_sql The SQL query to be used to create the dataset.
#' @return A list containing the OMOPCONCEPTID, query_sql, and query_result_path.
#' @export
create_procedure_dataset_with_OMOP_CONCEPT_ID <- function(OMOPCONCEPTID, query_sql = dataset_procedure_sql) {
  query_sql <- query_sql %>%
    stringr::str_replace_all("XYZ_OMOPCONCEPTID_XYZ", as.character(OMOPCONCEPTID))

  query_result_path <- file.path(
    Sys.getenv("WORKSPACE_BUCKET"),
    "bq_exports",
    "procedure_OMOP_ID_XYZ_OMOPCONCEPTID_XYZ",
    "procedure_OMOP_ID_XYZ_OMOPCONCEPTID_XYZ_*.csv"
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

# This query represents dataset "kidney_procedure_OMOP_4322471" for domain "procedure" and was generated for All of Us Controlled Tier Dataset v7
dataset_procedure_sql <- "
    SELECT
        procedure.person_id,
        procedure.procedure_concept_id,
        p_standard_concept.concept_name as standard_concept_name,
        p_standard_concept.concept_code as standard_concept_code,
        p_standard_concept.vocabulary_id as standard_vocabulary,
        procedure.procedure_datetime,
        procedure.procedure_type_concept_id,
        p_type.concept_name as procedure_type_concept_name,
        procedure.modifier_concept_id,
        p_modifier.concept_name as modifier_concept_name,
        procedure.quantity,
        procedure.visit_occurrence_id,
        p_visit.concept_name as visit_occurrence_concept_name,
        procedure.procedure_source_value,
        procedure.procedure_source_concept_id,
        p_source_concept.concept_name as source_concept_name,
        p_source_concept.concept_code as source_concept_code,
        p_source_concept.vocabulary_id as source_vocabulary,
        procedure.modifier_source_value
    FROM
        ( SELECT
            *
        FROM
            `procedure_occurrence` procedure
        WHERE
            (
                procedure_concept_id IN (SELECT
                    DISTINCT c.concept_id
                FROM
                    `cb_criteria` c
                JOIN
                    (SELECT
                        CAST(cr.id as string) AS id
                    FROM
                        `cb_criteria` cr
                    WHERE
                        concept_id IN (XYZ_OMOPCONCEPTID_XYZ)
                        AND full_text LIKE '%_rank1]%'      ) a
                        ON (c.path LIKE CONCAT('%.', a.id, '.%')
                        OR c.path LIKE CONCAT('%.', a.id)
                        OR c.path LIKE CONCAT(a.id, '.%')
                        OR c.path = a.id)
                WHERE
                    is_standard = 1
                    AND is_selectable = 1)
            )
            AND (
                procedure.PERSON_ID IN (SELECT
                    distinct person_id
                FROM
                    `cb_search_person` cb_search_person
                WHERE
                    cb_search_person.person_id IN (SELECT
                        criteria.person_id
                    FROM
                        (SELECT
                            DISTINCT person_id, entry_date, concept_id
                        FROM
                            `cb_search_all_events`
                        WHERE
                            (concept_id IN(SELECT
                                DISTINCT c.concept_id
                            FROM
                                `cb_criteria` c
                            JOIN
                                (SELECT
                                    CAST(cr.id as string) AS id
                                FROM
                                    `cb_criteria` cr
                                WHERE
                                    concept_id IN (XYZ_OMOPCONCEPTID_XYZ)
                                    AND full_text LIKE '%_rank1]%'      ) a
                                    ON (c.path LIKE CONCAT('%.', a.id, '.%')
                                    OR c.path LIKE CONCAT('%.', a.id)
                                    OR c.path LIKE CONCAT(a.id, '.%')
                                    OR c.path = a.id)
                            WHERE
                                is_standard = 1
                                AND is_selectable = 1)
                            AND is_standard = 1 )) criteria ) )
            )) procedure
    LEFT JOIN
        `concept` p_standard_concept
            ON procedure.procedure_concept_id = p_standard_concept.concept_id
    LEFT JOIN
        `concept` p_type
            ON procedure.procedure_type_concept_id = p_type.concept_id
    LEFT JOIN
        `concept` p_modifier
            ON procedure.modifier_concept_id = p_modifier.concept_id
    LEFT JOIN
        `visit_occurrence` v
            ON procedure.visit_occurrence_id = v.visit_occurrence_id
    LEFT JOIN
        `concept` p_visit
            ON v.visit_concept_id = p_visit.concept_id
    LEFT JOIN
        `concept` p_source_concept
            ON procedure.procedure_source_concept_id = p_source_concept.concept_id"

