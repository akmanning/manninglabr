#' Map a BigQuery column type to a readr column type
#'
#' @param bq_type A BigQuery type string, e.g. "STRING", "INTEGER", "DATE".
#' @return A readr collector.
#' @noRd
bq_type_to_readr <- function(bq_type) {
  switch(toupper(bq_type),
    STRING    = readr::col_character(),
    INTEGER   = , INT64 = ,
    FLOAT     = , FLOAT64 = ,
    NUMERIC   = , BIGNUMERIC = readr::col_double(),
    BOOLEAN   = , BOOL = readr::col_logical(),
    DATE      = readr::col_date(),
    # exported timestamps look like "2020-01-01 00:00:00 UTC"; keep as text
    readr::col_character()
  )
}

#' Build readr col_types from a query's BigQuery schema
#'
#' Uses a dry run (no cost, query is not executed) to get the result schema.
#'
#' @param sql SQL query string.
#' @param billing Billing project ID.
#' @return A `readr::cols()` specification.
#' @export
col_types_from_query <- function(sql, billing = Sys.getenv("GOOGLE_PROJECT")) {
  fields <- bigrquery::bq_perform_query_schema(sql, billing = billing)
  types  <- lapply(fields, function(f) bq_type_to_readr(f$type))
  names(types) <- vapply(fields, function(f) f$name, character(1))
  do.call(readr::cols, types)
}
