#' Creates a summary table (default: demographics) for a given dataset
#'
#' @param person_df the dataset to be summarized
#' @param variables character vector of column names in \code{person_df} to summarize
#' @param labels character vector of display names, same length as \code{variables}.
#'   Use NA (or NULL for all) to fall back to the column name.
#' @param units character vector of units, same length as \code{variables}.
#'   Use NA or "" for variables without units. NULL means no units for any variable.
#' @param types character vector, same length as \code{variables}, each
#'   "continuous" or "categorical" (abbreviations "cont"/"cat" are accepted).
#' @param true_false_variable the name of a true/false variable in the dataset to stratify upon
#' @return the table1 object, displayed as HTML (invisibly returned)
#' @examples
#' \dontrun{
#' cohorts_summary_demographics(
#'   df,
#'   variables = c("age", "sex_at_birth", "bmi", "egfr"),
#'   labels    = c("Age", "Biological Sex", "BMI", "eGFR"),
#'   units     = c("Years", NA, "kg/m2", "mL/min/1.73m2"),
#'   types     = c("continuous", "categorical", "continuous", "continuous"),
#'   true_false_variable = "t2d"
#' )
#' }
#' @export

cohorts_summary_demographics <- function(person_df,
                                         variables = c("age", "gender", "sex_at_birth", "race", "ethnicity"),
                                         labels = c("Age", "Reported Gender", "Biological Sex",
                                                    "Reported Race", "Reported Ethnicity"),
                                         units = c("Years", NA, NA, NA, NA),
                                         types = c("continuous", "categorical", "categorical",
                                                   "categorical", "categorical"),
                                         true_false_variable = NA) {

  n <- length(variables)
  if (is.null(labels)) labels <- rep(NA_character_, n)
  if (is.null(units))  units  <- rep(NA_character_, n)

  # Check argument lengths
  if (!all(c(length(labels), length(units), length(types)) == n)) {
    stop("variables, labels, units, and types must all be the same length")
  }

  # Check variables exist
  missing_vars <- setdiff(variables, names(person_df))
  if (length(missing_vars) > 0) {
    stop("variables not found in the dataset: ", paste(missing_vars, collapse = ", "))
  }

  # Check types
  types <- tolower(types)
  types[types == "cont"] <- "continuous"
  types[types == "cat"]  <- "categorical"
  if (!all(types %in% c("continuous", "categorical"))) {
    stop("types must each be 'continuous' or 'categorical'")
  }

  # Check true_false_variable
  if (!is.na(true_false_variable)) {
    if (!true_false_variable %in% names(person_df)) {
      stop("true_false_variable must be a variable in the dataset")
    }
    if (!all(person_df[[true_false_variable]] %in% c(TRUE, FALSE))) {
      stop("true_false_variable must be a TRUE/FALSE variable")
    }
  }

  # Coerce types, then apply labels and units
  for (i in seq_len(n)) {
    v <- variables[i]
    if (types[i] == "continuous") {
      if (!is.numeric(person_df[[v]])) {
        person_df[[v]] <- suppressWarnings(as.numeric(as.character(person_df[[v]])))
      }
    } else if (!is.factor(person_df[[v]])) {
      person_df[[v]] <- factor(person_df[[v]])
    }
    lab <- if (is.na(labels[i])) v else labels[i]
    table1::label(person_df[[v]]) <- lab
    if (!is.na(units[i]) && nzchar(units[i])) {
      table1::units(person_df[[v]]) <- units[i]
    }
  }

  # Build the formula; backticks allow non-syntactic column names
  rhs <- paste0("`", variables, "`", collapse = " + ")
  if (!is.na(true_false_variable)) {
    form <- stats::as.formula(paste0("~ ", rhs, " | as.factor(`", true_false_variable, "`)"))
  } else {
    form <- stats::as.formula(paste0("~ ", rhs))
  }

  to.html <- table1::table1(form,
                            data = person_df,
                            render.continuous = my.render.cont,
                            render.categorical = my.render.cat)

  IRdisplay::display_html(to.html)
  invisible(to.html)
}


my.render.cont <- function(x) {
  with( table1::stats.apply.rounding(table1::stats.default(x), digits = 2), c("",
    "Mean (SD)" = sprintf("%s (&plusmn; %s)", MEAN, SD)
  ))
}

my.render.cat <- function(x) {
  c("", sapply(table1::stats.default(x), function(y) {
    with(
      y,
      sprintf("%d (%0.0000f %%)", FREQ, PCT)
    )
  }))

}
