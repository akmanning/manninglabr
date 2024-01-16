#' Creates demographic summary table
#' @param person_df the dataset to be summarized
#' @param true_false_variable the name of a true/false variable in the dataset to stratify upon
#' @return a table of demographics for a given dataset
#' @export

cohorts_summary_demographics <- function(person_df, true_false_variable = NA) {
  # This function creates a table of demographics for a given dataset
  # new.data.set is the dataset to be summarized

  # Check true_false_variable
  if (!is.na(true_false_variable)) {
    if (!true_false_variable %in% names(person_df)) {
      stop("true_false_variable must be a variable in the dataset")
    }
  }
  # check that variable is a TRUE/FALSE variable
  if (!is.na(true_false_variable)) {
    if (!all(person_df[[true_false_variable]] %in% c(TRUE, FALSE))) {
      stop("true_false_variable must be a TRUE/FALSE variable")
    }
  }

  # options(repr.vector.quote=FALSE)
  table1::label(person_df$age) <- "Age"
  table1::units(person_df$age) <- "Years"
  table1::label(person_df$gender) <- "Reported Gender"
  table1::label(person_df$sex_at_birth) <- "Biological Sex"
  table1::label(person_df$race) <- "Reported Race"
  table1::label(person_df$ethnicity) <- "Reported Ethnicity"
  if(!is.na(true_false_variable)) {
    table1::label(person_df$true_false_variable) <- "Stratification Variable"
    to.html <- table1::table1(stats::as.formula(paste("~ age + gender + sex_at_birth + race + ethnicity | as.factor(",true_false_variable,")")),
                              data = person_df,
                              render.continuous = my.render.cont,
                              render.categorical = my.render.cat
    )
  } else {
    to.html <- table1::table1(~ age + gender + sex_at_birth + race + ethnicity,
                              data = person_df,
                              render.continuous = my.render.cont,
                              render.categorical = my.render.cat)
  }

  IRdisplay::display_html(to.html)
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
