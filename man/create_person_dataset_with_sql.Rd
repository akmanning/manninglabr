% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_person_dataset_with_sql.R
\name{create_person_dataset_with_sql}
\alias{create_person_dataset_with_sql}
\title{Create a person dataset}
\usage{
create_person_dataset_with_sql(dataset_person_sql, label)
}
\arguments{
\item{dataset_person_sql}{SQL query to create the dataset}

\item{label}{the label for the dataset}
}
\value{
a list with the OMOP concept ID, the query SQL, and the path to the query results
}
\description{
Create a person dataset
}
