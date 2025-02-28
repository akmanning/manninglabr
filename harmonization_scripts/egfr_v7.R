### set up
#devtools::install_github("r-lib/devtools")
#devtools::install_github("https://github.com/akmanning/manninglabr",force = TRUE)
library(manninglabr)

#install.packages("nephro")
library(nephro)

library(tidyverse)

##################### "All Participants" for domain "person"
#####################
# This query represents dataset "All Participants" for domain "person" and was generated for All of Us Controlled Tier Dataset v7
dataset_92885121_person_sql <- paste("
    SELECT
        person.person_id,
        person.gender_concept_id,
        p_gender_concept.concept_name as gender,
        person.birth_datetime as date_of_birth,
        person.race_concept_id,
        p_race_concept.concept_name as race,
        person.ethnicity_concept_id,
        p_ethnicity_concept.concept_name as ethnicity,
        person.sex_at_birth_concept_id,
        p_sex_at_birth_concept.concept_name as sex_at_birth 
    FROM
        `person` person 
    LEFT JOIN
        `concept` p_gender_concept 
            ON person.gender_concept_id = p_gender_concept.concept_id 
    LEFT JOIN
        `concept` p_race_concept 
            ON person.race_concept_id = p_race_concept.concept_id 
    LEFT JOIN
        `concept` p_ethnicity_concept 
            ON person.ethnicity_concept_id = p_ethnicity_concept.concept_id 
    LEFT JOIN
        `concept` p_sex_at_birth_concept 
            ON person.sex_at_birth_concept_id = p_sex_at_birth_concept.concept_id", sep="")
            
all_participants <- create_person_dataset_with_sql(dataset_92885121_person_sql,"all participants")
head(all_participants$dataset_person_df)
cohorts_summary_demographics(all_participants$dataset_person_df)

# Creatinine (blood) and eGFR
## Extract Creatinine
## OMOP Concept ID: 3016723
## LOGIC:
### Remove rows with missing value in value_as_number column TRUE
### Minumum possible value 0.01 Maximum possible value Values greater than 73.8 mg/dL are implausible. https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8531789/
### Occurance Concept Filter Only include outpatient measurement values Exclude: c("Emergency Room - Hospital", "Emergency Room and Inpatient Visit", "Emergency Room Visit", "Inpatient Hospital", "Inpatient Visit", "Intensive Care", "Rehabilitation Hospital")
### Unit Concept Filter “milligram per deciliter” “no matching concept” and “NA”
### Operator Concept Filter Any value except “>” and “<”
### Variable for data frame creatinine_blood_mg_dL_measurement_filtered_df

creatinine_blood_OMOP_3016723 <- create_measurement_dataset_with_OMOP_CONCEPT_ID(3016723)

names(creatinine_blood_OMOP_3016723)
colnames(creatinine_blood_OMOP_3016723$dataset_measurement_df)

## 
## get_measurement_table_summary looks at the values in each column and produces a table of summary statistics grouped by each of these columns.
## The output is used to decide the ** Filtering Criteria ** that is defined below
creatinine_blood_OMOP_3016723_summary <- get_measurement_table_summary(creatinine_blood_OMOP_3016723$dataset_measurement_df)
creatinine_blood_OMOP_3016723_summary

#
#
#
keep.visits <- setdiff(unique(creatinine_blood_OMOP_3016723$dataset_measurement_df$visit_occurrence_concept_name),
                       c("Emergency Room - Hospital",
  "Emergency Room and Inpatient Visit",
  "Emergency Room Visit",
  "Inpatient Hospital",
  "Inpatient Visit",
  "Intensive Care",
  "Rehabilitation Hospital"))
keep.visits

#
#
#

keep.operator <- setdiff(unique(creatinine_blood_OMOP_3016723$dataset_measurement_df$operator_concept_name),
                         c('>','<'))
keep.operator

#
#
#
keep.unit <- setdiff(unique(creatinine_blood_OMOP_3016723$dataset_measurement_df$unit_concept_name),
                     c('milliliter per minute','milliliter per minute per 1.73 square meter','millimole per liter','gram per deciliter'))
keep.unit


### This is the critical function that restricts the creatinine data frame 
### based on the decision made in examining the results of get_measurement_table_summary
###
creatinine_blood_mg_dL_measurement_filtered_df <- get_filtered_measurement_df(dataset_measurement_df = creatinine_blood_OMOP_3016723$dataset_measurement_df,
                                                                                    filter_missing_values_as_number=TRUE, 
                                                                                 filter_min=0.01, 
                                                                                 filter_max=73.8,
                                                                                filter_visit_occurrence_concept_name=keep.visits,
                                                                                filter_operator_concept_name=keep.operator,
                                                                                filter_unit_concept_name=keep.unit)
creatinine_blood_OMOP_3016723_FILTERED_summary <- get_measurement_table_summary(creatinine_blood_mg_dL_measurement_filtered_df)
creatinine_blood_OMOP_3016723_FILTERED_summary


#options(repr.plot.width = 40, repr.plot.height = 40)

ggplot(creatinine_blood_mg_dL_measurement_filtered_df,aes(value_as_number)) + geom_histogram(bins=30) + 
facet_wrap(~ unit_concept_name + visit_occurrence_concept_name + measurement_type_concept_name,scales = "free")



### Merge creatinine_blood_mg_dL_measurement_filtered_df with all_participants
###
###
# Add age at measurement and biological sex

head(all_participants$dataset_person_df)

head(creatinine_blood_mg_dL_measurement_filtered_df)
dim(creatinine_blood_mg_dL_measurement_filtered_df)

creatinine_blood_mg_dL_measurement_filtered_df <- merge(creatinine_blood_mg_dL_measurement_filtered_df,
                                                           all_participants$dataset_person_df,by="person_id")

head(creatinine_blood_mg_dL_measurement_filtered_df)
dim(creatinine_blood_mg_dL_measurement_filtered_df)

table(creatinine_blood_mg_dL_measurement_filtered_df$sex_at_birth_concept_id, 
      creatinine_blood_mg_dL_measurement_filtered_df$sex_at_birth, useNA="always")


# Create Age at Creatinine measurement

creatinine_blood_mg_dL_measurement_filtered_df <- dplyr::mutate(creatinine_blood_mg_dL_measurement_filtered_df,
                                                                     age_at_measurement = lubridate::year(measurement_datetime) - lubridate::year(date_of_birth),
                                                                    biological_sex_male_female = case_when(sex_at_birth_concept_id==45878463 ~ 0,
                                                                                                          sex_at_birth_concept_id==45880669 ~ 1,
                                                                                                          .default=NA))

table(creatinine_blood_mg_dL_measurement_filtered_df$sex_at_birth_concept_id, 
      creatinine_blood_mg_dL_measurement_filtered_df$biological_sex_male_female, useNA="always")


# calculate eGFR based on CKDEpi_RF.creat function from nephro package
creatinine_blood_mg_dL_measurement_filtered_df <- dplyr::mutate(creatinine_blood_mg_dL_measurement_filtered_df,
                                                                     egfr=CKDEpi_RF.creat(creatinine=value_as_number,
                                                                                         sex=biological_sex_male_female,
                                                                                         age=age_at_measurement))
# Explore eGFR variable
table(is.na(creatinine_blood_mg_dL_measurement_filtered_df$egfr))


options(repr.plot.width = 5, repr.plot.height = 5)

ggplot(creatinine_blood_mg_dL_measurement_filtered_df,aes(egfr)) + geom_histogram(bins=40)
table(creatinine_blood_mg_dL_measurement_filtered_df$egfr < 90,useNA="always")
