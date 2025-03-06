#' @title dataset_to_csv_to_bucket
#' @description This function takes a dataset and saves it as a csv file in the bucket
#' @param name_to_save the name of the dataset to be saved
#' @return a csv file in the bucket
#' @export
dataset_to_csv_to_bucket <- function(name_to_save, dataset_df = all_participants$dataset_person_df) {
  # create time stamp and get username
  today <-  strftime(lubridate::now(), "%Y-%m-%d")
  owner <- str_replace(string = Sys.getenv("OWNER_EMAIL"),pattern="@researchallofus.org",replacement = "")

  filename <- paste(name_to_save,"_by_",owner,"_on_",today,".csv",sep="")

  # store the dataframe in current workspace
  write_excel_csv(dataset_df, filename)

  # Get the bucket name
  my_bucket <- Sys.getenv('WORKSPACE_BUCKET')

  # Upload file to google bucket
  # Copy the file from current workspace to the bucket
  system(paste0("gsutil cp ./", filename, " ", my_bucket, "/data/"), intern=T)

  # Check if file is in the bucket
  print(paste("Listing files in bucket with the label:", name_to_save, "user and the filename:",filename))
  print(system(paste0("gsutil ls ", my_bucket, "/data/*.csv | grep ",name_to_save), intern=T))

  }
