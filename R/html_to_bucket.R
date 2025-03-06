#' @title html_to_bucket
#' @description This function takes a html file and saves it as a html file in the bucket
#' @param name_to_save the name of the html file to be saved
#' @return a html file in the bucket
#' @export
html_to_bucket <- function(name_to_save, bucket_path_to_save="data") {
  # create time stamp and get username
  today <-  strftime(lubridate::now(), "%Y-%m-%d")
  owner <- stringr::str_replace(string = Sys.getenv("OWNER_EMAIL"),pattern="@researchallofus.org",replacement = "")

  filename <- paste(name_to_save,"_by_",owner,"_on_",today,".html",sep="")

  system(paste0("cp ./", name_to_save, ".html ", filename), intern=T)
  # Get the bucket name
  my_bucket <- Sys.getenv('WORKSPACE_BUCKET')

  # Upload file to google bucket
  # Copy the file from current workspace to the bucket
  system(paste0("gsutil cp ./", filename, " ", my_bucket, "/",bucket_path_to_save,"/"), intern=T)

  # Check if file is in the bucket
  print(paste("Listing files in bucket with the label:", name_to_save, "user and the filename:",filename))
  print(system(paste0("gsutil ls ", my_bucket, "/",bucket_path_to_save,"/*.html | grep ",name_to_save), intern=T))

}
