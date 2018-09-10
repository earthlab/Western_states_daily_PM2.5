which_type_of_grib_file.fn <- function(list.available.models) {
  #### is this a grib1 (.grb) or grib2 (.grb2) type of file? ####
  first_file_name <- as.character(list.available.models$file.name[[1]]) # grab first file name in list
  last_character <- substr(first_file_name,nchar(first_file_name),nchar(first_file_name)) # find the last character in the file name - determines which type of file it is
  if (last_character == "b") { # grib1 files
    print("These are grib1 files")
    this_file_type <- "grib1"
  } else if (last_character == "2") { # grib2 files
    print("These are grib2 files")
    this_file_type <- "grib2"
  } else {error("Unknown file type")} # check code
  rm(first_file_name,last_character)
  
  # function output
  return(this_file_type)
}