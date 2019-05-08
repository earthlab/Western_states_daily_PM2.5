convert_grib1to2.fn <- function(this_model.info, this_file_type) {
  
  if (this_file_type == "grib1") { # convert grib1 files to grib2 and then run GribInfo
    
    # run the script to convert to grib2
    # ./grb1to2.pl namanl_218_20080101_0000_000.grb
    system(paste("./grb1to2.pl",this_model.info[[1]]$file.name,sep = " "))
    grib2_file_name <- paste(this_model.info[[1]]$file.name,".grb2",sep = "")
    if (file.exists(grib2_file_name) == TRUE) { # check whether the converted file was successfully created
      thisGribInfo <- GribInfo(grib.file = grib2_file_name, file.type = "grib2") 
    } else { # if (file.exists(grib2_file_name) == TRUE) { # check whether the converted file was successfully created
      thisGribInfo <- NULL 
      print(paste("*** convert_grip1to2.fn failed to convert ",grib2_file_name," ***"))
    } # if (file.exists(grib2_file_name) == TRUE) { # check whether the converted file was successfully created
    
  } else if (this_file_type == "grib2") { # run GribInfo

    thisGribInfo <- GribInfo(grib.file = this_model.info[[1]]$file.name, file.type = this_file_type)

  } else {
    error("Invalid this_file_type. It should be either grib1 or grib2")
  } # end if
  
  return(thisGribInfo)
} # end function