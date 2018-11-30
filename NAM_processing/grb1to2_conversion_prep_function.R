grb1to2_conversion_prep.fn <- function() {

  # this function needs to be done prior to trying to convert grib1 files to grib2 files
  # many thanks to Max Joseph for figuring these commands out
  
  grb_conv_files <- c("wgrib2", "wgrib", "grb1to2.pl", "smallest_grib2", "gmerge", "grib1to2_metadata.pl",
                      "my.inv", "global_template.g2")
  list_files_exist <- unlist(lapply(grb_conv_files, function(x){file.exists(uppermost.directory,x)})) 
  if (min(list_files_exist)==1) {
    print("all files necessary for grib1 to grib2 conversion already exist")
  } else {
    print("not all files necessary for grib1 to grib2 conversion exist, so they will be downloaded")
  # these are the files 
  #wgrib2
  #wgrib
  #grb1to2.pl
  #smallest_grib2
  #gmerge
  #grip1to2_metadata.pl
  #my.inv
  #global_template.g2
  
  # get the perl scripts
  system("wget ftp://ftp.cpc.ncep.noaa.gov/wd51we/grb1to2/grb1to2.pl")
  system("wget ftp://ftp.cpc.ncep.noaa.gov/wd51we/grb1to2/grib1to2_metadata.pl")
  system("wget ftp://ftp.cpc.ncep.noaa.gov/wd51we/grb1to2/global_template.g2")
  
  # create symbolic links to some executables
  system("ln -s /usr/local/bin/wgrib wgrib")
  system("ln -s /usr/local/bin/wgrib2 wgrib2")
  system("ln -s /usr/local/grib2/grib2/aux_progs/gmerge gmerge")
  system("ln -s /usr/local/grib2/grib2/aux_progs/smallest_grib2 smallest_grib2")
  
  # modify the perl script so that it is executable
  system("chmod +x grb1to2.pl")
  
  } # if (file.exists(this_file)) { # only run code if files don't already exist
} # end function