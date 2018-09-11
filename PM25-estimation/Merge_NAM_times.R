# Merge_NAM_times.R

#### Write function to fill in 24-hr averages in PM25DateLoc_orig ####
# need time zone info from Gina

# inputs:
# PM25DateLoc_0000 
# PM25DateLoc_0600 
# PM25DateLoc_1200 
# PM25DateLoc_1800
# PM25DateLoc_orig

# outputs:
# PM25DateLoc_meteo

rm(PM25DateLoc_0000, PM25DateLoc_0600, PM25DateLoc_1200, PM25DateLoc_1800, PM25DateLoc_orig)
#### Save output to csv file ####
# save PM25DateLoc_meteo

#### End of file cleanup
rm(start_study_year,stop_study_year)
rm(uppermost.directory,output.directory)
rm(working.directory,ProcessedData.directory,UintahData.directory,USMaps.directory,PCAPSData.directory)
rm(AQSData.directory,FMLE.directory,FireCache.directory,CARB.directory,UTDEQ.directory,NVDEQ.directory)
rm(writingcode.directory,computer_system)