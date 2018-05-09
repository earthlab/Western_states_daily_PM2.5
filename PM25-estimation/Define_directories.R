#### Clear all variables and start fresh ####
rm(list = ls())
options(warn=2) # throw an error when there's a warning and stop the code from running further
# make sure it isn't outputing text or figures to any files
if (max(dev.cur())>1) { # make sure it isn't outputting to any figure files
  dev.off(which = dev.cur())
} # if (max(dev.cur())>1) {
while (sink.number()>0) {
  sink()
} # while (sink.number()>0) {
sink.number()

#### Directories that change when running on different computers ####

## define uppermost directory for data location
#uppermost.directory="/home/rstudio" # In Docker
uppermost.directory="D:/S3_bucket_image" # without docker on work PC
#uppermost.directory="C:/Users/Maestas/S3_bucket_image" # without docker on home PC

## define directory for latex code and images
output.directory=file.path("C:","Users","mema2636","MMM_GitHub","estimate-pm25","LaTeX_documentation","Code_Outputs") # without docker on work PC

#### Directories that should not need to be changed when switching between computers ####
working.directory=uppermost.directory 
setwd(working.directory)
ProcessedData.directory=file.path(working.directory,"Processed_Data")
UintahData.directory=file.path(working.directory,"PM25_Uintah_Basin")
USMaps.directory=file.path(working.directory,"Shapefiles_for_mapping","cp_2016_us_state_500k")
PCAPSData.directory=file.path(working.directory,"PM25_PCAPS_Salt_Lake")
AQSData.directory=file.path(working.directory,"AQS_Daily_Summaries")
FMLE.directory=file.path(working.directory,"Federal_Land_Manager_Environmental_Database")
FireCache.directory=file.path(working.directory,"Fire_Cache_Smoke_DRI")
CARB.directory=file.path(working.directory,"PM25_CARB")
UTDEQ.directory=file.path(working.directory,"PM25_UTDEQ")
NVDEQ.directory=file.path(working.directory,"PM25_NV-DEQ")

#### listing of variables to be cleared at end of each script ####
rm(uppermost.directory,output.directory)
rm(working.directory,ProcessedData.directory,StartData.directory)