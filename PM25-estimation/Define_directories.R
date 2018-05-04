# To clear all variables and start fresh:
rm(list = ls())
options(warn=2) # throw an error when there's a warning and stop the code from running further


#### define directories and constants ####
#setwd("D:/S3_bucket_image/")
#uppermost.directory="/home/rstudio" # In Docker
uppermost.directory="D:/S3_bucket_image" # without docker on work PC
#uppermost.directory="C:/Users/Maestas/MMMGitRepository/estimate-pm25/PM25-estimation" # without docker on work PC


working.directory=uppermost.directory 
setwd(working.directory)
ProcessedData.directory=file.path(working.directory,"Processed_Data")
StartData.directory=file.path(working.directory,"PM25_Uintah_Basin")
USMaps.directory=file.path(working.directory,"Shapefiles_for_mapping","cp_2016_us_state_500k")
PCAPSData.directory=file.path(working.directory,"PM25_PCAPS_Salt_Lake")
AQSData.directory=file.path(working.directory,"AQS_Daily_Summaries")
FMLE.directory=file.path(working.directory,"Federal_Land_Manager_Environmental_Database")
FireCache.directory=file.path(working.directory,"Fire_Cache_Smoke_DRI")
CARB.directory=file.path(working.directory,"PM25_CARB")
UTDEQ.directory=file.path(working.directory,"PM25_UTDEQ")
NVDEQ.directory=file.path(working.directory,"PM25_NV-DEQ")