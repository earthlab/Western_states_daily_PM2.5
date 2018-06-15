# Define_directories.R clears all variables and defines directories
# Lines 17-19 need to be updated when switching between computer systems

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

#### Set the computer system in use ####
# Uncomment exactly one of the following lines to match which computer system is in use
computer_system = "Docker"
#computer_system = "workPC"
#computer_system = "homePC"

#### Directories that change when running on different computers ####

if (computer_system == "Docker") {
  ## define uppermost directory for data location
  uppermost.directory="/home/rstudio" # In Docker
  writingcode.directory = "/home/rstudio/estimate-pm25/PM25-estimation"
  ## define directory for latex code and images
  output.directory=file.path("home","rstudio","estimate-pm25","LaTeX_documentation","Code_Outputs")#  stop("define output.directory for docker")
  
} else if (computer_system == "workPC") {
  ## define uppermost directory for data location
  uppermost.directory="D:/S3_bucket_image" # without docker on work PC
  writingcode.directory = "C:/Users/mema2636/MMM_GitHub/estimate-pm25/PM25-estimation"
  
  ## define directory for latex code and images
  output.directory=file.path("C:","Users","mema2636","MMM_GitHub","estimate-pm25","LaTeX_documentation","Code_Outputs") # without docker on work PC
  
} else if (computer_system == "homePC") {
  ## define uppermost directory for data location
  #uppermost.directory="C:/Users/Maestas/S3_bucket_image" # without docker on home PC, C drive
  uppermost.directory="F:/S3_bucket_image" # thumbdrive
  writingcode.directory = "C:/Users/Maestas/MMMGitRepository/estimate-pm25/PM25-estimation"
  
  ## define directory for latex code and images
  output.directory=file.path("C:","Users","Maestas","MMMGitRepository","estimate-pm25","LaTeX_documentation","Code_Outputs") # without docker on work PC
 
} else {stop("Invalid option for computer_system")}

#### Directories that should not need to be changed when switching between computers ####
working.directory <- uppermost.directory 
setwd(working.directory)
ProcessedData.directory <- file.path(working.directory,"Processed_Data")
PythonProcessedData.directory <- file.path(working.directory,"Python_Processed_Data")
UintahData.directory <- file.path(working.directory,"PM25_Uintah_Basin")
USMaps.directory <- file.path(working.directory,"Shapefiles_for_mapping","cp_2016_us_state_500k")
PCAPSData.directory <- file.path(working.directory,"PM25_PCAPS_Salt_Lake")
AQSData.directory <- file.path(working.directory,"AQS_Daily_Summaries")
FMLE.directory <- file.path(working.directory,"Federal_Land_Manager_Environmental_Database")
FireCache.directory <- file.path(working.directory,"Fire_Cache_Smoke_DRI")
CARB.directory <- file.path(working.directory,"PM25_CARB")
UTDEQ.directory <- file.path(working.directory,"PM25_UTDEQ")
NVDEQ.directory <- file.path(working.directory,"PM25_NV-DEQ")
NARR.directory <- file.path(working.directory,"NARR")

#### listing of variables to be cleared at end of each script ####
#rm(uppermost.directory,output.directory)
#rm(working.directory,ProcessedData.directory,UintahData.directory,USMaps.directory,PCAPSData.directory)
#rm(AQSData.directory,FMLE.directory,FireCache.directory,CARB.directory,UTDEQ.directory,NVDEQ.directory)
#rm(writingcode.directory,computer_system,NARR.directory)

#### Display computer setting ####
print(paste("Set to run on this computer: ",computer_system,sep = ""))
