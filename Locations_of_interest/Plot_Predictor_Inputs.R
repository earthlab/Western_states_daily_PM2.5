# Plot_Predictor_Inputs.R >> plot input merged input files for predictors (not training data)

#### Clear variables and sinks; define working directory ####
rm(list  =  ls())
options(warn  =  2) # throw an error when there's a warning and stop the code from running further
if (max(dev.cur())>1) { # make sure it isn't outputting to any figure files
  dev.off(which  =  dev.cur())
} # if (max(dev.cur())>1) {
while (sink.number()>0) {
  sink()
} # while (sink.number()>0) {
working.directory  <-  "/home/rstudio"
setwd(working.directory) # set working directory

#### Call Packages (Library) ####
library(parallel) # see http://gforge.se/2015/02/how-to-go-parallel-in-r-basics-tips/
library(plyr)
library(rgdal)
library(geosphere)

#### Load Functions that I created ####
source(file.path("estimate-pm25","General_Project_Functions","general_project_functions.R"))
source(file.path(define_file_paths.fn("ML_Code.directory"),"ML_processing_functions.R"))
source(file.path(define_file_paths.fn("ML_Code.directory"),"Plotting_and_LaTex_functions.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"State_Abbrev_Definitions_function.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"input_mat_functions.R"))

#### Load data ####
print("could create a for loop or lapply or parallize when there is more than just county centroids for geographic scale")
# Load input file
data_descriptor <- "County Centroid Predictors"
data_descriptor_short <- "CountyCentroid"
this_source_file <- "ML_predictors_CountyCentroid_Locations_Dates_2008-01-01to2018-12-31.csv"
sub_folder <- "ML_prediction_inputs"
Full_Predictors_w_NA<-read.csv(file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder,this_source_file),header=TRUE) # load the AQS file

#### For new data ####
# Define columns to keep 
predictor_variables <- c("Date","GASP_AOD","MAIAC_AOD","elevation",              
                         "NLCD_1km_Fraction_Urban","NLCD_5km_Fraction_Urban","NLCD_10km_Fraction_Urban","HPBL.surface",
                         "TMP.2.m.above.ground","RH.2.m.above.ground","DPT.2.m.above.ground","APCP.surface","WEASD.surface",         
                         "SNOWC.surface","UGRD.10.m.above.ground","VGRD.10.m.above.ground","PRMSL.mean.sea.level",
                         "PRES.surface","DZDT.850.mb","DZDT.700.mb", "A_100","C_100","Both_100","A_250","C_250",                
                         "Both_250","A_500","C_500","Both_500","A_1000","C_1000","Both_1000")
                          # "Latitude","Longitude","Datum","Easting","Northing",

study_states_abbrev <- define_study_constants.fn("study_states_abbrev") #c("AZ","CA","CO", "ID", "MT", "NV", "NM", "OR", "UT", "WA", "WY")
this_datum <- define_study_constants.fn("study_datum") #"NAD83"
print(predictor_variables)
col_name_interest <- NA #"PM2.5_Obs" #"logpm25"
# # Get rid of extra columns and rows with NA
#Full_PM25_obs_w_NA <- Full_PM25_obs_extra_cols_and_NA[ ,c(col_name_interest,predictor_variables)]
#rm(Full_PM25_obs_extra_cols_and_NA)
#Full_PM25_obs <- Full_PM25_obs_w_NA
#Full_PM25_obs <- Full_PM25_obs_w_NA[complete.cases(Full_PM25_obs_w_NA), ]
#print("comment line 38 and uncomment line 39 to remove rows with any NA values")

# Set classes of columns
Full_Predictors_w_NA$Date <- as.Date(Full_Predictors_w_NA$Date,"%Y-%m-%d") # recognize dates as dates: 'Date_Local' 

##### create reports ####
#with plots/maps about the input data, consider removing any columns that have nearly constant values
print("create report with plots/maps about the input data, consider removing any columns that have nearly constant values")

large_df_report.fn(df_in = Full_Predictors_w_NA,this_source_file = this_source_file, data_descriptor = data_descriptor, col_name_interest = col_name_interest, predictor_variables = predictor_variables)

