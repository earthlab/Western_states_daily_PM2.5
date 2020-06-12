# trouble shooting differences between part b & part d

#### record of various parts ####
# part a: early version created while writing code. Disregard
# part b: first batch of PM2.5 data that was used to exctract predictor data, years 2008-2014
# part c: county centroids, 2008-2014. This work flow has now been moved to the "Locations_of_interest" folder.
# part d: second batch of PM2.5 data, adds EPA AQS data for 2014-2018

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
library(dplyr)

#### Source functions I've written ####
source(file.path("estimate-pm25","General_Project_Functions","general_project_functions.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"Deduplication_functions.R"))
source(file.path(define_file_paths.fn("ML_Code.directory"),"Plotting_and_LaTex_functions.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"State_Abbrev_Definitions_function.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"reprojection_functions.R"))
source(file.path(define_file_paths.fn("General_functions.directory"),"merging_data_functions.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"input_mat_functions.R"))


#### Define constants #####
study_states_abbrev <- define_study_constants.fn("study_states_abbrev") # c("AZ","CA","CO", "ID", "MT", "NV", "NM", "OR", "UT", "WA", "WY")
ProcessedData.directory <-  define_file_paths.fn("ProcessedData.directory")
#drop_cols <- c("old_lon","old_lat","old_Datum","Easting","Northing","Datum") # list extraneous columns
drop_cols <- c("")

#### Load locations/dates by batch ####

# 4 digits
part_b_loc_4_digits <- PM25_lat_lon_part.fn(this_part = "b", Locations_Only = TRUE, round_lat_lon_digits = 4) # Load part b locations, round to 4 digits
part_b_loc_dates_4_digits <- PM25_lat_lon_part.fn(this_part = "b", Locations_Only = FALSE, round_lat_lon_digits = 4) # Load part b locations & dates, round to 4 digits
part_d_loc_4_digits <- PM25_lat_lon_part.fn(this_part = "d", Locations_Only = TRUE, round_lat_lon_digits = 4) # Load part b locations, round to 4 digits
part_d_loc_dates_4_digits <- PM25_lat_lon_part.fn(this_part = "d", Locations_Only = FALSE, round_lat_lon_digits = 4) # Load part b locations & dates, round to 4 digits

part_b_not_in_d_loc_4_digits <- anti_join(part_b_loc_4_digits, part_d_loc_4_digits, by=c("Lat","Lon"))


#### load file created by step 1 - part b ####
processed_data_version <- "b"
this_source_file <- paste("PM25_Step1_part_",processed_data_version,".csv",sep = "") # define file name
sub_folder <- paste("PM25_data_part_",processed_data_version,sep = "")
file_sub_label <- paste("PM25_Step2_part_",processed_data_version,sep = "")
# load data file
part_b_input_mat1 <- read.csv(file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder,this_source_file),header=TRUE)
part_b_input_mat1 <- input_mat_change_data_classes.fn(part_b_input_mat1)

#### load file created by step 2 - part b ####
processed_data_version <- "b"
this_source_file <- paste("PM25_Step2_part_",processed_data_version,".csv",sep = "") # define file name
sub_folder <- paste("PM25_data_part_",processed_data_version,sep = "")
file_sub_label <- paste("PM25_Step2_part_",processed_data_version,sep = "")
# load data file
part_b_input_mat2 <- read.csv(file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder,this_source_file),header=TRUE)
part_b_input_mat2 <- input_mat_change_data_classes.fn(part_b_input_mat2)

unique(unlist(lapply(part_b_input_mat2$PM2.5_Lat, decimalplaces)))
which_2b_1_dec <- which(unlist(lapply(part_b_input_mat2$PM2.5_Lat, decimalplaces)) == 1)
part_2b_1_dec <- part_b_input_mat2[which_2b_1_dec, ]

#### load file created by step 1 - part d ####
processed_data_version <- "d"
this_source_file <- paste("PM25_Step1_part_",processed_data_version,".csv",sep = "") # define file name
sub_folder <- paste("PM25_data_part_",processed_data_version,sep = "")
file_sub_label <- paste("PM25_Step2_part_",processed_data_version,sep = "")
# load data file
part_d_input_mat1 <- read.csv(file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder,this_source_file),header=TRUE)
part_d_input_mat1 <- input_mat_change_data_classes.fn(part_d_input_mat1)

#### load file created by step 2 - part d ####
processed_data_version <- "d"
this_source_file <- paste("PM25_Step2_part_",processed_data_version,".csv",sep = "") # define file name
sub_folder <- paste("PM25_data_part_",processed_data_version,sep = "")
file_sub_label <- paste("PM25_Step2_part_",processed_data_version,sep = "")
# load data file
part_d_input_mat2 <- read.csv(file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder,this_source_file),header=TRUE)
part_d_input_mat2 <- input_mat_change_data_classes.fn(part_d_input_mat2)

# step 1 d
which_this_file <- which(part_d_input_mat1$Source_File == "Fire_Cache_Smoke_DRI_Smoke_USFS_R9-17.csv")
this_file_1d <- part_d_input_mat1[which_this_file, ]

# step 2 d
which_this_file <- which(part_d_input_mat2$Source_File == "Fire_Cache_Smoke_DRI_Smoke_USFS_R9-17.csv")
this_file_2d <- part_d_input_mat2[which_this_file, ]

# find the stations that are in part b but not d within the full step 1 input_mat
#list_rows_interest <- lapply(X = vector_for_subset, FUN = fancy_which.fn, col_for_subset = col_for_subset, full_data_frame = full_data_frame)

unique(unlist(lapply(part_b_input_mat1$PM2.5_Lat, decimalplaces)))
which_0_dec <- which(unlist(lapply(part_b_input_mat1$PM2.5_Lat, decimalplaces)) == 0)
part_b_0_dec <- part_b_input_mat1[which_0_dec, ]

unique(unlist(lapply(part_b_input_mat2$PM2.5_Lat, decimalplaces)))

for (station_i in 1:length(part_b_not_in_d_loc_4_digits)) {
  print(station_i)
  orig_lat <- part_b_not_in_d_loc_4_digits[station_i, c("old_lat")]
  orig_lon <- part_b_not_in_d_loc_4_digits[station_i, c("old_lon")]
  # find this orig lat/lon in step 1 input_mat
  which_in_step1b <- which(part_b_input_mat1$PM2.5_Lat == orig_lat & part_b_input_mat1$PM2.5_Lon == orig_lon)
  row_interest <- part_b_input_mat1[which_in_step1b, ]
  #which_in_d_step1 <- which(part_d_input_mat1$PM2.5_Lat == orig_lat & part_d_input_mat1$PM2.5_Lon == orig_lon)
  #subset_b_input_mat1 <- part_b_input_mat1[1438500:1438700, ] # for station_i = 1, row of interest: 1438593
  #subset_b_input_mat1 <- part_b_input_mat1[1439360:1439390, ] # for station_i = 2, row of interest: 1439372
  #row_interest_d <- part_d_input_mat1[which_in_d_step1, ]
  which_in_d_step1 <- which(part_d_input_mat1$Date_Local %in% row_interest$Date_Local & as.character(part_d_input_mat1$Source_File) %in% as.character(row_interest$Source_File) & as.character(part_d_input_mat1$Address) %in% as.character(row_interest$Address))
  row_interest_d <- part_d_input_mat1[which_in_d_step1, ]
  
  
  which_in_d_step2 <- which(part_d_input_mat2$Date_Local %in% row_interest$Date_Local & as.character(part_d_input_mat2$Source_File) %in% as.character(row_interest$Source_File) & as.character(part_d_input_mat2$Address) %in% as.character(row_interest$Address))
  row_interest_2d <- part_d_input_mat2[which_in_d_step2, ] 
  
  part_1b_not_in_2d_loc_4_digits <- anti_join(row_interest, row_interest_2d, by=c("PM2.5_Lat","PM2.5_Lon"))
  
  
  #subset_d_input_mat1 <- part_d_input_mat1[1444200:1444220, ] # for station_i = 6, row of interest: 1444210
  #subset_b_input_mat1 <- part_b_input_mat1[1444200:1444220, ] # for station_i = 6, row of interest: 1444210
  
  this_voltage <- row_interest$Battery.Voltage.volts
  print(this_voltage)
  if (max(this_voltage) < define_study_constants.fn("voltage_threshold_lower")) {
    print("voltage out of bounds, too low")
  } else if (min(this_voltage) > define_study_constants.fn("voltage_threshold_upper")) {
    print("voltage out of bounds, too high")
    } else {
      
      if (row_interest_d$flg.BatteryVoltage != "0 0") { # newer version handles voltage flags differently
        print("voltage flag out of bounds")
        print("voltage flags in version d")
        print(row_interest_d$flg.BatteryVoltage)
      } else {
        stop("take a closer look at data")
      } # if
    } # if
  } # for

#row number old_lon   old_lat  old_Datum Lon       Lat     Datum Easting    Northing  Latitude Longitude
#1          -110.3983 43.56511 NAD83     -110.3983 43.5651 NAD83 -1153569.1 766570.88 43.56511 -110.3983 this from September 28, 2012 from Fire_Cache_Smoke_DRI_Smoke_N13.csv - several columns have questionable data, including not matching surrounding lat/lon and voltage out of bounds
#2          -116.3732 43.90336 NAD83     -116.3732 43.9034 NAD83 -1617986.5 890855.43 43.90336 -116.3732
#3
-116.1992
43.56511
NAD83
-116.1992
43.5651
NAD83
-1612336.6
851014.31
43.56511
-116.1992
4
-109.3422
46.92686
NAD83
-109.3422
46.9269
NAD83
-1017230.0
1124334.88
46.92686
-109.3422
5
-108.9099
46.88476
NAD83
-108.9099
46.8848
NAD83
-985111.4
1115164.89
46.88476
-108.9099
6
-123.5207
48.44399
NAD83
-123.5207
48.4440
NAD83
-2028004.2
1516009.83
48.44399
-123.5207
7
-116.6295
48.44407
NAD83
-116.6295
48.4441
NAD83
-1529567.5
1386975.97
48.44407
-116.6295
# 8 -108.2208 36.72750 NAD27 -108.2215 36.7275 NAD83 -1078399.2 -17123.29 36.72750 -108.2215 # -seems to show up in all sets of data, probably a rounding issue
9
-105.2358
43.67694
NAD27
-105.2364
43.6769
NAD83
-740464.1
727504.69
43.67692
-105.2364
10
-111.6631
40.25361
NAD27
-111.6638
40.2536
NAD83
-1314858.0
417207.12
40.25357
-111.6638






part_d_not_in_b_loc_4_digits <- anti_join(part_d_loc_4_digits, part_b_loc_4_digits, by=c("Lat","Lon"))


