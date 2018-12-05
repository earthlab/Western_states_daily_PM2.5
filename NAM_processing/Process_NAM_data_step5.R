# Process_NAM_data_step5.R - take 24-hr summaries of data

# load the data created in step 4, which has all of the observations for the 4 timesteps per day in one data frame
part_number <- "bc"
Step3_NAM_data <- read.csv(file.path(ProcessedData.directory,sub_folder,paste("NAM_Step3_part_",part_number,".csv",sep = ""))) # open data file
stop("read in step4 data instead of step3")
Step4_NAM_data <- Step3_NAM_data
stop("remove line 7")
Step4_NAM_data$Local.Date.Time <- Step4_NAM_data$UTC.Date.Time
stop("remove line 9")
Step4_NAM_data$Local.Date <- date(Step4_NAM_data$Local.Date.Time)
stop("remove line 11")

# load information about meteo variables
this_source_file <- paste("MeteoVariablesNAM.csv")
MeteoVarsMultiType <- read.csv(file.path(NAM_Code.directory,this_source_file))
# grab the list of relevant meteo variables for this file type from MeteoVars
which_meteo <- which(MeteoVarsMultiType$file_type == "grib2") # get grib2 files because grib1 files will be converted to grib2
MeteoVars <- MeteoVarsMultiType[which_meteo,] # matrix with just the relevant rows

#one_day_seconds <- 60*60*24
#All_dates <- unique(Step4_NAM_data$Local.Date) # get a list of dates
All_date_loc <- unique(Step4_NAM_data[ ,c("Local.Date","Lat","Lon")]) # get a list of dates
# Create data frame for output data
Step5_NAM_data <- data.frame(matrix(NA,nrow=dim(All_date_loc)[1],ncol=length(colnames(Step4_NAM_data)))) # create data frame for input_mat1
names(Step5_NAM_data) <- colnames(Step4_NAM_data) # assign the header to input_mat1
# drop extraneous columns that don't apply to 24-hr data
Step5_NAM_data$Time.UTC <- NULL
Step5_NAM_data$Date <- NULL
Step5_NAM_data$Local.Date.Time <- NULL
Step5_NAM_data$Local.Date <- as.Date(Step5_NAM_data$Local.Date)

lapply(1:dim(All_date_loc)[1], function(x){ # x <- 1
  #which_this_date <- which(Step4_NAM_data[ , c("Local.Date","Lat","Lon")] == All_date_loc[x, ]) # find all data points with this date
  which_this_date_loc <- which(Step4_NAM_data$Local.Date == All_date_loc[x, c("Local.Date")] & Step4_NAM_data$Lat == All_date_loc[x, c("Lat")] & Step4_NAM_data$Lon == All_date_loc[x, c("Lon")])
  #which_this_date <- which(Step4_NAM_data$Local.Date == All_da)
  this_date_loc <- Step4_NAM_data[which_this_date_loc, ]
  
  Step5_NAM_data[x, c("Lat","Lon", "Datum", "Easting","Northing", "TimeZone", "Local.Date")] <- unique(this_date_loc[ , c("Lat","Lon","Datum", "Easting","Northing", "TimeZone", "Local.Date")])
  for (meteo_var_counter in 1:dim(MeteoVars)[1]) { # cycle through variables(levels) of interest
    thisMeteo_var_Name <- MeteoVars[meteo_var_counter,c("VariableName")] # get variable full name
    thisMeteo_variable <- MeteoVars[meteo_var_counter,c("VariableCode")] # get variable coded name
    thisMeteo_level <- MeteoVars[meteo_var_counter,c("AtmosLevelCode")] # get variable level name
    thisMeteo_units <- MeteoVars[meteo_var_counter,c("Units")] # get variable units
    thisMeteo_24_summary <- MeteoVars[meteo_var_counter,c("X24.hr.summary")]
    if (thisMeteo_24_summary == "max") {
    this_meteo_value <- max(this_date_loc[ , c(as.character(paste(thisMeteo_variable,".",thisMeteo_level,sep = "")))]) # what is the value for this variable at this level?
    } else if (thisMeteo_24_summary == "mean") {
    this_meteo_value <- mean(this_date_loc[ , c(as.character(paste(thisMeteo_variable,".",thisMeteo_level,sep = "")))]) # what is the value for this variable at this level?
    } else if (thisMeteo_24_summary == "sum") {
    this_meteo_value <- sum(this_date_loc[ , c(as.character(paste(thisMeteo_variable,".",thisMeteo_level,sep = "")))]) # what is the value for this variable at this level?
    }
    
    Step5_NAM_data[x, c(as.character(paste(thisMeteo_variable,".",thisMeteo_level,sep = "")))] <- this_meteo_value
    
    
    
  } # for (meteo_var_counter in 1:dim(MeteoVars)[1]) { # cycle through variables(levels) of interest
  
               
                         
#"HPBL.surface"           "TMP.2.m.above.ground"  
#  [10] "RH.2.m.above.ground"    "DPT.2.m.above.ground"   "APCP.surface"          
#  [13] "WEASD.surface"          "SNOWC.surface"          "UGRD.10.m.above.ground"
#  [16] "VGRD.10.m.above.ground" "PRMSL.mean.sea.level"   "PRES.surface"          
#  [19] "DZDT.850.mb"            "DZDT.700.mb"            "UTC.Date.Time"         

  }) # end lapply command