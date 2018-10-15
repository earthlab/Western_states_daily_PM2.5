# Various functions for the PM2.5 input_mat

# Change class of various columns, e.g., get it to recognize dates as dates, etc
input_mat_change_data_classes.fn <- function(input_mat_step1) {
  input_mat_step2 <- input_mat_step1 # new name for output of function
  
  #c('PM2.5_Obs','PM2.5_Lat','PM2.5_Lon','Datum',
  
  input_mat_step2$Date_Local <- as.Date(input_mat_step1$Date_Local,"%Y-%m-%d") # recognize dates as dates: 'Date_Local' 
  input_mat_step2$Year <- as.numeric(input_mat_step1$Year) # 'Year' as numeric
  input_mat_step2$Month <- as.numeric(input_mat_step1$Month) # 'Month' as numeric
  input_mat_step2$Day <- as.numeric(input_mat_step1$Day) # 'Day' as numeric
  
  # State_Code','County_Code',
  #  'Site_Num','Parameter_Code','POC','Parameter_Name','Sample_Duration','Pollutant_Standard','Units_of_Measure',
  #  'Event_Type','Observation_Count','Observation_Percent','1st_Max_Value','1st_Max_Hour','AQI','Method_Code',
  #  'Method_Name',
  # 'PM25_Station_Name'
  input_mat_step2$PM25_Station_Name <- as.character(input_mat_step1$PM25_Station_Name)
  
  #'Address','State_Name','County_Name','City_Name','CBSA_Name',
  #  'Date_of_Last_Change', # columns in AQS data
  #  'State_Abbrev','Winter','Data_Source_Name_Display','Data_Source_Name_Short','Data_Source_Counter',
  #  'Source_File','Composite_of_N_rows','N_Negative_Obs', # other columns to include
  #  "flg.Lat","flg.Lon","Type","flg.Type","flg.Site_Num","flg.PM25_Obs","l/m Ave. Air Flw", # DRI variables
  #  "flg.AirFlw","Deg C Av Air Temp","flg.AirTemp","% Rel Humidty","flg.RelHumid","mbar Barom Press","flg.Barom Press", # DRI variables
  #  "deg C Sensor  Int AT","flg.deg C Sensor Int AT","% Sensor Int RH","flg.%SensorIntRH", # DRI variables
  #  "Wind Speed m/s","flg.WindSpeed","Battery Voltage volts","flg.BatteryVoltage","Alarm","flg.Alarm", # DRI variables
  #  "InDayLatDiff","InDayLonDiff","PlottingColor","SerialNumber")
  
  return(input_mat_step2)
} # end of Fire_Cache_change_data_classes.fn function

# extract year from date information
input_mat_extract_year_from_date.fn <- function(date_vec_in) {
  # date_vec_in <- small_input_mat$Date_Local
  date_vec <- as.Date(date_vec_in, "%Y-%m-%d") # make sure the year is the first 4 digits in the date formatting
  date_char <- as.character(date_vec) # convert the date to character
  year_char <- substr(date_char,1,4) # extract the first 4 characters in date, i.e., the year
  year_vec <- as.numeric(year_char) # convert the year to numerical value
  rm(date_vec, date_char, year_char)
  return(year_vec) # function output
} # end of input_mat_extract_year_from_date.fn function

# extract month from date information
input_mat_extract_month_from_date.fn <- function(date_vec_in) {
  # date_vec_in <- small_input_mat$Date_Local
  date_vec <- as.Date(date_vec_in, "%Y-%m-%d") # make sure the month is digits 6-7 in the date formatting
  date_char <- as.character(date_vec) # convert the date to character
  month_char <- substr(date_char,6,7) # extract characters 6-7 in date, i.e., the month
  month_vec <- as.numeric(month_char) # convert the month to numerical value
  rm(date_vec, date_char, month_char)
  return(month_vec) # function output
} # end of input_mat_extract_month_from_date.fn function

# extract day from date information
input_mat_extract_day_from_date.fn <- function(date_vec_in) {
  # date_vec_in <- small_input_mat$Date_Local
  date_vec <- as.Date(date_vec_in, "%Y-%m-%d") # make sure the day is digits 9-10 in the date formatting
  date_char <- as.character(date_vec) # convert the date to character
  day_char <- substr(date_char,9,10) # extract the first 4 characters in date, i.e., the day
  day_vec <- as.numeric(day_char) # convert the day to numerical value
  rm(date_vec, date_char, day_char)
  return(day_vec) # function output
} # end of input_mat_extract_day_from_date.fn function

# small function used in subset_data_frame_via_vector.fn
fancy_which.fn <- function(value_2_subset, col_for_subset, full_data_frame) {
  # col_for_subset <- "State"
  # full_data_frame <- FMLEdata_all_states
  # value_2_subset <- "AZ"
  
  rows_interest <- which(full_data_frame[, col_for_subset]== value_2_subset)
  
} # end of fancy_which.fn function

# subset data frame by vector
subset_data_frame_via_vector.fn <- function(vector_for_subset, full_data_frame,col_for_subset) {
  # col_for_subset <- "State"
  # vector_for_subset <- c("AZ","CA","CO", "ID", "MT", "NV", "NM", "OR", "UT", "WA", "WY")
  # full_data_frame <- FMLEdata_all_states
  # isolate data from the Study area
  # FMLE_StudyStates <- FMLEdata_all_states[which(FMLEdata_all_states$State=="AZ"|
  #                                                 FMLEdata_all_states$State=="CA"|
  #                                                 FMLEdata_all_states$State=="CO"|
  #                                                 FMLEdata_all_states$State=="ID"|
  #                                                 FMLEdata_all_states$State=="MT"|
  #                                                 FMLEdata_all_states$State=="NV"|
  #                                                 FMLEdata_all_states$State=="NM"|
  #                                                 FMLEdata_all_states$State=="OR"|
  #                                                 FMLEdata_all_states$State=="UT"|
  #                                                 FMLEdata_all_states$State=="WA"|
  #                                                 FMLEdata_all_states$State=="WY"|
  #                                                 FMLEdata_all_states$State=="ND"|
  #                                                 FMLEdata_all_states$State=="SD"|
  #                                                 FMLEdata_all_states$State=="NE"|
  #                                                 FMLEdata_all_states$State=="KS"|
  #                                                 FMLEdata_all_states$State=="OK"|
  #                                                 FMLEdata_all_states$State=="TX"), ]
  
  # value_2_subset <- vector_for_subset[1]
  # rows_interest <- fancy_which.fn(col_for_subset, full_data_frame, value_2_subset) 
  
  list_rows_interest <- lapply(X = vector_for_subset, FUN = fancy_which.fn, col_for_subset = col_for_subset, full_data_frame = full_data_frame)
  
  which_rows_interest <- do.call("c", list_rows_interest)
  
  subset_data_frame <- full_data_frame[which_rows_interest, ]
  
  return(subset_data_frame)
  
} # end of subset_data_frame_via_vector.fn function

# Split EPA site codes (no hyphens) into state, county, site number components as a new data frame - write new function for codes with hyphens
EPA_codes_2_components_no_hyphens.fn <- function(EPA_codes_vec) {
  #EPA_codes_vec <- unique(FMLE_StudyStates$EPACode) # example input
  
  EPACode_components_header <-  c("EPACode","StateCode","CountyCode","SiteNum") # define header for data frame for EPA code components
  EPACode_components <- data.frame(matrix(NA,nrow=length(EPA_codes_vec),ncol=length(EPACode_components_header))) # create data frame for EPA codes split into components
  names(EPACode_components) <- EPACode_components_header # assign the header to data frame
  rm(EPACode_components_header) # clear variable
  
  EPACode_components$EPACode <- EPA_codes_vec # input the original code into data frame3
  
  # Split FMLE EPACode into State_Code, County_Code and Site_Num 
  for (this_row in 1:length(EPA_codes_vec)) { # cycle through each row in FMLE data to determine state code, county code, and site num and put into input_mat1
    this_EPACode <- as.character((EPACode_components[this_row,c("EPACode")])) # isolate the EPA code for this row of data
    #print(this_EPACode) # COMMENT
    if (is.na(this_EPACode)==TRUE) { # if the EPA code is NA, then all components are NA
      EPACode_components[this_row,c("StateCode")] <- NA # define state code
      EPACode_components[this_row,c("CountyCode")] <- NA # define county code component
      EPACode_components[this_row,c("SiteNum")] <- NA # define site number component
    } else if (nchar(this_EPACode)==8) { # determine how many characters are in EPACode (leading zeros are not in the data)
      # print("8 characters") # COMMENTS
      EPACode_components[this_row,c("StateCode")] <- substr(this_EPACode,1,1) # isolate state code
      EPACode_components[this_row,c("CountyCode")] <- substr(this_EPACode,2,4) # isolate county code
      EPACode_components[this_row,c("SiteNum")] <- substr(this_EPACode,5,8)  # isolate site num
    } else if (nchar(this_EPACode)==9) { # 9 characters in EPA code
      # print("9 characters") #COMMENTS
      EPACode_components[this_row,c("StateCode")] <- substr(this_EPACode,1,2) # isolate state code
      EPACode_components[this_row,c("CountyCode")] <- substr(this_EPACode,3,5) # isolate county code
      EPACode_components[this_row,c("SiteNum")] <- substr(this_EPACode,6,9)  # isolate site num
    } else { # unexpected number of characters in EPA code
      stop("check data/code") # unexpected input
    } # if (is.na(this_EPACode)==TRUE) { # if the EPA code is NA, then all components are NA
    rm(this_EPACode)
  } # for (this_row in row_start:row_stop) { # cycle through each row in FMLE data to determine state code, county code, and site num and put into input_mat1
  rm(this_row) # clear variable
  
  return(EPACode_components) # output from function
  
} # end of extract_state_from_EPA_code_no_hyphens function
  
# fill in the EPA code components into input_mat1
# #fill_in_input_mat1_EPA_code_components.fn <- function(input_mat1,EPACode_components,source_mat,source_code_col) {
# # input_mat1 and source_mat must have the same number of rows, which will be kept in the same order
#   #source_mat <- CARB_data # example input
#   #source_code_col <- "AQS.Site.ID" # example input
#     for (this_row in 1:dim(EPACode_components)[1]) { # put columns of state code, county code, and site number into input_mat1_StudyStates_sepCodes
#     #this_code <- EPACode_components[this_row,c("EPACode")] # what are the codes for this row of EPACode_components?
#     #print(this_code) # this row of code
#     rows_of_interest <- which(source_mat[ ,source_code_col]==EPACode_components[this_row,c("EPACode")]) # what rows in input_mat1 has this EPA code?
#     length(rows_of_interest)
#     input_mat1[rows_of_interest,c("StateCode")] <- EPACode_components[this_row,c("StateCode")] # input state code
#     print(input_mat1[rows_of_interest,c("StateCode")])
#     input_mat1[rows_of_interest,c("CountyCode")] <- EPACode_components[this_row,c("CountyCode")] # input county code
#     input_mat1[rows_of_interest,c("SiteNum")] <- EPACode_components[this_row,c("SiteNum")] # input site number
#     rm(rows_of_interest) # clear variable
#   } # for (this_row in 1:dim(EPACode_components)[1]) { # put columns of state code, county code, and site number into input_mat1
#   rm(this_row) # clear variable
#   return(input_mat1) # output from function
# } # end of fill_in_input_mat1_code_components.fn function  
  


