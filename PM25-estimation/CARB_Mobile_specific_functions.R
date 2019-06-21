# CARB_Mobile_specific_functions.R

drag_values_to_next_value.fn <- function(df_in, col_interest){
  # fill in column values to all of the rows (when raw data only shows value at the beginning of a sequence of observations)
  which_val <- which(!is.na(df_in[ , col_interest])) # which rows of data have lat/lon information?
  if (length(which_val) > 0) { # check that there is data to drag down
    for (counter_i in 1:length(which_val)) { # cycle through the rows with lat/lon obs and fill in the rest of the rows
      obs_row_i <- which_val[counter_i] # get the row number of a lat/lon obs
      if (counter_i < length(which_val)) { # if-statement to handle end of file slightly different
        counter_i_plus_1 <- counter_i + 1 # count up to find the next lat/lon obs row
        next_obs_row <- which_val[counter_i_plus_1] # get the row number of the next lat/lon observation - to avoid over-writing it
      } else { # handle end of file slightly different
        counter_i_plus_1 <- NA # not needed for end of file
        next_obs_row <- dim(df_in)[1]+1 # find last row number in file and add one so that the code below works whether or not it's on the last lat/lon obs
      } # if (counter_i < length(which_val)) { # if-statement to handle end of file slightly different
      rm(counter_i_plus_1)
      # fill in rows of data with missing lat/lon data
      df_in[(obs_row_i+1):(next_obs_row-1),col_interest] <- df_in[obs_row_i,col_interest] # fill in column of interest
    } # for (counter_i in 1:length(which_val)) { # cycle through the rows with lat/lon obs and fill in the rest of the rows
  } # if (length(which_val) > 0) { # check that there is data to drag down
  return(df_in)
} # end of drag_values_to_next_value.fn function

# Change class of various columns, e.g., get it to recognize dates as dates, etc
CARB_Mobile_change_data_classes.fn <- function(Merged_CARB_Mobile) {
  options(warn  =  1) # don't throw an error when there's a warning and stop the code from running further
  Merged_CARB_Mobile$ConcHr <- as.numeric(Merged_CARB_Mobile$ConcHr)
  Merged_CARB_Mobile$Latitude <- as.numeric(Merged_CARB_Mobile$Latitude)
  Merged_CARB_Mobile$Longitude <- as.numeric(Merged_CARB_Mobile$Longitude)
  options(warn  =  2) # throw an error when there's a warning and stop the code from running further
  return(Merged_CARB_Mobile)
} # end of CARB_Mobile_change_data_classes.fn function

# Loop through days to create data frame of 24-hr averages (used in CARB_Mobile_1_file_to_small_input_mat.fn below)
CARB_Mobile_daily_averages.fn <- function(Merged_CARB_Mobile) {
  dates_unique <- unique(Merged_CARB_Mobile$Date.Local)
  print(paste("CARB_Mobile data spans ",length(dates_unique) ," dates between ",min(dates_unique)," -- ",max(dates_unique),sep = ""))
  outer_lapply_output <- lapply(1:length(dates_unique), function(this_date_i) { # start lapply function - cycle through all dates
    this_date <- dates_unique[this_date_i] # get the date
    # isolate data for this date
    which_this_date <- which(Merged_CARB_Mobile$Date.Local == this_date)
    this_date_data <- Merged_CARB_Mobile[which_this_date, ] 
    rm(which_this_date)
    unique_monitors <- unique(this_date_data$FileName)
    #print(paste("There were ",length(unique_monitors)," monitors operating on ",this_date,".",sep = ""))
    
    # cycle through monitors within a date
    inner_lapply_output <- lapply(1:length(unique_monitors), function(this_mon_i) { # start lapply function - cycle through all dates
      this_monitor <- unique_monitors[this_mon_i]
      #print(paste(this_date,"--",this_monitor))
      # isolate data for this monitor (on this date)
      which_this_monitor <- which(this_date_data$FileName == this_monitor)
      this_monitor_day_data_step <- this_date_data[which_this_monitor, ]
      this_monitor_day_data <- make_unique_hours_obs.fn(this_monitor_day_data_step)
      if (dim(this_monitor_day_data)[1] > 24) {stop("more than 24 rows of data for a given day/monitor. Investigate.")} 
      # initialize data frame for output row of data
      this_day_mon_ave <- data.frame(matrix(NA,nrow=1,ncol=length(input_header))) # create data frame for this_day_mon_ave
      names(this_day_mon_ave) <- input_header # assign the header to this_day_mon_ave
      this_day_mon_ave <- input_mat_change_data_classes.fn(this_day_mon_ave)
      
      this_day_mon_ave[ ,"PM2.5_Obs"] <- mean(this_monitor_day_data$ConcHr_mug_m3) # PM2.5 concentration
      this_day_mon_ave[ ,"PM2.5_Lat"] <- mean(this_monitor_day_data$Latitude) # Latitude       
      this_day_mon_ave[ ,"PM2.5_Lon"] <- mean(this_monitor_day_data$Longitude) # Longitude              
      this_day_mon_ave[ ,"Datum"] <- this_Datum # datum
      this_day_mon_ave[ ,"Date_Local"] <- unique(this_monitor_day_data$Date.Local) # local date             
      this_day_mon_ave[ ,"Year"] <- year(unique(this_monitor_day_data$Date.Local))                     
      this_day_mon_ave[ ,"Month"] <- month(unique(this_monitor_day_data$Date.Local))     
      this_day_mon_ave[ ,"Day"] <-  day(unique(this_monitor_day_data$Date.Local))                  
      #this_day_mon_ave[ ,"State_Code"] <-                
      #this_day_mon_ave[ ,"County_Code"] <-               
      #this_day_mon_ave[ ,"Site_Num"] <-                  
      #this_day_mon_ave[ ,"Parameter_Code"] <-           
      #this_day_mon_ave[ ,"POC"] <-                       
      #this_day_mon_ave[ ,"Parameter_Name"] <-            
      this_day_mon_ave[ ,"Sample_Duration"] <- "1 HOUR"       
      #this_day_mon_ave[ ,"Pollutant_Standard"] <-       
      this_day_mon_ave[ ,"Units_of_Measure"] <- "ug/m3"      
      #this_day_mon_ave[ ,"Event_Type"] <-                
      this_day_mon_ave[ ,"Observation_Count"] <- dim(this_monitor_day_data)[1]       
      this_day_mon_ave[ ,"Observation_Percent"] <- dim(this_monitor_day_data)[1]/24*100  
      #this_day_mon_ave[ ,"1st_Max_Value"] <-             
      #this_day_mon_ave[ ,"1st_Max_Hour"] <-              
      #this_day_mon_ave[ ,"AQI"] <-                       
      #this_day_mon_ave[ ,"Method_Code"] <-              
      #this_day_mon_ave[ ,"Method_Name"] <-               
      this_day_mon_ave[ ,"PM25_Station_Name"] <- unique(this_monitor_day_data$Alias)
      #this_day_mon_ave[ ,"Address"] <-                   
      this_day_mon_ave[ ,"State_Name"] <- "California"              
      #this_day_mon_ave[ ,"County_Name"] <-               
      #this_day_mon_ave[ ,"City_Name"] <-                 
      #this_day_mon_ave[ ,"CBSA_Name"] <-                 
      #this_day_mon_ave[ ,"Date_of_Last_Change"] <-      
      this_day_mon_ave[ ,"State_Abbrev"] <- "CA"          
      #this_day_mon_ave[ ,"Winter"] <-                    
      this_day_mon_ave[ ,"Data_Source_Name_Display"] <- Data_Source_Name_Display
      this_day_mon_ave[ ,"Data_Source_Name_Short"] <- Data_Source_Name_Short
      this_day_mon_ave[ ,"Data_Source_Counter"] <- data_set_counter    
      this_day_mon_ave[ ,"Source_File"] <-  unique(this_monitor_day_data$FileName)         
      this_day_mon_ave[ ,"Composite_of_N_rows"] <- dim(this_monitor_day_data)[1]  
        #which_neg_obs <- which(this_monitor_day_data$ConcHr_mug_m3 < 0)
      this_day_mon_ave[ ,"N_Negative_Obs"] <- sum(this_monitor_day_data$N_neg_obs) #length(which_neg_obs)         
       # rm(which_neg_obs)
      #this_day_mon_ave[ ,"flg.Lat"] <-                   
      #this_day_mon_ave[ ,"flg.Lon"] <-                   
      #this_day_mon_ave[ ,"Type"] <-                      
      #this_day_mon_ave[ ,"flg.Type"] <-                 
      #this_day_mon_ave[ ,"flg.Site_Num"] <-              
      #this_day_mon_ave[ ,"flg.PM25_Obs"] <-              
      #this_day_mon_ave[ ,"l/m Ave. Air Flw"] <-          
      #this_day_mon_ave[ ,"flg.AirFlw"] <-               
      #this_day_mon_ave[ ,"Deg C Av Air Temp"] <-         
      #this_day_mon_ave[ ,"flg.AirTemp"] <-               
      #this_day_mon_ave[ ,"% Rel Humidty"] <-             
      #this_day_mon_ave[ ,"flg.RelHumid"] <-             
      #this_day_mon_ave[ ,"mbar Barom Press"] <-          
      #this_day_mon_ave[ ,"flg.Barom Press"] <-           
      #this_day_mon_ave[ ,"deg C Sensor  Int AT"] <-      
      #this_day_mon_ave[ ,"flg.deg C Sensor Int AT"] <-  
      #this_day_mon_ave[ ,"% Sensor Int RH"] <-           
      #this_day_mon_ave[ ,"flg.%SensorIntRH"] <-          
      #this_day_mon_ave[ ,"flg.WindSpeed"] <-            
      this_day_mon_ave[ ,"Battery Voltage volts"] <- mean(this_monitor_day_data$Sys..Volts)
      if (max(this_monitor_day_data$Sys..Volts) > voltage_threshold_upper | min(this_monitor_day_data$Sys..Volts) < voltage_threshold_lower) {
        this_day_mon_ave[ ,"flg.BatteryVoltage"] <- c(max(this_monitor_day_data$Sys..Volts), min(this_monitor_day_data$Sys..Volts))
      } else {
        this_day_mon_ave[ ,"flg.BatteryVoltage"] <- 0
      }
      #this_day_mon_ave[ ,"Alarm"] <-                     
      #this_day_mon_ave[ ,"flg.Alarm"] <-                
      this_day_mon_ave[ ,"InDayLatDiff"] <- max(this_monitor_day_data$Latitude) - min(this_monitor_day_data$Latitude)            
      this_day_mon_ave[ ,"InDayLonDiff"] <- max(this_monitor_day_data$Longitude) - min(this_monitor_day_data$Longitude)
      this_day_mon_ave[ ,"PlottingColor"] <- this_plotting_color           
      #this_day_mon_ave[ ,"SerialNumber"] <- 
    
      return(this_day_mon_ave) # return processed data
    }) # end lapply function
    One_Day_all_monitors <- do.call("rbind", inner_lapply_output) #concatinate the output from each iteration  
    rm(inner_lapply_output)  
    return(One_Day_all_monitors) # return processed data
  }) # end lapply function
  Daily_CARB_Mobile <- do.call("rbind", outer_lapply_output) #concatinate the output from each iteration
  
  return(Daily_CARB_Mobile)
} # end of CARB_Mobile_daily_averages.fn function

make_unique_hours_obs.fn <- function(this_monitor_day_data_step) {
  unique_times <- unique((this_monitor_day_data_step$Date.Time.Local))
  lapply_output_hours <- lapply(1:length(unique_times), function(x){
    this_time <- unique_times[x]
    which_this_time <- which(this_monitor_day_data_step$Date.Time.Local == this_time)
    #print(paste(this_time,"has",length(which_this_time),"observations"))  
    if (length(which_this_time) == 1) {
      this_monitor_hour <- this_monitor_day_data_step[which_this_time, ]
      if (this_monitor_hour$ConcHr_mug_m3 < 0) {
        this_monitor_hour$N_neg_obs <- 1
      } else {
        this_monitor_hour$N_neg_obs <- 0
      }
    } else {
      this_monitor_hour_step <- this_monitor_day_data_step[which_this_time, ]
      this_monitor_hour <- data.frame(matrix(data = NA,nrow = 1,ncol = length(names(this_monitor_day_data_step))))
      names(this_monitor_hour) <- names(this_monitor_day_data_step)
      this_monitor_hour$MasterTable_ID <- mean(this_monitor_hour_step$MasterTable_ID)
      this_monitor_hour$Alias <- unique(this_monitor_hour_step$Alias)
      this_monitor_hour$Latitude <- mean(this_monitor_hour_step$Latitude)              
      this_monitor_hour$Longitude <- mean(this_monitor_hour_step$Longitude)           
      this_monitor_hour$Date.Time.GMT <- unique(this_monitor_hour_step$Date.Time.GMT)      
      this_monitor_hour$Start.Date.Time..GMT. <- unique(this_monitor_hour_step$Start.Date.Time..GMT.)
      this_monitor_hour$COncRT <- mean(as.numeric(as.character(this_monitor_hour_step$COncRT)))
      this_monitor_hour$ConcHr <- mean(this_monitor_hour_step$ConcHr)       
      this_monitor_hour$Flow <- mean(as.numeric(as.character(this_monitor_hour_step$Flow)))
      this_monitor_hour$W.S <- mean(as.numeric(as.character(this_monitor_hour_step$W.S)))
      this_monitor_hour$W.D <- mean(as.numeric(as.character(this_monitor_hour_step$W.D)))
      this_monitor_hour$AT <- mean(as.numeric(as.character(this_monitor_hour_step$AT)))         
      this_monitor_hour$RHx <- mean(as.numeric(as.character(this_monitor_hour_step$RHx)))
      this_monitor_hour$RHi <- mean(as.numeric(as.character(this_monitor_hour_step$RHi)))
      this_monitor_hour$BV <- mean(as.numeric(as.character(this_monitor_hour_step$BV)))
      this_monitor_hour$FT <- mean(as.numeric(as.character(this_monitor_hour_step$FT)))                
      this_monitor_hour$Alarm <- mean(as.numeric(as.character(this_monitor_hour_step$Alarm)))
      this_monitor_hour$Type <- unique(this_monitor_hour_step$Type)
      this_monitor_hour$Serial.Number <- mean(as.numeric(as.character(this_monitor_hour_step$Serial.Number)))
      this_monitor_hour$Version <- mean(as.numeric(as.character(this_monitor_hour_step$Version)))
      this_monitor_hour$Sys..Volts <- mean(as.numeric(as.character(this_monitor_hour_step$Sys..Volts)))
      this_monitor_hour$TimeStamp <- paste(as.character(this_monitor_hour_step$TimeStamp), collapse = "; ")
      this_monitor_hour$PDate <- paste(as.character(this_monitor_hour_step$PDate), collapse = "; ")
      this_monitor_hour$FileName <- unique(this_monitor_hour_step$FileName)
      this_monitor_hour$TimeStampParsed <- mean(this_monitor_hour_step$TimeStampParsed)
      this_monitor_hour$TimeStampTruncated <- unique(this_monitor_hour_step$TimeStampTruncated)
      this_monitor_hour$TimeStampLocal <- unique(this_monitor_hour_step$TimeStampLocal)
      this_monitor_hour$Date.Local <- unique(this_monitor_hour_step$Date.Local)
      this_monitor_hour$Date.Time.GMT.Parsed <- unique(this_monitor_hour_step$Date.Time.GMT.Parsed)
      this_monitor_hour$Date.Time.Local <- unique(this_monitor_hour_step$Date.Time.Local)
      this_monitor_hour$ConcHr_mug_m3 <- mean(this_monitor_hour_step$ConcHr_mug_m3)
      which_neg_obs <- which(this_monitor_hour_step$ConcHr_mug_m3 < 0)
      this_monitor_hour$N_neg_obs <- length(which_neg_obs)
    }
    this_monitor_hour$N_composite_rows <- length(which_this_time)
    return(this_monitor_hour)  
  })
  this_monitor_day_data <- do.call("rbind",lapply_output_hours)
  return(this_monitor_day_data)
} # end of reduce_more_than_24_obs.fn function
