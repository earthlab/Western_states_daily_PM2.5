process_PM25_Fire_Cache_data_source.fn <- function(input_header, ProcessedData.directory, FireCache.directory, data_set_counter) {
  # combineFire Cache. PM2.5 data files into 1 dataframe
  
  ##### Create Sink output file and create its header ####
  # sink command sends R output to a file. Don't try to open file until R has closed it at end of script. https://www.rdocumentation.org/packages/base/versions/3.4.1/topics/sink
  SinkFileName=file.path(ProcessedData.directory,"PM25_data_source_Fire_Cache_combining_sink.txt")
  sink(file =SinkFileName, append = FALSE, type = c("output","message"), split = FALSE) # UNCOMMENT
  cat("Code and R output for process_PM25_Fire_Cache_data_source_function.R \n \n")
  cat("Title: process_PM25_Fire_Cache_data_source_function.R \n")
  cat("Author: Melissa May Maestas, PhD \n")
  cat("Original Date: September 24, 2018 \n")
  cat("Latest Update: September 24, 2018 \n")
  cat("This program reads in and PM2.5 data from theFire Cache.. \n")
  
  #### Create data frame ####
  N_columns <- length(input_header) # how many columns are in header?
  input_mat1 <- data.frame(matrix(NA,nrow=10,ncol=N_columns)) # create data frame for input_mat1
  names(input_mat1) <- input_header # assign the header to input_mat1
  rm(N_columns)
  
  #### Pull in Fire Cache Smoke (DRI) data #################
  print('still need to download the files that have been password protected.')
  data_source_counter <- data_set_counter #data_source_counter+1 # counter to distinguish between the various data sources (differentiate by color on  maps)
  Data_Source_Name_Short <- "FireCacheDRI"
  Data_Source_Name_Display <- "Fire Cache Smoke Monitor (DRI)"
  this_Datum <- "NAD83" # per email from Joshua Walston on 5/29/2018

  # what files are in the FireCache.directory?
  # https://stat.ethz.ch/R-manual/R-devel/library/base/html/list.files.html
  all_DRI_Files <- list.files(path = file.path(FireCache.directory,"."), pattern = NULL, all.files = FALSE,
                              full.names = FALSE, recursive = FALSE,
                              ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  #print(all_DRI_Files)
  
  # cycle through files
  for (this_file_counter in 1:length(all_DRI_Files)){  
    #print(paste('this_file_counter =',this_file_counter))
    this_source_file <- all_DRI_Files[this_file_counter]
    print(this_source_file)
    
    # load monitor name
    this_name <- as.character(read.csv(file.path(FireCache.directory,this_source_file),header = F,nrows = 1)[1,1])
    print(this_name)
    
    # get file header, consolidated
    one_row_header <- Fire_Cache_consolidate_file_header.fn(FireCache.directory,this_source_file)
    
    # load main part of this data file
    this_Fire_Cache_data_step <- read.csv(file.path(FireCache.directory,this_source_file),header = F,skip = 4)
    
    # attach the header compiled in the for loop above to the data
    names(this_Fire_Cache_data_step) <- one_row_header
    rm(one_row_header)
    
    # not all Fire Cache files have the same header; create a comprehensive header, adding columns that are in the current
    # file that have not been in previous files. Also, add a few columns to the header
    comprehensive.header <- Fire_Cache_comprehensive_header.fn(this_file_counter, this_Fire_Cache_data_step)

    # The header is (sometimes/always?) repeated further down in the data. These rows need to be found and removed.
    this_Fire_Cache_data <- Fire_Cache_remove_repeat_headers.fn(this_Fire_Cache_data_step)
    rm(this_Fire_Cache_data_step) # clear variables
    
    # I don't think these 4 lines should be here# handle date information
    #new_col_number <- length(this_Fire_Cache_data)+1 # figure out how many columns are in data and then add 1
    #this_Fire_Cache_data[,new_col_number] <- as.Date(this_Fire_Cache_data[,1],"%m/%d/%Y") # add column at end of data and fill it with dates in format R will recognize https://www.statmethods.net/input/dates.html
    #colnames(this_Fire_Cache_data)[new_col_number] <- "R_Dates"
    #rm(new_col_number)
    
    #### take 24-hr averages
    Fire_Cache_daily_averages.fn()
    
    #### Input Fire Cache data into input_mat ####
    # figure out what row we're on in input_mat
    row_stop <- row_start+dim(Daily_Fire_Cache)[1]-1
    
    ## fill in input_mat1 with Daily_Fire_Cache data for this DRI file
    
    # put serial # in the Site_Num column of input_mat1
    print(paste("input DRI serial number into ","Site_Num" ," in input_mat1"))   
    input_mat1[row_start:row_stop,c("Site_Num")] <- as.numeric(as.character(Daily_Fire_Cache[,c("ser # Serial  Number ")]))
    
    # input lat and lon ("PM2.5_Lat" and "PM2.5_Lon")
    which_colLat <- which(colnames(Daily_Fire_Cache)==" Deg    GPS     Lat. ")
    input_mat1[row_start:row_stop,c('PM2.5_Lat')] <- as.numeric(as.character(Daily_Fire_Cache[,which_colLat]))
    rm(which_colLat)
    which_colLon <- which(colnames(Daily_Fire_Cache)==" Deg    GPS     Lon. ")
    input_mat1[row_start:row_stop,c('PM2.5_Lon')] <- as.numeric(as.character(Daily_Fire_Cache[,which_colLon]))
    rm(which_colLon)
    # input InDayLatDiff and InDayLonDiff - tells how much variation there is in the Lat & Lon obs within a day (relevant for hourly observations)
    which_colLatDayDiff <- which(colnames(Daily_Fire_Cache) == "InDayLatDiff") # identify which column in Daily_Fire_Cache we are looking for 
    input_mat1[row_start:row_stop,c("InDayLatDiff")] <- as.numeric(as.character(Daily_Fire_Cache[,which_colLatDayDiff])) # input that column into input_mat1
    rm(which_colLatDayDiff) # remove column variable
    print("line 1040")
    which_colLonDayDiff <- which(colnames(Daily_Fire_Cache) == "InDayLonDiff") # identify which column in Daily_Fire_Cache we are looking for 
    input_mat1[row_start:row_stop,c("InDayLonDiff")] <- as.numeric(as.character(Daily_Fire_Cache[,which_colLonDayDiff])) # input that column into input_mat1
    print("line 1042")
    rm(which_colLonDayDiff) # remove column variable  
    
    # input flags for Lat & Lon observations
    which_colLatFlg <- which(colnames(Daily_Fire_Cache)=="           flg. Deg    GPS     Lat. ") # identify which column in Daily_Fire_Cache we are looking for 
    input_mat1[row_start:row_stop,c("flg.Lat")] <- as.character(Daily_Fire_Cache[,which_colLatFlg])# input that column into input_mat1
    rm(which_colLatFlg)
    which_colLonFlg <- which(colnames(Daily_Fire_Cache)=="           flg. Deg    GPS     Lon. ") # identify which column in Daily_Fire_Cache we are looking for 
    input_mat1[row_start:row_stop,c("flg.Lon")] <- as.character(Daily_Fire_Cache[,which_colLonFlg])# input that column into input_mat1
    rm(which_colLonFlg)
    
    # input "Sample_Duration"          
    input_mat1[row_start:row_stop,c("Sample_Duration")] <- "1 HOUR"
    
    # input "Date_Local" into input_mat1
    this_col_input_mat <- "Date_Local"
    this_col_source <- ":           :   Date    :MM/DD/YYYY"
    SourceVar <- as.Date(Daily_Fire_Cache[,c(this_col_source)],"%Y-%m-%d")
    #print(SourceVar)
    SourceVarChar <- format(SourceVar,"%Y-%m-%d")
    #print(SourceVarChar)
    input_mat1[row_start:row_stop,c(this_col_input_mat)] <- SourceVarChar
    rm(this_col_input_mat,this_col_source,SourceVar,SourceVarChar)
    
    # input "Units_of_Measure" into input_mat1
    input_mat1[row_start:row_stop,c("Units_of_Measure")] <- "ug/m3 Conc     RT    "
    
    # input "Observation_Count" 
    input_mat1[row_start:row_stop,c("Observation_Count")] <- Daily_Fire_Cache[,c("N_Obs")]
    
    # input "Observation_Percent"     
    input_mat1[row_start:row_stop,c("Observation_Percent")] <- Daily_Fire_Cache[,c("N_Obs")]/24*100
    
    # input "PM2.5_Obs"               
    which_colConc <- which(colnames(Daily_Fire_Cache)=="ug/m3 Conc     RT    ")
    #concentration_vector <- Daily_Fire_Cache[,which_colConc]
    input_mat1[row_start:row_stop,c('PM2.5_Obs')] <- as.numeric(as.character(Daily_Fire_Cache[,which_colConc]))
    rm(which_colConc)
    
    # input flag for PM2.5 Obs
    which_ConcFlg <- which(colnames(Daily_Fire_Cache)=="           flg.ug/m3 Conc     RT    ") # identify which column in Daily_Fire_Cache we are looking for 
    input_mat1[row_start:row_stop,c("flg.PM25_Obs")] <- as.character(Daily_Fire_Cache[,which_ConcFlg])# input that column into input_mat1
    rm(which_ConcFlg)
    
    # input "1st_Max_Value"            
    which_col <- which(colnames(Daily_Fire_Cache)=="1st_Max_Value")
    data_vector <- Daily_Fire_Cache[,which_col]
    input_mat1[row_start:row_stop,c("1st_Max_Value")] <- as.numeric(as.character(Daily_Fire_Cache[,which_col]))
    rm(which_col,data_vector)
    
    # "1st_Max_Hour"            
    which_col <- which(colnames(Daily_Fire_Cache)=="1st_Max_Hour")
    data_vector <- Daily_Fire_Cache[,which_col]
    input_mat1[row_start:row_stop,c("1st_Max_Hour")] <- as.character(Daily_Fire_Cache[,which_col])#as.integer(as.character(Daily_Fire_Cache[,which_col]))
    rm(which_col,data_vector)
    
    # input "PM25_Station_Name"        
    input_mat1[row_start:row_stop,c("PM25_Station_Name")] <- this_name
    rm(this_name)
    
    #"Data_Source_Name_Display" 
    input_mat1[row_start:row_stop,c("Data_Source_Name_Display")] <- Data_Source_Name_Display
    
    # "Data_Source_Name_Short"  
    input_mat1[row_start:row_stop,c("Data_Source_Name_Short")] <- Data_Source_Name_Short
    
    # input "Data_Source_Counter" - indicates if this isFire Cache. data or field data, etc.
    input_mat1[row_start:row_stop,c("Data_Source_Counter")] <- data_source_counter
    
    # input color for plotting this data source (totally arbitrary choice of color)
    input_mat1[row_start:row_stop,c("PlottingColor")] <- "red"
    #c("darkgoldenrod","green","blue") "PlottingColor"
    
    # input source file name ("Source_File")
    input_mat1[row_start:row_stop,c('Source_File')] <- this_source_file
    
    # "Composite_of_N_rows"      
    input_mat1[row_start:row_stop,c("Composite_of_N_rows")] <- Daily_Fire_Cache[,c("N_Obs")] 
    
    # "N_Negative_Obs"   
    input_mat1[row_start:row_stop,c("N_Negative_Obs")] <- Daily_Fire_Cache[,c("N_neg")]
    
    # input air flow and flag information
    which_col <- which(colnames(Daily_Fire_Cache)==" l/m   Ave.   Air Flw")
    input_mat1[row_start:row_stop,c("l/m Ave. Air Flw")] <- as.numeric(as.character(Daily_Fire_Cache[,which_col]))
    rm(which_col)
    which_col <- which(colnames(Daily_Fire_Cache)=="           flg. l/m   Ave.   Air Flw") # identify which column in Daily_Fire_Cache we are looking for 
    input_mat1[row_start:row_stop,c("flg.AirFlw")] <- as.character(Daily_Fire_Cache[,which_col])# input that column into input_mat1
    rm(which_col)
    
    # input Air Temp and flag information                  
    which_col <- which(colnames(Daily_Fire_Cache)=="Deg C  Av Air   Temp ")
    input_mat1[row_start:row_stop,c("Deg C Av Air Temp")] <- as.numeric(as.character(Daily_Fire_Cache[,which_col]))
    rm(which_col)
    which_col <- which(colnames(Daily_Fire_Cache)=="           flg.Deg C  Av Air   Temp ") # identify which column in Daily_Fire_Cache we are looking for 
    input_mat1[row_start:row_stop,c("flg.AirTemp")] <- as.character(Daily_Fire_Cache[,which_col])# input that column into input_mat1
    rm(which_col)
    
    # input Relative Humidity and flag information                  
    which_col <- which(colnames(Daily_Fire_Cache)=="  %     Rel   Humidty")
    input_mat1[row_start:row_stop,c("% Rel Humidty")] <- as.numeric(as.character(Daily_Fire_Cache[,which_col]))
    rm(which_col)
    which_col <- which(colnames(Daily_Fire_Cache)=="           flg.  %     Rel   Humidty") # identify which column in Daily_Fire_Cache we are looking for 
    input_mat1[row_start:row_stop,c("flg.RelHumid")] <- as.character(Daily_Fire_Cache[,which_col])# input that column into input_mat1
    rm(which_col)            
    
    # input Barometric Pressure and flag information                  
    which_col <- which(colnames(Daily_Fire_Cache)=="mbar   Barom   Press ")
    input_mat1[row_start:row_stop,c("mbar Barom Press")] <- as.numeric(as.character(Daily_Fire_Cache[,which_col]))
    rm(which_col)
    which_col <- which(colnames(Daily_Fire_Cache)=="           flg.mbar   Barom   Press ") # identify which column in Daily_Fire_Cache we are looking for 
    input_mat1[row_start:row_stop,c("flg.Barom Press")] <- as.character(Daily_Fire_Cache[,which_col])# input that column into input_mat1
    rm(which_col)  
    
    # input Internal Temperature and flag information                  
    which_col <- which(colnames(Daily_Fire_Cache)=="deg C Sensor  Int AT ")
    input_mat1[row_start:row_stop,c("deg C Sensor  Int AT")] <- as.numeric(as.character(Daily_Fire_Cache[,which_col]))
    rm(which_col)
    which_col <- which(colnames(Daily_Fire_Cache)=="           flg.deg C Sensor  Int AT ") # identify which column in Daily_Fire_Cache we are looking for 
    input_mat1[row_start:row_stop,c("flg.deg C Sensor Int AT")] <- as.character(Daily_Fire_Cache[,which_col])# input that column into input_mat1
    rm(which_col)    
    
    # input Internal Relative Humidity and flag information                  
    which_col <- which(colnames(Daily_Fire_Cache)=="  %   Sensor  Int RH ")
    input_mat1[row_start:row_stop,c("% Sensor Int RH")] <- as.numeric(as.character(Daily_Fire_Cache[,which_col]))
    rm(which_col)
    which_col <- which(colnames(Daily_Fire_Cache)=="           flg.  %   Sensor  Int RH ") # identify which column in Daily_Fire_Cache we are looking for 
    input_mat1[row_start:row_stop,c("flg.%SensorIntRH")] <- as.character(Daily_Fire_Cache[,which_col])# input that column into input_mat1
    rm(which_col) 
    
    # input Wind Speed and flag information                  
    which_col <- which(colnames(Daily_Fire_Cache)==" m/s    Wind    Speed")
    input_mat1[row_start:row_stop,c("Wind Speed m/s")] <- as.numeric(as.character(Daily_Fire_Cache[,which_col]))
    rm(which_col)
    which_col <- which(colnames(Daily_Fire_Cache)=="           flg. m/s    Wind    Speed") # identify which column in Daily_Fire_Cache we are looking for 
    input_mat1[row_start:row_stop,c("flg.WindSpeed")] <- as.character(Daily_Fire_Cache[,which_col])# input that column into input_mat1
    rm(which_col) 
    
    # input Battery Voltage and flag information                  
    which_col <- which(colnames(Daily_Fire_Cache)=="volts Battery Voltage")
    input_mat1[row_start:row_stop,c("Battery Voltage volts")] <- as.numeric(as.character(Daily_Fire_Cache[,which_col]))
    rm(which_col)
    which_col <- which(colnames(Daily_Fire_Cache)=="           flg.volts Battery Voltage") # identify which column in Daily_Fire_Cache we are looking for 
    input_mat1[row_start:row_stop,c("flg.BatteryVoltage")] <- as.character(Daily_Fire_Cache[,which_col])# input that column into input_mat1
    rm(which_col) 
    
    # input Alarm and flag information                  
    which_col <- which(colnames(Daily_Fire_Cache)=="      Alarm          ")
    input_mat1[row_start:row_stop,c("Alarm")] <- as.numeric(as.character(Daily_Fire_Cache[,which_col]))
    rm(which_col)
    which_col <- which(colnames(Daily_Fire_Cache)=="           flg.      Alarm          ") # identify which column in Daily_Fire_Cache we are looking for 
    input_mat1[row_start:row_stop,c("flg.Alarm")] <- as.character(Daily_Fire_Cache[,which_col])# input that column into input_mat1
    rm(which_col) 
    
    # "Datum"
    input_mat1[row_start:row_stop,c("Datum")] <- this_Datum
    
    ## columns of data in input_mat1 to figure out in DRI data
    # "State_Name" "County_Name" "State_Abbrev"  "Month" "Day"  
    # "Winter" "Year"  
    
    ## think about whether we need to figure out how to fill in these variables in input_mat1:
    # "Parameter_Code"  "Parameter_Name" "Pollutant_Standard" "Event_Type" "AQI" "Method_Code"              "Method_Name" 
    # "City_Name" "POC"                               
    # "Address" "CBSA_Name"  "Date_of_Last_Change"                                      
    # "flg.Site_Num"   "Type"                     "flg.Type"       
    
    ## think about whether any of thes variables in Daily_Fire_Cache need to be brought into input_mat1
    # " GMT  Time    hh:mm "  "      Type           " , "           flg.      Type           "
    # "           flg.ser # Serial  Number ", " Unk   Misc     #1   " ,"           flg. Unk   Misc     #1   "
    #  " Deg   Wind    Direc "                "           flg. Deg   Wind    Direc "
    
    # if (this_file_counter==length(all_DRI_Files)){stop("on last file")}
    rm(Daily_Fire_Cache,this_Fire_Cache_data)
    # tick up the row counter
    row_start <- row_stop+1
    print(paste("Done processing ",this_source_file))
    rm(this_source_file)
  } # for (this_file_counter in 1:length(all_DRI_Files)){
  rm(all_DRI_Files,this_file_counter,comprehensive.header)
  
  rm(Data_Source_Name_Display,Data_Source_Name_Short)
  rm(this_Datum) 
  
#### output input_mat1 from function ####  
  return(input_mat1) # output from function
} # end function