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
  
  #### Create data frame - WILL MOVE THIS FURTHER DOWN IN CODE ####
  #N_columns <- length(input_header) # how many columns are in header?
  #input_mat1 <- data.frame(matrix(NA,nrow=10,ncol=N_columns)) # create data frame for input_mat1
  #names(input_mat1) <- input_header # assign the header to input_mat1
  #rm(N_columns)
  
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
    this_Fire_Cache_data_step2 <- Fire_Cache_remove_repeat_headers.fn(this_Fire_Cache_data_step)
    rm(this_Fire_Cache_data_step) # clear variables
    
    # I don't think these 4 lines should be here# handle date information
    #new_col_number <- length(this_Fire_Cache_data)+1 # figure out how many columns are in data and then add 1
    #this_Fire_Cache_data[,new_col_number] <- as.Date(this_Fire_Cache_data[,1],"%m/%d/%Y") # add column at end of data and fill it with dates in format R will recognize https://www.statmethods.net/input/dates.html
    #colnames(this_Fire_Cache_data)[new_col_number] <- "R_Dates"
    #rm(new_col_number)
    
    # Change class of various columns, e.g., get it to recognize dates as dates, etc
    this_Fire_Cache_data <- Fire_Cache_change_data_classes.fn(this_Fire_Cache_data_step2)
    rm(this_Fire_Cache_data_step2)
    
    #### take 24-hr averages for this 1 file
    Daily_Fire_Cache <- Fire_Cache_daily_averages.fn(this_Fire_Cache_data,comprehensive.header)
    
    #### Input Fire Cache data into input_mat ####
    Fire_Cache_1_file_to_small_input_mat.fn # for 1 file
    
    # write code to concatinate data from the various files
    
    print(paste("Done processing ",this_source_file))
    rm(this_source_file)
  } # for (this_file_counter in 1:length(all_DRI_Files)){
  rm(all_DRI_Files,this_file_counter,comprehensive.header)
  
  rm(Data_Source_Name_Display,Data_Source_Name_Short)
  rm(this_Datum) 
  
#### output input_mat1 from function ####  
  return(input_mat1) # output from function
} # end function