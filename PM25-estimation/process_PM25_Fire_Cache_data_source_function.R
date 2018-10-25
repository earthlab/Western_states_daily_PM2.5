process_PM25_Fire_Cache_data_source.fn <- function(input_header, ProcessedData.directory, FireCache.directory, data_set_counter, this_plotting_color = "red") {
  # combineFire Cache. PM2.5 data files into 1 dataframe
  
  #### Pull in Fire Cache Smoke (DRI) data #################
  print('still need to download the files that have been password protected.')
  data_source_counter <- data_set_counter #data_source_counter+1 # counter to distinguish between the various data sources (differentiate by color on  maps)
  Data_Source_Name_Short <- "FireCacheDRI"
  Data_Source_Name_Display <- "Fire Cache Smoke Monitor (DRI)"
  this_Datum <- "NAD83" # per email from Joshua Walston on 5/29/2018
  
  ##### Create Sink output file and create its header ####
  # sink command sends R output to a file. Don't try to open file until R has closed it at end of script. https://www.rdocumentation.org/packages/base/versions/3.4.1/topics/sink
  #SinkFileName=file.path(ProcessedData.directory,"PM25_data_source_Fire_Cache_combining_sink.txt")
  #file_sub_label <- paste("PM25_",Data_Source_Name_Short,"_Step1_",Sys.Date(),"_part_",processed_data_version,sep = "")
  file_sub_label <- paste("PM25_",Data_Source_Name_Short,"_Step1_part_",processed_data_version,sep = "")
  SinkFileName=file.path(ProcessedData.directory,paste(file_sub_label,"_combining_sink.txt",sep = ""))
  
  sink(file =SinkFileName, append = FALSE, type = c("output","message"), split = FALSE) # UNCOMMENT
  cat("Code and R output for process_PM25_Fire_Cache_data_source_function.R \n \n")
  cat("Title: process_PM25_Fire_Cache_data_source_function.R \n")
  cat("Author: Melissa May Maestas, PhD \n")
  cat("Original Date: September 24, 2018 \n")
  cat("Latest Update: October 10, 2018 \n")
  cat(paste("Script ran and this text file created ",Sys.time(),sep = ""))
  cat("This program reads in and PM2.5 data from the Fire Cache Smoke Monitor Archive \n")
  
  #### Create data frame  ####
  input_mat1 <- data.frame(matrix(NA,nrow=0,ncol=length(input_header))) # create data frame for input_mat1
  names(input_mat1) <- input_header # assign the header to input_mat1
  input_mat1 <- input_mat_change_data_classes.fn(input_mat1)
  
  

  # what files are in the FireCache.directory?
  # https://stat.ethz.ch/R-manual/R-devel/library/base/html/list.files.html
  all_DRI_Files <- list.files(path = file.path(FireCache.directory,"."), pattern = NULL, all.files = FALSE,
                              full.names = FALSE, recursive = FALSE,
                              ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  #print(all_DRI_Files)
  #comprehensive_header <- NA # needs null value to start
  # cycle through files
  for (this_file_counter in 1:length(all_DRI_Files)){  
    #print(paste('this_file_counter =',this_file_counter)) #COMMENT
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
    if(this_file_counter == 1) {
    comprehensive_header <- Fire_Cache_comprehensive_header.fn(this_file_counter, this_Fire_Cache_data_step)
    } else {
    comprehensive_header <- Fire_Cache_comprehensive_header.fn(this_file_counter, this_Fire_Cache_data_step, comprehensive_header = comprehensive_header)
    }
    
    # The header is (sometimes/always?) repeated further down in the data. These rows need to be found and removed.
    this_Fire_Cache_data_step2 <- Fire_Cache_remove_repeat_headers.fn(this_Fire_Cache_data_step)
    rm(this_Fire_Cache_data_step) # clear variables
    
    # Change class of various columns, e.g., get it to recognize dates as dates, etc
    this_Fire_Cache_data_step3 <- Fire_Cache_change_data_classes.fn(this_Fire_Cache_data_step2)
    rm(this_Fire_Cache_data_step2)
    
    # some longitudes are positive, but should be negative - make them all negative and 
    # note which data sets were changed
    this_Fire_Cache_data <- Fire_Cache_negative_longitudes.fn(this_Fire_Cache_data_step3,this_source_file)
    
    #### take 24-hr averages for this 1 file
    Daily_Fire_Cache <- Fire_Cache_daily_averages.fn(this_Fire_Cache_data,comprehensive_header)
    
    #### Input Fire Cache data into input_mat
    one_file_small_input_mat <- Fire_Cache_1_file_to_small_input_mat.fn(Daily_Fire_Cache, input_header, this_name,
                                                                        this_Datum, this_plotting_color = this_plotting_color, this_source_file = this_source_file,
                                                                        Data_Source_Name_Display = Data_Source_Name_Display, Data_Source_Name_Short = Data_Source_Name_Short,
                                                                        data_source_counter = data_source_counter)
    
    # write code to concatinate data from the various files
    input_mat1 <- rbind(input_mat1,one_file_small_input_mat)
    
    #print(paste("Done processing ",this_source_file))
    rm(this_source_file)
  } # for (this_file_counter in 1:length(all_DRI_Files)){
  rm(all_DRI_Files,this_file_counter,comprehensive_header)
  
  print(paste("This data has",dim(input_mat1)[1],"rows of PM2.5 observations.")) # how many rows of data?
  
  # output to file #  
  #write.csv(input_mat1,file = file.path(ProcessedData.directory,paste(Data_Source_Name_Short,"_",Sys.Date(),'_Step1_part_',processed_data_version,'.csv',sep = "")),row.names = FALSE)
  write.csv(input_mat1,file = file.path(ProcessedData.directory,paste(file_sub_label,'.csv',sep = "")),row.names = FALSE)
  
  # clear variables
  #rm(Data_Source_Name_Display,Data_Source_Name_Short)
  rm(this_Datum) 
  
#### output input_mat1 from function ####  
  return(input_mat1) # output from function
} # end function