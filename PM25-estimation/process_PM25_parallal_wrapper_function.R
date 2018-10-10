process_PM25_parallal_wrapper.fn <- function(data_set_counter, input_header, ProcessedData.directory, AQSData.directory) {
  
  if (data_set_counter == 1) {
    print("Process EPA data")
    this_plotting_color <- "black"
    EPA_input_mat1 <- process_PM25_EPA_data_source.fn(input_header, ProcessedData.directory, AQSData.directory, data_set_counter, this_plotting_color) 
    
  } else if (data_set_counter == 2) {
    print("Process Fire Cache data source")
    this_plotting_color <- "red"
    #data_set_counter <- 2
    Fire_Cache_input_mat1 <- process_PM25_Fire_Cache_data_source.fn(input_header, ProcessedData.directory, FireCache.directory, data_set_counter, this_plotting_color)
    
    
  } else if (data_set_counter == 3) {
    
    print(paste("FUNCTION for 3rd data source starting for",sep = " "))
    
    print("Process 3rd data source")
    # insert function for processing 3rd data source here
    
    print(paste("FUNCTION for 3rd data source finished for",sep = " "))
    
  }
  
} # end function