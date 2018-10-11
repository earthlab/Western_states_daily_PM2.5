process_PM25_parallal_wrapper.fn <- function(data_set_counter, input_header, ProcessedData.directory, AQSData.directory, FireCache.directory, UintahData.directory) {
  
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
    print("Process Lyman Uintah data source")
    this_plotting_color <-  "darkgoldenrod"
    #data_set_counter <- 3
    Uintah_input_mat1 <- process_PM25_Lyman_Uintah_data_source.fn(input_header, ProcessedData.directory, UintahData.directory, data_set_counter, this_plotting_color = this_plotting_color)
  } else if (data_set_counter == 4) {
    
    print("Process PCAPS data source")
    this_plotting_color <- "green"
    #data_set_counter <- 4
    PCAPS_input_mat1 <- process_PM25_PCAPS_data_source.fn(input_header, ProcessedData.directory, PCAPSData.directory, data_set_counter, this_plotting_color)
    
  } # if (data_set_counter == 1) {
  
} # end function