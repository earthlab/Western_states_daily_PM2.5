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
    
  } else if (data_set_counter == 5) {
    this_source_file <- "Federal_Land_Manager_IMPROVE_RHR_II_88101_20183151757452922Mvw0s_top_removed.csv"
    this_source_file_full <- "Federal_Land_Manager_IMPROVE_RHR_II_88101_20183151757452922Mvw0s.csv"
    short_name <- "IMPROVE_RHR_II_88101"
    skip_n_lines <- 240
    this_plotting_color
    #data_set_counter <- 5
    IMPROVE_a_input_mat1 <- process_PM25_IMPROVE_data_source.fn(input_header, ProcessedData.directory, IMPROVEData.directory, 
                                                                data_set_counter, this_plotting_color, this_source_file, 
                                                                this_source_file_full, short_name, skip_n_lines)
    
    }# if (data_set_counter == 1) {
  
} # end function
