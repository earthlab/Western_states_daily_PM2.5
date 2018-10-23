process_PM25_parallal_wrapper.fn <- function(data_set_counter){ #, input_header, ProcessedData.directory, AQSData.directory, FireCache.directory, UintahData.directory) {
  
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
    # IMPROVE RHR II (88101)
    this_source_file <- "Federal_Land_Manager_IMPROVE_RHR_II_88101_20183151757452922Mvw0s_top_removed.csv"
    this_source_file_full <- "Federal_Land_Manager_IMPROVE_RHR_II_88101_20183151757452922Mvw0s.csv"
    #short_name <- "IMPRHR2MF88101" #"IMPROVE_RHR_II_88101"
    skip_n_lines <- 240
    this_plotting_color <- "lightsalmon4"
    column_prefix <- "MF"
    #data_set_counter <- 5
    IMPROVE_a_input_mat1 <- process_PM25_IMPROVE_data_source.fn(input_header, ProcessedData.directory, #IMPROVEData.directory, 
                                                                data_set_counter, this_plotting_color, this_source_file, 
                                                                this_source_file_full, skip_n_lines, column_prefix) # short_name,
  } else if (data_set_counter == 6) {
    # IMPROVE RHR II (88401)
    this_source_file <- "Federal_Land_Manager_IMPROVE_RHR_II_88401_20185113533660420xLwJ_top_removed.csv"
    this_source_file_full <- "Federal_Land_Manager_IMPROVE_RHR_II_88401_20185113533660420xLwJ.csv"
    #short_name <- "IMPRHR2RCFM88401" 
    skip_n_lines <- 242
    this_plotting_color <- "orange"
    column_prefix <- "RCFM"
    #data_set_counter <- 6
    IMPROVE_a_input_mat1 <- process_PM25_IMPROVE_data_source.fn(input_header, ProcessedData.directory, #IMPROVEData.directory, 
                                                                data_set_counter, this_plotting_color, this_source_file, 
                                                                this_source_file_full, skip_n_lines, column_prefix) # short_name,
  } else if (data_set_counter == 7) {
    # IMPROVE RHR III 'first param' (88101)
    this_source_file <- "Federal_Land_Manager_RHR_III_88101_first_param_201851152033932P22My0_top_removed.csv"
    this_source_file_full <- "Federal_Land_Manager_RHR_III_88101_first_param_201851152033932P22My0.csv"
    #short_name <- "IMPRHR3MF1st88101" 
    skip_n_lines <- 209
    this_plotting_color <- "seagreen"
    column_prefix <- "MF"
    #data_set_counter <- 7
    IMPROVE_a_input_mat1 <- process_PM25_IMPROVE_data_source.fn(input_header, ProcessedData.directory, 
                                                                data_set_counter, this_plotting_color, this_source_file, 
                                                                this_source_file_full, skip_n_lines, column_prefix) 

  } else if (data_set_counter == 8) {
    # California PM2.5
    this_plotting_color <- "blueviolet"
    #data_set_counter <- 8
    #stop("finish code")
    CARB_input_mat1 <- process_PM25_CARB_data_source.fn(input_header, data_set_counter, this_plotting_color)
   } else if (data_set_counter == 9) {
     # Utah DEQ
     this_plotting_color <- "darkcyan"
     #data_set_counter <- 9
     UDEQ_input_mat1 <- process_PM25_UDEQ_data_source.fn(input_header, data_set_counter, this_plotting_color)
     #stop("finish code")
    }# if (data_set_counter == 1) {
  
} # end function
