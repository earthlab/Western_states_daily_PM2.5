process_PM25_parallal_wrapper.fn <- function(data_set_counter){ #, input_header, ProcessedData.directory, AQSData.directory, FireCache.directory, UintahData.directory) {
  
  if (data_set_counter == 1) {
    #print("Process EPA data")
    this_plotting_color <- "black"
    EPA_input_mat1 <- process_PM25_EPA_data_source.fn(input_header, ProcessedData.directory = define_file_paths.fn("ProcessedData.directory"), AQSData.directory = define_file_paths.fn("AQSData.directory"), data_set_counter, this_plotting_color) 
    
  } else if (data_set_counter == 2) {
    #print("Process Fire Cache data source")
    this_plotting_color <- "red"
    #data_set_counter <- 2
    Fire_Cache_input_mat1 <- process_PM25_Fire_Cache_data_source.fn(input_header, ProcessedData.directory = define_file_paths.fn("ProcessedData.directory"), FireCache.directory = define_file_paths.fn("FireCache.directory"), data_set_counter, this_plotting_color)
    
  } else if (data_set_counter == 3) {
    #print("Process Lyman Uintah data source")
    this_plotting_color <-  "darkgoldenrod"
    #data_set_counter <- 3
    Uintah_input_mat1 <- process_PM25_Lyman_Uintah_data_source.fn(input_header, ProcessedData.directory = define_file_paths.fn("ProcessedData.directory"), UintahData.directory = define_file_paths.fn("UintahData.directory"), data_set_counter, this_plotting_color = this_plotting_color)

  } else if (data_set_counter == 4) {
    #print("Process PCAPS data source")
    this_plotting_color <- "green"
    #data_set_counter <- 4
    PCAPS_input_mat1 <- process_PM25_PCAPS_data_source.fn(input_header, ProcessedData.directory = define_file_paths.fn("ProcessedData.directory"), PCAPSData.directory = define_file_paths.fn("PCAPSData.directory"), data_set_counter, this_plotting_color)
    
  } else if (data_set_counter == 5) {
    # IMPROVE RHR II (88101)
    this_source_file <- "Federal_Land_Manager_IMPROVE_RHR_II_88101_201922513514530P22rMs_top_removed.csv" # 2008-2017 file
    this_source_file_full <- "Federal_Land_Manager_IMPROVE_RHR_II_88101_201922513514530P22rMs.csv" # 2008-2017 file
    master_locations_file <- "Site_Meta_Master_10_2018.csv"
    FMLE_locations_file <- "IMPROVE_sites_Oct2017.csv"
    #short_name <- "IMPRHR2MF88101" #"IMPROVE_RHR_II_88101"
    skip_n_lines <- 251#240
    this_plotting_color <- "lightsalmon4"
    column_prefix <- "MF"
    #data_set_counter <- 5
    IMPROVE_a_input_mat1 <- process_PM25_IMPROVE_data_source.fn(input_header, ProcessedData.directory = define_file_paths.fn("ProcessedData.directory"), #IMPROVEData.directory, 
                                                                data_set_counter, this_plotting_color, this_source_file, 
                                                                this_source_file_full, skip_n_lines, column_prefix) # short_name,
    
    #this_source_file <- "Federal_Land_Manager_IMPROVE_RHR_II_88101_20183151757452922Mvw0s_top_removed.csv" # 2008-2014 file
    #this_source_file_full <- "Federal_Land_Manager_IMPROVE_RHR_II_88101_20183151757452922Mvw0s.csv" # 2008-2014 file
    
  } else if (data_set_counter == 6) {
    # IMPROVE RHR II (88401)
    this_source_file <- "Federal_Land_Manager_IMPROVE_RHR_II_88401_20192251356232212121t_top_removed.csv" # 2008-2017 file
    this_source_file_full <- "Federal_Land_Manager_IMPROVE_RHR_II_88401_20192251356232212121t.csv" # 2008-2017 file
    master_locations_file <- "Site_Meta_Master_10_2018.csv"
    FMLE_locations_file <- "IMPROVE_sites_Oct2017.csv"
    skip_n_lines <- 251
    this_plotting_color <- "orange"
    column_prefix <- "RCFM"
    #data_set_counter <- 6
    IMPROVE_a_input_mat1 <- process_PM25_IMPROVE_data_source.fn(input_header, ProcessedData.directory = define_file_paths.fn("ProcessedData.directory"), #IMPROVEData.directory, 
                                                                data_set_counter, this_plotting_color, this_source_file, 
                                                                this_source_file_full, skip_n_lines, column_prefix) # short_name,
    
    #this_source_file <- "Federal_Land_Manager_IMPROVE_RHR_II_88401_20185113533660420xLwJ_top_removed.csv" # 2008-2014 file
    #this_source_file_full <- "Federal_Land_Manager_IMPROVE_RHR_II_88401_20185113533660420xLwJ.csv" # 2008-2014 file
    
  } else if (data_set_counter == 7) {
    # IMPROVE RHR III 'first param' (88101)
    this_source_file <- "Federal_Land_Manager_IMRPOVE_RHR_III_88101_2019225135946946xJ0L22_top_removed.csv"  # 2008-2017 file
    this_source_file_full <- "Federal_Land_Manager_IMRPOVE_RHR_III_88101_2019225135946946xJ0L22.csv"  # 2008-2017 file
    master_locations_file <- "Site_Meta_Master_10_2018.csv"
    FMLE_locations_file <- "IMPROVE_sites_Oct2017.csv"
    #short_name <- "IMPRHR3MF1st88101" 
    skip_n_lines <- 216#209
    this_plotting_color <- "seagreen"
    column_prefix <- "MF"
    #data_set_counter <- 7
    IMPROVE_a_input_mat1 <- process_PM25_IMPROVE_data_source.fn(input_header, ProcessedData.directory = define_file_paths.fn("ProcessedData.directory"), 
                                                                data_set_counter, this_plotting_color, this_source_file, 
                                                                this_source_file_full, skip_n_lines, column_prefix) 
    
    #this_source_file <- "Federal_Land_Manager_RHR_III_88101_first_param_201851152033932P22My0_top_removed.csv"  # 2008-2014 file
    #this_source_file_full <- "Federal_Land_Manager_RHR_III_88101_first_param_201851152033932P22My0.csv"  # 2008-2014 file
  } else if (data_set_counter == 8) {
    # California PM2.5
    this_plotting_color <- "blueviolet"
    CARB_input_mat1 <- process_PM25_CARB_data_source.fn(input_header, data_set_counter, this_plotting_color)
   
  } else if (data_set_counter == 9) {
     # Utah DEQ
     this_plotting_color <- "darkcyan"
     #data_set_counter <- 9
     UDEQ_input_mat1 <- process_PM25_UDEQ_data_source.fn(input_header, data_set_counter, this_plotting_color)
    }# if (data_set_counter == 1) {
  
} # end function
