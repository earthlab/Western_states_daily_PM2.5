process_PM25_parallal_wrapper.fn <- function(data_set_counter){ #, input_header, ProcessedData.directory, AQSData.directory, FireCache.directory, UintahData.directory) {
  
  if (data_set_counter == 1) { # Process EPA data
    this_plotting_color <- "black"
    EPA_input_mat1 <- process_PM25_EPA_data_source.fn(input_header, ProcessedData.directory = define_file_paths.fn("ProcessedData.directory"), AQSData.directory = define_file_paths.fn("AQSData.directory"), data_set_counter, this_plotting_color) 
    #plot time series and map observation locations
    file_sub_label = paste("PM25Source",data_set_counter,"TSstep1",sep = "")
    LatexFileName=file.path(define_file_paths.fn("output.directory"),paste("Rgenerated_",file_sub_label,"Images.tex",sep = "")) # Start file for latex code images
    if (file.exists(LatexFileName)) {file.remove(LatexFileName)} # Delete file if it exists
    source_name <- "(EPA)"
    df_report.fn(df = EPA_input_mat1, cols_interest = "PM2.5_Obs", x_axis_var = "Date_Local", output.directory = define_file_paths.fn("output.directory"), output.directory.short = define_file_paths.fn("output.directory.short"), file_sub_label = file_sub_label, title_string_partial = paste(source_name,"Time Series","(No Quality Checks; Processing Step 1)"), plot_color = "black", LatexFileName = LatexFileName, SinkFileName = NA, image_format = "jpg")
    title_string <- "EPA Monitor Locations (No Quality Checks; Processing Step 1)"
    plot_name_extension <- "MapEPALocations"
    map_data_locations.fn(this_df = EPA_input_mat1, var_interest = "PM2.5_Obs", Latitude_var_name = "PM2.5_Lat", Longitude_var_name = "PM2.5_Lon", point_color = this_plotting_color, point_symbol = 19, output.directory = define_file_paths.fn("output.directory"), file_sub_label = file_sub_label, plot_name_extension = plot_name_extension, study_states_abbrev = study_states_abbrev, title_string = title_string, ClearPage = FALSE, LatexFileName = LatexFileName) # plot points of observations on map 
    return(EPA_input_mat1)
    
  } else if (data_set_counter == 2) { # Process Fire Cache data source
    this_plotting_color <- "red"
    Fire_Cache_input_mat1 <- process_PM25_Fire_Cache_data_source.fn(input_header, ProcessedData.directory = define_file_paths.fn("ProcessedData.directory"), FireCache.directory = define_file_paths.fn("FireCache.directory"), data_set_counter, this_plotting_color)
    
    #plot time series and map observation locations
    file_sub_label = paste("PM25Source",data_set_counter,"TSstep1",sep = "")
    LatexFileName=file.path(define_file_paths.fn("output.directory"),paste("Rgenerated_",file_sub_label,"Images.tex",sep = "")) # Start file for latex code images
    if (file.exists(LatexFileName)) {file.remove(LatexFileName)} # Delete file if it exists
    source_name <- "(Fire Cache)"
    df_report.fn(df = Fire_Cache_input_mat1, cols_interest = "PM2.5_Obs", x_axis_var = "Date_Local", output.directory = define_file_paths.fn("output.directory"), output.directory.short = define_file_paths.fn("output.directory.short"), file_sub_label = file_sub_label, title_string_partial = paste(source_name,"Time Series","(No Quality Checks; Processing Step 1)"), plot_color = "black", LatexFileName = LatexFileName, SinkFileName = NA, image_format = "jpg")
    title_string <- "Fire Cache Monitor Locations (No Quality Checks; Processing Step 1)"
    plot_name_extension <- "MapFireCacheLocations"
    map_data_locations.fn(this_df = Fire_Cache_input_mat1, var_interest = "PM2.5_Obs", Latitude_var_name = "PM2.5_Lat", Longitude_var_name = "PM2.5_Lon", point_color = this_plotting_color, point_symbol = 19, output.directory = define_file_paths.fn("output.directory"), file_sub_label = file_sub_label, plot_name_extension = plot_name_extension, study_states_abbrev = study_states_abbrev, title_string = title_string, ClearPage = FALSE, LatexFileName = LatexFileName) # plot points of observations on map 
    return(Fire_Cache_input_mat1)
    
  } else if (data_set_counter == 3) { # Process Lyman Uintah data source
    this_plotting_color <-  "darkgoldenrod"
    Uintah_input_mat1 <- process_PM25_Lyman_Uintah_data_source.fn(input_header, ProcessedData.directory = define_file_paths.fn("ProcessedData.directory"), UintahData.directory = define_file_paths.fn("UintahData.directory"), data_set_counter, this_plotting_color = this_plotting_color)

    #plot time series and map observation locations
    file_sub_label = paste("PM25Source",data_set_counter,"TSstep1",sep = "")
    LatexFileName=file.path(define_file_paths.fn("output.directory"),paste("Rgenerated_",file_sub_label,"Images.tex",sep = "")) # Start file for latex code images
    if (file.exists(LatexFileName)) {file.remove(LatexFileName)} # Delete file if it exists
    source_name <- "(Uintah)"
    title_string <- "Uintah Monitor Time Series (No Quality Checks)"
    fig_caption <- "Uintah Monitor Time Series (No Quality Checks; PM2.5 Processing Step 1)"
    #df_report.fn(df = Uintah_input_mat1, cols_interest = "PM2.5_Obs", x_axis_var = "Date_Local", output.directory = define_file_paths.fn("output.directory"), output.directory.short = define_file_paths.fn("output.directory.short"), file_sub_label = file_sub_label, title_string_partial = paste(source_name,"Time Series","(No Quality Checks; Processing Step 1)"), plot_color = "black", LatexFileName = LatexFileName, SinkFileName = NA, image_format = "jpg", fig_caption = fig_caption)
    df_report.fn(df = Uintah_input_mat1, cols_interest = "PM2.5_Obs", x_axis_var = "Date_Local", output.directory = define_file_paths.fn("output.directory"), output.directory.short = define_file_paths.fn("output.directory.short"), file_sub_label = file_sub_label, title_string_partial = title_string, plot_color = "black", LatexFileName = LatexFileName, SinkFileName = NA, image_format = "jpg", fig_caption = fig_caption)
    plot_name_extension <- "MapUintahLocations"
    title_string <- "Uintah Monitor Locations (No Quality Checks)"
    fig_caption <- "Uintah Monitor Locations (No Quality Checks; PM2.5 Processing Step 1)"
    map_data_locations.fn(this_df = Uintah_input_mat1, var_interest = "PM2.5_Obs", Latitude_var_name = "PM2.5_Lat", Longitude_var_name = "PM2.5_Lon", point_color = this_plotting_color, point_symbol = 19, output.directory = define_file_paths.fn("output.directory"), file_sub_label = file_sub_label, plot_name_extension = plot_name_extension, study_states_abbrev = study_states_abbrev, title_string = title_string, ClearPage = FALSE, LatexFileName = LatexFileName, fig_caption = fig_caption) # plot points of observations on map 
    return(Uintah_input_mat1)
    
  } else if (data_set_counter == 4) { # Process PCAPS data source
    this_plotting_color <- "green"
    PCAPS_input_mat1 <- process_PM25_PCAPS_data_source.fn(input_header, ProcessedData.directory = define_file_paths.fn("ProcessedData.directory"), PCAPSData.directory = define_file_paths.fn("PCAPSData.directory"), data_set_counter, this_plotting_color)
    
    print("need to get code for plotting to work for PCAPS data")
    # #plot time series and map observation locations
    # file_sub_label = paste("PM25Source",data_set_counter,"TSstep1",sep = "")
    # LatexFileName=file.path(define_file_paths.fn("output.directory"),paste("Rgenerated_",file_sub_label,"Images.tex",sep = "")) # Start file for latex code images
    # if (file.exists(LatexFileName)) {file.remove(LatexFileName)} # Delete file if it exists
    # source_name <- "(PCAPS)"
    # df_report.fn(df = PCAPS_input_mat1, cols_interest = "PM2.5_Obs", x_axis_var = "Date_Local", output.directory = define_file_paths.fn("output.directory"), output.directory.short = define_file_paths.fn("output.directory.short"), file_sub_label = file_sub_label, title_string_partial = paste(source_name,"Time Series","(No Quality Checks; Processing Step 1)"), plot_color = "black", LatexFileName = LatexFileName, SinkFileName = NA, image_format = "jpg")
    # title_string <- "PCAPS Monitor Locations (No Quality Checks; Processing Step 1)"
    # plot_name_extension <- "MapPCAPSLocations"
    # map_data_locations.fn(this_df = PCAPS_input_mat1, var_interest = "PM2.5_Obs", Latitude_var_name = "PM2.5_Lat", Longitude_var_name = "PM2.5_Lon", point_color = this_plotting_color, point_symbol = 19, output.directory = define_file_paths.fn("output.directory"), file_sub_label = file_sub_label, plot_name_extension = plot_name_extension, study_states_abbrev = study_states_abbrev, title_string = title_string, ClearPage = FALSE, LatexFileName = LatexFileName) # plot points of observations on map 
    return(PCAPS_input_mat1)
    
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
                                                                this_source_file_full, skip_n_lines, column_prefix, master_locations_file = master_locations_file, FMLE_locations_file = FMLE_locations_file)
    
    #plot time series and map observation locations
    file_sub_label = paste("PM25Source",data_set_counter,"TSstep1",sep = "")
    LatexFileName=file.path(define_file_paths.fn("output.directory"),paste("Rgenerated_",file_sub_label,"Images.tex",sep = "")) # Start file for latex code images
    if (file.exists(LatexFileName)) {file.remove(LatexFileName)} # Delete file if it exists
    source_name <- "(IMPROVE RHR II 88101)"
    df_report.fn(df = IMPROVE_a_input_mat1, cols_interest = "PM2.5_Obs", x_axis_var = "Date_Local", output.directory = define_file_paths.fn("output.directory"), output.directory.short = define_file_paths.fn("output.directory.short"), file_sub_label = file_sub_label, title_string_partial = paste(source_name,"Time Series","(No Quality Checks; Processing Step 1)"), plot_color = "black", LatexFileName = LatexFileName, SinkFileName = NA, image_format = "jpg")
    title_string <- "IMPROVE RHR II 88101 Monitor Locations (No Quality Checks; Processing Step 1)"
    plot_name_extension <- "MapIMPRHRII88101Locations"
    map_data_locations.fn(this_df = IMPROVE_a_input_mat1, var_interest = "PM2.5_Obs", Latitude_var_name = "PM2.5_Lat", Longitude_var_name = "PM2.5_Lon", point_color = this_plotting_color, point_symbol = 19, output.directory = define_file_paths.fn("output.directory"), file_sub_label = file_sub_label, plot_name_extension = plot_name_extension, study_states_abbrev = study_states_abbrev, title_string = title_string, ClearPage = FALSE, LatexFileName = LatexFileName) # plot points of observations on map 
    return(IMPROVE_a_input_mat1)
    
    #this_source_file <- "Federal_Land_Manager_IMPROVE_RHR_II_88101_20183151757452922Mvw0s_top_removed.csv" # 2008-2014 file
    #this_source_file_full <- "Federal_Land_Manager_IMPROVE_RHR_II_88101_20183151757452922Mvw0s.csv" # 2008-2014 file
    
  } else if (data_set_counter == 6) { # IMPROVE RHR II (88401)
    this_source_file <- "Federal_Land_Manager_IMPROVE_RHR_II_88401_20192251356232212121t_top_removed.csv" # 2008-2017 file
    this_source_file_full <- "Federal_Land_Manager_IMPROVE_RHR_II_88401_20192251356232212121t.csv" # 2008-2017 file
    master_locations_file <- "Site_Meta_Master_10_2018.csv"
    FMLE_locations_file <- "IMPROVE_sites_Oct2017.csv"
    skip_n_lines <- 251
    this_plotting_color <- "orange"
    column_prefix <- "RCFM"
    IMPROVE_a_input_mat1 <- process_PM25_IMPROVE_data_source.fn(input_header, ProcessedData.directory = define_file_paths.fn("ProcessedData.directory"), #IMPROVEData.directory, 
                                                                data_set_counter, this_plotting_color, this_source_file, 
                                                                this_source_file_full, skip_n_lines, column_prefix, master_locations_file = master_locations_file, FMLE_locations_file = FMLE_locations_file) # short_name,
    
    #plot time series and map observation locations
    file_sub_label = paste("PM25Source",data_set_counter,"TSstep1",sep = "")
    LatexFileName=file.path(define_file_paths.fn("output.directory"),paste("Rgenerated_",file_sub_label,"Images.tex",sep = "")) # Start file for latex code images
    if (file.exists(LatexFileName)) {file.remove(LatexFileName)} # Delete file if it exists
    source_name <- "(IMPROVE RHR II 88401)"
    df_report.fn(df = IMPROVE_a_input_mat1, cols_interest = "PM2.5_Obs", x_axis_var = "Date_Local", output.directory = define_file_paths.fn("output.directory"), output.directory.short = define_file_paths.fn("output.directory.short"), file_sub_label = file_sub_label, title_string_partial = paste(source_name,"Time Series","(No Quality Checks; Processing Step 1)"), plot_color = "black", LatexFileName = LatexFileName, SinkFileName = NA, image_format = "jpg")
    title_string <- "IMPROVE RHR II 88401 Monitor Locations (No Quality Checks; Processing Step 1)"
    plot_name_extension <- "MapIMPRHRII88401Locations"
    map_data_locations.fn(this_df = IMPROVE_a_input_mat1, var_interest = "PM2.5_Obs", Latitude_var_name = "PM2.5_Lat", Longitude_var_name = "PM2.5_Lon", point_color = this_plotting_color, point_symbol = 19, output.directory = define_file_paths.fn("output.directory"), file_sub_label = file_sub_label, plot_name_extension = plot_name_extension, study_states_abbrev = study_states_abbrev, title_string = title_string, ClearPage = FALSE, LatexFileName = LatexFileName) # plot points of observations on map 
    return(IMPROVE_a_input_mat1)
    
    #this_source_file <- "Federal_Land_Manager_IMPROVE_RHR_II_88401_20185113533660420xLwJ_top_removed.csv" # 2008-2014 file
    #this_source_file_full <- "Federal_Land_Manager_IMPROVE_RHR_II_88401_20185113533660420xLwJ.csv" # 2008-2014 file
    
  } else if (data_set_counter == 7) {
    # IMPROVE RHR III (88101)
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
                                                                this_source_file_full, skip_n_lines, column_prefix, master_locations_file = master_locations_file, FMLE_locations_file = FMLE_locations_file) 
    
    #plot time series and map observation locations
    file_sub_label = paste("PM25Source",data_set_counter,"TSstep1",sep = "")
    LatexFileName=file.path(define_file_paths.fn("output.directory"),paste("Rgenerated_",file_sub_label,"Images.tex",sep = "")) # Start file for latex code images
    if (file.exists(LatexFileName)) {file.remove(LatexFileName)} # Delete file if it exists
    source_name <- "(IMPROVE RHR III 88401)"
    df_report.fn(df = IMPROVE_a_input_mat1, cols_interest = "PM2.5_Obs", x_axis_var = "Date_Local", output.directory = define_file_paths.fn("output.directory"), output.directory.short = define_file_paths.fn("output.directory.short"), file_sub_label = file_sub_label, title_string_partial = paste(source_name,"Time Series","(No Quality Checks; Processing Step 1)"), plot_color = "black", LatexFileName = LatexFileName, SinkFileName = NA, image_format = "jpg")
    title_string <- "IMPROVE RHR III 88401 Monitor Locations (No Quality Checks; Processing Step 1)"
    plot_name_extension <- "MapIMPRHRIII88401Locations"
    map_data_locations.fn(this_df = IMPROVE_a_input_mat1, var_interest = "PM2.5_Obs", Latitude_var_name = "PM2.5_Lat", Longitude_var_name = "PM2.5_Lon", point_color = this_plotting_color, point_symbol = 19, output.directory = define_file_paths.fn("output.directory"), file_sub_label = file_sub_label, plot_name_extension = plot_name_extension, study_states_abbrev = study_states_abbrev, title_string = title_string, ClearPage = FALSE, LatexFileName = LatexFileName) # plot points of observations on map 
    return(IMPROVE_a_input_mat1)
    
    #this_source_file <- "Federal_Land_Manager_RHR_III_88101_first_param_201851152033932P22My0_top_removed.csv"  # 2008-2014 file
    #this_source_file_full <- "Federal_Land_Manager_RHR_III_88101_first_param_201851152033932P22My0.csv"  # 2008-2014 file
  } else if (data_set_counter == 8) {
    # California PM2.5
    this_plotting_color <- "blueviolet"
    CARB_input_mat1 <- process_PM25_CARB_data_source.fn(input_header, data_set_counter, this_plotting_color)
   
    #plot time series and map observation locations
    file_sub_label = paste("PM25Source",data_set_counter,"TSstep1",sep = "")
    LatexFileName=file.path(define_file_paths.fn("output.directory"),paste("Rgenerated_",file_sub_label,"Images.tex",sep = "")) # Start file for latex code images
    if (file.exists(LatexFileName)) {file.remove(LatexFileName)} # Delete file if it exists
    source_name <- "(CARB)"
    df_report.fn(df = CARB_input_mat1, cols_interest = "PM2.5_Obs", x_axis_var = "Date_Local", output.directory = define_file_paths.fn("output.directory"), output.directory.short = define_file_paths.fn("output.directory.short"), file_sub_label = file_sub_label, title_string_partial = paste(source_name,"Time Series","(No Quality Checks; Processing Step 1)"), plot_color = "black", LatexFileName = LatexFileName, SinkFileName = NA, image_format = "jpg")
    title_string <- "CARB Monitor Locations (No Quality Checks; Processing Step 1)"
    plot_name_extension <- "MapCARBLocations"
    map_data_locations.fn(this_df = CARB_input_mat1, var_interest = "PM2.5_Obs", Latitude_var_name = "PM2.5_Lat", Longitude_var_name = "PM2.5_Lon", point_color = this_plotting_color, point_symbol = 19, output.directory = define_file_paths.fn("output.directory"), file_sub_label = file_sub_label, plot_name_extension = plot_name_extension, study_states_abbrev = study_states_abbrev, title_string = title_string, ClearPage = FALSE, LatexFileName = LatexFileName) # plot points of observations on map 
    return(CARB_input_mat1)
    
  } else if (data_set_counter == 9) {
     # Utah DEQ
     this_plotting_color <- "darkcyan"
     #data_set_counter <- 9
     UDEQ_input_mat1 <- process_PM25_UDEQ_data_source.fn(input_header, data_set_counter, this_plotting_color)
     print("need to get code for plotting to work for PCAPS data")
     ##plot time series and map observation locations
     #file_sub_label = paste("PM25Source",data_set_counter,"TSstep1",sep = "")
     #LatexFileName=file.path(define_file_paths.fn("output.directory"),paste("Rgenerated_",file_sub_label,"Images.tex",sep = "")) # Start file for latex code images
     #if (file.exists(LatexFileName)) {file.remove(LatexFileName)} # Delete file if it exists
     #source_name <- "(UDEQ)"
     #df_report.fn(df = UDEQ_input_mat1, cols_interest = "PM2.5_Obs", x_axis_var = "Date_Local", output.directory = define_file_paths.fn("output.directory"), output.directory.short = define_file_paths.fn("output.directory.short"), file_sub_label = file_sub_label, title_string_partial = paste(source_name,"Time Series","(No Quality Checks; Processing Step 1)"), plot_color = "black", LatexFileName = LatexFileName, SinkFileName = NA, image_format = "jpg")
     #title_string <- "UDEQ Monitor Locations (No Quality Checks; Processing Step 1)"
     #plot_name_extension <- "MapUDEQLocations"
     #map_data_locations.fn(this_df = UDEQ_input_mat1, var_interest = "PM2.5_Obs", Latitude_var_name = "PM2.5_Lat", Longitude_var_name = "PM2.5_Lon", point_color = this_plotting_color, point_symbol = 19, output.directory = define_file_paths.fn("output.directory"), file_sub_label = file_sub_label, plot_name_extension = plot_name_extension, study_states_abbrev = study_states_abbrev, title_string = title_string, ClearPage = FALSE, LatexFileName = LatexFileName) # plot points of observations on map 
     return(UDEQ_input_mat1)
     
  }# if (data_set_counter == 1) {
} # end function
