# Process_PM25_data_step5.R - plots and maps

#### Clear variables and sinks; define working directory ####
rm(list  =  ls())
options(warn  =  2) # throw an error when there's a warning and stop the code from running further
if (max(dev.cur())>1) { # make sure it isn't outputting to any figure files
  dev.off(which  =  dev.cur())
} # if (max(dev.cur())>1) {
while (sink.number()>0) {
  sink()
} # while (sink.number()>0) {
working.directory  <-  "/home/rstudio"
setwd(working.directory) # set working directory

# start timer for code
start_code_timer <- proc.time()
print(paste("Start Process_PM25_data_step5.R at",Sys.time(),sep = " "))

#### Call Packages (Library) ####
library(plyr)
library(rgdal)
library(geosphere)

#### Source Functions that I created ####
source(file.path("estimate-pm25","General_Project_Functions","general_project_functions.R"))
source(file.path(define_file_paths.fn("ML_Code.directory"),"Plotting_and_LaTex_functions.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"State_Abbrev_Definitions_function.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"input_mat_functions.R"))

#### define constants and file names ####
processed_data_version <- define_study_constants.fn("processed_data_version")
sub_folder <- paste("PM25_data_part_",processed_data_version,sep = "") #sub_folder <- "PM25_data_part_d"
start_study_year <- input_mat_extract_year_from_date.fn(define_study_constants.fn("start_date")) #2008
stop_study_year <- input_mat_extract_year_from_date.fn(define_study_constants.fn("end_date"))#2014
study_states_abbrev <- define_study_constants.fn("study_states_abbrev") 

#### Loop through the two versions of PM25 data and make plots ####
# data_set <- 1
for (data_set in 1){#:2) { # Loop through the two versions of PM25 data and make plots
  if (data_set == 1) { # extract names of files for each data set
  #this_source_file <- paste("TEST1-1000-PM25_Step4_part_",processed_data_version,"_de_duplicated_aves_ML_input.csv", sep = "") # PM25_Step4_part_e_de_duplicated_aves_ML_input.csv
  this_source_file <- paste("PM25_Step4_part_",processed_data_version,"_de_duplicated_aves_ML_input.csv", sep = "") # PM25_Step4_part_e_de_duplicated_aves_ML_input.csv
  
  this_merging_method <- "Include All Monitors"
  } else if (data_set == 2) { # if (data_set == 1) { # extract names of files for each data set
    this_source_file <- paste("PM25_Step4_part_",processed_data_version,"_de_duplicated_aves_prioritize_24hr_obs_ML_input.csv", sep = "") # #PM25_Step4_part_e_de_duplicated_aves_prioritize_24hr_obs_ML_input.csv
    this_merging_method <- "Prioritize 24-HR Obs"
  } # if (data_set == 1) { # extract names of files for each data set
  print(this_source_file)
  file_sub_label <- paste("Report_",substr(this_source_file, 1, (nchar(this_source_file)-4)),sep = "") # file partial name, decide whether to include date in file name
  LatexFileName=file.path(define_file_paths.fn("output.directory"),paste("Rgenerated_",file_sub_label,"Images.tex",sep = "")) # Start file for latex code images
  title_string_partial <- paste("Report PM2.5 part ",processed_data_version," Data Version: ",this_merging_method,sep = "") #Step 3"
  LaTex_code_start_section.fn(LatexFileName, title_string = title_string_partial, append_option = FALSE)
  sink.number()
  SinkFileName=file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder,paste(file_sub_label,"_sink.txt",sep = "")) # define full file name
  #sink(file =SinkFileName, append = FALSE, type = c("output","message"), split = FALSE) # start output to text file #UNCOMMENT
  print(paste("Sink for Process_PM25_data_step5.R for ",this_source_file , sep = ""))
  
  #### Load Data ####
  PM25_data_step <- read.csv(file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder,this_source_file),header=TRUE) # load the PM25 data file
  PM25_data_step <- input_mat_change_data_classes.fn(PM25_data_step)
  #PM25_data$Data_Source_Name_Display <- as.character(PM25_data$Data_Source_Name_Display)
  
  #### get rid of duplicated rows
  n_rows_orig <- dim(PM25_data_step)[1]
  PM25_data <- PM25_data_step[!duplicated(PM25_data_step),]
  n_rows_de_dup <- dim(PM25_data)[1]
  
  Check_data_step5 <- check_4_NAs.fn(no_NAs_allowed_cols = c("Lat","Lon","NewDatum","PM2.5_Obs","Date_Local","Year","Month","Day"), input_data = PM25_data)
  if (length(Check_data_step5)>0) {stop("***Check_4_NAs.fn found questionable data. Investigate.***")}
  #summarize data
  summary(PM25_data) # give summary of current state of data
  print("file names still included")
  unique(PM25_data$Source_File)
  
  # find the data above 2000 ug/m3
  which_really_high <- which(PM25_data$PM2.5_Obs > 2000)
  really_high_PM25_data <- PM25_data[which_really_high, ]
  
  numerical_variables <- c("Lat","Lon","Year","Month","Day","Composite_of_N_rows", "Data_Source_Counter",
                           "N_Negative_Obs","InDayLatDiff","InDayLonDiff")         
  col_name_interest <- "PM2.5_Obs"
  #[9] "State_Code"               "County_Code"              "Site_Num"                 "Parameter_Code"          
  #[13] "POC"                      "Parameter_Name"           "Sample_Duration"          "Pollutant_Standard"      
  #[17] "Units_of_Measure"         "Event_Type"               "Observation_Count"        "Observation_Percent"     
  #[21] "X1st_Max_Value"           "X1st_Max_Hour"            "AQI"                      "Method_Code"             
  #[25] "Method_Name"              "PM25_Station_Name"        "Address"                  "State_Name"              
  #[29] "County_Name"              "City_Name"                "CBSA_Name"                "Date_of_Last_Change"     
  #[33] "State_Abbrev"             "Winter"                   "Data_Source_Name_Display" "Data_Source_Name_Short"  
  #[37]       "Source_File"                              
  #[41] "flg.Lat"                  "flg.Lon"                  "Type"                     "flg.Type"                
  #[45] "flg.Site_Num"             "flg.PM25_Obs"             "l.m.Ave..Air.Flw"         "flg.AirFlw"              
  #[49] "Deg.C.Av.Air.Temp"        "flg.AirTemp"              "X..Rel.Humidty"           "flg.RelHumid"            
  #[53] "mbar.Barom.Press"         "flg.Barom.Press"          "deg.C.Sensor..Int.AT"     "flg.deg.C.Sensor.Int.AT" 
  #[57] "X..Sensor.Int.RH"         "flg..SensorIntRH"         "Wind.Speed.m.s"           "flg.WindSpeed"           
  #[61] "Battery.Voltage.volts"    "flg.BatteryVoltage"       "Alarm"                    "flg.Alarm"               
  #[65]              "PlottingColor"            "SerialNumber" 
  #"NewDatum" "Date_Local" 
  
  #title_string_partial <- "Variables Plot against Time" # used in plot titles and subsection name
  title_string_partial <- "vs Time" # used in plot titles and subsection name
  df_report.fn(df = PM25_data, cols_interest = c(col_name_interest,numerical_variables), x_axis_var = "Date_Local", output.directory = define_file_paths.fn("output.directory"),
               output.directory.short = define_file_paths.fn("output.directory.short"), file_sub_label = file_sub_label, title_string_partial = title_string_partial, plot_color = "black",
               LatexFileName = LatexFileName, SinkFileName = NA, image_format = "jpg")
  
  #### Map locations - all together and then by year ####
  subsection_name <- "All PM2.5 Monitor Locations"
  LaTex_code_start_subsection.fn(LatexFileName, title_string = subsection_name, append_option = TRUE) # start subsection for latex code
  
  this_image_format <- "jpg" #"pdf" # options are: #"jpg" #"pdf" #"png" 
  for (plot_year in c(0,start_study_year:stop_study_year)) { # plot all years together and then plot map of data by year
    file_sub_label_plots <- paste("MapPM25_All_Sites","Y",plot_year,sep = "")
    print(plot_year)
    this_plot_name_extension <-  paste("ObsMapY",plot_year, sep = "")
    this_title_string <- "All PM2.5 Observation Locations"
    if (plot_year==0){ # year 0 is all years, or just one year at a time
      this_title_string <- "All PM2.5 Observation Locations"
      fig_caption <- paste("Map of locations of PM2.5 observations for entire study period, ",start_study_year," to ",stop_study_year,".",sep = "")
      year_data <- PM25_data
    } else { # year 0 is all years, or just one year at a time
      this_title_string <- paste("PM2.5 Observation Locations, ",plot_year,sep = "")
      fig_caption <- paste("Map of locations of PM2.5 observations during ",plot_year,".",sep = "")
      which_this_year <- which(PM25_data$Year == plot_year)
      year_data <- PM25_data[which_this_year, ]
    } # if (plot_year==0){ # year 0 is all years, or just one year at a time
    
    plot_name_extension <- paste("PlotLoc",plot_year,sep = "")
    map_data_locations.fn(this_df = year_data, var_interest = col_name_interest, Latitude_var_name = "Lat", Longitude_var_name = "Lon", point_color = "blue", point_symbol = 19, output.directory = define_file_paths.fn("output.directory"), file_sub_label, plot_name_extension = plot_name_extension, study_states_abbrev = study_states_abbrev, title_string = this_title_string, ClearPage = FALSE, Cut_points_set = FALSE, color_cut_points = NA, color_vec = NA, LatexFileName = LatexFileName, fig_caption = this_title_string) # plot points of observations on map
  
  } # for (plot_year in c(0,start_study_year:stop_study_year)) { # plot all years together and then plot map of data by year
  
} # Loop through the two versions of PM25 data and make plots

## replace some of the names with shorter versions
#which_Fire_Cache <- which(PM25_data$Data_Source_Name_Display == "Fire Cache Smoke Monitor (DRI)")
#PM25_data[which_Fire_Cache, c("Data_Source_Name_Display")] <- "Fire Cache (DRI)"
#which_this <- which(PM25_data$Data_Source_Name_Display == "IMPRHR2 MF 88101 10010")
#PM25_data[which_this, c("Data_Source_Name_Display")] <- "IMPRHR2 MF 88101"
#which_this <- which(PM25_data$Data_Source_Name_Display == "IMPRHR2 RCFM 88401 10010")
#PM25_data[which_this, c("Data_Source_Name_Display")] <- "IMPRHR2 RCFM 88401"
#which_this <- which(PM25_data$Data_Source_Name_Display == "IMPRHR3 MF 88101 10006")
#PM25_data[which_this, c("Data_Source_Name_Display")] <- "IMPRHR3 MF 88101"

#### Make plots ####
# plot map of monitor locations by data source

# this_image_format <- "pdf" # options are: #"jpg" #"pdf" #"png" 
# for (plot_year in 0){#c(0,start_study_year:stop_study_year)) { # plot all years together and then plot map of data by year
#   file_sub_label_plots <- paste("MapPM25_All_Sites","Y",plot_year,sep = "")
#   print(plot_year)
#   this_plot_name_extension <-  paste("ObsMapY",plot_year, sep = "")
#   this_title_string <- "All PM2.5 Observation Locations"
#   if (plot_year==0){ # year 0 is all years, or just one year at a time
#     this_title_string <- "All PM2.5 Observation Locations"
#     fig_caption <- paste("Map of locations of PM2.5 observations for entire study period, ",start_study_year," to ",stop_study_year,".",sep = "")
#   } else { # year 0 is all years, or just one year at a time
#     this_title_string <- paste("PM2.5 Observation Locations, ",plot_year,sep = "")
#     fig_caption <- paste("Map of locations of PM2.5 observations during ",plot_year,".",sep = "")
#   } # if (plot_year==0){ # year 0 is all years, or just one year at a time
#   
#   
#   FigFileName <- Plot_to_ImageFile_TopOnly.fn(output.directory, file_sub_label = file_sub_label_plots, 
#                                               plot_name_extension = this_plot_name_extension, image_format = this_image_format) # start of image file
#   # plot data
#   map_base_layer.fn(USMaps.directory = USMaps.directory, study_states_abbrev = study_states_abbrev) # map state boundaries
#   
  # # cycle through each data source (EPA and various field campaigns) and plot each in a different color
  # for(this_data_source_counter in sort(unique(PM25_data$Data_Source_Counter))){     
  #   print(this_data_source_counter) 
  #   # isolate data from this data source (in loop iteration) 
  #   if (plot_year==0){
  #     This_data <- PM25_data[which(PM25_data$Data_Source_Counter==this_data_source_counter), ]
  #   } else {
  #     This_data <- PM25_data[which(PM25_data$Data_Source_Counter==this_data_source_counter & PM25_data$Year==plot_year), ]
  #   } # if (plot_year==)
  #   
  #   repeated_locations=This_data[,c("Lat","Lon")] # find unique locations in data https://stats.stackexchange.com/questions/6759/removing-duplicated-rows-data-frame-in-r
  #   non_repeat_locations <- repeated_locations[!duplicated(repeated_locations), ]
  #   rm(repeated_locations)
  #   this_plot_color <- as.character(unique(This_data$PlottingColor))
  #   print(this_plot_color)
  #   points(non_repeat_locations[,2],non_repeat_locations[,1],col=this_plot_color,cex=1-1/(2*(this_data_source_counter+1))) # http://www.milanor.net/blog/maps-in-r-plotting-data-points-on-a-map/
  #   print(paste(unique(This_data$Data_Source_Name_Display),dim(non_repeat_locations)[1]," Locations"))
  #   if (this_data_source_counter==min(sort(unique(PM25_data$Data_Source_Counter)))) { # start or add to ledend
  #     legend_names <- as.character(unique(This_data$Data_Source_Name_Display))
  #     legend_colors <- this_plot_color
  #   } else {
  #     legend_names <- c(legend_names,as.character(unique(This_data$Data_Source_Name_Display)))
  #     legend_colors <- c(legend_colors,this_plot_color)
  #   } # if (this_data_source_counter==min(sort(unique(PM25_data$Data_Source_Counter)))) { # start or add to ledend
  #   print(legend_names)
  #   rm(This_data,non_repeat_locations,this_plot_color) # UNCOMMENT
  # } # for(this_data_source_counter in 0:data_source_counter){    
  # 
  # ## Add legend to figure
  # legend("bottomleft", # position
  #        legend = legend_names, 
  #        col = legend_colors,
  #        pch = 1,
  #        title = "Data Source",
  #        cex = 0.56,
  #        bty = "n") # border
  # rm(legend_names,legend_colors)
  # 
  # 
  # Plot_to_ImageFile_BottomOnly.fn(FigFileName = FigFileName, title_string = this_title_string) # finish image file
  # LaTex_code_4_figure.fn(LatexFileName = LatexFileName, title_string = this_title_string, file_sub_label = file_sub_label_plots, 
  #                        plot_name_extension = this_plot_name_extension, output.directory.short = output.directory.short, 
  #                        image_format = this_image_format)

#} # for (plot_year in c(0,start_study_year:stop_study_year)) { # plot all years together and then plot map of data by year

#rm(FigFileName_nopath,this_image_file_name,subsection_name,fig_label,fig_caption,image_format,this_fig_title)
#rm(LatexFileName,FigFileName_extension)#,FigFileName)
#rm(this_data_source_counter)



##### data frame report ####
# plot all/multiple variables in a data frame
#df_report.fn(df = Predictor_data, cols_interest = c(predictor_variables), x_axis_var = "Date", output.directory = output.directory,
#             output.directory.short = output.directory.short, file_sub_label = file_sub_label, title_string_partial = title_string_partial, plot_color = "black",
#             LatexFileName = LatexFileName, SinkFileName = NA, image_format = image_format)


# stop the timer
proc.time() - start_code_timer
rm(start_code_timer)
