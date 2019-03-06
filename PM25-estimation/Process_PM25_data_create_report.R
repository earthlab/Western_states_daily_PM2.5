# Process_PM25_data_create_report.R - plots and maps

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
print(paste("Start Process_PM25_data_create_report.R at",Sys.time(),sep = " "))

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
#this_source_file <- paste("PM25_Step3_part_",processed_data_version,"_Projected.csv")
this_source_file <- paste("PM25_Step4_part_",processed_data_version,"_NAD83.csv")
sub_folder <- paste("PM25_data_part_",processed_data_version,sep = "") 
file_sub_label <- paste("Report_",substr(this_source_file, 1, (nchar(this_source_file)-4)),sep = "") # file partial name, decide whether to include date in file name
title_string_partial <- paste("Report PM2.5 part ",processed_data_version,sep = "") 
LatexFileName=file.path(output.directory,paste("Rgenerated_",file_sub_label,"Images.tex",sep = "")) # Start file for latex code images
LaTex_code_start_section.fn(LatexFileName, title_string = title_string_partial, append_option = FALSE)
sink.number()
start_study_year <- input_mat_extract_year_from_date.fn(define_study_constants.fn("start_date")) #2008
stop_study_year <- input_mat_extract_year_from_date.fn(define_study_constants.fn("end_date"))#2014
study_states_abbrev <- define_file_paths.fn("study_states_abbrev") #c("AZ","CA","CO", "ID", "MT", "NV", "NM", "OR", "UT", "WA", "WY")

#### Load Data ####
PM25_data <- read.csv(file.path(ProcessedData.directory,sub_folder,this_source_file),header=TRUE) # load the AQS file
PM25_data <- input_mat_change_data_classes.fn(PM25_data)
PM25_data$Data_Source_Name_Display <- as.character(PM25_data$Data_Source_Name_Display)

#PM25_data$PM2.5_Lat <- NA # overwrite unprojected lat/lon so it isn't accidentally used
#PM25_data$PM2.5_Lon <- NA # overwrite unprojected lat/lon so it isn't accidentally used
if (max(is.na(PM25_data$Year))>0) {
  stop("not all data rows have year filled in - fix this in Step1")
}
#print("not all data rows have year filled in, filling it in now - eventually do this in Step1")
#PM25_data$Year <- input_mat_extract_year_from_date.fn(PM25_data$Date_Local)

#keep? # replace some of the names with shorter versions
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
#### Map locations - all together and then by year ####
subsection_name <- "All PM2.5 Monitor Locations"
LaTex_code_start_subsection.fn(LatexFileName, title_string = subsection_name, append_option = FALSE) # start subsection for latex code

this_image_format <- "jpg" #"pdf" # options are: #"jpg" #"pdf" #"png" 
#for (plot_year in 0){#c(0,start_study_year:stop_study_year)) { # plot all years together and then plot map of data by year
for (plot_year in c(0,start_study_year:stop_study_year)) { # plot all years together and then plot map of data by year
    
  file_sub_label_plots <- paste("MapPM25_All_Sites","Y",plot_year,sep = "")
  print(plot_year)
  this_plot_name_extension <-  paste("ObsMapY",plot_year, sep = "")
  this_title_string <- "All PM2.5 Observation Locations"
  if (plot_year==0){ # year 0 is all years, or just one year at a time
    this_title_string <- "All PM2.5 Observation Locations"
    fig_caption <- paste("Map of locations of PM2.5 observations for entire study period, ",start_study_year," to ",stop_study_year,".",sep = "")
  } else { # year 0 is all years, or just one year at a time
    this_title_string <- paste("PM2.5 Observation Locations, ",plot_year,sep = "")
    fig_caption <- paste("Map of locations of PM2.5 observations during ",plot_year,".",sep = "")
  } # if (plot_year==0){ # year 0 is all years, or just one year at a time
  
  
  FigFileName <- Plot_to_ImageFile_TopOnly.fn(output.directory, file_sub_label = file_sub_label_plots, 
                                              plot_name_extension = this_plot_name_extension, image_format = this_image_format) # start of image file
  # plot data
  map_base_layer.fn(USMaps.directory = USMaps.directory, study_states_abbrev = study_states_abbrev) # map state boundaries
  
  # cycle through each data source (EPA and various field campaigns) and plot each in a different color
  for(this_data_source_counter in sort(unique(PM25_data$Data_Source_Counter))){     
    print(this_data_source_counter) 
    # isolate data from this data source (in loop iteration) 
    if (plot_year==0){
      This_data <- PM25_data[which(PM25_data$Data_Source_Counter==this_data_source_counter), ]
    } else {
      This_data <- PM25_data[which(PM25_data$Data_Source_Counter==this_data_source_counter & PM25_data$Year==plot_year), ]
    } # if (plot_year==)
    
    repeated_locations=This_data[,c("Lat","Lon")] # find unique locations in data https://stats.stackexchange.com/questions/6759/removing-duplicated-rows-data-frame-in-r
    non_repeat_locations <- repeated_locations[!duplicated(repeated_locations), ]
    rm(repeated_locations)
    this_plot_color <- as.character(unique(This_data$PlottingColor))
    print(this_plot_color)
    points(non_repeat_locations[,2],non_repeat_locations[,1],col=this_plot_color,cex=1-1/(2*(this_data_source_counter+1))) # http://www.milanor.net/blog/maps-in-r-plotting-data-points-on-a-map/
    print(paste(unique(This_data$Data_Source_Name_Display),dim(non_repeat_locations)[1]," Locations"))
    if (this_data_source_counter==min(sort(unique(PM25_data$Data_Source_Counter)))) { # start or add to ledend
      legend_names <- as.character(unique(This_data$Data_Source_Name_Display))
      legend_colors <- this_plot_color
    } else {
      legend_names <- c(legend_names,as.character(unique(This_data$Data_Source_Name_Display)))
      legend_colors <- c(legend_colors,this_plot_color)
    } # if (this_data_source_counter==min(sort(unique(PM25_data$Data_Source_Counter)))) { # start or add to ledend
    print(legend_names)
    rm(This_data,non_repeat_locations,this_plot_color) # UNCOMMENT
  } # for(this_data_source_counter in 0:data_source_counter){    
  
  ## Add legend to figure
  legend("bottomleft", # position
         legend = legend_names, 
         col = legend_colors,
         pch = 1,
         title = "Data Source",
         cex = 0.56,
         bty = "n") # border
  rm(legend_names,legend_colors)
  
  
  Plot_to_ImageFile_BottomOnly.fn(FigFileName = FigFileName, title_string = this_title_string) # finish image file
  LaTex_code_4_figure.fn(LatexFileName = LatexFileName, title_string = this_title_string, file_sub_label = file_sub_label_plots, 
                         plot_name_extension = this_plot_name_extension, output.directory.short = output.directory.short, 
                         image_format = this_image_format)

} # for (plot_year in c(0,start_study_year:stop_study_year)) { # plot all years together and then plot map of data by year

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
