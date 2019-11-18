# Choropleths_at_ZipCodes_4panel.R
# choropleths at zip code level - 4-panel

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

#### Load Functions that I created ####
source(file.path("estimate-pm25","General_Project_Functions","general_project_functions.R"))
source(file.path(define_file_paths.fn("ML_Code.directory"),"Plotting_and_LaTex_functions.R"))
source(file.path(define_file_paths.fn("ML_Code.directory"),"Mapping_functions.R"))

#### Define file path ####
output.directory <- file.path("/home/rstudio","estimate-pm25","LaTeX_documentation","SciData_manuscript")

#### Define Variables ####
colname_interest_start <- "PM2.5_Obs"
col_name_interest <- "PM2.5"
study_states_abbrev = define_study_constants.fn("study_states_abbrev")
Region = "County"
RegionMaps.directory = define_file_paths.fn("CountyMaps.directory")
Date_col <- "Date"
Lat_col <- "Latitude"
Lon_col <-  "Longitude"
Var_col <-  col_name_interest
This_Var_col <- Var_col
Cut_points_set <-  TRUE
cut_point_scale <-  col_name_interest
Time_aggregation <-  "Monthly"
custom_breakpoints <- NA
this_summary_value <- "median"
time_counter <- 1
Var_col_title <- replace_character_in_string.fn(input_char = This_Var_col,char2replace = "_",replacement_char = " ")
Var4Name <- replace_character_in_string.fn(input_char = This_Var_col,char2replace = ".",replacement_char = "")
file_sub_label <- "Fig2_choropleth_4-panel"
plot_name_extension <- paste(Region,Var4Name,this_summary_value,sep = "")
RegionMapGeom <- load_County_Boundaries.fn(USMaps.directory, study_states_abbrev)
print("change code from using counties to using zip codes")
image_file_type <-  "pdf"
caption_string <- "Median PM\\textsubscript{2.5} concentrations by County for August 2017 (upper left), February 2017 (upper right), August 2010 (lower left), and February 2010 (lower right)."

#### Load data file ####
full_df <- read.csv(file.path("/home","rstudio","Processed_Data","ML_input_files","ML_input_PM25_Step4_part_f_de_duplicated_aves.csv"),header=TRUE) # load the file

#### Process the data ####

full_df$Date <- as.Date(full_df$Date,"%Y-%m-%d") # recognize dates as dates
df_step <- full_df[ , c("Date",colname_interest_start,"Latitude","Longitude","Month","Year")] # only keep Date, PM2.5, Lat, & Lon columns
df_step2 <- df_step[ complete.cases(df_step) , ] # get rid of any rows that have NAs
df_start <- df_step2[which(df_step2$PM2.5_Obs<1014), ] # remove any data above 1014 ug/m3, could probably go with a lower threshold
colnames(df_start)[colnames(df_start) == colname_interest_start] <- col_name_interest #"PM2.5"

#Jan_only <- df_start[which(df_start$Month == 1), ] # skipping January because many places can have very high concentrations due to winter accumulation during stagnent periods
Feb_only <- df_start[which(df_start$Month == 2), ]
Aug_only <- df_start[which(df_start$Month == 8), ]
Sep_only <- df_start[which(df_start$Month == 9), ]

# Display median concentration for February and August for each year
for (y in 2008:2018) {
  year_Feb <- Feb_only[which(Feb_only$Year == y), ]
  print(paste("Median PM2.5 during February of ",y,"is",round(median(year_Feb$PM2.5),1),"ug/m3"))
}

for (y in 2008:2018) {
  year_Aug <- Aug_only[which(Aug_only$Year == y), ]
  print(paste("Median PM2.5 during August of ",y,"is",round(median(year_Aug$PM2.5),1),"ug/m3"))
}

for (y in 2008:2018) {
  year_Sep <- Sep_only[which(Sep_only$Year == y), ]
  print(paste("Median PM2.5 during September of ",y,"is",round(median(year_Sep$PM2.5),1),"ug/m3"))
}

# I looked over the output and chose these years for the plots: (-MMM)
high_year <- 2017
low_year <- 2010
autumn_month <- "August"
winter_month <- "February"

# isolate the years/months to be plotted
high_year_autumn <- Aug_only[which(Aug_only$Year == high_year), ]
low_year_autumn <- Aug_only[which(Aug_only$Year == low_year), ]
high_year_winter <- Feb_only[which(Feb_only$Year == high_year), ]
low_year_winter <- Feb_only[which(Feb_only$Year == low_year), ]

#### Define file names ####
Top_latex_file_name <- file.path(define_file_paths.fn("output.directory"),"Report_top_template.tex")
Main_latex_file_name <- file.path(output.directory,paste("Rgenerated_Fig2_choropleth_4-panel_main.tex",sep = ""))
Bottom_latex_file_name <- file.path(define_file_paths.fn("output.directory"),"Report_bottom_template.tex")
Report_latex_file_name <- file.path(output.directory,paste("Rgenerated_Fig2_choropleth_4-panel.tex",sep = ""))
LatexFileName=file.path(output.directory,paste("Rgenerated_",file_sub_label,"MapCountyMonthlyImages.tex",sep = "")) # Start file for latex code images

#### Start setting up Figure ####
if (sink.number()>0) {sink()} # get stop any lingering sinks
if (file.exists(LatexFileName) == TRUE) {file.remove(LatexFileName)}
all_max_val <- max(df_start[ ,This_Var_col], na.rm = TRUE)
all_min_val <- min(df_start[ ,This_Var_col], na.rm = TRUE)
FigFileName <- Plot_to_ImageFile_TopOnly.fn(output.directory, file_sub_label, plot_name_extension = plot_name_extension, image_file_type) # start image file
nclr <- 7 # number of colors for choropleth
if (all_max_val > 350.5) {
  base_breaks <-  c(0, 12.1, 35.5, 55.5, 150.5, 250.5, all_max_val)
} else {
  base_breaks <-  c(0, 12.1, 35.5, 55.5, 150.5, 250.5, 350.5)
}
color_vec = c("green", "yellow", "orange", "red", "hotpink2", "hotpink3", "hotpink4")

#### Set up 4-panel plot ####
par(mfrow=c(2,2)) # 2x2 subplots

#### Create Upper-Left Plot of 4-panel ####
#Highest year PM2.5, Aug or Sept
# August 2017
df_subset <-  high_year_autumn
title_string <- paste(autumn_month,high_year)
Output_plot <- map_avg.fn(shp = RegionMapGeom, data = df_subset, nclr = nclr, plotclr = color_vec, breaks = base_breaks, Map_Var_col = This_Var_col) # plot values
title(main = title_string)
  
#### Create Upper-Right Plot of 4-panel ####
#Highest year PM2.5, Jan/Feb
df_subset <-  high_year_winter
title_string <- paste(winter_month,high_year)
Output_plot <- map_avg.fn(shp = RegionMapGeom, data = df_subset, nclr = nclr, plotclr = color_vec, breaks = base_breaks, Map_Var_col = This_Var_col) # plot values
title(main = title_string)

#### Create Lower-Left Plot of 4-panel ####
#Lowest year PM2.5, Aug or Sept
df_subset <-  low_year_autumn
title_string <- paste(autumn_month,low_year)
Output_plot <- map_avg.fn(shp = RegionMapGeom, data = df_subset, nclr = nclr, plotclr = color_vec, breaks = base_breaks, Map_Var_col = This_Var_col) # plot values
title(main = title_string)

#### Create Lower-Right Plot of 4-panel ####
#Lowest year PM2.5, Jan/Feb
df_subset <-  low_year_winter
title_string <- paste(winter_month,low_year)
Output_plot <- map_avg.fn(shp = RegionMapGeom, data = df_subset, nclr = nclr, plotclr = color_vec, breaks = base_breaks, Map_Var_col = This_Var_col) # plot values
title(main = title_string)

#### Finish figure and Tidy tex code ####
  dev.off() # stop trying to write to image file
  # output corresponding LaTex code
  LaTex_code_4_figure.fn(LatexFileName = LatexFileName, title_string = "", file_sub_label = file_sub_label, plot_name_extension = plot_name_extension, output.directory.short = output.directory.short, image_format = image_file_type, ClearPage = FALSE, fig_caption = caption_string) # write latex code for this image
 