# # plot the input_mat File

#### Useful websites ####
# https://www.statmethods.net/advgraphs/parameters.html
# http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
#https://www.stat.berkeley.edu/classes/s133/saving.html

#### Call Load Functions that I created ####
source(file.path(writingcode.directory,"tell_R_column_classes_input_mat_function.R"))

#### define  constants ####
# Define study period
start_study_year <- 2008
stop_study_year <- 2014

start_study_date <- as.Date("2008-01-01","%Y-%m-%d")
stop_study_date <- as.Date("2014-12-31","%Y-%m-%d")

##### Create Sink output file ####
# start sink for output
SinkFileName=file.path(output.directory,"Plot_ML_Input_File_sink.txt")
sink(file =SinkFileName, append = FALSE, type = c("output","message"), split = FALSE) #UNCOMMENT

#### Call Packages (Library) ####
cat("Libraries")
#library(maps)
#library(mapproj)
library(ggplot2)
library(ggmap)
library(rgdal)
library(rgeos)
library(maptools)
library(dplyr)
library(tidyr)

#library(tmap)

#### Read in Data file ####



# load input_mat from file
#input_mat <- read.csv(file.path(ProcessedData.directory,this_file),header=TRUE)
# load input_mat from file
#Aves_input_mat <- read.csv(file.path(ProcessedData.directory,"de_duplicated_aves_ML_input.csv"),header=TRUE)
#Coloc_input_mat <- read.csv(file.path(ProcessedData.directory,"de_duplicated_colocated_ML_input.csv"),header=TRUE)

these_files <- c("de_duplicated_aves_ML_input.csv","de_duplicated_colocated_ML_input.csv")
# this_file <- "de_duplicated_aves_ML_input.csv"
for (this_file in these_files) {
  # load input_mat from file
  input_mat_step <- read.csv(file.path(ProcessedData.directory,this_file),header=TRUE)
  print(colnames(input_mat_step))
  
  # Tell R which columns to recognize as factors #
  input_mat <- tell_R_column_classes_input_mat.fn(input_mat_step)
  
  #### Summarize data as a whole ####
  print('structure: str() of input_mat')
  str(input_mat)
  
  print('top few rows of input_mat:')
  head(input_mat)
  
  print('last few rows of input_mat:')
  tail(input_mat)
  
  print("summary(input_mat)")
  summary(input_mat)
  
  
  
  } # for (this_file in these_files) {

print("output images in pngs or tiff formats")
stop("re-write this code as several functions and an overarching plot/check data function then run on both colocated and aves files (and earlier files) to be able to see differences.")
stop("write code to check if there are repeated date/location combinations in the data, the aves file shouldnt have any")





#summary(input_mat$PM2.5_Obs)

#### compare 88101 data to 88502 data ####
which_88101 <- which(input_mat$Parameter_Code == 88101)
Only_88101 <- input_mat[which_88101,]
print("summary(Only_88101)")
summary(Only_88101)
rm(which_88101)

which_88502 <- which(input_mat$Parameter_Code == 88502)
Only_88502 <- input_mat[which_88502,]
print("summary(Only_88502)")
summary(Only_88502)
rm(which_88502)

## Names for figure ##
FigFileName_nopath <- paste("TimeSeries_88101v88502",sep = "")
this_image_file_name <- paste("Compare_88101v88502",sep = "")
subsection_name <- paste("Compare 88101 to 88502 PM2.5",sep = "")
this_fig_title <- paste("88101 and 88502 Time Series",sep = "")
fig_label <- paste("TS101v502",sep = "")
fig_caption <- paste("Time series of 88101 and 88502 PM2.5 data.",sep = "")
image_format <- "jpg" #"pdf" #"jpg" #"png" #

# start file for plot
if (max(dev.cur())>1) { # make sure it isn't outputting to any figure files
  dev.off(which = dev.cur())
} # if (max(dev.cur())>1) {
FigFileName_extension <- as.character(paste(FigFileName_nopath,".",image_format,sep = "")) # define file name for the figure to be created
FigFileName <- file.path(output.directory,FigFileName_extension)
print(FigFileName)
if (image_format=="pdf") {
  pdf(file=FigFileName, height = 3.5, width = 5, onefile=FALSE) # start pdf document to put figure into
}  else if (image_format=="jpg") {
  jpeg(file=FigFileName) # start jpg document to put figure into
} else if (image_format == "png"){
  png(file=FigFileName)# , height = 3.5, width = 5, onefile=FALSE)
} else {stop("invalid option for image file type")}
plot.new() # clear the plot to have a clean canvas to draw on

## Do plotting/mapping
# loop through data sources to plot 88101 and 88502 data with the color of the data source and different shapes for 88101 vs 88502
shape_88101 = 0
shape_88502 = 1
for(this_data_source_counter in 0:max(input_mat[,c("Data_Source_Counter")])){    
  print('still need to pull in Fed Land Management Database concentrations')
  print(this_data_source_counter) 
  
  # isolate data from this data source (in loop iteration) 
  This_data_88101 <- Only_88101[which(Only_88101$Data_Source_Counter==this_data_source_counter), ]

  if (dim(This_data_88101)[1]>0) {
    This_Data_Source_Name_Short <- unique(This_data_88101[,c("Data_Source_Name_Short")])
    #This_Data_Source_Name_Display <- unique(This_data_88101[,c("Data_Source_Name_Display")])
    print(This_Data_Source_Name_Short)
    this_plot_color <- as.character(unique(This_data_88101$PlottingColor))
    print(this_plot_color)
    print("summary(This_data_88101)") 
    summary(This_data_88101)
  #points(x=This_data_88101$Date_Local,y=This_data_88101$PM2.5_Obs,xlim=c(start_study_date,stop_study_date),col = this_plot_color, pch = shape_88101, cex=1-1/(2*(this_data_source_counter+1)))
  
  if (this_data_source_counter==0) {
   #legend_names <- paste(as.character(unique(This_data_88101$Data_Source_Name_Display)),"88101",sep = " ")
    legend_names <- paste(This_Data_Source_Name_Short,"88101",sep = " ")
    print(legend_names)
    legend_shapes <- shape_88101
    print(legend_shapes)
    legend_colors <- this_plot_color
    print(legend_colors)
    plot(x=This_data_88101$Date_Local,y=This_data_88101$PM2.5_Obs,xlim=c(start_study_date,stop_study_date),col = this_plot_color, pch = shape_88101, cex=1-1/(2*(this_data_source_counter+1)))
    
  } else {
    #legend_names <- c(legend_names,paste(as.character(unique(This_data_88502$Data_Source_Name_Display)),"88502",sep = " "))
    legend_names <- c(legend_names,paste(This_Data_Source_Name_Short,"88101",sep = " "))
    print(legend_names)
    legend_shapes <- c(legend_shapes,shape_88101)
    legend_colors <- c(legend_colors,this_plot_color)
    points(x=This_data_88101$Date_Local,y=This_data_88101$PM2.5_Obs,xlim=c(start_study_date,stop_study_date),col = this_plot_color, pch = shape_88101, cex=1-1/(2*(this_data_source_counter+1)))
    
  } # if (this_data_source_counter==0) {
  } # if (dim(This_data_88101)[1]>0) {
  
  This_data_88502 <- Only_88502[which(Only_88502$Data_Source_Counter==this_data_source_counter), ]

  
  if (dim(This_data_88502)[1]>0) {
    print(paste(This_Data_Source_Name_Short,"summary(This_data_88502)"))
    summary(This_data_88502)
  points(x=This_data_88502$Date_Local,y=This_data_88502$PM2.5_Obs,xlim=c(start_study_date,stop_study_date),col = this_plot_color, pch = shape_88502, cex=1-1/(2*(this_data_source_counter+1)))
  
    legend_names <- c(legend_names,paste(This_Data_Source_Name_Short,"88502",sep = " "))
    print(legend_names)
    legend_shapes <- c(legend_shapes,shape_88502)
    print(legend_shapes)
    legend_colors <- c(legend_colors,this_plot_color)
    print(legend_colors)
  }  # if (dim(This_data_88101)[1]>0) {
    
} # for(this_data_source_counter in 0:max(input_mat[,c("Data_Source_Counter")])){  

## Add legend to figure
legend("topleft", # position
       legend = legend_names, 
       col = legend_colors,
       pch = legend_shapes,
       title = "Data Source",
       cex = 0.56,
       bty = "n") # border
rm(legend_names,legend_colors,legend_shapes)

## Code to finish figure and write latex code
par(mar=c(4.2, 3.8, 1, 0.2)) # trim off extra white space (bottom, left, top, right)
title(main = this_fig_title)
#dev.off() # stop writing to pdf file
if (max(dev.cur())>1) { # make sure it isn't outputting to any figure files
  dev.off(which = dev.cur())
} # if (max(dev.cur())>1) {
remove(FigFileName) # delete pdf file name variable
sink() # stop putting text into SinkFileName
sink.number()
LatexFileName=file.path(output.directory,paste("Rgenerated_Images",this_image_file_name,".tex",sep = "")) # Start file for latex code images
sink(file = LatexFileName, append = FALSE, type = c("output","message"),split = FALSE)
cat(paste("\n\\subsection{",subsection_name,"}",sep = ""))
#cat(paste("\n\\subsection*{",subsection_name,"}",sep = ""))
cat("\n\\begin{figure} \n")
cat("\\centering \n")
#if (Animation_subdirectory==""){
cat(paste("\\includegraphics[width=0.77\\textwidth]{Code_Outputs/",FigFileName_nopath,".",image_format,"} \n",sep = "")) 
#} else {
#  cat(paste("\\includegraphics[width=0.77\\textwidth]{Code_Outputs/",Animation_subdirectory,"/",FigFileName_nopath,".",image_format,"} \n",sep = "")) 
#}
cat(paste("\\caption{\\label{fig:",fig_label,"}",fig_caption,"} \n",sep = "")) 
cat("\\end{figure} \n \n")
sink() # stop writing to latex file
sink(file =SinkFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting output into SinkFileName
rm(FigFileName_nopath,this_image_file_name,subsection_name,fig_label,fig_caption,image_format,this_fig_title)
rm(LatexFileName,FigFileName_extension)
rm(this_data_source_counter)

#stop("pick up writing code here - loop through data sources to plot 88101 and 88502 data with the color of the data source and different shapes for 88101 vs 88502")
#### NEED TO FINISH Monthly concentrations by month ####
# for (plot_month in c(1:12)) { # plot all years together and then plot map of data by year
#   print(plot_month)
#   ## Names for figure ##
#   FigFileName_nopath <- paste("MapPM25_Conc","plot_month",plot_month,sep = "")
#     this_fig_title <- paste("Map PM2.5 Concentrations by Month, ",plot_year,sep = "")
#     fig_caption <- paste("Map of locations of PM2.5 concentrations during ",plot_year,".",sep = "")
#     Animation_subdirectory <- "MapPM25ByMonth"
#     dir.create(file.path(output.directory,Animation_subdirectory),showWarnings = FALSE)
# 
#   this_image_file_name <- "Map_PM25_Conc_by_Month"
#   subsection_name <- "PM2.5 Concentrations by Month"
#   fig_label <- paste("MapPM25Conc",plot_year,sep = "")
#   image_format <- "pdf" #"png" #"pdf" 
#   
#   print(FigFileName_nopath)
#   print(this_image_file_name)
#   print(subsection_name)
#   print(this_fig_title)
#   print(fig_label)
#   print(fig_caption)
#   print(image_format)
#   
#   # Resources for mapping
#   # http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
#   #print('still need to put in the latex code for this plot')
#   # map boundaries of western US states
#   USmap=readOGR(dsn=file.path(USMaps.directory),layer = "cb_2016_us_state_500k")
# 
#   # have R recognize state FP's as numerical values (in a new column)
#   USmap$STATEFP_NUM <- as.numeric(as.character(USmap$STATEFP))
#   
#   # display the State FP values and state abbreviations next to each other
#   USmap@data[,c("STATEFP_NUM","STUSPS")]
#   
#   # find the 11 western states included in the study
#   WestUSmap=USmap@data[USmap$STATEFP_NUM==4|USmap$STATEFP_NUM==6|USmap$STATEFP_NUM==8|USmap$STATEFP_NUM==16|USmap$STATEFP_NUM==30|USmap$STATEFP_NUM==32|USmap$STATEFP_NUM==35|USmap$STATEFP_NUM==49|USmap$STATEFP_NUM==56|USmap$STATEFP_NUM==41|USmap$STATEFP_NUM==53|USmap$STATEFP_NUM==38|USmap$STATEFP_NUM==46|USmap$STATEFP_NUM==31|USmap$STATEFP_NUM==20|USmap$STATEFP_NUM==40|USmap$STATEFP_NUM==48,]
#   #print(WestUSmap)
#   
#   # start file for map
#   FigFileName_extension <- as.character(paste(FigFileName_nopath,".",image_format,sep = ""))
#   FigFileName <- file.path(output.directory,Animation_subdirectory,FigFileName_extension)
#   print(FigFileName)
#   if (image_format=="pdf") {
#     pdf(file=FigFileName, height = 3.5, width = 5, onefile=FALSE) # start pdf document to put figure into
#   }  else if (image_format=="jpg") {
#     jpeg(file=FigFileName) # start jpg document to put figure into
#   } else if (image_format == "png"){
#     png(file=FigFileName) #, height = 3.5, width = 5, onefile=FALSE)
#   } else {stop("invalid option for image file type")}
#   plot.new() # clear the plot to have a clean canvas to draw on
#   
#   ## Do plotting/mapping
#   WestUSmapGeom=USmap[USmap$STATEFP_NUM==4|USmap$STATEFP_NUM==6|USmap$STATEFP_NUM==8|USmap$STATEFP_NUM==16|USmap$STATEFP_NUM==30|USmap$STATEFP_NUM==32|USmap$STATEFP_NUM==35|USmap$STATEFP_NUM==49|USmap$STATEFP_NUM==56|USmap$STATEFP_NUM==41|USmap$STATEFP_NUM==53|USmap$STATEFP_NUM==38|USmap$STATEFP_NUM==46|USmap$STATEFP_NUM==31|USmap$STATEFP_NUM==20|USmap$STATEFP_NUM==40|USmap$STATEFP_NUM==48,]
#   plot(WestUSmapGeom)
#   
#   print(summary(input_mat$PM2.5_Obs))
#   these_quartiles <- quantile(input_mat$PM2.5_Obs)
#   print(these_quartiles)
#   # cycle through each data source (EPA and various field campaigns) and plot each in a different color
#   for(this_quartile in 1:4){     
#     print(this_quartile) 
#     # isolate data from this data source (in loop iteration) 
#     if (this_quartile==1) {
#       This_data <- input_mat[which(input_mat$PM2.5_Obs>=these_quartiles[1] & input_mat$PM2.5_Obs<these_quartiles[2] & input_mat$Month == plot_month), ]
#     }
#       
#     # do a basic check of the data
#     print("summary(This_data)")
#     summary(This_data)
#     # find unique locations in data https://stats.stackexchange.com/questions/6759/removing-duplicated-rows-data-frame-in-r
#     repeated_locations=This_data[,c("PM2.5_Lat","PM2.5_Lon")]
#     #duplicated(repeated_locations)
#     #repeated_locations[duplicated(repeated_locations), ]
#     non_repeat_locations <- repeated_locations[!duplicated(repeated_locations), ]
#     rm(repeated_locations)
#     #plot(non_repeat_locations[,2],non_repeat_locations[,1])
#     this_plot_color <- as.character(unique(This_data$PlottingColor))
#     print(this_plot_color)
#     #points(non_repeat_locations[,2],non_repeat_locations[,1],col="black",cex=unique(This_data$Data_Source_Counter)+.3) # http://www.milanor.net/blog/maps-in-r-plotting-data-points-on-a-map/
#     points(non_repeat_locations[,2],non_repeat_locations[,1],col=this_plot_color,cex=1-1/(2*(this_data_source_counter+1))) # http://www.milanor.net/blog/maps-in-r-plotting-data-points-on-a-map/
#     if (this_data_source_counter==0) {
#       legend_names <- as.character(unique(This_data$Data_Source_Name_Display))
#       legend_colors <- this_plot_color
#     } else {
#       legend_names <- c(legend_names,as.character(unique(This_data$Data_Source_Name_Display)))
#       legend_colors <- c(legend_colors,this_plot_color)
#     } # if (this_data_source_counter==0) {
#     print(legend_names)
#     rm(This_data,non_repeat_locations,this_plot_color) # UNCOMMENT
#   } # for(this_data_source_counter in 0:data_source_counter){    
#   
#   ## Add legend to figure
#   legend("bottomleft", # position
#          legend = legend_names, 
#          col = legend_colors,
#          pch = 1,
#          title = "Data Source",
#          cex = 0.56,
#          bty = "n") # border
#   rm(legend_names,legend_colors)
#   
#   ## Code to finish figure and write latex code
#   par(mar=c(4.2, 3.8, 1, 0.2)) # trim off extra white space (bottom, left, top, right)
#   #summary(gbmtrainonly)
#   title(main = this_fig_title)
#   dev.off() # stop writing to pdf file
#   rm(FigFileName) # delete pdf file name variable
#   sink() # stop putting text into SinkFileName
#   LatexFileName=file.path(output.directory,paste("Rgenerated_Images",this_image_file_name,".tex",sep = "")) # Start file for latex code images
#   if (plot_year==0) {
#     sink(file = LatexFileName, append = FALSE, type = c("output","message"),split = FALSE) # start new file the first time
#     cat(paste("\n\\subsubsection*{",subsection_name,"}",sep = ""))
#   } else {
#     sink(file = LatexFileName, append = TRUE, type = c("output","message"),split = FALSE)
#   }
#   cat("\n\\begin{figure} \n")
#   cat("\\centering \n")
#   #cat(paste("\\includegraphics[width=0.77\\textwidth]{Code_Outputs/",FigFileName_nopath,".",image_format,"} \n",sep = "")) 
#   if (Animation_subdirectory==""){
#     cat(paste("\\includegraphics[width=0.77\\textwidth]{Code_Outputs/",FigFileName_nopath,".",image_format,"} \n",sep = "")) 
#   } else {
#     cat(paste("\\includegraphics[width=0.77\\textwidth]{Code_Outputs/",Animation_subdirectory,"/",FigFileName_nopath,".",image_format,"} \n",sep = "")) 
#   }
#   
#   cat(paste("\\caption{\\label{fig:",fig_label,"}",fig_caption,"} \n",sep = "")) 
#   cat("\\end{figure} \n \n")
#   sink() # stop writing to latex file
#   sink(file =SinkFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting output into SinkFileName
#   rm(Animation_subdirectory)
#   
# } # for (plot_year in c(0,start_study_year:stop_study_year)) { # plot all years together and then plot map of data by year
# 



######## Loop through data sources and do a series of plots #####
#for(this_data_source_counter in 0:max(input_mat[,c("Data_Source_Counter")])){  
for(this_data_source_counter in unique(input_mat$Data_Source_Counter)){ # cycle through only the data sources that made it through cleaning   
  #if (this_data_source_counter!=1){
  print('still need to pull in Fed Land Management Database concentrations')

  print(this_data_source_counter) 

  
  # isolate data from this data source (in loop iteration) 
  This_data <- input_mat[which(input_mat$Data_Source_Counter==this_data_source_counter), ]
  This_Data_Source_Name_Short <- unique(This_data[,c("Data_Source_Name_Short")])
  This_Data_Source_Name_Display <- unique(This_data[,c("Data_Source_Name_Display")])
  print(This_Data_Source_Name_Display)
  this_plot_color <- as.character(unique(This_data$PlottingColor))
  print(this_plot_color)
  
  summary(This_data)
  
## start with a basic time series plot:
  ## Names for figure ##
  FigFileName_nopath <- paste(This_Data_Source_Name_Short,"_time_series",sep = "")
  this_image_file_name <- paste(This_Data_Source_Name_Short,"DataSummary",sep = "")
  subsection_name <- paste(This_Data_Source_Name_Display," Plots",sep = "")
  this_fig_title <- paste(This_Data_Source_Name_Display," Time Series",sep = "")
  fig_label <- paste(This_Data_Source_Name_Short,"TS",sep = "")
  fig_caption <- paste(This_Data_Source_Name_Display," time series.",sep = "")
  image_format <- "png" #"jpg"
  
  # start file for plot
  FigFileName_extension <- as.character(paste(FigFileName_nopath,".",image_format,sep = "")) # define file name for the figure to be created
  FigFileName <- file.path(output.directory,FigFileName_extension)
  print(FigFileName)
  if (image_format=="pdf") {
    pdf(file=FigFileName, height = 3.5, width = 5, onefile=FALSE) # start pdf document to put figure into
  }  else if (image_format=="jpg") {
    jpeg(file=FigFileName) # start jpg document to put figure into
  } else if (image_format == "png"){
    png(file=FigFileName)# , height = 3.5, width = 5, onefile=FALSE)
  } else {stop("invalid option for image file type")}
  plot.new() # clear the plot to have a clean canvas to draw on
  
  ## Do plotting/mapping
  #plot(x=This_data$Date_Local,y=This_data$PM2.5_Obs,xlim=c(start_study_date,stop_study_date))
  plot(x=This_data$Date_Local,y=This_data$PM2.5_Obs,xlim=c(start_study_date,stop_study_date),col = this_plot_color)
  
  ## Code to finish figure and write latex code
  par(mar=c(4.2, 3.8, 1, 0.2)) # trim off extra white space (bottom, left, top, right)
  #summary(gbmtrainonly)
  title(main = this_fig_title)
  dev.off() # stop writing to pdf file
  remove(FigFileName) # delete pdf file name variable
  sink() # stop putting text into SinkFileName
  LatexFileName=file.path(output.directory,paste("Rgenerated_Images",this_image_file_name,".tex",sep = "")) # Start file for latex code images
  sink(file = LatexFileName, append = FALSE, type = c("output","message"),split = FALSE)
  cat(paste("\n\\subsection{",subsection_name,"}",sep = ""))
  #cat(paste("\n\\subsection*{",subsection_name,"}",sep = ""))
  cat("\n\\begin{figure} \n")
  cat("\\centering \n")
  #if (Animation_subdirectory==""){
    cat(paste("\\includegraphics[width=0.77\\textwidth]{Code_Outputs/",FigFileName_nopath,".",image_format,"} \n",sep = "")) 
  #} else {
  #  cat(paste("\\includegraphics[width=0.77\\textwidth]{Code_Outputs/",Animation_subdirectory,"/",FigFileName_nopath,".",image_format,"} \n",sep = "")) 
  #}
  cat(paste("\\caption{\\label{fig:",fig_label,"}",fig_caption,"} \n",sep = "")) 
  cat("\\end{figure} \n \n")
  sink() # stop writing to latex file
  sink(file =SinkFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting output into SinkFileName
  rm(FigFileName_nopath,this_image_file_name,subsection_name,fig_label,fig_caption,image_format,this_fig_title)
  rm(LatexFileName,FigFileName_extension)
  rm(this_data_source_counter)
  
  testing_threshold <- 1000
  find_absurd_high <- which(This_data$PM2.5_Obs>testing_threshold) #43565
  N_absurd_high <- length(find_absurd_high)
  Absurd_high <- This_data[find_absurd_high,]
  
  ## Make an additional plot for DRI data without the highest data point so that the majority of data can be seen
  if (max(This_data$PM2.5_Obs)>testing_threshold) { # plot data without highest points
    #(unique(This_data$Data_Source_Name_Short) == "FireCacheDRI") {
    print("plot DRI data without highest point so the majority of data can be seen")
    
    ## Names for figure ##
    FigFileName_nopath <- paste(This_Data_Source_Name_Short,"_time_series_below_",testing_threshold,"ugm3",sep = "")
    this_image_file_name <- paste(This_Data_Source_Name_Short,"DataSummary",sep = "")
    subsection_name <- "" #paste(This_Data_Source_Name_Display," Plots",sep = "")
    #this_fig_title <- paste(This_Data_Source_Name_Display," Time Series, only showing data below ",testing_threshold," ug/m3",sep = "")
    this_fig_title <- paste(This_Data_Source_Name_Display," Time Series, < ",testing_threshold," ug/m3",sep = "")
    fig_label <- paste(This_Data_Source_Name_Short,"TSnoHigh",sep = "")
    fig_caption <- paste(This_Data_Source_Name_Display," time series without data above ",testing_threshold," ug/m3 so that the majority of data can be seen.",sep = "")
    image_format <- "png" #"jpg"
    
    # start file for plot
    FigFileName_extension <- as.character(paste(FigFileName_nopath,".",image_format,sep = "")) # define file name for the figure to be created
    FigFileName <- file.path(output.directory,FigFileName_extension)
    print(FigFileName)
    if (image_format=="pdf") {
      pdf(file=FigFileName, height = 3.5, width = 5, onefile=FALSE) # start pdf document to put figure into
    }  else if (image_format=="jpg") {
      jpeg(file=FigFileName) # start jpg document to put figure into
    } else if (image_format == "png"){
      png(file=FigFileName)# , height = 3.5, width = 5, onefile=FALSE)
    } else {stop("invalid option for image file type")}
    plot.new() # clear the plot to have a clean canvas to draw on
    
    ## Do plotting/mapping
    which_no_high <- This_data$PM2.5_Obs<testing_threshold
    NoHigh_data <- This_data[which_no_high,]
    plot(x=NoHigh_data$Date_Local,y=NoHigh_data$PM2.5_Obs,xlim=c(start_study_date,stop_study_date),col = this_plot_color)
    rm(which_no_high,NoHigh_data)
    
    ## Code to finish figure and write latex code
    par(mar=c(4.2, 3.8, 1, 0.2)) # trim off extra white space (bottom, left, top, right)
    title(main = this_fig_title)
    dev.off() # stop writing to pdf file
    remove(FigFileName) # delete pdf file name variable
    sink() # stop putting text into SinkFileName
    LatexFileName=file.path(output.directory,paste("Rgenerated_Images",this_image_file_name,".tex",sep = "")) # Start file for latex code images
    #sink(file = LatexFileName, append = FALSE, type = c("output","message"),split = FALSE)
    sink(file = LatexFileName, append = TRUE, type = c("output","message"),split = FALSE)
    #cat(paste("\n\\subsection{",subsection_name,"}",sep = ""))
    #cat(paste("\n\\subsection*{",subsection_name,"}",sep = ""))
    cat("\n\\begin{figure} \n")
    cat("\\centering \n")
    #if (Animation_subdirectory==""){
    cat(paste("\\includegraphics[width=0.77\\textwidth]{Code_Outputs/",FigFileName_nopath,".",image_format,"} \n",sep = "")) 
    #} else {
    #  cat(paste("\\includegraphics[width=0.77\\textwidth]{Code_Outputs/",Animation_subdirectory,"/",FigFileName_nopath,".",image_format,"} \n",sep = "")) 
    #}
    cat(paste("\\caption{\\label{fig:",fig_label,"}",fig_caption,"} \n",sep = "")) 
    cat("\\end{figure} \n \n")
    sink() # stop writing to latex file
    sink(file =SinkFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting output into SinkFileName
    rm(FigFileName_nopath,this_image_file_name,subsection_name,fig_label,fig_caption,image_format,this_fig_title)
    rm(LatexFileName,FigFileName_extension)
    
  } # if (max(This_data$PM2.5_Obs)>testing_threshold) { # plot data without highest points
  
  # find data from specific file
  #which_one_file <- which(This_data$Source_File=="Fire_Cache_Smoke_DRI_Smoke_NCFS_E_BAM_N1.csv")
  #One_File_data <- 
  
  
  
  ## get some stats about the data
  N_data_points <- dim(This_data)[1]
  find_high_points <- which(This_data$PM2.5_Obs>200)
  N_points_gt200 <- length(find_high_points)
  
  High_points <- This_data[find_high_points,]
  
  print('write code to plot high data points')
  summary(High_points)  
  rm(This_data,This_Data_Source_Name_Display,This_Data_Source_Name_Short)
#  }
} # for(this_data_source_counter in 0:max(input_mat[,c("Data_Source_Counter")])){  

#### Look at the high points in the data set as a whole ####

# get some stats about the data
High_points <- input_mat[which(input_mat$PM2.5_Obs>=200), ]

High_points <- subset(input_mat,PM2.5_Obs>=200)

print('write code to plot high data points')
summary(High_points)

Ordered_High_points <- High_points[order(High_points$PM2.5_Obs),]


print('get the high points of the data as a whole and look at it')




#### End of file clean up ####
rm(uppermost.directory,output.directory)
rm(working.directory,ProcessedData.directory,UintahData.directory,USMaps.directory,PCAPSData.directory)
rm(AQSData.directory,FMLE.directory,FireCache.directory,CARB.directory,UTDEQ.directory,NVDEQ.directory)