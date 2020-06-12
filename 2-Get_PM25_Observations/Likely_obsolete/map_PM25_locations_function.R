map_PM25_locations.fn <- function(start_study_year,stop_study_year) {

#### Map locations - all together and then by year ####
for (plot_year in c(0,start_study_year:stop_study_year)) { # plot all years together and then plot map of data by year
  print(plot_year)
  ## Names for figure ##
  FigFileName_nopath <- paste("MapPM25_All_Sites","plot_year",plot_year,sep = "")
  if (plot_year==0){
    this_fig_title <- "All PM2.5 Observation Locations"
    fig_caption <- paste("Map of locations of PM2.5 observations for entire study period, ",start_study_year," to ",stop_study_year,".",sep = "")
    Animation_subdirectory <- ""
  } else {
    this_fig_title <- paste("PM2.5 Observation Locations, ",plot_year,sep = "")
    fig_caption <- paste("Map of locations of PM2.5 observations during ",plot_year,".",sep = "")
    Animation_subdirectory <- "MonitorLocationsByYear"
    dir.create(file.path(output.directory,Animation_subdirectory),showWarnings = FALSE)
  } # if (plot_year==)
  this_image_file_name <- "All_Monitor_Locations" #paste("All_Monitor_Locations","plot_year",plot_year,sep = "")
  subsection_name <- "All PM2.5 Monitor Locations"
  fig_label <- paste("MapPM25Loc",plot_year,sep = "")
  image_format <- "pdf" #"png" #"pdf" 
  
  print(FigFileName_nopath)
  print(this_image_file_name)
  print(subsection_name)
  print(this_fig_title)
  print(fig_label)
  print(fig_caption)
  print(image_format)
  
  # Resources for mapping
  # http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
  #print('still need to put in the latex code for this plot')
  # map boundaries of western US states
  USmap=readOGR(dsn=file.path(USMaps.directory),layer = "cb_2016_us_state_500k")
  
  # COMMENT following lines
  #head(USmap@data,n=2)
  #sapply(USmap@data,class)
  #USmap$ALAND_NUM <- as.numeric(as.character(USmap$ALAND))
  #mean(USmap$ALAND_NUM)
  #nrow(USmap)
  #ncol(USmap)
  #plot(USmap)
  
  # have R recognize state FP's as numerical values (in a new column)
  USmap$STATEFP_NUM <- as.numeric(as.character(USmap$STATEFP))
  
  # display the State FP values and state abbreviations next to each other
  USmap@data[,c("STATEFP_NUM","STUSPS")]
  
  # find the 11 western states included in the study
  WestUSmap=USmap@data[USmap$STATEFP_NUM==4|USmap$STATEFP_NUM==6|USmap$STATEFP_NUM==8|USmap$STATEFP_NUM==16|USmap$STATEFP_NUM==30|USmap$STATEFP_NUM==32|USmap$STATEFP_NUM==35|USmap$STATEFP_NUM==49|USmap$STATEFP_NUM==56|USmap$STATEFP_NUM==41|USmap$STATEFP_NUM==53|USmap$STATEFP_NUM==38|USmap$STATEFP_NUM==46|USmap$STATEFP_NUM==31|USmap$STATEFP_NUM==20|USmap$STATEFP_NUM==40|USmap$STATEFP_NUM==48,]
  print(WestUSmap)
  
  # start file for map
  FigFileName_extension <- as.character(paste(FigFileName_nopath,".",image_format,sep = ""))
  #FigFileName <- file.path(output.directory,FigFileName_extension)
  print("might need if- statement if FigFileName doesn't work")
  FigFileName <- file.path(output.directory,Animation_subdirectory,FigFileName_extension)
  print(FigFileName)
  if (image_format=="pdf") {
    pdf(file=FigFileName, height = 3.5, width = 5, onefile=FALSE) # start pdf document to put figure into
  }  else if (image_format=="jpg") {
    jpeg(file=FigFileName) # start jpg document to put figure into
  } else if (image_format == "png"){
    png(file=FigFileName) #, height = 3.5, width = 5, onefile=FALSE)
  } else {stop("invalid option for image file type")}
  plot.new() # clear the plot to have a clean canvas to draw on
  
  ## Do plotting/mapping
  WestUSmapGeom=USmap[USmap$STATEFP_NUM==4|USmap$STATEFP_NUM==6|USmap$STATEFP_NUM==8|USmap$STATEFP_NUM==16|USmap$STATEFP_NUM==30|USmap$STATEFP_NUM==32|USmap$STATEFP_NUM==35|USmap$STATEFP_NUM==49|USmap$STATEFP_NUM==56|USmap$STATEFP_NUM==41|USmap$STATEFP_NUM==53|USmap$STATEFP_NUM==38|USmap$STATEFP_NUM==46|USmap$STATEFP_NUM==31|USmap$STATEFP_NUM==20|USmap$STATEFP_NUM==40|USmap$STATEFP_NUM==48,]
  plot(WestUSmapGeom)
  
    # isolate data from this data source (in loop iteration) 
    if (plot_year==0){
      #This_data <- input_mat[which(input_mat$Data_Source_Counter==this_data_source_counter), ]
      This_data <- input_mat
    } else {
      This_data <- input_mat[which(input_mat$Year==plot_year), ]
    } # if (plot_year==)
    
    # do a basic check of the data
    summary(This_data)
    # find unique locations in data https://stats.stackexchange.com/questions/6759/removing-duplicated-rows-data-frame-in-r
    repeated_locations=This_data[,c("PM2.5_Lat","PM2.5_Lon")]
    non_repeat_locations <- repeated_locations[!duplicated(repeated_locations), ]
    rm(repeated_locations)
    this_plot_color <- "red"  #as.character(unique(This_data$PlottingColor))
    print(this_plot_color)
    points(non_repeat_locations[,2],non_repeat_locations[,1],col=this_plot_color,cex=1) # http://www.milanor.net/blog/maps-in-r-plotting-data-points-on-a-map/

  ## Code to finish figure and write latex code
  par(mar=c(4.2, 3.8, 1, 0.2)) # trim off extra white space (bottom, left, top, right)
  #summary(gbmtrainonly)
  title(main = this_fig_title)
  dev.off() # stop writing to pdf file
  rm(FigFileName) # delete pdf file name variable
  
  
  
  
  
} # for (plot_year in c(0,start_study_year:stop_study_year)) { # plot all years together and then plot map of data by year

#make.mov <- function(){
#  unlink("plot.mpg")
#  system("convert -delay 0.5 MapPM25_All_Sitesplot_year*.jpg plot.mpg")
#}
# https://www.r-graph-gallery.com/166-basic-animated-graph-with-imagemagick/

# https://stackoverflow.com/questions/1298100/creating-a-movie-from-a-series-of-plots-in-r

# system("convert -delay 0.5 C:\Users\mema2636\MMM_GitHub\estimate-pm25\LaTeX_documentation\Code_Outputs\MapPM25_All_Sitesplot_year*.jpg plot.mpg")

rm(FigFileName_nopath,this_image_file_name,subsection_name,fig_label,fig_caption,image_format,this_fig_title)
rm(LatexFileName,FigFileName_extension)#,FigFileName)
rm(this_data_source_counter)

} # function