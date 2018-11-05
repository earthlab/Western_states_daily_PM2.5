# functions for plotting and puting plots in LaTex

# plot model from ML run; was Plot_and_latex.fn
Plot_to_ImageFile.fn <- function(output.directory, file_sub_label, plot_name_extension, plotting_string, data_for_plotting, title_string, image_format = "jpg") {
  ### plot this_model_run_name
  #FigFileName=file.path(output.directory,paste(file_sub_label,"_",plot_name_extension,".pdf",sep = "")) # define file name for the figure to be created
  FigFileName=file.path(output.directory,paste(file_sub_label,"_",plot_name_extension,".",image_format,sep = "")) # define file name for the figure to be created
  print(FigFileName)
  #pdf(file=FigFileName, height = 3.5, width = 5, onefile=FALSE) # start pdf document to put figure into
  if (image_format=="pdf") {
    pdf(file=FigFileName, height = 3.5, width = 5, onefile=FALSE) # start pdf document to put figure into
  }  else if (image_format=="jpg") {
    jpeg(file=FigFileName) # start jpg document to put figure into
  } else if (image_format == "png"){
    png(file=FigFileName)# , height = 3.5, width = 5, onefile=FALSE)
  } else {stop("invalid option for image file type")}
  
  plot.new() # clear the plot to have a clean canvas to draw on
  par(mar=c(4.2, 3.8, 1, 0.2)) # trim off extra white space (bottom, left, top, right)
  print(plotting_string)
  #print(data_for_plotting)
  #print(colnames(data_for_plotting))
  eval(parse(text = paste("print(",plotting_string,")",sep = ""))) #plot(this_model_output) 
  title(main = title_string)
  dev.off() # stop writing to pdf file
  remove(FigFileName)
} # end of ML_plot_model.fn function

# plot to image file - the part that comes before the actual plotting commands
Plot_to_ImageFile_TopOnly.fn <- function(output.directory, file_sub_label, plot_name_extension, image_format = "jpg") {
  ### plot this_model_run_name
  FigFileName=file.path(output.directory,paste(file_sub_label,"_",plot_name_extension,".",image_format,sep = "")) # define file name for the figure to be created
  print(FigFileName)
  #pdf(file=FigFileName, height = 3.5, width = 5, onefile=FALSE) # start pdf document to put figure into
  if (image_format=="pdf") {
    pdf(file=FigFileName, height = 3.5, width = 5, onefile=FALSE) # start pdf document to put figure into
  }  else if (image_format=="jpg") {
    jpeg(file=FigFileName) # start jpg document to put figure into
  } else if (image_format == "png"){
    png(file=FigFileName)# , height = 3.5, width = 5, onefile=FALSE)
  } else {stop("invalid option for image file type")}
  plot.new() # clear the plot to have a clean canvas to draw on
  par(mar=c(4.2, 3.8, 1, 0.2)) # trim off extra white space (bottom, left, top, right)
  return(FigFileName)
} # end of ML_plot_model.fn function

# plot to image file - the part that comes after the actual plotting commands
Plot_to_ImageFile_BottomOnly.fn <- function(FigFileName = NA, title_string) {
  title(main = title_string)
  dev.off() # stop writing to pdf file
  if (is.na(FigFileName)==FALSE) { # remove name if it's there
  remove(FigFileName)
  } # if (is.na(FigFileName)==FALSE) { # remove name if it's there
} # end of ML_plot_model.fn function

LaTex_code_4_figure.fn <- function(LatexFileName, title_string, file_sub_label, plot_name_extension, output.directory.short, image_format = "jpg") {
  if (sink.number()>0) {sink()} # get stop any lingering sinks
  if (max(dev.cur())>1) { # make sure it isn't outputting to any figure files
    dev.off(which  =  dev.cur())
  } # if (max(dev.cur())>1) { # make sure it isn't outputting to any figure files
  sink(file = LatexFileName, append = TRUE, type = c("output","message"),split = FALSE)
  cat(paste("\n\\begin{figure} \n"))
  cat(paste("\\centering "," \n",sep = ""))
  cat(paste("\\includegraphics[width=0.77\\textwidth]{",output.directory.short,"/",file_sub_label,"_",plot_name_extension,".",image_format,"} \n",sep = "")) 
  #cat(paste("\\includegraphics[width=0.77\\textwidth]{",output.directory.short,"/",file_sub_label,"_",plot_name_extension,".pdf} \n",sep = "")) 
  
  title_string_mod <- replace_character_in_string.fn(input_char = title_string,char2replace = "_",replacement_char = "-") 
  cat(paste("\\caption{\\label{fig:",file_sub_label,plot_name_extension,"}",title_string_mod,"} \n",sep = "")) 
cat(paste("\\end{figure} \n \n"))
sink() # stop writing to latex file
#sink(file =SinkFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting output into SinkFileName
} # end of LaTex_code_4_figuer_function - need to finish

LaTex_code_start_subsection.fn <- function(LatexFileName, title_string, append_option = TRUE) {
  if (sink.number()>0) {sink()} # get stop any lingering sinks
  sink(file = LatexFileName, append = append_option, type = c("output","message"),split = FALSE)
  cat(paste("\n\\subsection{",title_string," Images} \n \n",sep = ""))
  sink() # stop output to file
} # end of LaTex_code_start_subsection function

Plot_and_latex.fn <- function(output.directory, output.directory.short, file_sub_label, plot_name_extension, plotting_string, data_for_plotting, title_string, LatexFileName, SinkFileName, image_format = "jpg") {
  if (sink.number()>0) {sink()} # get stop any lingering sinks
  if (max(dev.cur())>1) { # make sure it isn't outputting to any figure files
    dev.off(which  =  dev.cur())
  } # if (max(dev.cur())>1) { # make sure it isn't outputting to any figure files
  # create plot
  Plot_to_ImageFile.fn(output.directory, file_sub_label, plot_name_extension, plotting_string, data_for_plotting, title_string, image_format = image_format)
  # create LaTex code for plot  
  if (is.na(LatexFileName) == FALSE) { # only output latex code if a file has been specified
    LaTex_code_4_figure.fn(LatexFileName, title_string, file_sub_label, plot_name_extension, output.directory.short, image_format = image_format)
  } else {
    print("No LatexFileName has been specified, LaTex code will not be output for this image")
  }
  # go back to outputing sink to main sink file
  if (is.na(SinkFileName) == FALSE) { # go back to outputing sink to main sink file
    sink(file =SinkFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting output into SinkFileName
  } # if (is.na(SinkFileName) == FALSE) { # go back to outputing sink to main sink file
} # end of Plot_and_latex.fn function

# map geopolitical bounaries
map_base_layer.fn <- function(USMaps.directory, study_states_abbrev) {
  
  # Resources for mapping
  # http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
  
  # map boundaries of western US states
  USmap=readOGR(dsn=file.path(USMaps.directory),layer = "cb_2016_us_state_500k")
  #USmap=readOGR(dsn=file.path(USMaps.directory,"cb_2016_us_state_500k"),layer = "cb_2016_us_state_500k")
  # https://www.census.gov/geo/maps-data/data/tiger-cart-boundary.html 
  # https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html
  
  # have R recognize state FP's as numerical values (in a new column)
  USmap$STATEFP_NUM <- as.numeric(as.character(USmap$STATEFP))
  
  State_Num_vec <- StateAbbrev2StateCode.fn(StateAbbrev_vec = study_states_abbrev)
  #   # display the State FP values and state abbreviations next to each other
  #   USmap@data[,c("STATEFP_NUM","STUSPS")]
  #   
  #   # find the 11 western states included in the study
     #WestUSmap=USmap@data[USmap$STATEFP_NUM==4|USmap$STATEFP_NUM==6|USmap$STATEFP_NUM==8|USmap$STATEFP_NUM==16|USmap$STATEFP_NUM==30|USmap$STATEFP_NUM==32|USmap$STATEFP_NUM==35|USmap$STATEFP_NUM==49|USmap$STATEFP_NUM==56|USmap$STATEFP_NUM==41|USmap$STATEFP_NUM==53|USmap$STATEFP_NUM==38|USmap$STATEFP_NUM==46|USmap$STATEFP_NUM==31|USmap$STATEFP_NUM==20|USmap$STATEFP_NUM==40|USmap$STATEFP_NUM==48,]
     #WestUSmap=USmap@data[USmap$STATEFP_NUM==4|USmap$STATEFP_NUM==6|USmap$STATEFP_NUM==8|USmap$STATEFP_NUM==16|USmap$STATEFP_NUM==30|USmap$STATEFP_NUM==32|USmap$STATEFP_NUM==35|USmap$STATEFP_NUM==41|USmap$STATEFP_NUM==49|USmap$STATEFP_NUM==53|USmap$STATEFP_NUM==56,]
  WestUSmapGeom=USmap[USmap$STATEFP_NUM==4|USmap$STATEFP_NUM==6|USmap$STATEFP_NUM==8|USmap$STATEFP_NUM==16|USmap$STATEFP_NUM==30|USmap$STATEFP_NUM==32|USmap$STATEFP_NUM==35|USmap$STATEFP_NUM==41|USmap$STATEFP_NUM==49|USmap$STATEFP_NUM==53|USmap$STATEFP_NUM==56,]
     #print(WestUSmap)
  
  #Area_interest <- subset_data_frame_via_vector.fn(vector_for_subset = State_Num_vec, full_data_frame = USmap,col_for_subset = c("STATEFP_NUM"))
     plot(WestUSmapGeom)

}

# map geopolitical bounaries
map_county_base_layer.fn <- function(CountyMaps.directory, study_states_abbrev) {
  
  # Resources for mapping
  # http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
  
  # Source for shapefiles:
  # https://www.census.gov/geo/maps-data/data/tiger-cart-boundary.html 
  # https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html
  
  # create map
  Countymap=readOGR(dsn=file.path(CountyMaps.directory),layer = "cb_2017_us_county_500k") # load county map shapefile
  Countymap$STATEFP_NUM <- as.numeric(as.character(Countymap$STATEFP)) # have R recognize state FP's as numerical values (in a new column)
  
  # this map is in NAD83, which can be verified with this command:
  summary(Countymap) # summarize data
  
  State_Num_vec <- StateAbbrev2StateCode.fn(StateAbbrev_vec = study_states_abbrev)
  #   # display the State FP values and state abbreviations next to each other
  #   
  #   # find the 11 western states included in the study
  WestCountymapGeom=Countymap[Countymap$STATEFP_NUM==4|Countymap$STATEFP_NUM==6|Countymap$STATEFP_NUM==8|Countymap$STATEFP_NUM==16|Countymap$STATEFP_NUM==30|Countymap$STATEFP_NUM==32|Countymap$STATEFP_NUM==35|Countymap$STATEFP_NUM==41|Countymap$STATEFP_NUM==49|Countymap$STATEFP_NUM==53|Countymap$STATEFP_NUM==56,]
  #print(WestUSmap)
  
  plot(WestCountymapGeom)
  
  return(WestCountymapGeom)
}

# data frame report
df_report.fn <- function(df, cols_interest, x_axis_var, output.directory, output.directory.short, file_sub_label, title_string_partial, plot_color = "black", LatexFileName, SinkFileName, image_format = "jpg") {
  # df <- PM25_obs_shuffled
  # cols_interest <- predictor_variables
  # x_axis_var <- "Date"
  #title_string_partial <- " Time Series"
  if (sink.number()>0) {sink()} # get stop any lingering sinks
  if (max(dev.cur())>1) { # make sure it isn't outputting to any figure files
    dev.off(which  =  dev.cur())
  } # if (max(dev.cur())>1) { # make sure it isn't outputting to any figure files
  
  for (this_col_i in 1:length(cols_interest)) {
    this_col <- cols_interest[this_col_i]
    #print(this_col)
    plot_name_extension <- paste(this_col,"v",x_axis_var, sep = "")
    plot_name_extension <- replace_character_in_string.fn(input_char = plot_name_extension, char2replace = ".",replacement_char = "") 
    #print(plot_name_extension)
    plotting_string <- paste("plot(x = data_for_plotting[ ,'",x_axis_var,"'], y = data_for_plotting[ ,'",this_col,"'])",sep = "")
    #print(plotting_string)
    title_string <- paste(this_col,title_string_partial,sep = " ")
    plot_name_extension <-  paste(this_col,"TS",sep = "")
    Plot_and_latex.fn(output.directory, output.directory.short, file_sub_label, plot_name_extension = plot_name_extension, plotting_string, data_for_plotting = df, title_string, LatexFileName = LatexFileName, SinkFileName = SinkFileName, image_format = image_format) 
    while (sink.number()>0) {
      sink()
    } # while (sink.number()>0) {
    sink.number()
  }
  
} # end of df_report.fn function
  
  