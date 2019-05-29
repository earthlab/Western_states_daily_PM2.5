# functions for plotting and puting plots in LaTex

# plot model from ML run; was Plot_and_latex.fn
Plot_to_ImageFile.fn <- function(output.directory, file_sub_label, plot_name_extension, plotting_string, data_for_plotting, title_string, image_format = "jpg") {
  ### plot this_model_run_name
  #FigFileName=file.path(output.directory,paste(file_sub_label,"_",plot_name_extension,".pdf",sep = "")) # define file name for the figure to be created
  FigFileName=file.path(output.directory,paste(file_sub_label,"_",plot_name_extension,".",image_format,sep = "")) # define file name for the figure to be created
  #print(FigFileName)
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
  eval(parse(text = paste("print(",plotting_string,")",sep = ""))) #plot(this_model_output) 
  title(main = title_string)
  dev.off() # stop writing to pdf file
  remove(FigFileName)
} # end of ML_plot_model.fn function

# plot to image file - the part that comes before the actual plotting commands
Plot_to_ImageFile_TopOnly.fn <- function(output.directory, file_sub_label, plot_name_extension, image_format = "jpg") {
  ### plot this_model_run_name
  FigFileName=file.path(output.directory,paste(file_sub_label,"_",plot_name_extension,".",image_format,sep = "")) # define file name for the figure to be created
  #print(FigFileName)
  #pdf(file=FigFileName, height = 3.5, width = 5, onefile=FALSE) # start pdf document to put figure into
  if (image_format=="pdf") {
    #pdf(file=FigFileName, height = 3.5, width = 5, onefile=FALSE) # start pdf document to put figure into
    pdf(file=FigFileName, height = 10, width = 10, onefile=FALSE) # start pdf document to put figure into
    
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

LaTex_code_4_figure.fn <- function(LatexFileName, title_string, file_sub_label, plot_name_extension, output.directory.short, image_format = "jpg", ClearPage = FALSE, fig_caption = title_string) {
  if (sink.number()>0) {sink()} # get stop any lingering sinks
  if (max(dev.cur())>1) { # make sure it isn't outputting to any figure files
    dev.off(which  =  dev.cur())
  } # if (max(dev.cur())>1) { # make sure it isn't outputting to any figure files
  sink(file = LatexFileName, append = TRUE, type = c("output","message"),split = FALSE)
  if (ClearPage == TRUE) { # check if ClearPage is set to true
    cat(paste("\n\\clearpage \n",sep = "")) # latex gets confused if there are too many figures consecutively. Putting in an occasional clearpage command helps with this.
  } # if (ClearPage == TRUE) { # check if ClearPage is set to true
  cat(paste("\n\\begin{figure} \n"))
  cat(paste("\\centering "," \n",sep = ""))
  #cat(paste("\\includegraphics[width=0.77\\textwidth]{",output.directory.short,"/",file_sub_label,"_",plot_name_extension,".",image_format,"} \n",sep = "")) 
  cat(paste("\\includegraphics[width=0.77\\textwidth]{",define_file_paths.fn("output.directory.short"),"/",file_sub_label,"_",plot_name_extension,".",image_format,"} \n",sep = "")) 
  
  #cat(paste("\\includegraphics[width=0.77\\textwidth]{",output.directory.short,"/",file_sub_label,"_",plot_name_extension,".pdf} \n",sep = "")) 
  
  title_string_mod <- replace_character_in_string.fn(input_char = title_string,char2replace = "_",replacement_char = "-") 
  #cat(paste("\\caption{\\label{fig:",file_sub_label,plot_name_extension,"}",title_string_mod,"} \n",sep = "")) 
  cat(paste("\\caption{\\label{fig:",file_sub_label,plot_name_extension,"}",fig_caption,"} \n",sep = "")) 
  
  cat(paste("\\end{figure} \n \n"))
sink() # stop writing to latex file
#sink(file =SinkFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting output into SinkFileName
} # end of LaTex_code_4_figure_function - need to finish

LaTex_code_start_subsection.fn <- function(LatexFileName, title_string, append_option = TRUE) {
  if (sink.number()>0) {sink()} # get stop any lingering sinks
  sink(file = LatexFileName, append = append_option, type = c("output","message"),split = FALSE)
  cat(paste("\n\\subsection{",title_string," Images} \n \n",sep = ""))
  sink() # stop output to file
} # end of LaTex_code_start_subsection function

LaTex_code_Note_in_text.fn <- function(LatexFileName, note_text, append_option = TRUE) {
  if (sink.number()>0) {sink()} # get stop any lingering sinks
  sink(file = LatexFileName, append = append_option, type = c("output","message"),split = FALSE)
  cat(paste("\n",note_text," \n \n",sep = ""))
  sink() # stop output to file
} # end of LaTex_code_start_subsection function

LaTex_code_start_subsubsection.fn <- function(LatexFileName, title_string, append_option = TRUE) {
  if (sink.number()>0) {sink()} # get stop any lingering sinks
  sink(file = LatexFileName, append = append_option, type = c("output","message"),split = FALSE)
  cat(paste("\n\\subsubsection{",title_string," Images} \n \n",sep = ""))
  sink() # stop output to file
} # end of LaTex_code_start_subsection function

LaTex_code_start_section.fn <- function(LatexFileName, title_string, append_option = TRUE) {
  if (sink.number()>0) {sink()} # get stop any lingering sinks
  sink(file = LatexFileName, append = append_option, type = c("output","message"),split = FALSE)
  cat(paste("\n\\section{",title_string,"} \n \n",sep = ""))
  sink() # stop output to file
} # end of LaTex_code_start_subsection function

Plot_and_latex.fn <- function(output.directory, output.directory.short, file_sub_label, plot_name_extension, plotting_string, data_for_plotting, title_string, LatexFileName, SinkFileName, image_format = "jpg", ClearPage = FALSE, fig_caption = title_string) {
  if (sink.number()>0) {sink()} # get stop any lingering sinks
  if (max(dev.cur())>1) { # make sure it isn't outputting to any figure files
    dev.off(which  =  dev.cur())
  } # if (max(dev.cur())>1) { # make sure it isn't outputting to any figure files
  # create plot
  Plot_to_ImageFile.fn(output.directory, file_sub_label, plot_name_extension, plotting_string, data_for_plotting, title_string, image_format = image_format)
  # create LaTex code for plot  
  if (is.na(LatexFileName) == FALSE) { # only output latex code if a file has been specified
    LaTex_code_4_figure.fn(LatexFileName = LatexFileName, title_string = title_string, file_sub_label = file_sub_label, plot_name_extension = plot_name_extension, output.directory.short = output.directory.short, image_format = image_format, ClearPage = ClearPage, fig_caption = fig_caption)
  } else {
    print("No LatexFileName has been specified, LaTex code will not be output for this image")
  }
  # go back to outputing sink to main sink file
  if (is.na(SinkFileName) == FALSE) { # go back to outputing sink to main sink file
    sink(file =SinkFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting output into SinkFileName
  } # if (is.na(SinkFileName) == FALSE) { # go back to outputing sink to main sink file
} # end of Plot_and_latex.fn function

# load state boundary shape file and cut it down to the study area
load_State_Boundaries.fn <- function(USMaps.directory, study_states_abbrev) {
  # map boundaries of western US states
  USmap=readOGR(dsn=file.path(USMaps.directory),layer = "cb_2016_us_state_500k")
  # https://www.census.gov/geo/maps-data/data/tiger-cart-boundary.html 
  # https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html
  
  # have R recognize state FP's as numerical values (in a new column)
  USmap$STATEFP_NUM <- as.numeric(as.character(USmap$STATEFP))
  # find the 11 western states included in the study
  WestUSmapGeom=USmap[USmap$STATEFP_NUM==4|USmap$STATEFP_NUM==6|USmap$STATEFP_NUM==8|USmap$STATEFP_NUM==16|USmap$STATEFP_NUM==30|USmap$STATEFP_NUM==32|USmap$STATEFP_NUM==35|USmap$STATEFP_NUM==41|USmap$STATEFP_NUM==49|USmap$STATEFP_NUM==53|USmap$STATEFP_NUM==56,]
  #print(WestUSmap)  
  return(WestUSmapGeom)
} # end of load_State_Boundaries.fn function

# map geopolitical boundaries
map_base_layer.fn <- function(USMaps.directory, study_states_abbrev) {
  # Resources for mapping
  # http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
  library(rgdal)
  WestUSmapGeom <- load_State_Boundaries.fn(USMaps.directory, study_states_abbrev)
  plot(WestUSmapGeom)
} # end of map_base_layer.fn function

# load state boundary shape file and cut it down to the study area
load_County_Boundaries.fn <- function(USMaps.directory, study_states_abbrev) {
  # Source for shapefiles:
  # https://www.census.gov/geo/maps-data/data/tiger-cart-boundary.html 
  # https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html
  # create map
  Countymap=readOGR(dsn=file.path(define_file_paths.fn("CountyMaps.directory")),layer = "cb_2017_us_county_500k") # load county map shapefile
  Countymap$STATEFP_NUM <- as.numeric(as.character(Countymap$STATEFP)) # have R recognize state FP's as numerical values (in a new column)
  
  # this map is in NAD83, which can be verified with this command:
  #summary(Countymap) # summarize data
  #State_Num_vec <- StateAbbrev2StateCode.fn(StateAbbrev_vec = study_states_abbrev) # display the State FP values and state abbreviations next to each other
  #   # find the 11 western states included in the study
  WestCountymapGeom <- Countymap[Countymap$STATEFP_NUM==4|Countymap$STATEFP_NUM==6|Countymap$STATEFP_NUM==8|Countymap$STATEFP_NUM==16|Countymap$STATEFP_NUM==30|Countymap$STATEFP_NUM==32|Countymap$STATEFP_NUM==35|Countymap$STATEFP_NUM==41|Countymap$STATEFP_NUM==49|Countymap$STATEFP_NUM==53|Countymap$STATEFP_NUM==56,]
  return(WestCountymapGeom)
} # end of load_County_Boundaries.fn function

# map geopolitical boundaries
map_county_base_layer.fn <- function(CountyMaps.directory, study_states_abbrev) {
  library(rgdal)
  # Resources for mapping
  # http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
  WestCountymapGeom <- load_County_Boundaries.fn(USMaps.directory, study_states_abbrev)
    #print(WestUSmap)
  plot(WestCountymapGeom)
  return(WestCountymapGeom)
}

# data frame report
df_report.fn <- function(df_in, cols_interest, x_axis_var, output.directory, output.directory.short, file_sub_label, title_string_partial, plot_color = "black", LatexFileName, SinkFileName, image_format = "jpg", fig_caption = title_string_partial) {
#  df_report.fn <- function(df, cols_interest, x_axis_var, output.directory, output.directory.short, file_sub_label, title_string_partial, plot_color = "black", LatexFileName, SinkFileName, image_format = "jpg", fig_caption = title_string_partial) {
    
  # df <- PM25_obs_shuffled
  # cols_interest <- predictor_variables
  # x_axis_var <- "Date"
  #title_string_partial <- " Time Series"
  if (sink.number()>0) {sink()} # get stop any lingering sinks
  if (max(dev.cur())>1) { # make sure it isn't outputting to any figure files
    dev.off(which  =  dev.cur())
  } # if (max(dev.cur())>1) { # make sure it isn't outputting to any figure files
  
  for (this_col_i in 1:length(cols_interest)) {
    if (this_col_i%%10==0) { # check for multiples of 10, if so, put in a clearpage command. Latex gets confused if there are too many consecutive figures, so an occasional clearpage command helps with this.
      ClearPage <- TRUE
    } else {
      ClearPage <- FALSE
    }
    this_df <- df_in
    this_col <- cols_interest[this_col_i]
    x_label <- replace_character_in_string.fn(input_char = x_axis_var, char2replace = "_",replacement_char = " ")
    y_label <- replace_character_in_string.fn(input_char = this_col, char2replace = "_",replacement_char = " ")
    #plotting_string <- paste("plot(x = data_for_plotting[ ,'",x_axis_var,"'], y = data_for_plotting[ ,'",this_col,"'], xlab = '",x_label,"', ylab = '",y_label,"')",sep = "")
    plotting_string <- paste("plot(x = data_for_plotting[ ,'",x_axis_var,"'], y = data_for_plotting[ ,'",this_col,"'], xlab = '",x_label,"', ylab = '",y_label,"', col = '",plot_color,"')",sep = "")
    
    title_string <- paste(y_label,title_string_partial,sep = " ")
    plot_name_extension <- paste(this_col,"v",x_axis_var, sep = "")
    plot_name_extension_mod <- replace_character_in_string.fn(input_char = plot_name_extension, char2replace = ".",replacement_char = "") 
    # remove rows of data with NA in column of interest
    which_na_x_var <- which(is.na(this_df[ ,x_axis_var])) # which rows have NA in the x-axis variable?
    if (length(which_na_x_var)>0) { # remove rows with NA in the x-axis variable
      which_not_na_x_var <- which(!is.na(this_df[ ,x_axis_var]))
      this_df <- this_df[which_not_na_x_var, ]
      rm(which_not_na_x_var)
    } # if (length(which_na_x_var)>0) { # remove rows with NA in the x-axis variable
    rm(which_na_x_var)
    which_na_y_var <- which(is.na(this_df[ ,this_col]))
    if (length(which_na_y_var)>0) { # remove the rows with NA in the y-axis variable
      which_not_na_y_var<- which(!is.na(this_df[ ,this_col]))
      this_df <- this_df[which_not_na_y_var, ]
      rm(which_not_na_y_var)
    }
    rm(which_na_y_var)
    if (dim(this_df)[1] > 0) { # only plot data if there actually are data values in this_df
    Plot_and_latex.fn(output.directory = output.directory, output.directory.short = output.directory.short, file_sub_label = file_sub_label, plot_name_extension = plot_name_extension_mod, plotting_string = plotting_string, data_for_plotting = this_df, title_string = title_string, LatexFileName = LatexFileName, SinkFileName = SinkFileName, image_format = image_format, ClearPage = ClearPage, fig_caption = fig_caption) 
    } # if (dim(this_df)[1] > 0) { # only plot data if there actually are data values in this_df
    while (sink.number()>0) {
      sink()
    } # while (sink.number()>0) {
    sink.number()
  } # for (this_col_i in 1:length(cols_interest))
  
} # end of df_report.fn function
  
# data frame report - map values for a subset of days
df_map_subset_days.fn <- function(this_df, cols_interest, dates_of_interest, output.directory, output.directory.short, file_sub_label, title_string_partial, plot_color = "black", LatexFileName, SinkFileName, image_format = "jpg",study_states_abbrev,this_datum) {
  print("line 217, inside df_map_subset_days.fn")
  print("LatexFileName")
  print(LatexFileName)
  if (sink.number()>0) {sink()} # get stop any lingering sinks
  if (max(dev.cur())>1) { # make sure it isn't outputting to any figure files
    dev.off(which  =  dev.cur())
  } # if (max(dev.cur())>1) { # make sure it isn't outputting to any figure files
  plot_counter <- 1
  for (this_col_i in 1:length(cols_interest)) { # cycle through and plot the columns of interest
    this_col <- cols_interest[this_col_i] # 
    print(this_col)
    if (this_col == "PM2.5_Obs") {
      Cut_points_set <- FALSE
      color_cut_points <- NA
      color_vec <- NA
    } else {
      Cut_points_set <- TRUE
      color_cut_points <- as.vector(c(quantile(this_df[ , this_col], na.rm = TRUE)))
      color_vec = c("darkolivegreen1","forestgreen","deepskyblue","dodgerblue3","darkorchid")
    }
    for (date_counter in 1:length(dates_of_interest)) { # cycle through dates of interest to make plots
      if (plot_counter%%10==0) { # check for multiples of 10, if so, put in a clearpage command. Latex gets confused if there are too many consecutive figures, so an occasional clearpage command helps with this.
        ClearPage <- TRUE
      } else {
        ClearPage <- FALSE
      } # if (this_col_i%%10==0) { # check for multiples of 10, if so, put in a clearpage command.
      date_i <- as.Date(dates_of_interest[date_counter],"%Y-%m-%d")
      which_this_day <- which(this_df$Date == date_i) # isolate the data for the date of interest
      This_day <- this_df[which_this_day, ]
      # plot map of data for this day
      plot_name_extension <-  paste("MapObs",replace_character_in_string.fn(this_col,char2replace = ".",replacement_char = ""),date_i,sep = "")
      #title_string <- paste(this_col,date_i,sep = " ") # used in figure titles, etc
      title_string <- paste(replace_character_in_string.fn(this_col,char2replace = "_",replacement_char = " "),date_i,sep = " ") # used in figure titles, etc
      if (this_col == "PM2.5_Obs") {
        cut_point_scale <- "PM2.5_Obs"
      } else {
        cut_point_scale <- "Other"
      }
      map_point_values.fn(this_df = This_day, var_interest = this_col, cut_point_scale = cut_point_scale, 
                          output.directory = output.directory, file_sub_label = file_sub_label, plot_name_extension = plot_name_extension, 
                          study_states_abbrev = study_states_abbrev, this_datum = this_datum, title_string = title_string, ClearPage = ClearPage, 
                          Cut_points_set = Cut_points_set, color_cut_points = color_cut_points, color_vec = color_vec, LatexFileName = LatexFileName) # plot points of observations on map and color points by concentration
      plot_counter <- plot_counter+1
    } # for (date_i in dates_of_interest) { # cycle through dates of interest to make plots
  } # for (this_col_i in 1:length(cols_interest)) { # cycle through and plot the columns of interest
} # end of df_map_subset_days.fn function

#data frame report - map values for a subset of days
df_map_monthly_summary.fn <- function(this_df, cols_interest, output.directory, output.directory.short, file_sub_label, title_string_partial, plot_color = "black", LatexFileName, SinkFileName, image_format = "jpg",study_states_abbrev,this_datum) {
  # this_df <- Full_PM25_obs
  # cols_interest = c(col_name_interest, predictor_variables[4:length(predictor_variables)])
  if (sink.number()>0) {sink()} # get stop any lingering sinks
  if (max(dev.cur())>1) { # make sure it isn't outputting to any figure files
    dev.off(which  =  dev.cur())
  } # if (max(dev.cur())>1) { # make sure it isn't outputting to any figure files
  plot_counter <- 1
  for (this_col_i in 1:length(cols_interest)) { # cycle through and plot the columns of interest
    this_col <- cols_interest[this_col_i] #
    print(this_col)
    if (this_col == "PM2.5_Obs") {
      Cut_points_set <- FALSE
      color_cut_points <- NA
      color_vec <- NA
    } else {
      Cut_points_set <- TRUE
      color_cut_points <- as.vector(c(quantile(this_df[ , this_col], na.rm = TRUE)))
      color_vec = c("darkolivegreen1","forestgreen","deepskyblue","dodgerblue3","darkorchid")
    }
    for (this_month in 1:12) { # cycle through dates of interest to make plots
      if (plot_counter%%10==0) { # check for multiples of 10, if so, put in a clearpage command. Latex gets confused if there are too many consecutive figures, so an occasional clearpage command helps with this.
        ClearPage <- TRUE
      } else {
        ClearPage <- FALSE
      } # if (this_col_i%%10==0) { # check for multiples of 10, if so, put in a clearpage command.
      # isolate the data of interest and summarize
      summary_value = "median"
      this_monthly_map_summary <- monthly_map_summary_all_yrs.fn(this_month = this_month, this_df = this_df, summary_value = summary_value, var_interest = this_col)
      if (max(!is.na(this_monthly_map_summary)) == 1) { # only try to plot if there's data for this month
      # plot map of data for this day
      plot_name_extension <-  paste("MapObsMo",this_month,replace_character_in_string.fn(this_col,char2replace = ".",replacement_char = ""),sep = "")
      title_string <- paste(replace_character_in_string.fn(this_col,char2replace = "_",replacement_char = " "),"Month",this_month,sep = " ") # used in figure titles, etc
      #map_point_values.fn(this_df = this_monthly_map_summary, var_interest = this_col, output.directory = output.directory, file_sub_label = file_sub_label, plot_name_extension = plot_name_extension, study_states_abbrev = study_states_abbrev, this_datum = this_datum, title_string = title_string, ClearPage = ClearPage) # plot points of observations on map and color points by concentration
      map_point_values.fn(this_df = this_monthly_map_summary, var_interest = summary_value, 
                          cut_point_scale = this_col, output.directory = output.directory, 
                          file_sub_label = file_sub_label, plot_name_extension = plot_name_extension, 
                          study_states_abbrev = study_states_abbrev, this_datum = this_datum, title_string = title_string, 
                          ClearPage = ClearPage, Cut_points_set = Cut_points_set, color_cut_points = color_cut_points, color_vec = color_vec, LatexFileName = LatexFileName) # plot points of observations on map and color points by concentration
      plot_counter <- plot_counter+1
      } # if (!is.na(this_monthly_map_summary)) { # only try to plot if there's data for this month
    } # for (date_i in dates_of_interest) { # cycle through dates of interest to make plots
  } # for (this_col_i in 1:length(cols_interest)) { # cycle through and plot the columns of interest
} # end of df_map_subset_days.fn function

# isolate the data of interest and summarize
monthly_map_summary_all_yrs.fn <- function(this_month, this_df, summary_value = "median", var_interest = "PM2.5_Obs") {
  this_df$AddMonth <- input_mat_extract_month_from_date.fn(this_df$Date) # add column with just month number
  which_this_month <- which(this_df$AddMonth == this_month) # which rows of data are for this month?
 if (length(which_this_month)>0) { # only try to plot if there is data for this month
  this_month_data <- this_df[which_this_month, ] # isolate data from this month
  rm(which_this_month) # clear variable
  all_locations <- unique(this_month_data[ ,c("Latitude","Longitude")]) # find unique locations for this month
  conc_summaries <- unlist(lapply(1:dim(all_locations)[1], function(x){
    this_lat <- all_locations[x,c("Latitude")] # what is the latitude for this iteration?
    this_lon <- all_locations[x,c("Longitude")] # what is the longitude for this iteration
    which_this_loc <- which(this_month_data$Latitude == this_lat & this_month_data$Longitude == this_lon) # what rows of this data are for this location?
    this_mo_loc <- this_month_data[which_this_loc,] # isolate the data for this location within this month
      if (summary_value == "median") { # create summary value
      this_mo_loc_summary <- median(this_mo_loc[ , var_interest], na.rm = TRUE) # median as summary
      } else {stop("write more code for other summary metrics")} # possibility to expand code for other summary values such as mean, min, max, etc.
    return(this_mo_loc_summary) # return variable from function
  }))# end of unlist(lapply(...))
  this_monthly_map_summary <- data.frame(matrix(NA,nrow=dim(all_locations)[1],ncol=3)) # create data frame
  names(this_monthly_map_summary) <- c("Latitude","Longitude",summary_value) # assign the header to input_mat1
  this_monthly_map_summary$Latitude <- all_locations$Latitude
  this_monthly_map_summary$Longitude <- all_locations$Longitude
  this_monthly_map_summary[ , summary_value] <- conc_summaries
 } else { # if (length(which_this_month)>0) # only try to plot if there is data for this month
   this_monthly_map_summary <- NA
 } # if (length(which_this_month)>0) # only try to plot if there is data for this month
  return(this_monthly_map_summary)
} # end of monthly_map_summary.fn function

cut_point_legend_text.fn <- function(color_cut_points) {
  legend_text <- unlist(lapply(1:length(color_cut_points), function(x){
    if (x == length(color_cut_points)) {
      this_legend_text <- paste("[",as.character(color_cut_points[x]),"-Inf)",sep = "")
    } else { # if
      this_legend_text <- paste("[",as.character(color_cut_points[x]),"-",as.character(color_cut_points[x+1]),")",sep = "")
    } # if
  }))#,color_cut_points))# end of lapply (legend_text)
  return(legend_text)
} # end of cut_point_legend_text.fn

map_point_values.fn <- function(this_df, var_interest, cut_point_scale = "PM2.5_Obs", output.directory, file_sub_label, plot_name_extension = plot_name_extension, study_states_abbrev,this_datum, title_string, ClearPage = FALSE, Cut_points_set = FALSE, color_cut_points = NA, color_vec = NA, LatexFileName) { # plot points of observations on map and color points by concentration
  FigFileName <- Plot_to_ImageFile_TopOnly.fn(output.directory, file_sub_label, plot_name_extension = plot_name_extension) # start image file
  # create map of counties
  WestCountymapGeom <- map_county_base_layer.fn(CountyMaps.directory, study_states_abbrev)
  # color list: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
  #if (var_interest == "PM2.5_Obs") {
  if (Cut_points_set == FALSE) {
    if (cut_point_scale == "PM2.5_Obs") {
    color_cut_points <-  c(0, 12.1, 35.5, 55.5, 150.5, 250.5, 350.5)
    color_vec = c("green", "yellow", "orange", "red", "hotpink2", "hotpink3", "hotpink4")
    #} else if (is.numeric(cut_point_scale)) {
    #  color_cut_points <-  cut_point_scale
    } else {
    color_cut_points <- as.vector(c(quantile(this_df[ , var_interest], na.rm = TRUE)))
    #color_vec = c("darkolivegreen1","darkolivegreen2","darkolivegreen3","darkolivegreen4","darkolivegreen")
    #color_vec = c("darkorchid","dodgerblue3","deepskyblue","forestgreen","darkolivegreen1")
    color_vec = c("darkolivegreen1","forestgreen","deepskyblue","dodgerblue3","darkorchid")
    } 
  } # if (Cut_points_set == FALSE) {
  #this_df <- color_by_conc.fn(this_df = this_df,color_cut_points = c(0, 12.1, 35.5, 55.5, 150.5, 250.5, 350.5), color_vec = c("green", "yellow", "orange", "red", "hotpink2", "hotpink3", "hotpink4"))
  #this_df <- color_by_conc.fn(this_df = this_df,color_cut_points = color_cut_points, color_vec = color_vec)
  this_df <- color_by_conc.fn(this_df = this_df,var_interest = var_interest,color_cut_points = color_cut_points, color_vec = color_vec)
  
  points(this_df$Longitude,this_df$Latitude,pch = 19,col=this_df$PlotColor)#, 
         #ylim = c(min(this_df$Latitude,na.rm = TRUE),(max(this_df$Latitude)+0.5*max(this_df$Latitude)+0.5), na.rm = TRUE))
         #ylim = c(max(this_df$Latitude,na.rm = TRUE),(max(this_df$Latitude)+0.5*max(this_df$Latitude)+0.5), na.rm = TRUE)) # http://www.milanor.net/blog/maps-in-r-plotting-data-points-on-a-map/
  #points(x = min(this_df$Latitude, na.rm = TRUE), y = max(this_df$Longitude, na.rm = TRUE), pch = 15, col = 631)
  legend_text <- cut_point_legend_text.fn(color_cut_points) 
  #y_placement <- min(this_df$Latitude, na.rm = TRUE)-0.5*mean(c(min(this_df$Latitude, na.rm = TRUE),max(this_df$Latitude, na.rm = TRUE)))
  #y_placement <- mean(c(min(this_df$Latitude, na.rm = TRUE),max(this_df$Latitude, na.rm = TRUE)))
  y_mean <- mean(c(min(this_df$Latitude, na.rm = TRUE),max(this_df$Latitude, na.rm = TRUE)))
  y_placement <- y_mean-0.1*y_mean
  x_placement <- (min(this_df$Longitude)+0.018*min(this_df$Longitude))
  legend(y = y_placement, x = x_placement, legend = legend_text, col = color_vec, pch = 19, bty = "n")#, xpd = TRUE)
  # legend(x = max(this_df$Latitude), y = NULL, legend = legend_text, fill = NULL, col = color_vec,#par("col"),
  #        border = "black", lty, lwd, pch,
  #        angle = 45, density = NULL, bty = "o", bg = par("bg"),
  #        box.lwd = par("lwd"), box.lty = par("lty"), box.col = par("fg"),
  #        pt.bg = NA, cex = 1, pt.cex = cex, pt.lwd = lwd,
  #        xjust = 0, yjust = 1, x.intersp = 1, y.intersp = 1,
  #        adj = c(0, 0.5), text.width = NULL, text.col = par("col"),
  #        text.font = NULL, merge = do.lines && has.pch, trace = FALSE,
  #        plot = TRUE, ncol = 1, horiz = FALSE, title = NULL,
  #        inset = 0, xpd, title.col = text.col, title.adj = 0.5,
  #        seg.len = 2)
  Plot_to_ImageFile_BottomOnly.fn(FigFileName = FigFileName, title_string = title_string) # finish image file
  
  #LaTex_code_4_figure.fn(LatexFileName = LatexFileName, title_string = title_string, file_sub_label = file_sub_label, plot_name_extension = plot_name_extension, output.directory.short = output.directory.short)
  #stop("finish code related to ClearPage")
  LaTex_code_4_figure.fn(LatexFileName = LatexFileName, title_string = title_string, file_sub_label = file_sub_label, plot_name_extension = plot_name_extension, output.directory.short = output.directory.short, image_format = "jpg", ClearPage = ClearPage)
} # end of map_point_values.fn function

map_data_locations.fn <- function(this_df, var_interest, Latitude_var_name = "Latitude", Longitude_var_name = "Longitude", point_color = "blue", point_symbol = 19, output.directory, file_sub_label, plot_name_extension = plot_name_extension, study_states_abbrev, title_string, ClearPage = FALSE, Cut_points_set = FALSE, color_cut_points = NA, color_vec = NA, LatexFileName, fig_caption = title_string) { # plot points of observations on map
  FigFileName <- Plot_to_ImageFile_TopOnly.fn(output.directory, file_sub_label, plot_name_extension = plot_name_extension) # start image file
  # create map of counties
  WestCountymapGeom <- map_county_base_layer.fn(define_file_paths.fn("CountyMaps.directory"), study_states_abbrev)
  points(this_df[ ,c(Longitude_var_name)],this_df[ ,c(Latitude_var_name)],pch = point_symbol, col=point_color)#, 
  #legend_text <- cut_point_legend_text.fn(color_cut_points) 
  #y_mean <- mean(c(min(this_df$Latitude, na.rm = TRUE),max(this_df$Latitude, na.rm = TRUE)))
  #y_placement <- y_mean-0.1*y_mean
  #x_placement <- (min(this_df$Longitude)+0.018*min(this_df$Longitude))
  #legend(y = y_placement, x = x_placement, legend = legend_text, col = color_vec, pch = 19, bty = "n")#, xpd = TRUE)
  Plot_to_ImageFile_BottomOnly.fn(FigFileName = FigFileName, title_string = title_string) # finish image file
  LaTex_code_4_figure.fn(LatexFileName = LatexFileName, title_string = title_string, file_sub_label = file_sub_label, plot_name_extension = plot_name_extension, output.directory.short = output.directory.short, image_format = "jpg", ClearPage = ClearPage, fig_caption = fig_caption)
} # end of map_data_locations.fn function

map_data_locations_by_set.fn <- function(this_df_list, legend_list, color_list, symbol_list, symbol_size_list, Latitude_var_name, Longitude_var_name,  output.directory, file_sub_label, plot_name_extension, study_states_abbrev, title_string, ClearPage = FALSE, LatexFileName, fig_caption = title_string) { # plot points of observations on map and color points by concentration
  FigFileName <- Plot_to_ImageFile_TopOnly.fn(output.directory, file_sub_label, plot_name_extension = plot_name_extension) # start image file
  WestCountymapGeom <- map_county_base_layer.fn(define_file_paths.fn("CountyMaps.directory"), study_states_abbrev) # create map of counties
  n_data_sets <- length(this_df_list) # how many data sets are to be plotted?
  for (this_set in 1:n_data_sets) { # cycle through data sets to plot them
    points(this_df_list[[this_set]][ ,c(Longitude_var_name)],this_df_list[[this_set]][ ,c(Latitude_var_name)],pch = symbol_list[[this_set]], col=color_list[[this_set]], cex = symbol_size_list[[this_set]]) # plot data set on map
  } # cycle through data sets to plot them
  legend_text <- unlist(legend_list) # define legend
  legend(x = "bottomleft", legend = legend_text, col = unlist(color_list), pch = unlist(symbol_list), bty = "n") # plot legend on text
  Plot_to_ImageFile_BottomOnly.fn(FigFileName = FigFileName, title_string = title_string) # finish image file
  LaTex_code_4_figure.fn(LatexFileName = LatexFileName, title_string = title_string, file_sub_label = file_sub_label, plot_name_extension = plot_name_extension, output.directory.short = output.directory.short, image_format = "jpg", ClearPage = ClearPage, fig_caption = fig_caption) # write latex code for this image
} # end of map_data_locations.fn function

top_bottom_dates.fn <- function(Full_PM25_obs) { # find the days with the overall highest and lowest max concentrations
  print("edit top_bottom_dates.fn so that the highest and lowest dates for any variable could be chosen, not just PM2.5")
  print("also edit so the name of the Date column could be specified to be something other than just 'Date'")
  Full_PM25_obs$Date <- as.Date(Full_PM25_obs$Date)
  date_vec <- sort(unique(Full_PM25_obs$Date))
  max_list <- unlist(lapply(date_vec,function(x){
    which_this_day <- which(Full_PM25_obs$Date == x)
    This_day <- Full_PM25_obs[which_this_day, ]
    if (dim(This_day)[1]>50) {# There must be at least 50 days to calculate max_conc 
    max_conc <- max(This_day$PM2.5_Obs)
    } else { # not at least 50 data points
    max_conc <- NA  
    }
    return(max_conc)
  })) # max_list
  # Create input_mat1 data frame
  date_max_conc <- data.frame(matrix(NA,nrow=length(date_vec),ncol=2)) # create data frame for input_mat1
  names(date_max_conc) <- c("Date","MaxConc") # assign the header to input_mat1
  #date_max_conc$Date <- as.Date(date_max_conc$Date,"%Y-%m-%d") # recognize dates as dates: 'Date_Local' 
  #date_max_conc <- cbind(date_vec,max_list)
  date_max_conc$Date <- date_vec
  date_max_conc$MaxConc <- max_list
  # remove rows with NA
  which_not_NA <- which(!is.na(date_max_conc$MaxConc))
  date_max_conc2 <- date_max_conc[which_not_NA, ]
  #Ordered_data_max <- date_max_conc[order(date_max_conc$MaxConc),]
  Ordered_data_max <- date_max_conc2[order(date_max_conc2$MaxConc),]
  highest_3_dates <- Ordered_data_max[(dim(Ordered_data_max)[1]:(dim(Ordered_data_max)[1]-2)),c("Date")]
  lowest_3_dates <- Ordered_data_max[1:3,c("Date")]
  dates_of_interest <- c(highest_3_dates,lowest_3_dates)
  return(dates_of_interest) # output from function
} # end of top_bottom_dates.fn function

color_by_conc.fn <- function(this_df,var_interest,color_cut_points, color_vec) {
  #color_by_conc.fn <- function(this_df,var_interest,color_cut_points = c(0, 12.1, 35.5, 55.5, 150.5, 250.5, 350.5), color_vec = c("green", "yellow", "orange", "red", "hotpink2", "hotpink3", "hotpink4")) {
    #unlist(lapply(color_cut_points, function(x,color_vec, this_df){
  for (i in 1:length(color_cut_points)) {
  #for (x in color_cut_points) {
    #lb <- x
    lb <- color_cut_points[i]
    #which_lb <- which(color_cut_points==lb)
    #which_lb <- i
    #if (which_lb != color_cut_points[length(color_cut_points)]) { # all but the last value fo color_cut_points
    if (i != length(color_cut_points)) { # all but the last value fo color_cut_points
    #ub <- color_cut_points[which_lb+1]
    ub <- color_cut_points[i+1]
    #which_in_range <- which(this_df$PM2.5_Obs>=lb & this_df$PM2.5_Obs < ub)
    which_in_range <- which(this_df[ ,var_interest]>=lb & this_df[,var_interest] < ub)
    } else {
      #which_in_range <- which(this_df$PM2.5_Obs>=lb)
      which_in_range <- which(this_df[ , var_interest] >=lb)
    } # if (which_lb != color_cut_points[length(color_cut_points)]) { # all but the last value fo color_cut_points
    #this_df[which_in_range, c("PlotColor")] <- color_vec[which_lb]  
    this_df[which_in_range, c("PlotColor")] <- color_vec[i]
  #  return(this_df)
  #},color_vec, this_df))
  } # for
  return(this_df)
} # end of color_by_conc.fn function

# create full suite of plots for a data frame
large_df_report.fn <- function(df_in, file_sub_label, title_string_starter, col_name_interest, predictor_variables, non_meta_predictors, dynamic_predictors) {#,this_source_file, data_descriptor, col_name_interest, predictor_variables, list_dates_interest = NA) {
  print("create tex file for report")
  Top_latex_file_name <- file.path(define_file_paths.fn("output.directory"),"Report_top_template.tex")
  Main_latex_file_name <- file.path(define_file_paths.fn("output.directory"),paste("Rgenerated_",file_sub_label,"_main.tex",sep = ""))
  Bottom_latex_file_name <- file.path(define_file_paths.fn("output.directory"),"Report_bottom_template.tex")
  Report_latex_file_name <- file.path(define_file_paths.fn("output.directory"),paste("Rgenerated_",file_sub_label,".tex",sep = ""))
  
  # plot predictor_variables against date
  print("plot predictor variables against date")
  if (sink.number()>0) {sink()} # get stop any lingering sinks
  sink(file = Main_latex_file_name, append = FALSE, type = c("output","message"),split = FALSE)
  cat(paste("\n% Time series for each predictor variable ",sep = ""))
  cat(paste("\n\\input{Code_Outputs/Rgenerated_",file_sub_label,"TimeSeriesImages} \n",sep = ""))
  sink() # stop output to file
  LatexFileName <- file.path(define_file_paths.fn("output.directory"),paste("Rgenerated_",file_sub_label,"TimeSeriesImages.tex",sep = "")) # Start file for latex code images
  if (file.exists(LatexFileName)) {file.remove(LatexFileName)}
  title_string_partial <- paste(title_string_starter,"Time Series")#"ML Inputs Time Series" # used in plot titles and subsection name
  LaTex_code_start_subsection.fn(LatexFileName, title_string = title_string_partial, append_option = FALSE) # start subsection for latex code
  sink.number()
  df_report.fn(df_in = df_in, cols_interest = c(col_name_interest,predictor_variables), x_axis_var = "Date", output.directory = define_file_paths.fn("output.directory"),
               output.directory.short = define_file_paths.fn("output.directory.short"), file_sub_label = file_sub_label, title_string_partial = title_string_partial, plot_color = "black",
               LatexFileName = LatexFileName, SinkFileName = NA, image_format = "jpg")

  # plot predictor variables against PM2.5
  print("plot predictor variables against PM2.5")
  if (sink.number()>0) {sink()} # get stop any lingering sinks
  sink(file = Main_latex_file_name, append = TRUE, type = c("output","message"),split = FALSE)
  cat(paste("\n% PM2.5 vs predictor for each predictor variable ",sep = ""))
  cat(paste("\n\\input{Code_Outputs/Rgenerated_",file_sub_label,"PredictorVPM25Images} \n",sep = ""))
  sink() # stop output to file
  print(file_sub_label)
  LatexFileName=file.path(define_file_paths.fn("output.directory"),paste("Rgenerated_",file_sub_label,"PredictorVPM25Images.tex",sep = "")) # Start file for latex code images
  if (file.exists(LatexFileName)) {file.remove(LatexFileName)}
  title_string_partial <- paste(title_string_starter,"Plot against", replace_character_in_string.fn(input_char = col_name_interest,char2replace = "_",replacement_char = " ")) #"ML Inputs Plot against PM2.5" # used in plot titles and subsection name
  LaTex_code_start_subsection.fn(LatexFileName, title_string = title_string_partial, append_option = FALSE) # start subsection for latex code
  df_report.fn(df = df_in, cols_interest = c(dynamic_predictors), x_axis_var = col_name_interest, output.directory = define_file_paths.fn("output.directory"),
               output.directory.short = define_file_paths.fn("output.directory.short"), file_sub_label = file_sub_label, title_string_partial = title_string_partial, plot_color = "black",
               LatexFileName = LatexFileName, SinkFileName = NA)

  # identify days of highest and lowest concentration
  print("identify days of highest and lowest concentration")
  dates_of_interest <- top_bottom_dates.fn(df_in) # find the days with the overall highest and lowest max concentrations
  print(dates_of_interest)
  # plot county-aggregated data for a few specific days
  print("plot county-aggregated data for a few specific days")
  if (sink.number()>0) {sink()} # get stop any lingering sinks
  sink(file = Main_latex_file_name, append = TRUE, type = c("output","message"),split = FALSE)
  cat(paste("\n% plot maps of a few specific days - aggregated to county averages ",sep = ""))
  cat(paste("\n\\input{Code_Outputs/Rgenerated_",file_sub_label,"MapCountySpecDaysImages} \n",sep = ""))
  sink() # stop output to file
  print(file_sub_label)
  LatexFileName=file.path(define_file_paths.fn("output.directory"),paste("Rgenerated_",file_sub_label,"MapCountySpecDaysImages.tex",sep = "")) # Start file for latex code images
  if (file.exists(LatexFileName) == TRUE) {file.remove(LatexFileName)}

  # settings slightly different for PM2.5
  print("map county values for specific days - PM2.5 only")
  map_spec_days_value_by_region.fn(Region = "County", RegionMaps.directory = define_file_paths.fn("CountyMaps.directory"),
                                   df_in = df_in, dates_of_interest = dates_of_interest, Date_col = "Date",
                                   Lat_col = "Latitude", Lon_col = "Longitude", Var_col = col_name_interest,
                                   Cut_points_set = TRUE, cut_point_scale = col_name_interest, study_states_abbrev = study_states_abbrev,
                                   output.directory = define_file_paths.fn("output.directory"),file_sub_label = file_sub_label,
                                   LatexFileName = LatexFileName,title_string_starter = title_string_starter,Time_aggregation = "OneDay")

  # plot Fire Count variables
  print("map county values for specific days - Fire Count variables")
  # identify fire count variables
  fire_count_TF <- unlist(lapply(1:length(dynamic_predictors), function(x){
    this_var <- dynamic_predictors[x]
    print(this_var)
    if (substr(this_var,1,10) == "Fire_Count") {
      this_fire_count <- TRUE
    } else {
      this_fire_count <- FALSE
    }
    return(this_fire_count)
  }))
  Fire_Count_variables <- dynamic_predictors[fire_count_TF]
  print(Fire_Count_variables)
  map_spec_days_value_by_region.fn(Region = "County", RegionMaps.directory = define_file_paths.fn("CountyMaps.directory"),
                                   df_in = df_in, dates_of_interest = dates_of_interest, Date_col = "Date",
                                   Lat_col = "Latitude", Lon_col = "Longitude", Var_col = Fire_Count_variables[3:4],
                                   Cut_points_set = TRUE, cut_point_scale = "Fire_Count", study_states_abbrev = study_states_abbrev,
                                   output.directory = define_file_paths.fn("output.directory"),file_sub_label = file_sub_label,
                                   LatexFileName = LatexFileName,title_string_starter = title_string_starter)

  # plot other variables
  print("map county values for specific days - other dynamic variables")
  non_fire_dyn_predictors <- dynamic_predictors[fire_count_TF==FALSE]
  map_spec_days_value_by_region.fn(Region = "County", RegionMaps.directory = define_file_paths.fn("CountyMaps.directory"),
                                   df_in = df_in, dates_of_interest = dates_of_interest, Date_col = "Date",
                                   Lat_col = "Latitude", Lon_col = "Longitude", Var_col = non_fire_dyn_predictors,
                                   Cut_points_set = FALSE, cut_point_scale = NA, study_states_abbrev = study_states_abbrev,
                                   output.directory = define_file_paths.fn("output.directory"),file_sub_label = file_sub_label,
                                   LatexFileName = LatexFileName,title_string_starter = title_string_starter)

  #### Monthly Medians ###
  # plot county-aggregated data for a few specific days
  print("plot county-aggregated data for by month")
  if (sink.number()>0) {sink()} # get stop any lingering sinks
  sink(file = Main_latex_file_name, append = TRUE, type = c("output","message"),split = FALSE)
  cat(paste("\n% plot maps of monthly median PM2.5 concentrations - aggregated to county averages ",sep = ""))
  cat(paste("\n\\input{Code_Outputs/Rgenerated_",file_sub_label,"MapCountyMonthlyImages} \n",sep = ""))
  sink() # stop output to file
  print(file_sub_label)
  LatexFileName=file.path(define_file_paths.fn("output.directory"),paste("Rgenerated_",file_sub_label,"MapCountyMonthlyImages.tex",sep = "")) # Start file for latex code images
  if (file.exists(LatexFileName) == TRUE) {file.remove(LatexFileName)}
  # settings slightly different for PM2.5
  print("map county monthly median values- PM2.5 only")
  map_spec_days_value_by_region.fn(Region = "County", RegionMaps.directory = define_file_paths.fn("CountyMaps.directory"),
                                   df_in = df_in, dates_of_interest = NA, Date_col = "Date", # df_in = Full_PM25_obs_w_NA
                                   Lat_col = "Latitude", Lon_col = "Longitude", Var_col = col_name_interest,
                                   Cut_points_set = TRUE, cut_point_scale = col_name_interest, study_states_abbrev = study_states_abbrev,
                                   output.directory = define_file_paths.fn("output.directory"),file_sub_label = file_sub_label,
                                   LatexFileName = LatexFileName,title_string_starter = title_string_starter, Time_aggregation = "Monthly")

  # plot Fire Count variables
  print("map county monthly median values - Fire Count variables")
  map_spec_days_value_by_region.fn(Region = "County", RegionMaps.directory = define_file_paths.fn("CountyMaps.directory"),
                                   df_in = df_in, dates_of_interest = NA, Date_col = "Date",
                                   Lat_col = "Latitude", Lon_col = "Longitude", Var_col = Fire_Count_variables[13:16],
                                   Cut_points_set = TRUE, cut_point_scale = "Fire_Count", study_states_abbrev = study_states_abbrev,
                                   output.directory = define_file_paths.fn("output.directory"),file_sub_label = file_sub_label,
                                   LatexFileName = LatexFileName,title_string_starter = title_string_starter, Time_aggregation = "Monthly")

  # plot other variables
  print("map county monthly median values - other dynamic variables")
  #non_fire_dyn_predictors <- dynamic_predictors[fire_count_TF==FALSE]
  map_spec_days_value_by_region.fn(Region = "County", RegionMaps.directory = define_file_paths.fn("CountyMaps.directory"),
                                   df_in = Full_PM25_obs_w_NA, dates_of_interest = NA, Date_col = "Date",
                                   Lat_col = "Latitude", Lon_col = "Longitude", Var_col = non_fire_dyn_predictors,
                                   Cut_points_set = FALSE, cut_point_scale = NA, study_states_abbrev = study_states_abbrev,
                                   output.directory = define_file_paths.fn("output.directory"),file_sub_label = file_sub_label,
                                   LatexFileName = LatexFileName,title_string_starter = title_string_starter, Time_aggregation = "Monthly")

  # plot maps of data for a few specific days
  print("plot maps of data for a few specific days")
  if (sink.number()>0) {sink()} # get stop any lingering sinks
  sink(file = Main_latex_file_name, append = TRUE, type = c("output","message"),split = FALSE)
  cat(paste("\n% plot maps of a few specific days - point values ",sep = ""))
  cat(paste("\n\\input{Code_Outputs/Rgenerated_",file_sub_label,"MapSpecDaysImages} \n",sep = ""))
  sink() # stop output to file
  LatexFileName=file.path(define_file_paths.fn("output.directory"),paste("Rgenerated_",file_sub_label,"MapSpecDaysImages.tex",sep = "")) # Start file for latex code images
  title_string_partial <- paste(title_string_starter,"Map subset of days") #"ML Inputs Map subset of days" # used in plot titles and subsection name
  LaTex_code_start_subsection.fn(LatexFileName, title_string = title_string_partial, append_option = FALSE) # start subsection for latex code
  df_map_subset_days.fn(this_df = df_in, cols_interest = c(col_name_interest, dynamic_predictors), dates_of_interest = dates_of_interest, output.directory = define_file_paths.fn("output.directory"), output.directory.short = define_file_paths.fn("output.directory.short"), file_sub_label = file_sub_label, title_string_partial = title_string_partial, plot_color = "black", LatexFileName = LatexFileName, SinkFileName = SinkFileName, image_format = "jpg",study_states_abbrev = study_states_abbrev,this_datum = this_datum)

  # plot maps of monthly medians
  print("plot maps of monthly medians")
  if (sink.number()>0) {sink()} # get stop any lingering sinks
  sink(file = Main_latex_file_name, append = TRUE, type = c("output","message"),split = FALSE)
  cat(paste("\n% plot monthly median values ",sep = ""))
  cat(paste("\n\\input{Code_Outputs/Rgenerated_",file_sub_label,"MapMonthlySummariesImages} \n",sep = ""))
  sink() # stop output to file
  LatexFileName=file.path(define_file_paths.fn("output.directory"),paste("Rgenerated_",file_sub_label,"MapMonthlySummariesImages.tex",sep = "")) # Start file for latex code images
  if (file.exists(LatexFileName)) {file.remove(LatexFileName)}
  title_string_partial <- paste(title_string_starter,"Map monthly medians") #"ML Inputs Map monthly medians" # used in plot titles and subsection name
  LaTex_code_start_subsection.fn(LatexFileName, title_string = title_string_partial, append_option = FALSE) # start subsection for latex code
  df_map_monthly_summary.fn(this_df = df_in, cols_interest = c(col_name_interest, dynamic_predictors), output.directory = define_file_paths.fn("output.directory"), output.directory.short = define_file_paths.fn("output.directory.short"), file_sub_label = file_sub_label, title_string_partial = title_string_partial, plot_color = "black", LatexFileName = LatexFileName, SinkFileName = SinkFileName, image_format = "jpg",study_states_abbrev,this_datum)

  # concatinate latex code to make a compile-able .tex file
  setwd(define_file_paths.fn("output.directory")) # go back to original working directory
  cat("Report_top_template.tex",paste("Rgenerated_",file_sub_label,"_main.tex",sep = ""))
  system(paste("cat ",Top_latex_file_name,Main_latex_file_name,Bottom_latex_file_name," > ",Report_latex_file_name,sep = " "))
  
} # end of large_df_report.fn function

