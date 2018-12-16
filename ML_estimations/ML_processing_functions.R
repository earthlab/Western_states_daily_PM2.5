# functions used for ML processing

# output latex report about the input data
ML_input_report.fn <- function(PM25_obs_shuffled, col_PM25_obs, predictor_variables) {
  for (this_col in c(col_PM25_obs,predictor_variables)) {
    # plot map of average value of data
    map_base_layer.fn(USMaps.directory, study_states_abbrev) # map state outlines
  }
} # end of ML_input_report.fn

# output report about model run
ML_run_report.fn <- function(SinkFileName, task_counter,fit_type,this_model,ProcessedData.directory) {
  
  sink(file =SinkFileName, append = TRUE, type = c("output","message"), split = FALSE) # start output to text file
  cat(paste(fit_type," model fitting (task ",task_counter,") \n",sep = ""))
  cat(paste("Report generated at ",Sys.Date()," \n",sep = ""))
  cat(paste("Ransomization seed = ",set_seed," \n",sep = ""))
  cat(paste("Validation: ",n_fold_validation,"-fold ",validation_method," \n",sep = ""))
  cat(paste("Tune Length = ",this_tuneLength,sep = ""))
  
  print(this_model)
  
  cat(paste("\n min RMSE = ",min(this_model$results$RMSE)))
  
  sink() # stop output to text file
  
} # end of ML_run_report.fn function

# plot model from ML run
ML_plot_model.fn <- function(file_sub_label, this_model, SinkFileName = NA, LatexFileName = NA, title_string, output.directory.short) {
  data_for_plotting <- this_model # the data to be plotted
  
  # RMSE plot
  plot_name_extension <- "RMSEvNVariables" # end of figure file name
  plotting_string <- "plot(data_for_plotting)" #"plot(this_model)" # the plotting command to be used
  Plot_and_latex.fn(output.directory, output.directory.short, file_sub_label, plot_name_extension, plotting_string, data_for_plotting, title_string = title_string, LatexFileName, SinkFileName)
  
  # Rsquared plot
  plot_name_extension <- "RsquaredvNVariables" # end of figure file name
  plotting_string <- "plot(data_for_plotting, metric='Rsquared')" #"plot(this_model)" # the plotting command to be used
  Plot_and_latex.fn(output.directory, output.directory.short, file_sub_label, plot_name_extension, plotting_string, data_for_plotting, title_string = title_string, LatexFileName, SinkFileName)
  
  # MAE plot
  plot_name_extension <- "MAEvNVariables" # end of figure file name
  plotting_string <- "plot(data_for_plotting, metric='MAE')" #"plot(this_model)" # the plotting command to be used
  Plot_and_latex.fn(output.directory, output.directory.short, file_sub_label, plot_name_extension, plotting_string, data_for_plotting, title_string = title_string, LatexFileName, SinkFileName)
  
  # if (is.na(SinkFileName) == FALSE) {sink()} # check if there's a sink that needs to be stopped
  # # define naming/labeling scheme
  # plot_name_extension <- "RMSEvNVariables" # end of figure file name
  # data_for_plotting <- this_model # the data to be plotted
  # plotting_string <- "plot(data_for_plotting)" #"plot(this_model)" # the plotting command to be used
  # # create plot
  # Plot_and_latex.fn(output.directory, file_sub_label, plot_name_extension, plotting_string, data_for_plotting, title_string)
  # # create LaTex code for plot  
  # if (is.na(LatexFileName) == FALSE) { # only output latex code if a file has been specified
  # LaTex_code_4_figure.fn(LatexFileName, title_string, file_sub_label, plot_name_extension, output.directory.short)
  # } else {
  #   print("No LatexFileName has been specified, LaTex code will not be output for this image")
  # }
  # # go back to outputing sink to main sink file
  # if (is.na(SinkFileName) == FALSE) { # go back to outputing sink to main sink file
  #   sink(file =SinkFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting output into SinkFileName
  #   } # if (is.na(SinkFileName) == FALSE) { # go back to outputing sink to main sink file
  
} # end of ML_plot_model.fn function

# compare multiple models
compare_multiple_models.fn <- function(par_output) {
  ## set up documentation files/variables
  #sink(file = LatexFileName, append = FALSE, type = c("output","message"),split = FALSE) # initialize file
  file_sub_label <- paste("ML_compare_models",sep = "") # file partial name, decide whether to include date in file name
  title_string <- paste("Compare multiple models") # used in figure titles, etc
  LatexFileName=file.path(output.directory,paste("Rgenerated_",file_sub_label,"Images.tex",sep = "")) # Start file for latex code images
  LaTex_code_start_subsection.fn(LatexFileName, title_string, append_option = FALSE) # start subsection for latex code
  SinkFileName=file.path(ProcessedData.directory,paste(file_sub_label,".txt",sep = "")) # file name
  sink(file =SinkFileName, append = FALSE, type = c("output","message"), split = FALSE) # start output to text file
  
  # par_out is a list containing all of the model runs that were done
  resamps <- resamples(par_output) # collect resamples from the CV folds
  resamps
  
  # re-name the models to be something useful
  #new_name_step1 <- cbind(resamps$methods, 1:n_task_sets)
  #new_name_step2 <- new_name_step1[ ,1]
  #for (i in 1:dim(new_name_step1)[1]) { 
  #  new_name_step2[i] <- paste(new_name_step1[i, ],collapse = "")
  #}
  #print(new_name_step2)
  #resamps$models <- new_name_step2
  summary(resamps) # summarize the results
  
  data_for_plotting <- resamps # the data to be plotted
  
  # Box and Whisker Plots, all metrics plotted together
  # define naming/labeling scheme
  plot_name_extension <- "bwplot_all" # end of figure file name
  #data_for_plotting <- resamps # the data to be plotted
  plotting_string <- "bwplot(data_for_plotting)" # the plotting command to be used
  this_caption <- "Box and whisker plots for all metrics together."
  Plot_and_latex.fn(output.directory, output.directory.short, file_sub_label, plot_name_extension, plotting_string, data_for_plotting, title_string = this_caption, LatexFileName, SinkFileName)
  
  # Box and Whisker Plots, each metric plotted separately
  # Box and Whisker Plots, RMSE
  # bwplot(resamps, metric = "RMSE")
  # define naming/labeling scheme
  plot_name_extension <- "bwplot_RMSE" # end of figure file name
  #data_for_plotting <- resamps # the data to be plotted
  plotting_string <- "bwplot(data_for_plotting, metric = 'RMSE')" # the plotting command to be used
  this_caption <- "Box and whisker plots for RMSE."
  Plot_and_latex.fn(output.directory, output.directory.short, file_sub_label, plot_name_extension, plotting_string, data_for_plotting, title_string = this_caption, LatexFileName, SinkFileName)
  
  # Box and Whisker Plots, Rsquared
  # bwplot(resamps, metric = "Rsquared")
  # define naming/labeling scheme
  plot_name_extension <- "bwplot_Rsquared" # end of figure file name
  #data_for_plotting <- resamps # the data to be plotted
  plotting_string <- "bwplot(data_for_plotting, metric = 'Rsquared')" # the plotting command to be used
  this_caption <- "Box and whisker plots for Rsquared."
  Plot_and_latex.fn(output.directory, output.directory.short, file_sub_label, plot_name_extension, plotting_string, data_for_plotting, title_string = this_caption, LatexFileName, SinkFileName)
  
  # Box and Whisker Plots, MAE
  # bwplot(resamps, metric = "MAE")
  # define naming/labeling scheme
  plot_name_extension <- "bwplot_MAE" # end of figure file name
  #data_for_plotting <- resamps # the data to be plotted
  plotting_string <- "bwplot(data_for_plotting, metric = 'MAE')" # the plotting command to be used
  this_caption <- "Box and whisker plots for MAE."
  Plot_and_latex.fn(output.directory, output.directory.short, file_sub_label, plot_name_extension, plotting_string, data_for_plotting, title_string = this_caption, LatexFileName, SinkFileName)
  
  # Dot plots - shows simplified version of box and whisker plots, better when comparing many models
  plot_name_extension <- "dotplot_RMSE" # end of figure file name
  plotting_string <- "dotplot(data_for_plotting, metric = 'RMSE')" # the plotting command to be used
  this_caption <- "Dot plots for RMSE."
  Plot_and_latex.fn(output.directory, output.directory.short, file_sub_label, plot_name_extension, plotting_string, data_for_plotting, title_string = this_caption, LatexFileName, SinkFileName)
  
  plot_name_extension <- "dotplot_Rsquared" # end of figure file name
  plotting_string <- "dotplot(data_for_plotting, metric = 'Rsquared')" # the plotting command to be used
  this_caption <- "Dot plots for Rsquared."
  Plot_and_latex.fn(output.directory, output.directory.short, file_sub_label, plot_name_extension, plotting_string, data_for_plotting, title_string = this_caption, LatexFileName, SinkFileName)

  plot_name_extension <- "dotplot_MAE" # end of figure file name
  plotting_string <- "dotplot(data_for_plotting, metric = 'MAE')" # the plotting command to be used
  this_caption <- "Dot plots for MAE."
  Plot_and_latex.fn(output.directory, output.directory.short, file_sub_label, plot_name_extension, plotting_string, data_for_plotting, title_string = this_caption, LatexFileName, SinkFileName)
  
  # Density plot
  #densityplot(data_for_plotting, metric = "RMSE")
  plot_name_extension <- "densityplot_RMSE" # end of figure file name
  plotting_string <- "densityplot(data_for_plotting, metric = 'RMSE')" # the plotting command to be used
  this_caption <- "Density plots for RMSE."
  Plot_and_latex.fn(output.directory, output.directory.short, file_sub_label, plot_name_extension, plotting_string, data_for_plotting, title_string = this_caption, LatexFileName, SinkFileName)
  
  #densityplot(resamps, metric = "Rsquared")
  plot_name_extension <- "densityplot_Rsquared" # end of figure file name
  plotting_string <- "densityplot(data_for_plotting, metric = 'Rsquared')" # the plotting command to be used
  this_caption <- "Density plots for Rsquared."
  Plot_and_latex.fn(output.directory, output.directory.short, file_sub_label, plot_name_extension, plotting_string, data_for_plotting, title_string = this_caption, LatexFileName, SinkFileName)
  
  plot_name_extension <- "densityplot_MAE" # end of figure file name
  plotting_string <- "densityplot(data_for_plotting, metric = 'MAE')" # the plotting command to be used
  this_caption <- "Density plots for MAE."
  Plot_and_latex.fn(output.directory, output.directory.short, file_sub_label, plot_name_extension, plotting_string, data_for_plotting, title_string = this_caption, LatexFileName, SinkFileName)
  
  # Scatter plot
  plot_name_extension <- "xyplot_RMSE" # end of figure file name
  plotting_string <- "xyplot(data_for_plotting, metric = 'RMSE')" # the plotting command to be used
  this_caption <- "Scatter plot for RMSE."
  Plot_and_latex.fn(output.directory, output.directory.short, file_sub_label, plot_name_extension, plotting_string, data_for_plotting, title_string = this_caption, LatexFileName, SinkFileName)
  
  #xyplot(resamps, metric = "Rsquared")
  plot_name_extension <- "xyplot_Rsquared" # end of figure file name
  plotting_string <- "xyplot(data_for_plotting, metric = 'Rsquared')" # the plotting command to be used
  this_caption <- "Scatter plot for Rsquared."
  Plot_and_latex.fn(output.directory, output.directory.short, file_sub_label, plot_name_extension, plotting_string, data_for_plotting, title_string = this_caption, LatexFileName, SinkFileName)
  
  plot_name_extension <- "xyplot_MAE" # end of figure file name
  plotting_string <- "xyplot(data_for_plotting, metric = 'MAE')" # the plotting command to be used
  this_caption <- "Scatter plot for MAE."
  Plot_and_latex.fn(output.directory, output.directory.short, file_sub_label, plot_name_extension, plotting_string, data_for_plotting, title_string = this_caption, LatexFileName, SinkFileName)
  
  #xyplot(resamps, metric = "RMSE")
  #xyplot(resamps, metric = "Rsquared")
  #xyplot(resamps, metric = "MAE")
  
} # end of compare_multiple_models.fn function

# create a date/location data frame given a list of dates and a data frame of locations
expand_date_location.fn <- function(locations_of_interest, date_vec, this_datum) {
  # locations_of_interest <- county_centroids # example input
  #date_place_header <- c("Latitude","Longitude","Datum","Date_Local") # define header for date_place data frame
  #date_place_header <- c("Lat","Lon","Datum","Date_Local") # define header for date_place data frame
  #date_place_header <- c("Lat","Lon","Datum","Date_Local", "Easting", "Northing") # define header for date_place data frame
  date_place_header <- c("Lat","Lon","Datum","Date", "Easting", "Northing") # define header for date_place data frame
  date_place <- data.frame(matrix(NA, nrow = (dim(locations_of_interest)[1]*length(date_vec)), ncol = length(date_place_header))) # create data frame
  names(date_place) <- date_place_header # assign the header to data frame
  date_place$Datum <- this_datum #"NAD83" # input datum to data
  #date_place$Date_Local <- as.Date(date_place$Date_Local,"%Y-%m-%d") # recognize dates as dates: 'Date_Local' 
  date_place$Date <- as.Date(date_place$Date,"%Y-%m-%d") # recognize dates as dates: 'Date_Local' 
  
  row_start <- 1 # start row counter
  for (day_i in 1:length(date_vec)) { # cycle through dates to fill in locations
    this_day <- as.Date(date_vec[day_i],"%Y-%m-%d") # get date for this iteration
    #print(this_day)
    row_stop <- row_start+dim(county_centroids)[1]-1 # end row counter
    date_place[row_start:row_stop,c("Date")] <- as.Date(this_day) # fill in date
    #date_place[row_start:row_stop,c("Date_Local")] <- as.Date(this_day) # fill in date
    #date_place[row_start:row_stop,c("Latitude")] <- county_centroids$Latitude # fill in latitute
    #date_place[row_start:row_stop,c("Longitude")] <- county_centroids$Longitude # fill in longitude
    date_place[row_start:row_stop,c("Lat")] <- county_centroids$Lat # fill in latitute
    date_place[row_start:row_stop,c("Lon")] <- county_centroids$Lon # fill in longitude
    date_place[row_start:row_stop,c("Easting")] <- county_centroids$Easting # fill in latitute
    date_place[row_start:row_stop,c("Northing")] <- county_centroids$Northing # fill in longitude
    row_start <- row_stop+1 # move row counter to next iteration
  } # for (day_i in 1:length(date_vec)) { # cycle through dates to fill in locations
  return(date_place) # output from function
} # end of expand_date_location.fn




