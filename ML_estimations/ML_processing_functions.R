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

# merge predictor variables together
merge_predictors.fn <- function(predictand_data,predictand_col,latitude_col_t,longitude_col_t,datum_col_t, Easting_col_t, Northing_col_t,Dates_col_t) {

  # Create data frame
  if (is.na(predictand_col)) { # is this data set is for predicting pm2.5 or training? 
    new_header <- c("Date","Latitude","Longitude","Datum","Easting","Northing")  
  } else {
    new_header <- c(predictand_col,"Date","Latitude","Longitude","Datum","Easting","Northing")
  } # if (is.na(predictant_col)) { # is this data set is for predicting pm2.5 or training? 
  ML_input <- data.frame(matrix(NA,nrow=dim(predictand_data)[1],ncol=length(new_header))) # create data frame for input_mat1
  names(ML_input) <- new_header # assign the header
#x_digits <- 9
  # fill in the predictand data
  if (is.na(predictand_col) == FALSE) { # is this data set is for predicting pm2.5 or training? 
  ML_input$PM2.5_Obs <- predictand_data[ , predictand_col]
  } # if (is.na(predictant_col)) { # is this data set is for predicting pm2.5 or training?
  ML_input[ , c("Date","Latitude","Longitude","Datum","Easting","Northing")] <- predictand_data[ , c(Dates_col_t,latitude_col_t,longitude_col_t,datum_col_t,Easting_col_t,Northing_col_t)]
  #ML_input[ , c("Date","Latitude","Longitude","Datum")] <- predictand_data[ , c(Dates_col_t,latitude_col_t,longitude_col_t,datum_col_t)]
  
  ML_input<- as.data.frame(ML_input)
  ML_input$Date <- as.Date(ML_input$Date,"%Y-%m-%d") # recognize dates as dates: 'Date_Local' 
  #ML_input$Latitude<- format(round(ML_input$Latitude, x_digits), nsmall = x_digits)#as.character(ML_input$Latitude)
  #ML_input$Longitude<- format(round(ML_input$Longitude, x_digits), nsmall = x_digits)

  # for a list of predictor data sets:
  # list.files(file.path(ProcessedData.directory,predictor_sub_folder))
  
  # Load Highways Data
  if (file.exists(file.path(ProcessedData.directory,predictor_sub_folder, Highways_file_name))) {
    
    Highway_cols <- c("A_100","C_100","Both_100","A_250","C_250","Both_250","A_500","C_500","Both_500","A_1000","C_1000","Both_1000")
    latitude_col_s <- "Latitude"
    longitude_col_s <- "Longitude"
    datum_col_s <- "Datum"
    Dates_col_s <- "Date"
    
    Highways_data <- read.csv(file.path(ProcessedData.directory,predictor_sub_folder, "Highways_part_c.csv"),header=TRUE) # load the AQS file
    Highways_data<- as.data.frame(Highways_data)
    #Highways_data[ , c(latitude_col_s)]<- format(round(Highways_data[ , c(latitude_col_s)], x_digits), nsmall = x_digits)#as.character(ML_input$Latitude)
    #as.character(Highways_data[ , c(latitude_col_s)])
    #Highways_data[ , c(longitude_col_s)]<- format(round(Highways_data[ , c(longitude_col_s)], x_digits), nsmall = x_digits) # as.character(Highways_data[ , c(longitude_col_s)])
    Highways_data[ , c(Dates_col_s)] <- as.Date(Highways_data[ , c(Dates_col_s)],"%m/%d/%Y") # recognize dates as dates
    

    
    
   
    
  } #if (file.exists(file.path(ProcessedData.directory,predictor_sub_folder, Highways_file_name))) {
  
} # end of merge_predictors.fn function

# merge time-varying datasets
merge_time_varying_data.fn <- function(ML_input_in,predictor_data,latitude_col_s,longitude_col_s,datum_col_s,Dates_col_s) {
  # ML_input_in <- ML_input
  # predictor_data <- Highways_data
  
  #write_out<- left_join(ML_input_in, predictor_data)
  #join(ML_input_in, predictor_data, by = c("Latitude", "Longitude", "Date"), type = "left", match = "all")
  
  #write.csv(write_out, "C:\\Users\\elco2649\\Documents\\Highways_part_c.csv")
  
  
  #write_out<- left_join(x = ML_input_in, y = predictor_data, by = c("Date","Latitude","Longitude"))
  #ML_input_out <- 
   # join(x = ML_input_in, y = predictor_data, by = c("Date","Latitude","Longitude"), type = "left", match = "all")
    #join(dates_data, All, by = c("Latitude", "Longitude"), type = "left", match = "all")
  #test_join <- join(x = ML_input_in, y = predictor_data)
  test7_join <- join(x = ML_input_in, y = predictor_data, by = c( "Latitude" = latitude_col_s, "Longitude" = longitude_col_s, "Date" = Dates_col_s)) # , "Date" = Dates_col_s
  
  dim(unique(ML_input_in[ ,c("Latitude", "Longitude", "Date")]))
  dim(unique(predictor_data[ ,c(latitude_col_s,longitude_col_s,Dates_col_s)]))
  
  x_digits <- 4
  predictor_data$Latitude_formatted <- format(round(predictor_data[ ,latitude_col_s], x_digits), nsmall = x_digits)
  
  
  for (row_i in 1:dim(ML_input_in)[1]) {
    this_lat <- format(round(ML_input_in[row_i, "Latitude"], x_digits), nsmall = x_digits)
    print(this_lat)
      
    find_match <- which(predictor_data[ ,"Latitude_formatted"] == this_lat)
    print(length(find_match))
  }
  
  return(ML_input_out)
} # end of merge_time_varying_data.fn function


