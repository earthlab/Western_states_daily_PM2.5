# functions used for ML processing

# output report about model run
ML_run_report.fn <- function(SinkFileName, task_counter,fit_type,this_model,ProcessedData.directory) {
  
  sink(file =SinkFileName, append = TRUE, type = c("output","message"), split = FALSE) # start output to text file
  cat(paste(fit_type," model fitting (task ",task_counter,") \n",sep = ""))
  cat(paste("Report generated at ",Sys.Date()," \n",sep = ""))
  cat(paste("Ransomization seed = ",set_seed," \n",sep = ""))
  cat(paste("Validation: ",n_fold_validation,"-fold ",validation_method," \n",sep = ""))
  cat(paste("Tune Length = ",this_tuneLength,sep = ""))
  
  print(this_model)
  
  cat(paste("min RMSE = ",min(this_model$results$RMSE)))
  
  sink() # stop output to text file
  
} # end of ML_run_report.fn function

# plot model from ML run
ML_plot_model.fn <- function(file_sub_label, this_model, SinkFileName = NA, LatexFileName = NA, title_string) {
  if (is.na(SinkFileName) == FALSE) {sink()} # check if there's a sink that needs to be stopped
  # define naming/labeling scheme
  plot_name_extension <- "RMSEvNVariables" # end of figure file name
  data_for_plotting <- this_model # the data to be plotted
  plotting_string <- "plot(data_for_plotting)" #"plot(this_model)" # the plotting command to be used
  
  # create plot
  Plot_and_latex.fn(output.directory, file_sub_label, plot_name_extension, plotting_string, data_for_plotting, title_string)
  
  # create LaTex code for plot  
  if (is.na(LatexFileName) == FALSE) { # only output latex code if a file has been specified
  LaTex_code_4_figure.fn(LatexFileName, title_string, file_sub_label, plot_name_extension)
  } else {
    print("No LatexFileName has been specified, LaTex code will not be output for this image")
  }
    
  # go back to outputing sink to main sink file
  if (is.na(SinkFileName) == FALSE) { # go back to outputing sink to main sink file
    sink(file =SinkFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting output into SinkFileName
    } # if (is.na(SinkFileName) == FALSE) { # go back to outputing sink to main sink file
  
} # end of ML_plot_model.fn function