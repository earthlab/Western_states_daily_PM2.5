# functions used for ML processing

ML_run_report.fn <- function(task_counter,fit_type,this_model,ProcessedData.directory) {
  file_sub_label <- paste("ML_report_","_task_",task_counter,fit_type,sep = "") # decide whether to include date in file name
  SinkFileName=file.path(ProcessedData.directory,paste(file_sub_label,"_combining_sink.txt",sep = ""))
  sink(file =SinkFileName, append = FALSE, type = c("output","message"), split = FALSE) # start output to text file
  cat(paste(fit_type," model fitting (task ",task_counter,") \n",sep = ""))
  cat(paste("Report generated at ",Sys.Date()," \n",sep = ""))
  cat(paste("Ransomization seed = ",set_seed," \n",sep = ""))
  cat(paste("Validation: ",n_fold_validation,"-fold ",validation_method," \n",sep = ""))
  cat(paste("Tune Length = ",this_tuneLength,sep = ""))
  
  print(this_model)
  
  cat(paste("min RMSE = ",min(this_model$results$RMSE)))
  
  sink() # stop output to text file
  
} # end of ML_run_report.fn function