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
ML_plot_model.fn <- function(file_sub_label, this_model) {
  ### plot this_model_run_name
  #FigFileName=file.path(output.directory,paste(this_model_run_name_short,"_RMSEvNVariables.pdf",sep = "")) # define file name for the figure to be created
  #pdf(file=FigFileName, height = 3.5, width = 5, onefile=FALSE) # start pdf document to put figure into
  #plot.new() # clear the plot to have a clean canvas to draw on
  plot_name_extension <- "RMSEvNVariables"
  #par(mar=c(4.2, 3.8, 1, 0.2)) # trim off extra white space (bottom, left, top, right)
  plotting_string <- "plot(this_model)"# ,axes=F, ann=T, cex.lab=0.8, lwd=2)
  data_for_plotting <- this_model
  # Make x axis tick marks without labels
  # axis(1, lab=F)
  title_string <- paste(fit_type,task_counter," RMSE vs Number of Variables")
  #title(main = paste(this_model_run_name_display," RMSE vs Number of Variables",sep = ""))
  dev.off() # stop writing to pdf file
  remove(FigFileName)
  sink() # stop putting text into SinkFileName
  LatexFileName=file.path(output.directory,paste(this_model_run_name_short,"Images.tex",sep = "")) # Start file for latex code images
  sink(file = LatexFileName, append = FALSE, type = c("output","message"),split = FALSE)
  cat(paste("\n\\subsection{",this_model_run_name_display," Images}",sep = ""))
  cat("\n\\begin{figure} \n")
  cat("\\centering \n")
  cat(paste("\\includegraphics[width=0.77\\textwidth]{","this_model_run_name_short","_RMSEvNVariables.pdf} \n",sep = "")) 
  cat(paste("\\caption{\\label{fig:","this_model_run_name_short","RMSEvNVar}","this_model_run_name_display"," RMSE vs variable number.} \n",sep = "")) 
  cat("\\end{figure} \n \n")
  sink() # stop writing to latex file
  sink(file =SinkFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting output into SinkFileName
  
} # end of ML_plot_model.fn function