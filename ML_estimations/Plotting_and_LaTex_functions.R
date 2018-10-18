# functions for plotting and puting plots in LaTex

# plot model from ML run
Plot_and_latex.fn <- function(output.directory, file_sub_label, plot_name_extension, plotting_string, data_for_plotting, title_string) {
  ### plot this_model_run_name
  FigFileName=file.path(output.directory,paste(file_sub_label,"_",plot_name_extension,".pdf",sep = "")) # define file name for the figure to be created
  pdf(file=FigFileName, height = 3.5, width = 5, onefile=FALSE) # start pdf document to put figure into
  plot.new() # clear the plot to have a clean canvas to draw on
  par(mar=c(4.2, 3.8, 1, 0.2)) # trim off extra white space (bottom, left, top, right)
  eval(parse(text = plotting_string))
  #plot(this_model_output)# ,axes=F, ann=T, cex.lab=0.8, lwd=2)
  # Make x axis tick marks without labels
  # axis(1, lab=F)
  title(main = title_string)
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