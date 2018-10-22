# functions for plotting and puting plots in LaTex

# plot model from ML run; was Plot_and_latex.fn
Plot_to_ImageFile.fn <- function(output.directory, file_sub_label, plot_name_extension, plotting_string, data_for_plotting, title_string) {
  ### plot this_model_run_name
  FigFileName=file.path(output.directory,paste(file_sub_label,"_",plot_name_extension,".pdf",sep = "")) # define file name for the figure to be created
  print(FigFileName)
  pdf(file=FigFileName, height = 3.5, width = 5, onefile=FALSE) # start pdf document to put figure into
  plot.new() # clear the plot to have a clean canvas to draw on
  par(mar=c(4.2, 3.8, 1, 0.2)) # trim off extra white space (bottom, left, top, right)
  print(plotting_string)
  print(data_for_plotting)
  #print(plot(data_for_plotting))
  #plot(x = 1:10, y = 20:29)
  eval(parse(text = paste("print(",plotting_string,")",sep = ""))) #plot(this_model_output) 
  title(main = title_string)
  dev.off() # stop writing to pdf file
  remove(FigFileName)
} # end of ML_plot_model.fn function

LaTex_code_4_figure.fn <- function(LatexFileName, title_string, file_sub_label, plot_name_extension, output.directory.short) {
sink(file = LatexFileName, append = TRUE, type = c("output","message"),split = FALSE)

  cat(paste("\n\\begin{figure} \n"))
  cat(paste("\\centering "," \n",sep = ""))
  cat(paste("\\includegraphics[width=0.77\\textwidth]{",output.directory.short,"/",file_sub_label,"_",plot_name_extension,".pdf} \n",sep = "")) 
  cat(paste("\\caption{\\label{fig:",file_sub_label,plot_name_extension,"}",title_string,"} \n",sep = "")) 
cat(paste("\\end{figure} \n \n"))
sink() # stop writing to latex file
#sink(file =SinkFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting output into SinkFileName
} # end of LaTex_code_4_figuer_function - need to finish

LaTex_code_start_subsection.fn <- function(LatexFileName, title_string, append_option = TRUE) {
  sink(file = LatexFileName, append = append_option, type = c("output","message"),split = FALSE)
  cat(paste("\n\\subsection{",title_string," Images} \n \n",sep = ""))
  sink() # stop output to file
} # end of LaTex_code_start_subsection function

Plot_and_latex.fn <- function(output.directory, output.directory.short, file_sub_label, plot_name_extension, plotting_string, data_for_plotting, title_string, LatexFileName, SinkFileName) {
  # create plot
  Plot_to_ImageFile.fn(output.directory, file_sub_label, plot_name_extension, plotting_string, data_for_plotting, title_string)
  # create LaTex code for plot  
  if (is.na(LatexFileName) == FALSE) { # only output latex code if a file has been specified
    LaTex_code_4_figure.fn(LatexFileName, title_string, file_sub_label, plot_name_extension, output.directory.short)
  } else {
    print("No LatexFileName has been specified, LaTex code will not be output for this image")
  }
  # go back to outputing sink to main sink file
  if (is.na(SinkFileName) == FALSE) { # go back to outputing sink to main sink file
    sink(file =SinkFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting output into SinkFileName
  } # if (is.na(SinkFileName) == FALSE) { # go back to outputing sink to main sink file
} # end of Plot_and_latex.fn function
