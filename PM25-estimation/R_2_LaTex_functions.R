# R_2_LaTex_functions.R

# write LaTex code to input figure
R_2_LaTex_figure.fn <- function(LatexFileName) {
# stop putting text into SinkFileName; make sure it isn't outputing text or figures to any files
if (max(dev.cur())>1) { # make sure it isn't outputting to any figure files
  dev.off(which  =  dev.cur())
} # if (max(dev.cur())>1) {
while (sink.number()>0) {
  sink()
} # while (sink.number()>0) {
sink.number()

LatexFileName=file.path(output.directory,paste("Rgenerated_Images",this_image_file_name,".tex",sep = "")) # Start file for latex code images
if (plot_year==0) {
  sink(file = LatexFileName, append = FALSE, type = c("output","message"),split = FALSE) # start new file the first time
  cat(paste("\n\\subsubsection*{",subsection_name,"}",sep = ""))
} else {
  sink(file = LatexFileName, append = TRUE, type = c("output","message"),split = FALSE)
}
cat("\n\\begin{figure} \n")
cat("\\centering \n")
#cat(paste("\\includegraphics[width=0.77\\textwidth]{Code_Outputs/",FigFileName_nopath,".",image_format,"} \n",sep = "")) 
if (Animation_subdirectory==""){
  cat(paste("\\includegraphics[width=0.77\\textwidth]{Code_Outputs/",FigFileName_nopath,".",image_format,"} \n",sep = "")) 
} else {
  cat(paste("\\includegraphics[width=0.77\\textwidth]{Code_Outputs/",Animation_subdirectory,"/",FigFileName_nopath,".",image_format,"} \n",sep = "")) 
}

cat(paste("\\caption{\\label{fig:",fig_label,"}",fig_caption,"} \n",sep = "")) 
cat("\\end{figure} \n \n")
sink() # stop writing to latex file
sink(file =SinkFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting output into SinkFileName
rm(Animation_subdirectory)
} # function