# Create_List_Variables_Tex_Table.R
# Create Variable List (Table 1) for manuscript

#### Clear variables and sinks; define working directory ####
rm(list  =  ls())
options(warn  =  2) # throw an error when there's a warning and stop the code from running further
if (max(dev.cur())>1) { # make sure it isn't outputting to any figure files
  dev.off(which  =  dev.cur())
} # if (max(dev.cur())>1) {
while (sink.number()>0) {
  sink()
} # while (sink.number()>0) {
uppermost.directory  <-  "/home/rstudio"
setwd(uppermost.directory) # set working directory

# define directories
functions.directory = file.path(uppermost.directory,"estimate-pm25","LaTeX_documentation","SciData_manuscript","Figure_Table_Scripts","Rcode_cvs_to_TexTables")
csv_file.directory = file.path(uppermost.directory,"estimate-pm25","LaTeX_documentation","SciData_manuscript")


#### Call Packages (Library) ####
#library(plyr)
#library(rgdal)
#library(geosphere)

#### Call Load Functions that I created ####
source(file.path("estimate-pm25","General_Project_Functions","general_project_functions.R"))
source(file.path(functions.directory,"csv_2_latex_table_function.R"))
source(file.path(functions.directory,"MultiLineInBox_function.R"))
source(file.path(functions.directory,"split_long_string_table_rows_function.R"))

#### Create Table ####
StandAloneDoc = FALSE #TRUE #FALSE # indicate whether the documents needs to be able to be compiled as a stand-alone document (TRUE) or if it will be input into another tex document

# Provide info for table 
csv_file_name_w_path <- file.path(csv_file.directory,"Table1_List_Variables.csv")
tex_file_name_w_path <- file.path(csv_file.directory,"Table1_List_Variables_Latex.tex")
caption_text <- "Variables used in the machine learning models."
table_label <- "Tab1"
text_justify_vert_line <- 0
append_tex_file = TRUE
MultiLineInBox = TRUE
NCharInLineBox = 30#20 # each row will only have this many characters of text before splitting to the next row within a box
NCharInLineBox_1st_Col <- 30
All_hlines = TRUE # horizontal lines between every row
Longtable = TRUE # allow tables to span multiple pages
First_col_font_type <-  "Normal" # other option is "bold"
Top_row_font_type <- "bold"
Main_text_font_type <- "Normal"
font_justification <- "l" #"c" # define left, center, or right justification: l, c, or r
vert_line <- "|" # or blank space for no vertical line
horiz_line <- TRUE # horizontal lines in table (TRUE/FALSE)

# start tex file
sink(file = tex_file_name_w_path, append = FALSE, type = c("output","message"), split = FALSE)

if (StandAloneDoc == TRUE) {
  cat("\\documentclass{article} \n")
  cat("\\usepackage[margin = 0.5 in, paperheight = 8.5in, paperwidth = 11 in]{geometry} \n")
  
  cat("\\usepackage[utf8]{inputenc} \n") 
  cat("\\usepackage{graphicx} \n")
  cat("\\usepackage{booktabs} \n") 
  cat("\\usepackage{array} \n")
  cat("\\usepackage{paralist} \n")
  cat("\\usepackage{verbatim} \n")
  cat("\\usepackage{subfig} \n")
  cat("\\usepackage{fancyhdr} \n")
  cat("\\pagestyle{fancy} \n")
  cat("\\renewcommand{\\headrulewidth}{0pt} \n") 
  cat("\\lhead{}\\chead{}\\rhead{} \n")
  cat("\\lfoot{}\\cfoot{\\thepage}\\rfoot{} \n")
  cat("\\usepackage{sectsty} \n")
  cat("\\usepackage[nottoc,notlof,notlot]{tocbibind} \n") 
  cat("\\usepackage[titles,subfigure]{tocloft} \n")
  cat("\\usepackage {amsmath} \n")
  cat("\\usepackage {amssymb} \n")
  cat("\\usepackage{pdflscape} \n")
  cat("\\usepackage{url} \n")
  cat("\\usepackage{cite} % from https://www.ctan.org/pkg/cite \n")
  cat("\\usepackage{chapterbib} \n")
  cat("\\usepackage{bibunits} \n")
  cat("\\usepackage{marvosym} \n")
  cat("\\usepackage[usenames,dvipsnames]{xcolor} \n")
  cat("\\usepackage{hyperref} \n")
  cat("\\definecolor{linkcolour}{rgb}{0,0.2,0.6} \n")
  cat("\\hypersetup{colorlinks,breaklinks,urlcolor=linkcolour, linkcolor=linkcolour} \n")
  cat("\\usepackage{ragged2e} \n")
  cat("\\hyphenation{im-pre-se} \n")
  cat("\\usepackage{lipsum} \n")
  cat("\\usepackage[english]{babel} \n")
  cat("\\usepackage[T1]{fontenc} \n")
  cat("\\usepackage{titlesec} \n")
  cat("\\usepackage{soul} % for underlining text \n")
  cat("\\usepackage[colorinlistoftodos]{todonotes} \n")
  cat("\\usepackage{times} \n")
  cat("\\usepackage{longtable} \n")
  
  cat("\\begin{document} \n")
  
  sink()
} # if standalone Doc

# call table function
csv_2_latex_table.fn(csv_file_name_w_path,tex_file_name_w_path,caption_text,
                     table_label = "Table1",
                     text_justify_vert_line,append_tex_file, # include_row_headers, 
                     MultiLineInBox, NCharInLineBox_1st_Col, NCharInLineBox, All_hlines, Longtable, First_col_font_type = First_col_font_type,
                     Top_row_font_type = Top_row_font_type, Main_text_font_type = Main_text_font_type,
                     font_justification = font_justification, vert_line = vert_line)

if (StandAloneDoc == TRUE) {
  sink(file = tex_file_name_w_path, append = TRUE, type = c("output","message"), split = FALSE)
  
  cat("\\bibliographystyle{vancouver} \n")
  cat("\\bibliography{ReviewPaperBibliography} \n")
  
  cat("\\end{document} \n")
  sink()
  sink()
} # if stand alone doc

# make sure it isn't outputing text or figures to any files
if (max(dev.cur())>1) { # make sure it isn't outputting to any figure files
  dev.off(which = dev.cur())
} # if (max(dev.cur())>1) {
while (sink.number()>0) {
  sink()
} # while (sink.number()>0) {
sink.number()


