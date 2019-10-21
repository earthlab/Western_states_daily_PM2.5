#### Clear all variables and start fresh ####
rm(list = ls())
options(warn = 2) # throw an error when there's a warning and stop the code from running further
# make sure it isn't outputing text or figures to any files
if (max(dev.cur())>1) { # make sure it isn't outputting to any figure files
  dev.off(which = dev.cur())
} # if (max(dev.cur())>1) {
while (sink.number()>0) {
  sink()
} # while (sink.number()>0) {
sink.number()

#### Define Directories and Source Functions ####
uppermost.directory <- "/Users/mema2636/Documents/MMM_GitHub/COPM-ReviewPaper"
#uppermost.directory <- "C:/Users/Maestas/MMMGitRepository/COPM-ReviewPaper"

setwd(uppermost.directory)

source(file.path(uppermost.directory,"check_all_entries_have_key_function.R"))
source(file.path(uppermost.directory,"extract_bib_item_name_function.R"))
source(file.path(uppermost.directory,"extract_bib_symbol_function.R"))

#### Input files ####
system("cp /Users/mema2636/Documents/MMM_GitHub/COPM-ReviewPaper/bu.aux /Users/mema2636/Documents/MMM_GitHub/COPM-ReviewPaper/bu_Rinput.csv")

this_source_file <-  "bu_Rinput.csv" #"bu_test.csv"
bu_file <- read.csv(file.path(uppermost.directory,this_source_file), header = FALSE)

this_source_file <- "bu_symbol_key.csv"
bu_key <- read.csv(file.path(uppermost.directory,this_source_file), header = FALSE)

#if (dim(bu_file)[1] != dim(bu_key)[1]) {
#  error("two files used for input should have same length - re-check the files for missing bib items")
#}

# check through bu_Rinput.csv to make sure that all entries are represented in bu_symbol_key.csv
check_all_entries_have_key.fn(bu_file, bu_key)

#### Output File ####
BuAuxFileName=file.path(uppermost.directory,paste("bu.aux",sep = "")) # Start file for latex code images
sink(file = BuAuxFileName, append = FALSE, type = c("output","message"),split = FALSE)

#### Cycle through bib items and output text into new bu.aux file
for (row_i in 1:dim(bu_file)[1]) { # Cycle through bib items and output text into new bu.aux file
 # find the bib item name in this row of the numbered bu file
  this_row_orig <- as.character(bu_file[row_i,1])
  
  if (this_row_orig == " ") {
    break
  }
  #print(this_row_orig)
  bib_item <- extract_bib_item_name.fn(this_row_orig)
  #print(bib_item)
  
  # what is the number for this bib item?
  bib_number <- extract_bib_symbol.fn(this_row_orig)
  #print(bib_number)
  
  # find the row with the same bib item in the symbol key file
  for (row_key_i in 1:dim(bu_key)[1]) { # cycle through rows of key to find the matching bib item
   this_row_key <- as.character(bu_key[row_key_i,1]) 
   #print(this_row_key) 
  
   bib_key <- extract_bib_item_name.fn(this_row_key)
   #print(bib_key)
   
   # do the two bib items match? If so, what is the symbol for this bib item?
   if (bib_item == bib_key) {
     #print("match found")
     bib_symbol <- extract_bib_symbol.fn(this_row_key)
     #print(bib_symbol)
   }
  } # for (row_key_i in 1:dim(bu_key)) { # cycle through rows of key to find the matching bib item
  
   # print a new bu.aux file with both the symbols and numbers
   cat(paste("\\bibcite{",as.character(bib_item),"}{",as.character(bib_number),as.character(bib_symbol),"} \n ",sep = ""))
   #cat(paste("\\bibcite{",bib_item,bib_number,bib_symbol,sep = ""))
 
} # for (row_i in 1:dim(bu_file)[1]) { # Cycle through bib items and output text into new bu.aux file

sink() # close out text file
