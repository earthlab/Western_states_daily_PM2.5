csv_2_latex_table.fn <- function(csv_file_name_w_path,tex_file_name_w_path,caption_text,
                                 table_label = "Table1",
                                 include_row_headers = 0, text_justify_vert_line = 0,append_tex_file = FALSE,
                                 MultiLineInBox = FALSE, NCharInLineBox = 10, All_hlines = FALSE, Longtable = TRUE,
                                 First_col_font_type = "bold", Top_row_font_type = "bold") {
    
# load the data file of interest
table_data_step1 <- read.csv(csv_file_name_w_path, header = FALSE)
  
#### parse the table ####
  # isolate the column headers
  table_header <- table_data_step1[1,2:dim(table_data_step1)[2]]

# separate the data from the row headers, if there are row headers  
  if (include_row_headers==1) { # there are row headers
    row_headers <- table_data_step1[2:dim(table_data_step1)[1],1] # data frame with just row headers
    table_data <- table_data_step1[2:dim(table_data_step1)[1],2:dim(table_data_step1)[2]] # data with no headers (no row headers and no column headers)
  } else { # if (include_row_headers==1) { # there are row headers
    table_data <- table_data_step1[2:dim(table_data_step1)[1],]
  } # if (include_row_headers==1) { # there are row headers
  
  # how many rows?
  n_rows <- dim(table_data)[1]
  n_cols <- dim(table_data)[2]
  
#### top matter that defines number of columns, table style, caption, etc. ####
  # define justification for table (left, center, or right) and whether there are vertical lines between columns
    default_justification <- "l" #"c" # define left, center, or right justification: l, c, or r
    default_vert_line <- "|" # or blank space for no vertical line
    # put together string indicating table style
    if (text_justify_vert_line == 0) { # define default style 
      if (include_row_headers==1) { # first column of csv file consists of row headers
      justify_vert_line_string <- paste(default_justification,default_vert_line,sep = "")
      } # if (include_row_headers==1) { # first column of csv file consists of row headers
      for (this_col in 1:(n_cols-1)) { # cycle through columns and paste together string that defines the table style
        justify_vert_line_string <- paste(justify_vert_line_string,default_justification,default_vert_line,sep = "")
      } # for (this_col in 1:(n_cols-1)) { # cycle through columns and paste together string that defines the table style
      justify_vert_line_string <- paste(justify_vert_line_string,default_justification,sep = "") # last row doesn't get a vertical line at end
    } # if (text_justify_vert_line == 0) { # define default style 

# open/create tex file that the table is going into
  sink(file = tex_file_name_w_path, append = append_tex_file, type = c("output","message"), split = FALSE)
  
  # see example at https://www.latex-tutorial.com/tutorials/tables/
  
  #\documentclass{article}
  #\begin{document}
  
  if (Longtable == FALSE) {
  cat("\\begin{center} \n")
  cat("\\begin{table}[h!] \n") #[h!]
  cat(paste("\\caption{",caption_text,"}",sep = ""))
  cat(paste("\\label{tab:",table_label,"} \n",sep = ""))
  cat(paste("\\begin{tabular}{",justify_vert_line_string,"} \n",sep = ""))
  } else if (Longtable == TRUE) {
    cat(paste("\\begin{longtable}{",justify_vert_line_string,"} \\label{tab:",table_label,"} \n",sep = "")) 
  }
  
#### lay out the top row (column headers) ####
  font_type <- Top_row_font_type #"bold"
  if (include_row_headers==1) { # there are row headers
    cat(" & ")
    for (this_col in 2:n_cols-1) {
      this_cell <- table_header[1,this_col]
      cat(paste("",MultiLineInBox.fn(this_cell,NCharInLineBox,font_type)," & ",sep = ""))
      rm(this_cell)
    } # for (this_col
    cat(paste("\\textbf{",as.character(table_header[1,this_col+1]),"} \\\\ \n ",sep = ""))
  } else { # no row headers # if (include_row_headers==1) { # there are row headers
    stop("finish writing code for when there are now row headers")
  } # if (include_row_headers==1) { # there are row headers
rm(font_type)  
# horizontal line under header 
  cat("\\hline \n")

#### Output Body of Table ####  
  # lay out row headers (if applicable) and body of table
  NCharInLineBox_1st_col <- 30#42
  if (include_row_headers==1) { # there are row headers
     for (this_row in 1:n_rows) { # cycle through rows of table
       this_cell <- row_headers[this_row]
       font_type <- First_col_font_type #"bold"
       cat(paste(MultiLineInBox.fn(this_cell,NCharInLineBox_1st_col,font_type)," & ",sep = "")) # input row header
       rm(this_cell,font_type)
     #cat(paste("\\textbf{",as.character(row_headers[this_row]),"} & ",sep = "")) # input row header
   
      if (MultiLineInBox == FALSE) { # all content within a cell will be on 1 rows    
       for (this_col in 1:(n_cols-1)) { # cycle through columns within this row and input content into table (MultiLineInBox == FALSE)
        cat(paste("",as.character(table_data[this_row,this_col])," & ",sep = ""))
       } # for (this_col in 1:(n_cols-1)) { # cycle through columns within this row and input content into table (MultiLineInBox == FALSE)
        cat(paste("",as.character(table_data[this_row,this_col+1])," \\\\ \n ",sep = "")) # latex code at end of last column is slightly different
          if (All_hlines==TRUE) { # put horizontal line between each row
            # horizontal line under header 
            cat("\\hline \n")
            } # if (All_hlines==TRUE) { # put horizontal line between each row
       } else if (MultiLineInBox == TRUE) { # 
        for (this_col in 1:(n_cols-1)) { # cycle through columns within this row and input content into table (MultiLineInBox == TRUE)
         # what is the content for this cell?
           this_cell <- as.character(table_data[this_row,this_col])
           #print(this_cell)
         # how many characters in this cell?
           this_nchar <- nchar(this_cell)
         # how many rows will this divide into?
           this_n_small_rows_step <- this_nchar/NCharInLineBox
         # round up the number of small rows (within box)
           this_n_small_rows <- ceiling(this_n_small_rows_step)
          
         # figure out if the content for this cell is longer than what NCharInLineBox is set to, in that case it needs to be 
           # broken into multiple lines within the cell
           # but don't split up the lines that start with an uparrow or a downarrow
           if (this_nchar>NCharInLineBox & substr(this_cell,1,30) != "\\begin{math}\\uparrow\\end{math}" & substr(this_cell,1,32) != "\\begin{math}\\downarrow\\end{math}") { # the content is longer than NCharInLineBox
           cat(paste("\\begin{tabular}[c]{@{}l@{}}",sep = ""))
             # divide rows at a space
             string_breakpoints <- split_long_string_table_rows.fn(this_cell, NCharInLineBox)
             #print(string_breakpoints)
             
             text_counter_start <- 1 
             for (this_small_row in 1:(length(string_breakpoints))) { # cycle through the small rows within a cell and input content
               text_counter_stop <- string_breakpoints[this_small_row]
               #print(text_counter_stop)
               if (text_counter_stop>this_nchar) {stop("check code, don't try to put in more characters than there are")} # sanity check
              small_row_text <- substr(this_cell,text_counter_start,text_counter_stop)
              cat(paste(small_row_text,"\\\\",sep = ""))
              text_counter_start <- text_counter_stop+1
             } # for (this_small_row in 1:(length(string_breakpoints)+1)) { # cycle through the small rows within a cell and input content
              text_counter_stop <- this_nchar
              small_row_text <- substr(this_cell,text_counter_start,text_counter_stop)
              cat(paste(small_row_text,"\\end{tabular} &",sep = ""))
           #stop("pick up writing code, line 103")  
          #### Start OLD segment   
           #text_counter_start <- 1
           #for (this_small_row in 1:(this_n_small_rows-1)) { # cycle through the small rows within a cell and input content
          #    text_counter_stop <- text_counter_start+NCharInLineBox-1
          #   if (text_counter_stop>this_nchar) {text_counter_stop <- this_nchar} # don't try to put in more characters than there are
           #   small_row_text <- substr(this_cell,text_counter_start,text_counter_stop)
          #    cat(paste(small_row_text,"\\\\",sep = ""))
          #    text_counter_start <- text_counter_stop+1
          # } # for (this_small_row in 1:(this_n_small_rows-1)) { # cycle through the small rows within a cell and input content
           # text_counter_stop <- this_nchar
          #  small_row_text <- substr(this_cell,text_counter_start,text_counter_stop)
          #  cat(paste(small_row_text,"\\end{tabular} &",sep = ""))
          #### End OLD segment  
            
           } else { # content is shorter than NCharInLineBox, and doesn't need to be broken into multiple lines # if (this_nchar>NCharInLineBox) { # the content is longer than NCharInLineBox
             cat(paste("",as.character(table_data[this_row,this_col])," & ",sep = ""))
           } # if (this_nchar>NCharInLineBox) { # the content is longer than NCharInLineBox
        } # for (this_col in 1:(n_cols-1)) { # cycle through columns within this row and input content into table (MultiLineInBox == TRUE)
        
########################### last column        
           # what is the content for this cell?
           this_cell <- as.character(table_data[this_row,this_col+1])
           MultiLineInBox.fn(this_cell,NCharInLineBox,font_type = "Normal")
          # horizontal line under header 
          cat("\\\\ \n \\hline \n")
         #   # how many characters in this cell?
         #   this_nchar <- nchar(this_cell)
         #   # how many rows will this divide into?
         #   this_n_small_rows_step <- this_nchar/NCharInLineBox
         #   # round up the number of small rows (within box)
         #   this_n_small_rows <- ceiling(this_n_small_rows_step)
         #   # figure out if the content for this cell is longer than what NCharInLineBox is set to, in that case it needs to be 
         #   # broken into multiple lines within the cell
         #   if (this_nchar>NCharInLineBox) { # the content is longer than NCharInLineBox
         #     cat(paste("\\begin{tabular}[c]{@{}l@{}}",sep = ""))
         #     # divide rows at a space
         #     string_breakpoints <- split_long_string_table_rows.fn(this_cell, NCharInLineBox)
         #     #print(string_breakpoints)
         #     
         #     text_counter_start <- 1
         #     #for (this_small_row in 1:(this_n_small_rows-1)) { # cycle through the small rows within a cell and input content
         #       #text_counter_stop <- text_counter_start+NCharInLineBox-1
         #       for (this_small_row in 1:(length(string_breakpoints))) { # cycle through the small rows within a cell and input content
         #         text_counter_stop <- string_breakpoints[this_small_row]
         #         #print(text_counter_stop)
         #         if (text_counter_stop>this_nchar) {stop("check code, don't try to put in more characters than there are")} # sanity check
         #         small_row_text <- substr(this_cell,text_counter_start,text_counter_stop)
         #         cat(paste(small_row_text,"\\\\",sep = ""))
         #         text_counter_start <- text_counter_stop+1
         #       } # for (this_small_row in 1:(length(string_breakpoints)+1)) { # cycle through the small rows within a cell and input content
         #       text_counter_stop <- this_nchar
         #       small_row_text <- substr(this_cell,text_counter_start,text_counter_stop)
         #       #cat(paste(small_row_text,"\\end{tabular} &",sep = ""))
         #     
         #        #if (text_counter_stop>this_nchar) {text_counter_stop <- this_nchar} # don't try to put in more characters than there are
         #       #small_row_text <- substr(this_cell,text_counter_start,text_counter_stop)
         #       #cat(paste(small_row_text,"\\\\",sep = ""))
         #       #text_counter_start <- text_counter_stop+1
         #     #} # for (this_small_row in 1:(this_n_small_rows-1)) { # cycle through the small rows within a cell and input content
         #     text_counter_stop <- this_nchar
         #     small_row_text <- substr(this_cell,text_counter_start,text_counter_stop)
         #     cat(paste(small_row_text,"\\end{tabular} \\\\ ",sep = ""))
         #     if (All_hlines==TRUE) { # put horizontal line between each row
         #       # horizontal line under header 
         #       cat("\\hline \n")
         #     } # if (All_hlines==TRUE) { # put horizontal line between each row
         #   } else { # content is shorter than NCharInLineBox, and doesn't need to be broken into multiple lines # if (this_nchar>NCharInLineBox) { # the content is longer than NCharInLineBox
         #     cat(paste("",as.character(table_data[this_row,this_col+1])," \\\\ \n ",sep = ""))
         #     if (All_hlines==TRUE) { # put horizontal line between each row
         #       # horizontal line under header 
         #       cat("\\hline \n")
         #     } # if (All_hlines==TRUE) { # put horizontal line between each row
         #   } # if (this_nchar>NCharInLineBox) { # the content is longer than NCharInLineBox
         #  
         # 
         # 
         # #cat(paste("",as.character(table_data[this_row,this_col+1])," \\\\ \n ",sep = "")) # latex code at end of last column is slightly different
   
###########################
       } # if (MultiLineInBox == FALSE) { # all content within a cell will be on 1 rows  
     } # for (this_row in 1:n_rows) { # cycle through rows of table  
  } else { # no row headers # # if (include_row_headers==1) { # there are row headers
     stop("finish writing code for when there are no row headers")
  } # if (include_row_headers==1) { # there are row headers
  
#### Bottom matter that closes table ####    
  if (Longtable == FALSE) {
  cat("\\end{tabular} \n")
  cat("\\end{table} \n")
  cat("\\end{center} \n")
  } else if (Longtable == TRUE) {
    cat("\\end{longtable} \n") 
  }
    
    
  #\end{document}
  sink()
} # function