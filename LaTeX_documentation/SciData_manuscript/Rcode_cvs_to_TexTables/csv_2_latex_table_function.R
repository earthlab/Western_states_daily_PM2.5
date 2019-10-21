csv_2_latex_table.fn <- function(csv_file_name_w_path,tex_file_name_w_path,caption_text,
                                 table_label = "Table1",
                                 text_justify_vert_line = 0,append_tex_file = FALSE, # include_row_headers = 0, 
                                 MultiLineInBox = FALSE, NCharInLineBox_1st_Col = 20, NCharInLineBox = 30, All_hlines = FALSE, Longtable = TRUE,
                                 First_col_font_type = "bold", Top_row_font_type = "bold", Main_text_font_type = "Normal", font_justification = "l",
                                 vert_line = "|", horiz_line = TRUE) {
  
  # Variables
  # caption_text only for non-longtables, code could be expanded
  # font_justification <- "l" # or "c" # define justification for table (left, center, or right) and whether there are vertical lines between columns
  # vert_line <- "|" # or blank space for no vertical line
  # 
  
  # load the data file of interest
  table_data <- read.csv(csv_file_name_w_path, header = FALSE)
  
  # how many rows?
  n_rows <- dim(table_data)[1]
  n_cols <- dim(table_data)[2]
  
  # top matter that defines number of columns, table style, caption, etc. 

  # # put together string indicating table style
  # if (text_justify_vert_line == 0) { # define default style 
  #   #if (include_row_headers==1) { # first column of csv file consists of row headers
  #   #  justify_vert_line_string <- paste(font_justification,vert_line,sep = "")
  #   justify_vert_line_string <- paste(vert_line,font_justification,vert_line,sep = "")
  #   #} # if (include_row_headers==1) { # first column of csv file consists of row headers
  #   for (this_col in 1:(n_cols-1)) { # cycle through columns and paste together string that defines the table style
  #     justify_vert_line_string <- paste(justify_vert_line_string,font_justification,vert_line,sep = "")
  #   } # for (this_col in 1:(n_cols-1)) { # cycle through columns and paste together string that defines the table style
  #   justify_vert_line_string <- paste(justify_vert_line_string,font_justification,sep = "") # last row doesn't get a vertical line at end
  # } # if (text_justify_vert_line == 0) { # define default style 
  
  if (text_justify_vert_line == 0) { # define default style 
    justify_vert_line_string <- paste(font_justification,vert_line,sep = "")
    #} # if (include_row_headers==1) { # first column of csv file consists of row headers
    for (this_col in 1:(n_cols-2)) { # cycle through columns and paste together string that defines the table style
      #justify_vert_line_string <- paste(justify_vert_line_string,font_justification,vert_line,sep = "")
      justify_vert_line_string <- paste(justify_vert_line_string,font_justification,vert_line,sep = "")
    } # for (this_col in 1:(n_cols-1)) { # cycle through columns and paste together string that defines the table style
    justify_vert_line_string <- paste(justify_vert_line_string,font_justification,sep = "") # last row doesn't get a vertical line at end
  } # if (text_justify_vert_line == 0) { # define default style 
  
  
  # open/create tex file that the table is going into
  sink(file = tex_file_name_w_path, append = append_tex_file, type = c("output","message"), split = FALSE)
  
  if (Longtable == FALSE) {
    cat("\\begin{center} \n")
    cat("\\begin{table}[h!] \n") #[h!]
    cat(paste("\\caption{",caption_text,"}",sep = ""))
    cat(paste("\\label{tab:",table_label,"} \n",sep = ""))
    cat(paste("\\begin{tabular}{",justify_vert_line_string,"} \n",sep = ""))
  } else if (Longtable == TRUE) {
    cat(paste("\\begin{longtable}{",justify_vert_line_string,"} \\caption{",caption_text,"} \\label{tab:",table_label,"} \\\\ \n",sep = "")) 
  }
  if (horiz_line == TRUE) { # put a horizontal line?
    cat("\\hline \n") # horizontal line
  } # if (horiz_line == TRUE) { # put a horizontal line?
  # lay out content of table, including headers
  #if (horiz_line == TRUE) { # put a horizontal line?
  #  cat("\\hline \n") # horizontal line
  #} # if (horiz_line == TRUE) { # put a horizontal line?
  for (this_row in 1:n_rows) {
    for (this_col in 1:(n_cols-1)) {
      if (this_col == 1 & this_row ==1) { # 1st col
        font_type <- Top_row_font_type
        this_NCharInLineBox <- NCharInLineBox_1st_Col
      } else if (this_col > 1 & this_row ==1) {
        font_type <- Top_row_font_type
        this_NCharInLineBox <- NCharInLineBox
      } else if (this_col == 1 & this_row > 1) {
        font_type <- First_col_font_type
        this_NCharInLineBox <- NCharInLineBox_1st_Col
      } else {
        font_type <- Main_text_font_type
        this_NCharInLineBox <- NCharInLineBox
      } # if (this_row == 1) { # 1st col      
      
      this_cell <- as.character(table_data[this_row,this_col])
      #print(this_col)
      #print(this_row)
      #print(this_cell)
      cat(paste("",MultiLineInBox.fn(this_cell,this_NCharInLineBox,font_type)," & ",sep = ""))

      rm(this_cell)
    } # for (this_col
    #cat(paste("\\textbf{",as.character(table_data[1,this_col+1]),"} \\\\ \n ",sep = ""))
    cat(paste("",MultiLineInBox.fn(table_data[this_row,this_col+1],this_NCharInLineBox,font_type)," \\\\ \n ",sep = ""))
    if (horiz_line == TRUE) { # put a horizontal line?
      cat("\\hline \n") # horizontal line
    } # if (horiz_line == TRUE) { # put a horizontal line?
  } # for (this_row
  rm(font_type)  

  # Bottom matter that closes table  
  if (Longtable == FALSE) {
    cat("\\end{tabular} \n")
    cat("\\end{table} \n")
    cat("\\end{center} \n")
  } else if (Longtable == TRUE) {
    cat("\\end{longtable} \n") 
  }
  
  sink()    

} # end csv_2_latex_table.fn function