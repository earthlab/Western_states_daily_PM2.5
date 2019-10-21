MultiLineInBox.fn <- function(this_cell,NCharInLineBox,font_type) {
 # this_cell <- table_header[1,this_col]
    this_cell <- as.character(this_cell)
    #print(this_cell)
    this_nchar <- nchar(this_cell)
    broken_string <- strsplit(this_cell,c("\\{","\\}"))
    if (length(broken_string[[1]]) > 0) {
    this_nchar_broken <- max(nchar(broken_string[[1]]))
    } else {
      this_nchar_broken <- this_nchar
    }
    
      #this_nchar <- nchar(this_cell)
      #if (length(this_nchar) > 0) { # cell is not empty
      #if (is.null(dim(this_cell)[1])) { 
      if (this_nchar > 0) {
      # how many rows will this divide into?
      #this_n_small_rows_step <- this_nchar/NCharInLineBox
      this_n_small_rows_step <- this_nchar_broken/NCharInLineBox
      # round up the number of small rows (within box)
      this_n_small_rows <- ceiling(this_n_small_rows_step)

      # figure out if the content for this cell is longer than what NCharInLineBox is set to, in that case it needs to be 
      # broken into multiple lines within the cell
      # but don't split up the lines that start with an uparrow or a downarrow
      if (this_nchar_broken>NCharInLineBox & substr(this_cell,1,30) != "\\begin{math}\\uparrow\\end{math}" & substr(this_cell,1,32) != "\\begin{math}\\downarrow\\end{math}") { # the content is longer than NCharInLineBox
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
          
          #cat(paste(small_row_text,"\\\\",sep = ""))
          if (font_type == "bold") {
            cat(paste("\\textbf{",small_row_text,"} \\\\ ",sep = ""))
          } else {
            cat(paste(small_row_text,"\\\\",sep = ""))
          }
          
          
          text_counter_start <- text_counter_stop+1
        } # for (this_small_row in 1:(length(string_breakpoints)+1)) { # cycle through the small rows within a cell and input content
        text_counter_stop <- this_nchar
        small_row_text <- substr(this_cell,text_counter_start,text_counter_stop)
        #cat(paste(small_row_text,"\\end{tabular} &",sep = ""))
        
        if (font_type == "bold") {
        cat(paste("\\textbf{",small_row_text,"} \\end{tabular} ",sep = ""))
        } else {
          cat(paste(small_row_text,"\\end{tabular} ",sep = ""))
        }
        
      } else { # content is shorter than NCharInLineBox, and doesn't need to be broken into multiple lines # if (this_nchar>NCharInLineBox) { # the content is longer than NCharInLineBox
        #cat(paste("",as.character(table_data[this_row,this_col])," & ",sep = ""))
        #cat(paste("",this_cell," ",sep = ""))
        
        if (font_type == "bold") {
          cat(paste("\\textbf{",this_cell,"} ",sep = ""))
        } else {
          cat(paste(this_cell," ",sep = ""))
        }
        
      } # if (this_nchar>NCharInLineBox) { # the content is longer than NCharInLineBox
      } # if (length(this_nchar) > 0) { # cell is not empty
} # end function