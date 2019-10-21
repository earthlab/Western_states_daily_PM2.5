split_long_string_table_rows.fn <- function(this_cell, NCharInLineBox) {
  
  # how many characters in this cell?
  this_nchar <- nchar(this_cell)
  broken_string <- strsplit(this_cell,c("\\{","\\}"))
  if (length(broken_string[[1]]) > 0) {
    this_nchar_broken <- max(nchar(broken_string[[1]]))
  } else {
    this_nchar_broken <- this_nchar
  }
  # how many rows will this divide into?
  #this_n_small_rows_step <- this_nchar/NCharInLineBox
  this_n_small_rows_step <- this_nchar_broken/NCharInLineBox
  # round up the number of small rows (within box)
  this_n_small_rows <- ceiling(this_n_small_rows_step)
  
  text_counter_start <- 1
  break_flag <- 0
  text_counter_stop_vec <- NA
  counter_vec <- NCharInLineBox
  for (i in 2:this_n_small_rows) {
    counter_vec <- c(counter_vec,i*NCharInLineBox)
  }
 #counter_vec <- c(NCharInLineBox:NCharInLineBox:this_nchar)
  
  #print(counter_vec)
  for (text_anchor in counter_vec) { # find break points within each long string
   # print(text_anchor)
  #} (text_counter_start < this_nchar) {
  #  text_anchor <- text_counter_start + NCharInLineBox - 1
    anchor_char <- substr(this_cell,text_anchor,text_anchor)
    #print(anchor_char)
    
    if (anchor_char == " ") { # find blank space
      text_counter_stop <- text_anchor
      text_counter_stop_vec <- c(text_counter_stop_vec,text_counter_stop)
    } else { # look for the closest blank space
      #for (text_counter_test_i in 1:NCharInLineBox) { # look through characters slightly before or after the text_anchor to find a blank space
      check_this_far_out <- ceiling(NCharInLineBox/1)
      for (text_counter_test_i in 1:check_this_far_out) { # look through characters slightly before or after the text_anchor to find a blank space  
        test_text_pos <- text_anchor + text_counter_test_i
        if (test_text_pos <= this_nchar) {
        test_text_pos_char <- substr(this_cell,test_text_pos,test_text_pos)
     #   print(test_text_pos_char)
        if (test_text_pos_char == " ") { # check if this character is a blank space
          text_counter_stop <- test_text_pos
          text_counter_stop_vec <- c(text_counter_stop_vec,text_counter_stop)
          #print(text_counter_stop_vec)
          break_flag <- 1
          #break
          #stop("finish code - pos text_counter_test_i"
          }
        } else { # try slightly shorter string to find a blank space # if (test_text_pos_char == " ") { # check if this character is a blank space
          test_text_neg <- text_anchor - text_counter_test_i
          test_text_neg_char <- substr(this_cell,test_text_neg,test_text_neg)
      #    print(test_text_neg_char)
          if (test_text_neg_char == " ") { # slightly shorter worked to find a blank space
            text_counter_stop <- test_text_neg
            text_counter_stop_vec <- c(text_counter_stop_vec,text_counter_stop)
       #     print(text_counter_stop_vec)
            break_flag <- 1
            #print("break flag <- 1")
            #break
            #stop("finish code - neg text_counter_test_i")
          } # if (test_text_neg_char == " ") { # slightly shorter worked to find a blank space
          #break
        } # if (test_text_pos_char == " ") { # check if this character is a blank space
        #text_counter_start <- text_counter_stop + NCharInLineBox - 1
        #print(text_counter_start)
        if (break_flag == 1) {
          break_flag <- 0
          break
        } # if (break_flag == 1) {
        
      } # for (text_counter_test_i in 1:NCharInLineBox) { # look through characters slightly before or after the text_anchor to find a blank space
      #print(text_counter_stop_vec)
      
    } # if (anchor_char == " ") { # find blank space
    
   # stop("look at output")
  } # for (text_i in counter_vec) { # find break points within each long string
  
  ## minor adjustments 
  string_breakpoints_step <- text_counter_stop_vec[2:length(text_counter_stop_vec)] # get rid of NA at beginning that was used to initialize vector
  string_breakpoints_step2 <- unique(string_breakpoints_step) # get rid of any repeats
  
  # how long is the second to last line?
  second_last_line <- string_breakpoints_step2[length(string_breakpoints_step2)] - string_breakpoints_step2[length(string_breakpoints_step2)-1]
  #print(second_last_line)
  
  # how long is the last line?
  last_line_length <-  this_nchar-string_breakpoints_step2[length(string_breakpoints_step2)]
  #print(last_line_length)
  
  
  # if the length of the second to last line and the last line are shorter than the allowed row length (NCharInLineBox), then combine them
  # but only if length(string_breakpoints_step2)>1
  if (length(string_breakpoints_step2)>1) {
  if (second_last_line + last_line_length < NCharInLineBox) {
    string_breakpoints <- string_breakpoints_step2[1:length(string_breakpoints_step2)-1]
  #  print(string_breakpoints)
  } else {
    string_breakpoints <- string_breakpoints_step2 # leave as is
  }
  } else {
    string_breakpoints <- string_breakpoints_step2 # leave as is
  }
  
  # if no reasonable breakpoints could be found, and string_breakpoints is NA, then just keep the full length in one row.
  if (length(string_breakpoints)==1) { # ignore if length is greater than 1
  if (is.na(string_breakpoints)) {
    string_breakpoints <- this_nchar
  } # if (is.na(string_breakpoints)) {
  } # if (length(string_breakpoints)==1) { # ignore if length is greater than 1
    
  return(string_breakpoints)
} # function