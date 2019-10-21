extract_bib_item_name.fn <- function(this_row_orig) {
  
  this_row_nchar <- nchar(this_row_orig) # how many characters are in this_row_orig?
  
  # find the bibitem name
  for (char_i in 1:this_row_nchar) { # cycle through characters in row one at a time
    this_char <- substr(this_row_orig,char_i,char_i) # what is the character?
    #print(this_char)
    if (this_char =="{") { # open brackets is the last symbol before the bib item name
      start_bib_item <- char_i+1 # add 1 to get the start of the bib item name
    } # if (this_char =="{") { # open brackets is the last symbol before the bib item name
    
    if (this_char == "}") { # closed brackets is the first symbol after the bib item name
      stop_bib_item <- char_i - 1 # subtract 1 to get the end of the bib item name
      break # don't continue on to the second set of brackets
    } # if (this_char == "}") { # closed brackets is the first symbol after the bib item name
    
  } # for (char_i in 1:this_row_nchar) { # cycle through characters in row one at a time
  bib_item <- substr(this_row_orig,start_bib_item,stop_bib_item) # get the bib item name
  
  return(bib_item) # output bib_item from function
} # end function