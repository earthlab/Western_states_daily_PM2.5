extract_bib_symbol.fn <- function(this_row_key) {
  
  this_row_nchar <- nchar(this_row_key) # how many characters are in this_row_key?
  
  # find the bibsymbol
  for (char_i in this_row_nchar:1) { # cycle through characters in row one at a time
    this_char <- substr(this_row_key,char_i,char_i) # what is the character?
    #print(this_char)
    if (this_char =="{") { # open brackets is the last symbol before the bib symbol
      start_bib_symbol <- char_i+1 # add 1 to get the start of the bib symbol
      break # don't continue after finding symbol
    } # if (this_char =="{") { # open brackets is the last symbol before the bib symbol
    
    #if (this_char == "}") { # closed brackets is the first symbol after the bib symbol
    #  stop_bib_symbol <- char_i - 1 # subtract 1 to get the end of the bib symbol
    #  break # don't continue on to the second set of brackets
    #} # if (this_char == "}") { # closed brackets is the first symbol after the bib symbol
    
    stop_bib_symbol <- this_row_nchar-1
    
  } # for (char_i in 1:this_row_nchar) { # cycle through characters in row one at a time
  bib_symbol <- substr(this_row_key,start_bib_symbol,stop_bib_symbol) # get the bib symbol
  
  return(bib_symbol) # output bib_symbol from function
} # end function