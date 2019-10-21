check_all_entries_have_key.fn <- function(bu_file, bu_key) {
  for (row_i in 1:dim(bu_file)[1]) { # Cycle through bib items and output text into new bu.aux file
    # find the bib item name in this row of the numbered bu file
    this_row_orig <- as.character(bu_file[row_i,1])
    #print(this_row_orig)
    
    if (this_row_orig == " ") {
      break
    }
    
    bib_item <- extract_bib_item_name.fn(this_row_orig)
    #print(bib_item)
    
    # what is the number for this bib item?
    bib_number <- extract_bib_symbol.fn(this_row_orig)
    #print(bib_number)
    N_matches <- 0
    # find the row with the same bib item in the symbol key file
    for (row_key_i in 1:dim(bu_key)[1]) { # cycle through rows of key to find the matching bib item
      this_row_key <- as.character(bu_key[row_key_i,1]) 
      #print(this_row_key) 
      
      bib_key <- extract_bib_item_name.fn(this_row_key)
      #print(bib_key)
      
      # do the two bib items match? If so, what is the symbol for this bib item?
      if (bib_item == bib_key) {
        N_matches <- N_matches + 1
        #print("match found")
        #bib_symbol <- extract_bib_symbol.fn(this_row_key)
        #print(bib_symbol)
      }
    } # for (row_key_i in 1:dim(bu_key)) { # cycle through rows of key to find the matching bib item
    
    # print a new bu.aux file with both the symbols and numbers
    #cat(paste("\\bibcite{",as.character(bib_item),"}{",as.character(bib_number),as.character(bib_symbol),"} \n ",sep = ""))
    #cat(paste("\\bibcite{",bib_item,bib_number,bib_symbol,sep = ""))
    
    if (N_matches != 1) {
      print(bib_item)
      error("bib_item not found in key file")
    }
    
  } # for (row_i in 1:dim(bu_file)[1]) { # Cycle through bib items and output text into new bu.aux file
  
} # end function