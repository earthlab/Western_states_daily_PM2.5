# separate_character_vec_at_comma_function.R
separate_character_vec_at_comma.fn <- function(input_vec) {
  #input_vec <- meta2_this_lat_lon
  #print(input_vec)
  input_vec_char <- as.character(input_vec)
  #print(input_vec_char)
  
  # pre-set values for output variables in case there is no comma
  this_pre_comma <- NA
  this_post_comma <- NA
  
  for (this_letter_i in 1:nchar(input_vec_char)) {
  #this_letter <- input_vec_char[this_letter_i]
  # what is the current letter?
  this_letter <- substr(input_vec_char, this_letter_i, this_letter_i)
  #print(this_letter) 
  # this this letter a comma?
  if (this_letter == ",") {
    #print("found the comma")
    comma_letter_number <- this_letter_i
    this_pre_comma <- substr(input_vec_char,1,comma_letter_number-1)
    #print(this_pre_comma)
    this_post_comma <- substr(input_vec_char,comma_letter_number+1,nchar(input_vec_char))
    #print(this_post_comma)
  } # if
  } # for (this_letter_i in 1:length(input_vec_char)) {   

  # output result out of function
  output_list <- list(this_pre_comma,this_post_comma)
  return(output_list)
}

is_there_a_space.fn <- function(input_char) {
  #input_char <- "33 34 60"
  #print(input_char)
  # pre-set values for output variables in case there is no space
  is_there_space <- 0 # default value is no
  for (this_letter_i in 1:nchar(input_char)) {
    # what is the current letter?
    this_letter <- substr(input_char, this_letter_i, this_letter_i)
    #print(this_letter) 
    if (this_letter == " ") { # this this letter a space?
      #print("found a space")
      is_there_space <- 1
    } # if (this_letter == " ") { # this this letter a space?
  } # for (this_letter_i in 1:length(input_vec_char)) {   
  
  # output result out of function
  output_list <- is_there_space
  return(output_list)
} # end of is_there_a_space.fn function
