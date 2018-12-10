State_Abbrev_Definitions.fn <- function(state_number) {
# define state abbreviations in function
  if (state_number == 4) {
    state_abbrev <- "AZ"
  } else if (state_number == 6) {
    state_abbrev <- "CA"
  } else if (state_number == 8) {
    state_abbrev <- "CO"
  } else if (state_number == 16) {
    state_abbrev <- "ID"
  } else if (state_number == 20) {
    state_abbrev <- "KS"
  } else if (state_number == 30) {
    state_abbrev <- "MT"
  } else if (state_number == 31) {
    state_abbrev <- "NE"
  } else if (state_number == 32) {
    state_abbrev <- "NV"
  } else if (state_number == 35) {
    state_abbrev <- "NM"
  } else if (state_number == 38) {
    state_abbrev <- "ND"
  } else if (state_number == 40) {
    state_abbrev <- "OK"
  } else if (state_number == 41) {
    state_abbrev <- "OR"
  } else if (state_number == 46) {
    state_abbrev <- "SD"
  } else if (state_number == 48) {
    state_abbrev <- "TX"
  } else if (state_number == 49) {
    state_abbrev <- "UT"
  } else if (state_number == 53) {
    state_abbrev <- "WA"
  } else if (state_number == 56) {
    state_abbrev <- "WY"
  } else {
    state_abbrev <- "--"
  }
  return(state_abbrev)
} # end function

StateCode2StateName.fn <- function(state_number) {
  if (is.na(state_number)) {
    state_name <- NA
  } else if (state_number == 4) {
    state_name <- "Arizona"
  } else if (state_number == 6) {
    state_name <- "California"
  } else if (state_number == 8) {
    state_name <- "Colorado"
  } else if (state_number == 16) {
    state_name <- "Idaho"
  } else if (state_number == 20) {
    state_name <- "Kansas"
  } else if (state_number == 30) {
    state_name <- "Montana"
  } else if (state_number == 31) {
    state_name <- "Nebraska"
  } else if (state_number == 32) {
    state_name <- "Nevada"
  } else if (state_number == 35) {
    state_name <- "New Mexico"
  } else if (state_number == 38) {
    state_name <- "North Dakota"
  } else if (state_number == 40) {
    state_name <- "Oklahoma"
  } else if (state_number == 41) {
    state_name <- "Oregon"
  } else if (state_number == 46) {
    state_name <- "South Dakota"
  } else if (state_number == 48) {
    state_name <- "Texas"
  } else if (state_number == 49) {
    state_name <- "Utah"
  } else if (state_number == 53) {
    state_name <- "Washington"
  } else if (state_number == 56) {
    state_name <- "Wyoming"
  } else {
    state_name <- "--"
  }
  #print("finished StateCode2StateName.fn")
  return(state_name)
} # end of StateCode2StateName.fn function

StateAbbrev2StateCode.fn <- function(StateAbbrev_vec) {
  #StateAbbrev_vec <- study_states_abbrev # example input
  all_state_abbrevs_in <- unique(StateAbbrev_vec)
  #all_state_abbrevs_out <- all_state_abbrevs_in
  StateNum_vec <- StateAbbrev_vec
  StateNum_vec[] <- NA
  for (state_abbrev in all_state_abbrevs_in) {
    #print(state_abbrev)
    if (state_abbrev == "AZ") {
      state_number <-  4
    } else if (state_abbrev == "CA") {
      state_number <-  6
    } else if (state_abbrev == "CO") {
      state_number <-  8
    } else if (state_abbrev == "ID") {
      state_number <-  16
    } else if (state_abbrev == "KS") {
      state_number <-  20
    } else if (state_abbrev == "MT") {
      state_number <-  30
    } else if (state_abbrev == "NE") {
      state_number <-  31
    } else if (state_abbrev == "NV") {
      state_number <-  32
    } else if (state_abbrev == "NM") {
      state_number <-  35
    } else if (state_abbrev == "ND") {
      state_number <-  38
    } else if (state_abbrev == "OK") {
      state_number <-  40
    } else if (state_abbrev == "OR") {
      state_number <-  41
    } else if (state_abbrev == "SD") {
      state_number <-  46
    } else if (state_abbrev == "TX") {
      state_number <-  48
    } else if (state_abbrev == "UT") {
      state_number <-  49
    } else if (state_abbrev == "WA") {
      state_number <-  53
    } else if (state_abbrev == "WY") {
      state_number <-  56
    } else {
      #state_abbrev <- "--"
      state_number <- NA
    } # if (state_abbrev == "AZ") {
    #print(state_number)
    which_this_abbrev <- which(StateAbbrev_vec == state_abbrev)
    StateNum_vec[which_this_abbrev] <- state_number
  } # for (this_state_abbrev in all_state_abbrevs_in) {
  return(StateNum_vec)
} # end of StateAbbrev2StateCode.fn function

# fill in State names, given state codes
fill_in_StateNames_from_Code.fn <- function(input_data_frame,state_code_col,state_name_col) {
  # input_data_frame <- input_mat1 # example input
  # state_code_col <- "State_Code" # example input
  # state_name_col <- "State_Name" # example input
  
  # put in state names
  all_state_codes <- unique(input_data_frame[,state_code_col]) # what state codes are in the data?
  #print(all_state_codes)
  for (state_code in all_state_codes) { # cycle through all of the state codes found in the data
    #print(state_code)
    state_rows <- which(input_data_frame[,state_code_col] == state_code) # which rows are for this state?
    #print(length(state_rows))
    state_name <- StateCode2StateName.fn(state_code) # what is the name for this state?
    #print(state_name)
    input_data_frame[state_rows,state_name_col] <- state_name # fill in the state names in the rows for this state
  } # for (state_code in all_state_codes) { # cycle through all of the state codes found in the data
  #print("finished fill_in_StateNames_from_Code.fn")
  return(input_data_frame) # output from function
} # end of fill_in_StateNames_from_Code.fn function

