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