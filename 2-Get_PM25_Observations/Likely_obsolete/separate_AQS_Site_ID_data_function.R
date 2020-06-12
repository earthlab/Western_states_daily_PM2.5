separate_AQS_Site_ID_data.fn <- function(input_mat3) {
  
  # find the data that has AQS Site ID information
  which_AQS_site_ID <- which(!is.na(input_mat3$State_Code) & !is.na(input_mat3$County_Code) & !is.na(input_mat3$Site_Num))
  
  
  
  output_list <- list(input_mat4_aves,rstart_aves)   # return value 
  return(output_list)
} # end of separate_AQS_Site_ID_data.fn function