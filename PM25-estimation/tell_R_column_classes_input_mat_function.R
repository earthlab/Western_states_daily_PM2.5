tell_R_column_classes_input_mat.fn <- function(input_mat) {
  
  #### Tell R which columns to recognize as factors ####
  
  print('structure: str() of input_mat')
  str(input_mat)
  
  # indicate which column should be interpreted as dates
  input_mat$Date_Local <- as.Date(input_mat$Date_Local,"%Y-%m-%d")
  
  input_mat$State_Code <- factor(input_mat$State_Code) # state code should be a factor variable
  
  input_mat$County_Code <- factor(input_mat$County_Code) # county code should be a factor variable
  
  input_mat$Site_Num <- factor(input_mat$Site_Num) # Site number should be a factor variable
  
  input_mat$Parameter_Code <- factor(input_mat$Parameter_Code) # Parameter code should be a factor variable
  
  input_mat$Winter <- factor(input_mat$Winter) # winter is a categorical variable, so it should be a factor
  
  input_mat$Month <- factor(input_mat$Month) # month should be a categorical variable, so it should be a factor
  
  input_mat$Day <- factor(input_mat$Day) # month should be a categorical variable, so it should be a factor
  
  input_mat$l.m.Ave..Air.Flw <- as.numeric(input_mat$l.m.Ave..Air.Flw) # air flow should be numerical
  
  input_mat$Deg.C.Av.Air.Temp <- as.numeric(input_mat$Deg.C.Av.Air.Temp) # temperature should be numerical
  
  print('fill in the data type for the rest of the variables')
  
  #output_list <- list(input_mat4_aves,rstart_aves,input_mat4_colocated,rstart_colocated)   # return value 
  #return(output_list)
  return(input_mat)
  
} # function
  