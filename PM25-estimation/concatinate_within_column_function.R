# function to concatinate different values within columns that are being merged.
# called in fill_in_aves_coloc_unique_PC_POC_MN.fn function

# var_interest <- "Units_of_Measure"
concatinate_within_column.fn <- function(var_interest, this_day_all_combined_true_dup) {
  if (length(unique(this_day_all_combined_true_dup[,var_interest]))>1) { # is there more than 1 value?
    unique_var_values <- unique(this_day_all_combined_true_dup[,var_interest])
    print(unique_var_values)
    for (Var_i in 1:length(unique_var_values)) { # loop through all values and paste them together
      if (Var_i==1) {
        all_Vars <- unique_var_values[Var_i]
      } else {
        all_Vars <- paste(all_Vars,unique_var_values[Var_i],sep = ", ")
      } # if (Var_i==1) {
    } # for (Var_i in 1:dim(unique_var_values)[1]) {
  } else { # if (length(unique(this_day_all_combined_true_dup[,var_interest]))>1) { # is there more than 1 value?
    all_Vars <- unique(unique_var_values[,var_interest])
  } # if (length(unique(this_day_all_combined_true_dup[,var_interest]))>1) { # is there more than 1 value?
  print(all_Vars)
  return(all_Vars)
  
  #input_mat4_aves[rstart_aves:rstop_aves,c(var_interest)] <- all_Vars # input composite of data

} # function