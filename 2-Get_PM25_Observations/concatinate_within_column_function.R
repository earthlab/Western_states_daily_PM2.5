# function to concatinate different values within columns that are being merged.
# called in Combine_true_replicates_R.fn function

# var_interest <- "Units_of_Measure"
concatinate_within_column.fn <- function(var_interest, this_day_all_combined_true_dup) {
  unique_var_values <- unique(this_day_all_combined_true_dup[,var_interest])
  if (length(unique(this_day_all_combined_true_dup[,var_interest]))>1) { # is there more than 1 value?
    #print(unique_var_values)
    for (Var_i in 1:length(unique_var_values)) { # loop through all values and paste them together
      if (Var_i==1) {
        all_Vars <- unique_var_values[Var_i]
      } else {
        all_Vars <- paste(all_Vars,unique_var_values[Var_i],sep = ", ")
      } # if (Var_i==1) {
    } # for (Var_i in 1:dim(unique_var_values)[1]) {
  } else { # if (length(unique(this_day_all_combined_true_dup[,var_interest]))>1) { # is there more than 1 value?
    all_Vars <- unique_var_values # unique(unique_var_values[,var_interest])
  } # if (length(unique(this_day_all_combined_true_dup[,var_interest]))>1) { # is there more than 1 value?
  return(all_Vars)
} # end of concatinate_within_column.fn function

concatinate_vector_of_strings.fn <- function(string_vector) {
  if (length(string_vector) == 1) { # check if there is more than one string
  concatinated_string <- string_vector
  } else { # if (length(string_vector) == 1) { # check if there is more than one string
    concatinated_string <- string_vector[1]
    for (this_entry_i in 2:length(string_vector)) { # append additional strings to first
      concatinated_string <- paste(concatinated_string,string_vector[this_entry_i],sep = "; ")
    } # for (this_entry_i in 2:length(string_vector)) { # append additional strings to first
  } # if (length(string_vector) == 1) { # check if there is more than one string
  return(concatinated_string)
} # end of concatonate_vector_of_strings.fn function
