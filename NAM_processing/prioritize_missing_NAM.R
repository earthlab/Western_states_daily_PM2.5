# prioritize missing NAM files to days with none of the 4 files

#NAM_folder <- "NAM_data"
#batch_date <- define_study_constants.fn("NAM_batch_date") #as.Date("2019-07-10") #as.Date("2019-04-05") # used to know whether data has already been ran. This


prioritize_missing_NAM.fn <- function(NAM_folder,batch_date){
# read in issues file  
ReportFileName_Attempt3=file.path(define_file_paths.fn("ProcessedData.directory"),NAM_folder,paste("Rgenerated_Report_NAM_Step2_Attempt3_issues_batch",batch_date,".csv",sep = "")) # name of file for latex code images
Issue_files3 <- read.csv(ReportFileName_Attempt3)

unique_day_counter <- unique(Issue_files3$day_counter)
print(paste("After attempt 3, there are still",dim(Issue_files3)[1],"files still missing from among",length(unique_day_counter),"days."))

lapply_output <- lapply(1:length(unique_day_counter),function(x){
  this_day_counter <- unique_day_counter[x]
  this_day_data <- Issue_files3[which(Issue_files3$day_counter == this_day_counter),]
  this_n_files_missing <- data.frame(matrix(NA,nrow=1,ncol=3)) # create data frame for input_mat1
  names(this_n_files_missing) <- c("Nmissing","day_counter","this_model.date")
  this_n_files_missing$Nmissing <- dim(this_day_data)[1]  
  this_n_files_missing$day_counter <- this_day_counter
  this_n_files_missing$this_model.date <- unique(this_day_data$this_model.date)
  return(this_n_files_missing)
})
  n_files_missing <- do.call("rbind", lapply_output) #concatinate the output from each iteration  

  write.csv(n_files_missing,file = file.path(define_file_paths.fn("ProcessedData.directory"),NAM_folder,paste("Rgenerated_Report_NAM_Step2_Attempt3_issues_batch",batch_date,"_Prioritize.csv",sep = "")),row.names = FALSE) # Write csv file
  
  for (files_missing in 4:1) {
    days_issues <- dim(n_files_missing[which(n_files_missing$Nmissing == files_missing),])[1]
    print(paste(days_issues,"days have ",files_missing,"files missing for the day (out of 4)."))
  }
  
  
} # end of prioritize_missing_NAM.fn function