# ML_PM25_estimation_step0a.R
# plot input merged input file

#### For new data ####
# Define columns to keep 
predictor_variables <- c("Date","Latitude","Longitude", "A_100" , "C_100","Both_100", "A_250","C_250","Both_250","A_500",               
                         "C_500","Both_500","A_1000","C_1000","Both_1000","AOD","MAIAC_AOD",          
                         "HPBL.surface","TMP.2.m.above.ground","RH.2.m.above.ground", "DPT.2.m.above.ground","APCP.surface","WEASD.surface", 
                         "SNOWC.surface","UGRD.10.m.above.ground","VGRD.10.m.above.ground", "PRMSL.mean.sea.level", "PRES.surface","DZDT.850.mb",      
                         "DZDT.700.mb", "elevation","NLCD")

predictor_variables <- c("Date","")

print(predictor_variables)
col_name_interest <- "PM2.5_Obs" #"logpm25"
# Load input file
this_source_file <- "ML_input_PM25_Step5_part_d_de_duplicated_aves_ML_input.csv"
sub_folder <- "ML_input_files"
Full_PM25_obs_extra_cols_and_NA<-read.csv(file.path(ProcessedData.directory,sub_folder,this_source_file),header=TRUE) # load the AQS file
# Get rid of extra columns and rows with NA
Full_PM25_obs_w_NA <- Full_PM25_obs_extra_cols_and_NA[ ,c(col_name_interest,predictor_variables)]
rm(Full_PM25_obs_extra_cols_and_NA)
Full_PM25_obs <- Full_PM25_obs_w_NA[complete.cases(Full_PM25_obs_w_NA), ]

# Set classes of columns
Full_PM25_obs$Date <- as.Date(Full_PM25_obs$Date,"%Y-%m-%d") # recognize dates as dates: 'Date_Local' 

##### create report ####
#with plots/maps about the input data, consider removing any columns that have nearly constant values
print("create report with plots/maps about the input data, consider removing any columns that have nearly constant values")

file_sub_label <- paste("ML_input_report_",substr(this_source_file, 1, (nchar(this_source_file)-4)),sep = "") # file partial name, decide whether to include date in file name
print(file_sub_label)
title_string_partial <- "ML Inputs Time Series"
## set up documentation files/variables
LatexFileName=file.path(output.directory,paste("Rgenerated_",file_sub_label,"Images.tex",sep = "")) # Start file for latex code images
LaTex_code_start_subsection.fn(LatexFileName, title_string = title_string_partial, append_option = FALSE) # start subsection for latex code
sink.number()

#,predictor_variables
df_report.fn(df = Full_PM25_obs, cols_interest = c(col_name_interest,predictor_variables), x_axis_var = "Date", output.directory = output.directory,
             output.directory.short = output.directory.short, file_sub_label = file_sub_label, title_string_partial = title_string_partial, plot_color = "black",
             LatexFileName = LatexFileName, SinkFileName = NA)

title_string_partial <- "ML Inputs Plot against PM2.5"
LaTex_code_start_subsection.fn(LatexFileName, title_string = title_string_partial, append_option = TRUE) # start subsection for latex code

#,predictor_variables
df_report.fn(df = Full_PM25_obs, cols_interest = c(predictor_variables), x_axis_var = col_name_interest, output.directory = output.directory,
             output.directory.short = output.directory.short, file_sub_label = file_sub_label, title_string_partial = title_string_partial, plot_color = "black",
             LatexFileName = LatexFileName, SinkFileName = NA)


SinkFileName=file.path(ProcessedData.directory,paste(file_sub_label,".txt",sep = "")) # file name
sink(file =SinkFileName, append = FALSE, type = c("output","message"), split = FALSE) # start output to text file

