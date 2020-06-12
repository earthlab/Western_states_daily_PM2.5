# ML_PM25_estimation_step2.R - make predictions of PM2.5 from trained models created in step1

print("run Define_directories.R before this script") 

# start timer for code
start_code_timer <- proc.time()
print(paste("Start ML_PM25_estimation_step1.R at",Sys.time(),sep = " "))

#### Call Packages (Library) ####


#### Call Load Functions that I created ####
source(file.path(ML_Code.directory,"Plotting_and_LaTex_functions.R"))
Plotting_and_LaTex_fn_list <- c("Plot_to_ImageFile.fn", "Plot_and_latex.fn", "LaTex_code_4_figure.fn", "LaTex_code_start_subsection.fn")
source(file.path(writingcode.directory,"input_mat_functions.R"))
input_mat_functions <- c("input_mat_change_data_classes.fn", "input_mat_extract_year_from_date.fn",
                         "input_mat_extract_month_from_date.fn", "input_mat_extract_day_from_date.fn",
                         "fancy_which.fn", "subset_data_frame_via_vector.fn", "EPA_codes_2_components_no_hyphens.fn")

#### define constants and variables needed for all R workers ####
this_source_file <- "ML_input_CountyGeometricCentroids_Locations_Dates_part_c_2008-01-01to2008-12-31.csv"
sub_folder <- "ML_input_files"

file_sub_label <- paste("ML_input_report_",substr(this_source_file, 1, (nchar(this_source_file)-4)),sep = "") # file partial name, decide whether to include date in file name
title_string_partial <- "ML Inputs Time Series (Counties)"

#### Set up Documentation ####
LatexFileName=file.path(output.directory,paste("Rgenerated_",file_sub_label,"Images.tex",sep = "")) # Start file for latex code images
LaTex_code_start_subsection.fn(LatexFileName, title_string = title_string_partial, append_option = FALSE) # start subsection for latex code
sink.number()


#### Load Data ####
Predictor_data<-read.csv(file.path(ProcessedData.directory,sub_folder,this_source_file),header=TRUE) # load the AQS file
predictor_variables <- c("Date","Latitude","Longitude", "A_100" , "C_100","Both_100", "A_250","C_250","Both_250","A_500",               
                         "C_500","Both_500","A_1000","C_1000","Both_1000","AOD","MAIAC_AOD",          
                         "HPBL.surface","TMP.2.m.above.ground","RH.2.m.above.ground", "DPT.2.m.above.ground","APCP.surface","WEASD.surface", 
                         "SNOWC.surface","UGRD.10.m.above.ground","VGRD.10.m.above.ground", "PRMSL.mean.sea.level", "PRES.surface","DZDT.850.mb",      
                         "DZDT.700.mb", "elevation","NLCD")

image_format <- "jpg" #"pdf" #"png" 

#### Create report of predictor_variables ####
df_report.fn(df = Predictor_data, cols_interest = c(predictor_variables), x_axis_var = "Date", output.directory = output.directory,
             output.directory.short = output.directory.short, file_sub_label = file_sub_label, title_string_partial = title_string_partial, plot_color = "black",
             LatexFileName = LatexFileName, SinkFileName = NA, image_format = image_format)


# # load files created in ML_PM25_estimation_step1.R, which trained the model
# 
# # set up parallel code again
# 
# # in the wrapper that cycles through models:
# # make predictions with the data
# PM25_prediction <- predict(this_model, PM25_obs_shuffled) # predict on the full data set
# print('change code to make predictions on the locations of interest instead of locations of monitors')
# 
# 
# ### Calculation: predicted PM2.5
# this_model_output$fit # display fit information about this_model_output
# predicted<-predict(this_model_output,newdata=FinalInputData[,c(9,10,23,25:30,32,34,36,38,39,41,43,58:61,63,64,67,70:75)]) # find predictions based on the rfe fit. https://www.rdocumentation.org/packages/pathClass/versions/0.9.4/topics/predict.rfe
# # predicted is in log scale of PM2.5
# 
# plot(x=FinalInputData$logpm25,y=predicted,xlim=c(0,7),ylim=c(0,7))
# abline(0,1) # add a straight line to plot; https://www.rdocumentation.org/packages/graphics/versions/3.4.3/topics/abline
# rsq<-this_model_output$results[which(this_model_output$results$RMSE==min(this_model_output$results$RMSE)),3]
# text(labels=paste("R-squared=",round(rsq,4)),x=5,y=1)
# mtext(paste(this_model_run_name," Log Predicted vs Observed Plot",sep = ""),side=3)
# 
# ### Calculation: exp(predicted), i.e., undo the log in the predicted data
# this_model_output.pm25pred=exp(predicted)
# 
# plot(x=FinalInputData$Monitor_PM25,y=this_model_output.pm25pred,xlab="Observed PM2.5",ylab="Predicted PM2.5")
# abline(0,1) # add a straight line to plot; https://www.rdocumentation.org/packages/graphics/versions/3.4.3/topics/abline
# rsq<-this_model_output$results[which(this_model_output$results$RMSE==min(this_model_output$results$RMSE)),3]
# text(labels=paste("R-squared=",round(rsq,4)),x=250,y=50)
# mtext(side=3,text=paste(this_model_run_name_display," Predicted vs Expected Plot - Regular Scale",sep = ""))
# 
# ### Calculation: calculate residuals (in log-land)
# this_model_output.resids<-FinalInputData[,52]-predicted
# 
# plot(predicted,this_model_output.resids,ylab="Residuals",xlab="logged PM2.5")
# abline(h=0)
# mtext(side=3,text=paste(this_model_run_name_display," Residual Plot",sep = ""))
# 
# BlandAltman(x=FinalInputData$logpm25,y=predicted, gui=FALSE, bandsOn=TRUE, biasOn=FALSE, regionOn=FALSE, smooth=FALSE, sig=2,
#             main="Random Forest Bland-Altman plot")
# 
# 
