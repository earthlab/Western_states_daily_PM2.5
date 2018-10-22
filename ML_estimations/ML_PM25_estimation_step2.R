# ML_PM25_estimation_step2.R - make predictions of PM2.5 from trained models created in step1

# load files created in ML_PM25_estimation_step1.R, which trained the model

# set up parallel code again

# in the wrapper that cycles through models:
# make predictions with the data
PM25_prediction <- predict(this_model, PM25_obs_shuffled) # predict on the full data set
print('change code to make predictions on the locations of interest instead of locations of monitors')
