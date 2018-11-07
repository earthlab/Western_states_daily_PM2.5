library(geosphere)
library(RColorBrewer)
library(classInt)

StateAbbrev2StateCode.fn <- function(StateAbbrev_vec) {
  #StateAbbrev_vec <- study_states_abbrev # example input
  all_state_abbrevs_in <- unique(StateAbbrev_vec)
  #all_state_abbrevs_out <- all_state_abbrevs_in
  StateNum_vec <- StateAbbrev_vec
  StateNum_vec[] <- NA
  for (state_abbrev in all_state_abbrevs_in) {
    print(state_abbrev)
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
    print(state_number)
    which_this_abbrev <- which(StateAbbrev_vec == state_abbrev)
    StateNum_vec[which_this_abbrev] <- state_number
  } # for (this_state_abbrev in all_state_abbrevs_in) {
  return(StateNum_vec)
} # end of StateAbbrev2StateCode.fn function

map_county_base_layer.fn <- function(CountyMaps.directory, study_states_abbrev, PM25) {

  # create map
  Countymap=readOGR(dsn=file.path(CountyMaps.directory),layer = "cb_2017_us_county_500k") # load county map shapefile
  Countymap$STATEFP_NUM <- as.numeric(as.character(Countymap$STATEFP)) # have R recognize state FP's as numerical values (in a new column)
  
  # this map is in NAD83, which can be verified with this command:
  summary(Countymap) # summarize data
  
  State_Num_vec <- StateAbbrev2StateCode.fn(StateAbbrev_vec = study_states_abbrev)
  #   # display the State FP values and state abbreviations next to each other
  #   
  #   # find the 11 western states included in the study
  WestCountymapGeom=Countymap[Countymap$STATEFP_NUM==4|Countymap$STATEFP_NUM==6|Countymap$STATEFP_NUM==8|Countymap$STATEFP_NUM==16|Countymap$STATEFP_NUM==30|Countymap$STATEFP_NUM==32|Countymap$STATEFP_NUM==35|Countymap$STATEFP_NUM==41|Countymap$STATEFP_NUM==49|Countymap$STATEFP_NUM==53|Countymap$STATEFP_NUM==56,]
  #print(WestUSmap)
  
  plotvar <- PM25
  class <- classIntervals(plotvar,
                          nclr,
                          style = "fixed",
                          fixedBreaks = breaks)
  colcode <- findColours(class, plotclr)
  
  plot(WestCountymapGeom, col = colcode)
  legend("bottomleft", # position
         legend = names(attr(colcode, "table")), 
         title = "Quantiles",
         fill = attr(colcode, "palette"),
         cex = 0.75,
         bty = "n")
}


CountyMaps.directory = "C:/Users/ellen/OneDrive/MyDocs/Earth Lab Internship/Presentations/CountyMap"
study_states_abbrev <- c("AZ","CA","CO", "ID", "MT", "NV", "NM", "OR", "UT", "WA", "WY")

preds<- read.csv("C:/Users/ellen/OneDrive/MyDocs/Earth Lab Internship/Presentations/ML_Predictions_CountieswPredictors.csv")
pred_data<- preds[which(preds$Date == "2008-07-11"),]

#Plotting help: https://www.r-bloggers.com/custom-legend-in-r/ 
nclr<- 8
plotclr <- brewer.pal(nclr, "YlOrRd")
# breaks <- round(quantile(base_data$PM2.5_Obs, seq(0, 1, 1/nclr)),4)
breaks <- round(quantile(pred_data$PM25_prediction, seq(0, 1, 1/nclr)), 4)

map_county_base_layer.fn(CountyMaps.directory, study_states_abbrev, pred_data$PM25_prediction)

base<- read.csv("C:/Users/ellen/OneDrive/MyDocs/Earth Lab Internship/Presentations/ML_input_PM25_Step5_part_d_de_duplicated_aves_ML_input.csv") 
base_data<- base[which(base$Date == "2008-07-11"),]
map_county_base_layer.fn(CountyMaps.directory, study_states_abbrev, base_data$PM2.5_Obs)

############ MAIAC AOD
breaks<- round(quantile(base_data$MAIAC_AOD, seq(0, 1, 1/nclr), na.rm = TRUE), 4)
map_county_base_layer.fn(CountyMaps.directory, study_states_abbrev, base_data$MAIAC_AOD)
