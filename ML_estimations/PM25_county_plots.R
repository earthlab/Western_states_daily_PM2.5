preds<- read.csv("C:/Users/ellen/OneDrive/MyDocs/Earth Lab Internship/Presentations/ML_Predictions_CountieswPredictors.csv")

pred_data<- preds[which(preds$Date == "2008-07-11"),]

library(geosphere)
library(gtools)
library(RColorBrewer)

colors<- brewer.pal(n = 8, "YlOrRd")

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
  
  # Resources for mapping
  # http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
  
  # Source for shapefiles:
  # https://www.census.gov/geo/maps-data/data/tiger-cart-boundary.html 
  # https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html
  
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
  
  q<- quantcut(PM25, q = 8)
  q_colors<- colors[as.integer(q)]
  
  plot(WestCountymapGeom, col = q_colors)
  legend(-101, 50, legend = levels(q), col = colors)
  
  return(WestCountymapGeom)
}


CountyMaps.directory = "C:/Users/ellen/OneDrive/MyDocs/Earth Lab Internship/Presentations/CountyMap"
study_states_abbrev <- c("AZ","CA","CO", "ID", "MT", "NV", "NM", "OR", "UT", "WA", "WY")
WestCountymapGeom <- map_county_base_layer.fn(CountyMaps.directory, study_states_abbrev, pred_data$PM25_prediction)
