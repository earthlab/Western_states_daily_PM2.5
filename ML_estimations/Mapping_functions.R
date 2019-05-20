# functions for mapping data

map_avg.fn <- function(shp, data, nclr, plotclr, breaks, Map_Var_col){ # function written by Ellen Considine
  library(raster)
  library(spatialEco)
  library(dplyr)
  library(classInt)
  library(RColorBrewer)
  #library(GiNA)
  data$ThisVar <- data[ , Map_Var_col]
  
  points<- SpatialPoints(data[,c("Longitude", "Latitude")], CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"), bbox = NULL)
  ptdf<- SpatialPointsDataFrame(data[,c("Longitude", "Latitude")], data.frame(data$ThisVar), proj4string = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"), bbox = NULL)
  
  #take intersection of points and polygons
  INT<- point.in.poly(ptdf, shp)
  int<- as.data.frame(INT)
  #Average all points inside each polygon
  Mean <- int %>% group_by(GEOID) %>% summarise(data.ThisVar = mean(data.ThisVar))
  #Get everything in the right format
  Mean$GEOID<- as.numeric(Mean$GEOID)
  shp$GEOID<- as.numeric(shp$GEOID)
  #match across the data frames
  pos<- which(shp$GEOID %in% Mean$GEOID)
  small<- shp[pos,]
  m<- match(small$GEOID, Mean$GEOID)
  aq<- Mean[m,2]
  #Setting up plotting variables
  plotvar <- aq$data.ThisVar#aq$data.var #aq$data.AQ
  options(warn  =  1) # dont' throw an error when there's a warning and stop the code from running further
  if (length(unique(breaks))==1) {# & unique(breaks) == 0) {
   #print("replace breaks with hard-coded numbers because (unique(breaks)) == 0 in map_avg.fn in Mapping_functions.R") 
   # breaks <- c(0, 12.5, 25, 37.5, 50, 62.5, 75, 87.5, 100)
    stop("evaluate if this code should be here. Mapping_functions.R, map_avg.fn. Line 34")
  }
    #}  else if (length(unique(breaks))==1) {
    #print("replace breaks with percentages of unique(breaks) because length(unique(breaks)) == 1 in map_avg.fn in Mapping_functions.R")
   #   breaks <- c(unique(breaks)*0,unique(breaks)*0.125, unique(breaks)*0.25, unique(breaks)*0.375, unique(breaks)*0.500, unique(breaks)*0.625, unique(breaks)*0.75, unique(breaks)*0.875,unique(breaks)*1.0)
  #    stop("evaluate if this code should be here. Mapping_functions.R, map_avg.fn. Line 34")
  #} else {
  class <- classIntervals(plotvar,
                          nclr,
                          style = "fixed",
                          fixedBreaks = breaks)
  #}
  options(warn  =  2) # throw an error when there's a warning and stop the code from running further
  colcode <- findColours(class, plotclr)
  #Plotting
  plot(shp)
  plot(small, col = colcode, add = TRUE)
  # plot(points, add= TRUE)
  legend("bottomleft", # position
         legend = names(attr(colcode, "table")), 
         title = "Legend", #title = "Quantiles",
         fill = attr(colcode, "palette"),
         cex = 0.75,
         bty = "n")
} # end of map_avg.fn

  
map_KNN.fn<- function(shp, data, K, nclr, plotclr, breaks,Var_col){ # function writtn by Ellen Considine
    library(RColorBrewer)
    library(classInt)
    library(sp)
    library(FNN)
    library(scales)
    library(rgdal)
    library(raster)
    query<- coordinates(shp)
    coords<- data[, c("Longitude", "Latitude")]
    knn<- get.knnx(coords, query, k = K)
    index<- knn$nn.index
    dist<- knn$nn.dist
    #mat<- matrix(data[t(index), "AQ"], nrow = nrow(query), byrow = TRUE)
    mat<- matrix(data[t(index), Var_col], nrow = nrow(query), byrow = TRUE)
    invdist<- t(apply(dist, 1, function(x)(x/sum(x))))
    
    aq<- rep(0,nrow(query))
    for (a in 1:length(aq)){
      aq[a]<- sum(invdist[a,]*mat[a,])
    }
    
    #Plotting help: https://www.r-bloggers.com/custom-legend-in-r/ 
    plotvar <- aq
    class <- classIntervals(plotvar,
                            nclr,
                            style = "fixed",
                            fixedBreaks = breaks)
    colcode <- findColours(class, plotclr)
    
    plot(shp, col = colcode)
    legend("bottomleft", # position
           legend = names(attr(colcode, "table")), 
           title = "Quantiles",
           fill = attr(colcode, "palette"),
           cex = 0.75,
           bty = "n")
  }
 
# create series of maps of data aggregated by spatial area
map_spec_days_value_by_region.fn <- function(Region,RegionMaps.directory, df_in, dates_of_interest, Date_col,
                                             Lat_col, Lon_col, Var_col, Cut_points_set = FALSE, cut_point_scale, study_states_abbrev,
                                             output.directory,file_sub_label,LatexFileName,title_string_starter) {
if (cut_point_scale == "Fire_Count") {
  #all_vars_max <- max(max(df_in[ , Var_col], na.rm = TRUE), na.rm = TRUE)
  stacked_columns <- do.call("rbind",df_in[ ,Var_col])
  which_not_NA <- !is.na(stacked_columns)
  all_vars_stacked <- stacked_columns[which_not_NA]
  which_non_zero <- which(all_vars_stacked > 0)
  non_zero_stacked <- all_vars_stacked[which_non_zero]
  #which_above_5 <- which(all_vars_stacked > 5)
  #above_5_stacked <- all_vars_stacked[which_above_5]
  non_zero_quintiles <- quantile(x = non_zero_stacked, probs = seq(0, 1, by = 0.2), na.rm = FALSE,
           names = FALSE, type = 7)
  custom_breakpoints <- c(min(all_vars_stacked, na.rm = TRUE), non_zero_quintiles)
} else {
  #all_vars_max <- NA
  custom_breakpoints <- NA
}
  
plot_counter <- 0 # count plots to know when to ClearPage
 for (var_i in 1:length(Var_col)) { # cycle through variables
  for (date_counter in 1:length(dates_of_interest)) { # cycle through dates of interest to make plots
    if (plot_counter%%10==0) { # check for multiples of 10, if so, put in a clearpage command. Latex gets confused if there are too many consecutive figures, so an occasional clearpage command helps with this.
      ClearPage <- TRUE
    } else {
      ClearPage <- FALSE
    } # if (this_col_i%%10==0) { # check for multiples of 10, if so, put in a clearpage command.
    start_date <- as.Date(dates_of_interest[date_counter],"%Y-%m-%d")
    end_date <- start_date
    map_value_by_region.fn(Region = Region,RegionMaps.directory = RegionMaps.directory, df_in = df_in, start_date = start_date, 
                           end_date = end_date, Date_col = Date_col, Lat_col = Lat_col, Lon_col = Lon_col, This_Var_col = Var_col[var_i],
                           Cut_points_set = Cut_points_set, cut_point_scale = cut_point_scale, study_states_abbrev = study_states_abbrev,
                           output.directory = output.directory,file_sub_label = file_sub_label,LatexFileName = LatexFileName,
                           title_string_starter = title_string_starter, summary_value = "mean", ClearPage = ClearPage,
                           custom_breakpoints = custom_breakpoints)
    plot_counter <- plot_counter+1
  } # for (date_counter in 1:length(dates_of_interest)) { # cycle through dates of interest to make plots
 } # for (var_i in 1:length(Var_col)) { # cycle through variables
} # end of map_spec_days_value_by_region.fn function
 
# aggregate data by spatial area and plot
map_value_by_region.fn <- function(Region,RegionMaps.directory, df_in, start_date, end_date, Date_col,
                                   Lat_col, Lon_col, This_Var_col, Cut_points_set = FALSE, cut_point_scale, study_states_abbrev,
                                   output.directory,file_sub_label,LatexFileName,title_string_starter,summary_value = "mean", ClearPage,
                                   custom_breakpoints = NA){ # = FALSE) { # map data aggregated by region
  Var_col_title <- replace_character_in_string.fn(input_char = This_Var_col,char2replace = "_",replacement_char = " ")
  start_date <- as.Date(start_date) # recognize as date
  end_date <- as.Date(end_date) # recognize as date
  Var4Name <- replace_character_in_string.fn(input_char = This_Var_col,char2replace = ".",replacement_char = "")
  all_max_val <- max(df_in[ ,This_Var_col], na.rm = TRUE)
  all_min_val <- min(df_in[ ,This_Var_col], na.rm = TRUE)
  # isolate date range of interest  
  if (start_date == end_date) { # if only including 1 day, there's no need to list the date twice in the 
    which_rows <- which(df_in[ ,Date_col] == start_date )
    title_string <- paste(title_string_starter,Var_col_title,start_date)
    plot_name_extension <- paste(Region,Var4Name,"Mean",start_date,sep = "")
  } else {
    which_rows <- which(df_in[ ,Date_col]>=start_date & df_in[ , Date_col]<=end_date)
    title_string <- paste(title_string_starter,Var_col_title,start_date,"to",end_date)
    plot_name_extension <- paste(Region,Var4Name,"Mean",start_date,"_",end_date,sep = "")
  }
  df_subset_step <- df_in[which_rows, c(Lat_col,Lon_col,This_Var_col)]
  rm(which_rows)
  which_not_NA <- which(!is.na(df_subset_step[ ,This_Var_col]))
  if (length(which_not_NA) > 50) { # only make plot if there are at least 50 data points
    df_subset <- df_subset_step[which_not_NA, ]
    if (length(unique(df_subset[ ,This_Var_col]))>1) { # only plot data if there is variation in the data (at least 2 unique values)
  # Var4Name <- replace_character_in_string.fn(input_char = This_Var_col,char2replace = ".",replacement_char = "")
  # plot_name_extension <- paste(Region,Var4Name,"Mean",start_date,"_",end_date,sep = "")
  FigFileName <- Plot_to_ImageFile_TopOnly.fn(output.directory, file_sub_label, plot_name_extension = plot_name_extension) # start image file
  
  if (Region == "County") {
    RegionMapGeom <- map_county_base_layer.fn(CountyMaps.directory, study_states_abbrev)
  } else {
    stop("write more code in map_value_by_region.fn in Mapping_function.R to handle addition geographic regions, such as ZIP code")
  }
  
  if (Cut_points_set == TRUE) {
    if (cut_point_scale == "PM2.5_Obs") {
      nclr <- 7
      if (all_max_val > 350.5) {
        base_breaks <-  c(0, 12.1, 35.5, 55.5, 150.5, 250.5, all_max_val)
      } else {
        base_breaks <-  c(0, 12.1, 35.5, 55.5, 150.5, 250.5, 350.5)
      }
      color_vec = c("green", "yellow", "orange", "red", "hotpink2", "hotpink3", "hotpink4")
      # plot values
      map_avg.fn(shp = RegionMapGeom, data = df_subset, nclr = nclr, plotclr = color_vec, breaks = base_breaks, Map_Var_col = This_Var_col)
    } else if (cut_point_scale == "Fire_Count") {
        nclr <- length(custom_breakpoints) #7
        #this_increment <- ceiling(all_vars_max/nclr_step)
        #base_breaks <- seq(from = all_min_val, to = nclr*this_increment, by = this_increment)
        base_breaks <- custom_breakpoints#c(0,5,seq(from = this_increment, to = nclr_step*this_increment, by = this_increment))
        #nclr <- nclr_step+1
        map_avg.fn(shp = RegionMapGeom, data = df_subset, nclr = nclr, plotclr = brewer.pal(nclr, "YlOrRd"), breaks = base_breaks, Map_Var_col = This_Var_col)
        
    } else {
      stop("write more code")
      #color_cut_points <- as.vector(c(quantile(df_subset[ , Var_col], na.rm = TRUE)))
      #color_vec = c("darkolivegreen1","forestgreen","deepskyblue","dodgerblue3","darkorchid")
    }
  } else {
    nclr<- 8
    base_breaks <- round(quantile(df_subset[ , This_Var_col], seq(0, 1, 1/nclr), na.rm = TRUE), 4)
    # plot values
    map_avg.fn(shp = RegionMapGeom, data = df_subset, nclr = nclr, plotclr = brewer.pal(nclr, "YlOrRd"), breaks = base_breaks, Map_Var_col = This_Var_col)
  }# if (Cut_points_set == FALSE) {
  
  # plot values
 # map_avg.fn(shp = RegionMapGeom, data = df_subset, nclr = nclr, plotclr = brewer.pal(nclr, "YlOrRd"), breaks = base_breaks, Map_Var_col = This_Var_col)
  # output image to file 
  Plot_to_ImageFile_BottomOnly.fn(FigFileName = FigFileName, title_string = title_string) # finish image file
  # output corresponding LaTex code
  LaTex_code_4_figure.fn(LatexFileName = LatexFileName, title_string = title_string, file_sub_label = file_sub_label, plot_name_extension = plot_name_extension, output.directory.short = output.directory.short, image_format = "jpg", ClearPage, fig_caption = title_string) # write latex code for this image
    } # if (length(unique(df_subset[ ,This_Var_col]))>1) { # only plot data if there is variation in the data (at least 2 unique values)
  } # if (length(which_not_NA) > 50) { # only make plot if there are at least 50 data points
} # end of map_value_by_region.fn function

df_map_monthly_summary_agg.fn <- function(Region,RegionMaps.directory, df_in, dates_of_interest, Date_col,
                                          Lat_col, Lon_col, cols_interest, Cut_points_set = FALSE, cut_point_scale = NA, study_states_abbrev,
                                          output.directory,file_sub_label,LatexFileName,title_string_starter) {
  #(this_df, cols_interest, output.directory, output.directory.short, file_sub_label, title_string_partial, plot_color = "black", LatexFileName, SinkFileName, image_format = "jpg",study_states_abbrev,this_datum) {
  # this_df <- Full_PM25_obs
  # cols_interest = c(col_name_interest, predictor_variables[4:length(predictor_variables)])
  if (sink.number()>0) {sink()} # get stop any lingering sinks
  if (max(dev.cur())>1) { # make sure it isn't outputting to any figure files
    dev.off(which  =  dev.cur())
  } # if (max(dev.cur())>1) { # make sure it isn't outputting to any figure files
  plot_counter <- 1
  for (this_col_i in 1:length(cols_interest)) { # cycle through and plot the columns of interest
    this_col <- cols_interest[this_col_i] #
    #print(this_col)
    if (this_col == "PM2.5_Obs") {
      Cut_points_set <- FALSE
      color_cut_points <- NA
      color_vec <- NA
    } else {
      Cut_points_set <- TRUE
      color_cut_points <- as.vector(c(quantile(this_df[ , this_col], na.rm = TRUE)))
      color_vec = c("darkolivegreen1","forestgreen","deepskyblue","dodgerblue3","darkorchid")
    }
    for (this_month in 1:12) { # cycle through dates of interest to make plots
      if (plot_counter%%10==0) { # check for multiples of 10, if so, put in a clearpage command. Latex gets confused if there are too many consecutive figures, so an occasional clearpage command helps with this.
        ClearPage <- TRUE
      } else {
        ClearPage <- FALSE
      } # if (this_col_i%%10==0) { # check for multiples of 10, if so, put in a clearpage command.
      # isolate the data of interest and summarize
      summary_value = "median"
      this_monthly_map_summary <- monthly_map_summary_all_yrs.fn(this_month = this_month, this_df = this_df, summary_value = summary_value, var_interest = this_col)
      if (max(!is.na(this_monthly_map_summary)) == 1) { # only try to plot if there's data for this month
        # plot map of data for this day
        plot_name_extension <-  paste("MapObsMo",this_month,replace_character_in_string.fn(this_col,char2replace = ".",replacement_char = ""),sep = "")
        title_string <- paste(replace_character_in_string.fn(this_col,char2replace = "_",replacement_char = " "),"Month",this_month,sep = " ") # used in figure titles, etc
        map_value_by_region.fn(Region,RegionMaps.directory, df_in, start_date, end_date, Date_col,
                                           Lat_col, Lon_col, This_Var_col, Cut_points_set = FALSE, cut_point_scale, study_states_abbrev,
                                           output.directory,file_sub_label,LatexFileName,title_string_starter) 
        #map_point_values.fn(this_df = this_monthly_map_summary, var_interest = summary_value, 
        #                    cut_point_scale = this_col, output.directory = output.directory, 
        #                    file_sub_label = file_sub_label, plot_name_extension = plot_name_extension, 
        #                    study_states_abbrev = study_states_abbrev, this_datum = this_datum, title_string = title_string, 
        #                    ClearPage = ClearPage, Cut_points_set = Cut_points_set, color_cut_points = color_cut_points, color_vec = color_vec, LatexFileName = LatexFileName) # plot points of observations on map and color points by concentration
        plot_counter <- plot_counter+1
      } # if (!is.na(this_monthly_map_summary)) { # only try to plot if there's data for this month
    } # for (date_i in dates_of_interest) { # cycle through dates of interest to make plots
  } # for (this_col_i in 1:length(cols_interest)) { # cycle through and plot the columns of interest
} # end of df_map_subset_days.fn function


