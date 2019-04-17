# functions for mapping data

map_avg.fn<- function(shp, data, nclr, plotclr, breaks, Var_col){ # function written by Ellen Considine
  library(raster)
  library(spatialEco)
  library(dplyr)
  data$ThisVar <- data[ , Var_col]
  
  points<- SpatialPoints(data[,c("Longitude", "Latitude")], CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"), bbox = NULL)
  #ptdf<- SpatialPointsDataFrame(data[,c("Longitude", "Latitude")], data.frame(data[ ,Var_col]), proj4string = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"), bbox = NULL)
  ptdf<- SpatialPointsDataFrame(data[,c("Longitude", "Latitude")], data.frame(data$ThisVar), proj4string = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"), bbox = NULL)
  
  #take intersection of points and polygons
  INT<- point.in.poly(ptdf, shp)
  int<- as.data.frame(INT)
  #Average all points inside each polygon
  #Mean<- int %>% group_by(GEOID) %>% summarise(data.AQ = mean(data$PM2.5_Obs))
  #Mean<- int %>% group_by(GEOID) %>% summarise(data.var = mean(data[ , Var_col]))
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
  class <- classIntervals(plotvar,
                          nclr,
                          style = "fixed",
                          fixedBreaks = breaks)
  colcode <- findColours(class, plotclr)
  #Plotting
  plot(shp)
  plot(small, col = colcode, add = TRUE)
  # plot(points, add= TRUE)
  legend("bottomleft", # position
         legend = names(attr(colcode, "table")), 
         title = "Quantiles",
         fill = attr(colcode, "palette"),
         cex = 0.75,
         bty = "n")
  
}

  
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
  
# aggregate data by spatial area and plot
map_value_by_region.fn <- function(Region,RegionMaps.directory, df_in, start_date, end_date, Date_col,
                                   Lat_col, Lon_col, Var_col, Cut_points_set = FALSE, cut_point_scale, study_states_abbrev,
                                   output.directory,file_sub_label,LatexFileName,title_string_starter) { # map data aggregated by region
  Var4Name <- replace_character_in_string.fn(input_char = Var_col,char2replace = ".",replacement_char = "")
  plot_name_extension <- paste(Region,Var4Name,"Mean",start_date,"_",end_date,sep = "")
  FigFileName <- Plot_to_ImageFile_TopOnly.fn(output.directory, file_sub_label, plot_name_extension = plot_name_extension) # start image file
  
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  if (Region == "County") {
    RegionMapGeom <- map_county_base_layer.fn(CountyMaps.directory, study_states_abbrev)
  } else {stop("write more code in map_value_by_region.fn in Mapping_function.R to handle addition geographic regions, such as ZIP code")}
  
  # isolate date range of interest  
  if (start_date == end_date) {
    which_rows <- which(df_in[ ,Date_col] == start_date )
    title_string <- paste(title_string_starter,start_date)
  } else {
    which_rows <- which(df_in[ ,Date_col]>=start_date & df_in[ , Date_col]<=end_date)
    title_string <- paste(title_string_starter,start_date,"to",end_date)
  }
  #which_rows <- which(df_in[ ,Date_col] >= start_date )#& df_in[ ,Date_col] <= end_date)
  df_subset <- df_in[which_rows, c(Lat_col,Lon_col,Var_col)]
  rm(which_rows)
  # if (Cut_points_set == FALSE) {
  #   if (cut_point_scale == "PM2.5_Obs") {
  #     color_cut_points <-  c(0, 12.1, 35.5, 55.5, 150.5, 250.5, 350.5)
  #     color_vec = c("green", "yellow", "orange", "red", "hotpink2", "hotpink3", "hotpink4")
  #     #} else if (is.numeric(cut_point_scale)) {
  #     #  color_cut_points <-  cut_point_scale
  #   } else {
  #     color_cut_points <- as.vector(c(quantile(df_subset[ , Var_col], na.rm = TRUE)))
  #     color_vec = c("darkolivegreen1","forestgreen","deepskyblue","dodgerblue3","darkorchid")
  #   } 
  # } # if (Cut_points_set == FALSE) {
  
  nclr<- 8
  #base_data<- base[which(base$Date == "2008-07-11"), c("Longitude", "Latitude", "PM2.5_Obs")]
  #colnames(base_data)<- c("Longitude", "Latitude", "AQ")
  base_breaks <- round(quantile(df_subset[ , Var_col], seq(0, 1, 1/nclr)), 4)
  
  # plot values
  #map_KNN.fn(shp = RegionMapGeom, data = df_subset, K = 2, nclr = nclr, plotclr = brewer.pal(nclr, "YlOrRd"), breaks = base_breaks)#(WestCountymapGeom, base_data, K = 2, nclr, plotclr= brewer.pal(nclr, "YlOrRd"), base_breaks)
  map_avg.fn(shp = RegionMapGeom, data = df_subset, nclr = nclr, plotclr = brewer.pal(nclr, "YlOrRd"), breaks = base_breaks, Var_col = Var_col)
  
  
  
  Plot_to_ImageFile_BottomOnly.fn(FigFileName = FigFileName, title_string = title_string) # finish image file
  LaTex_code_4_figure.fn(LatexFileName = LatexFileName, title_string = title_string, file_sub_label = file_sub_label, plot_name_extension = plot_name_extension, output.directory.short = output.directory.short, image_format = "jpg", ClearPage = FALSE, fig_caption = title_string) # write latex code for this image
  
    
} # end of map_value_by_region.fn function