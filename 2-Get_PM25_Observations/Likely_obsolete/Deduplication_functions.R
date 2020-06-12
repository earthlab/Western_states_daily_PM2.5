# Deduplication_functions.R

# # isolate the data that is only in the second of two data frames
# only_in_new_df.fn <- function(old_df, new_df, cols_interest) {
#   test_tf <- true(new_df[ , cols_interest] == old_df[ , cols_interest])
#   new_df[ , cols_interest] == old_df[ , cols_interest]
#   # based on http://www.cookbook-r.com/Manipulating_data/Comparing_data_frames/
#   # add column to each dataframe (df) indicating whether row is from old or new df
#   old_df$WhichSet <- "old"
#   new_df$WhichSet <- "new"
#   #df <- rbind(old_df[,cols_interest],new_df[,cols_interest])
#   df <- rbind(old_df[ ,c("WhichSet", cols_interest)],new_df[ ,c("WhichSet", cols_interest)])
#   # Find the rows which have duplicates in a different group.
#   dupRows <- dupsBetweenGroups(df, "WhichSet")
#   #cbind(df, dup=dupRows) # Print it alongside the data frame
#   dfDup <- cbind(df, dup = dupRows)
#   only_in_new_step <- subset(dfDup, WhichSet = "new", select = -WhichSet)
#   return(only_in_new_df) # output from function
# } # end of only_in_later_df.fn function

# dupsBetweenGroups <- function (df, idcol) { # copied directly from http://www.cookbook-r.com/Manipulating_data/Comparing_data_frames/
#   # df: the data frame
#   # idcol: the column which identifies the group each row belongs to
#   
#   # Get the data columns to use for finding matches
#   datacols <- setdiff(names(df), idcol)
#   
#   # Sort by idcol, then datacols. Save order so we can undo the sorting later.
#   sortorder <- do.call(order, df)
#   df <- df[sortorder,]
#   
#   # Find duplicates within each id group (first copy not marked)
#   dupWithin <- duplicated(df)
#   
#   # With duplicates within each group filtered out, find duplicates between groups. 
#   # Need to scan up and down with duplicated() because first copy is not marked.
#   dupBetween = rep(NA, nrow(df))
#   dupBetween[!dupWithin] <- duplicated(df[!dupWithin,datacols])
#   dupBetween[!dupWithin] <- duplicated(df[!dupWithin,datacols], fromLast=TRUE) | dupBetween[!dupWithin]
#   
#   # ============= Replace NA's with previous non-NA value ==============
#   # This is why we sorted earlier - it was necessary to do this part efficiently
#   
#   # Get indexes of non-NA's
#   goodIdx <- !is.na(dupBetween)
#   
#   # These are the non-NA values from x only
#   # Add a leading NA for later use when we index into this vector
#   goodVals <- c(NA, dupBetween[goodIdx])
#   
#   # Fill the indices of the output vector with the indices pulled from
#   # these offsets of goodVals. Add 1 to avoid indexing to zero.
#   fillIdx <- cumsum(goodIdx)+1
#   
#   # The original vector, now with gaps filled
#   dupBetween <- goodVals[fillIdx]
#   
#   # Undo the original sort
#   dupBetween[sortorder] <- dupBetween
#   
#   # Return the vector of which entries are duplicated across groups
#   return(dupBetween)
# }


rows.in.a1.that.are.not.in.a2  <- function(a1,a2) # copied directly from https://stackoverflow.com/questions/3171426/compare-two-data-frames-to-find-the-rows-in-data-frame-1-that-are-not-present-in
{
  a1.vec <- apply(a1, 1, paste, collapse = "")
  a2.vec <- apply(a2, 1, paste, collapse = "")
  a1.without.a2.rows <- a1[!a1.vec %in% a2.vec,]
  return(a1.without.a2.rows)
}

full_mat_a1.that.are.not.in.a2.fn <- function(only_in_new, new_df) {
# create output dataframe
full_mat_a1.that.are.not.in.a2 <- data.frame(matrix(NA,nrow=dim(only_in_new)[1],ncol=length(colnames(new_df)))) # create data frame for input_mat1
names(full_mat_a1.that.are.not.in.a2) <- colnames(new_df) # assign the header to full_mat_a1.that.are.not.in.a2
# fill in the Lat/Lon columns from only_in_new
full_mat_a1.that.are.not.in.a2[ ,colnames(only_in_new)] <- only_in_new
#loc_date$Northing<- Final[match(loc_date$Latitude, Final$old_lat), 'Northing']
#full_mat_a1.that.are.not.in.a2$old_Datum <- NA
#full_mat_a1.that.are.not.in.a2$old_Datum <- new_df[match(full_mat_a1.that.are.not.in.a2$Lat, new_df$Lat), 'old_Datum']
return(full_mat_a1.that.are.not.in.a2)
}

fill_in_match_col_interest.fn <- function(fill_in_df, fill_in_col, match_col_fill_in, source_df, match_col_source) {
  # Example input
  # fill_in_df <- full_mat_a1.that.are.not.in.a2
  # fill_in_col <- "old_Datum"
  # match_col_fill_in <- "Lat"
  # source_df <- new_df
  # match_col_source <- "Lat"
  #full_mat_a1.that.are.not.in.a2$old_Datum <- new_df[match(full_mat_a1.that.are.not.in.a2$Lat, new_df$Lat), 'old_Datum']
  fill_in_df[ , fill_in_col] <- source_df[match(fill_in_df[ , match_col_fill_in], source_df[ , match_col_source]), fill_in_col]
  return(fill_in_df)
} # end of fill_in_match_col_interet.fn function

df_only_in_new.fn <- function(old_df, new_df) {
  # Take the difference in locations from part b and a
  #old_df <- part_a_loc 
  #new_df <- part_b_loc
  cols_interest <- c("Lon","Lat")#names(old_df)
  only_in_new <- rows.in.a1.that.are.not.in.a2(a1 = new_df[ ,cols_interest],a2 = old_df[ ,cols_interest])
  #only_in_old <- rows.in.a1.that.are.not.in.a2(a2 = new_df[ ,cols_interest],a1 = old_df[ ,cols_interest])
  full_mat_a1.that.are.not.in.a2 <- full_mat_a1.that.are.not.in.a2.fn(only_in_new = only_in_new, new_df = new_df) 

  cols_to_fill_in <- c("old_lon","old_lat","old_Datum","Datum","Easting","Northing") # list the columns that need to be filled in
  for (this_col_i in 1:length(cols_to_fill_in)) { # cycle through the columns that need to be filled in
    fill_in_this_col <- cols_to_fill_in[this_col_i] # name of column to be filled in
    #print(fill_in_this_col)
    #full_mat_a1.that.are.not.in.a2 <- fill_in_match_col_interest.fn(fill_in_df = full_mat_a1.that.are.not.in.a2, fill_in_col = "old_Datum", match_col_fill_in = "Lat", source_df = new_df, match_col_source = "Lat")
    #if () {}
    # fill in mat based on matching on "Lat"
    full_mat_a1.that.are.not.in.a2 <- fill_in_match_col_interest.fn(fill_in_df = full_mat_a1.that.are.not.in.a2, fill_in_col = fill_in_this_col, match_col_fill_in = "Lat", source_df = new_df, match_col_source = "Lat")
  }
} # end of df_only_in_new.fn function
