# Deduplication_functions.R

# isolate the data that is only in the second of two data frames
only_in_new_df.fn <- function(old_df, new_df, cols_interest) {
  test_tf <- true(new_df[ , cols_interest] == old_df[ , cols_interest])
  new_df[ , cols_interest] == old_df[ , cols_interest]
  
  
  # based on http://www.cookbook-r.com/Manipulating_data/Comparing_data_frames/
  # add column to each dataframe (df) indicating whether row is from old or new df
  old_df$WhichSet <- "old"
  new_df$WhichSet <- "new"
  
  #df <- rbind(old_df[,cols_interest],new_df[,cols_interest])
  df <- rbind(old_df[ ,c("WhichSet", cols_interest)],new_df[ ,c("WhichSet", cols_interest)])
  
  # Find the rows which have duplicates in a different group.
  dupRows <- dupsBetweenGroups(df, "WhichSet")
  #cbind(df, dup=dupRows) # Print it alongside the data frame
  
  dfDup <- cbind(df, dup = dupRows)
  
  only_in_new_step <- subset(dfDup, WhichSet = "new", select = -WhichSet)
  
  return(only_in_new_df) # output from function
} # end of only_in_later_df.fn function

dupsBetweenGroups <- function (df, idcol) { # copied directly from http://www.cookbook-r.com/Manipulating_data/Comparing_data_frames/
  # df: the data frame
  # idcol: the column which identifies the group each row belongs to
  
  # Get the data columns to use for finding matches
  datacols <- setdiff(names(df), idcol)
  
  # Sort by idcol, then datacols. Save order so we can undo the sorting later.
  sortorder <- do.call(order, df)
  df <- df[sortorder,]
  
  # Find duplicates within each id group (first copy not marked)
  dupWithin <- duplicated(df)
  
  # With duplicates within each group filtered out, find duplicates between groups. 
  # Need to scan up and down with duplicated() because first copy is not marked.
  dupBetween = rep(NA, nrow(df))
  dupBetween[!dupWithin] <- duplicated(df[!dupWithin,datacols])
  dupBetween[!dupWithin] <- duplicated(df[!dupWithin,datacols], fromLast=TRUE) | dupBetween[!dupWithin]
  
  # ============= Replace NA's with previous non-NA value ==============
  # This is why we sorted earlier - it was necessary to do this part efficiently
  
  # Get indexes of non-NA's
  goodIdx <- !is.na(dupBetween)
  
  # These are the non-NA values from x only
  # Add a leading NA for later use when we index into this vector
  goodVals <- c(NA, dupBetween[goodIdx])
  
  # Fill the indices of the output vector with the indices pulled from
  # these offsets of goodVals. Add 1 to avoid indexing to zero.
  fillIdx <- cumsum(goodIdx)+1
  
  # The original vector, now with gaps filled
  dupBetween <- goodVals[fillIdx]
  
  # Undo the original sort
  dupBetween[sortorder] <- dupBetween
  
  # Return the vector of which entries are duplicated across groups
  return(dupBetween)
}


rows.in.a1.that.are.not.in.a2  <- function(a1,a2) # copied directly from https://stackoverflow.com/questions/3171426/compare-two-data-frames-to-find-the-rows-in-data-frame-1-that-are-not-present-in
{
  a1.vec <- apply(a1, 1, paste, collapse = "")
  a2.vec <- apply(a2, 1, paste, collapse = "")
  a1.without.a2.rows <- a1[!a1.vec %in% a2.vec,]
  return(a1.without.a2.rows)
}

