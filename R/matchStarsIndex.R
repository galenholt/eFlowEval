# Function to sort stars according to their index files and deal with any duplicated ANAEs

matchStarsIndex <- function(index1, stars1 = NULL, index2, stars2, indexcol = c(1, 1), testfinal = TRUE) {
  # index1 and index2 expected to be an sf with an index in the first column and geometry
  # testfinal does a big st_intersects on the stars, and so can take FOREVER. Turn it off for speed
  # If stars1 is NULL, this just matches to index1. particularly useful for matching to the ANAE in sf form,
    # e.g. resortFromANAE <- matchStarsIndex(index1 = EdwardWakoolANAE, stars1 = NULL,
              # index2 = predictIndices, stars2 = catchPredict, indexcol = c(1, 1))
  
  ## 
  # Fix single indexcol to match for both datasets
  if (length(indexcol) == 1) {
    indexcol <- rep(indexcol, 2)
  }
  
  # deal with the time and geometry dimensions being flopped
  # Note that this assumes there are only time and geometry dimensions
  # do it off the bat to catch the flopped issue even if they're sorted correctly
  if (attributes(st_dimensions(stars2))$name[1] != 'geometry') {
    stars2 <- aperm(stars2, c(2,1))
    warning('re-set geometry dimension in stars2 to be first. 
            Probably should do that earlier (e.g. aperm(stars2, c(2,1))) 
            to avoid other hidden problems with this dataset')
  }
  
  # if stars1 is null, skip
  if(!is.null(stars1)) {
    # To test, need geometry of stars1 to be correct too- this might really slow things down though
    if (attributes(st_dimensions(stars1))$name[1] != 'geometry') {
      stars1 <- aperm(stars1, c(2,1))
      warning('stars1 has geometry dimension not first. 
    DIMENSION SWAP FOR TESTING HERE, DOES NOT GET RETURNED. 
            Probably should swap dims earlier (e.g. aperm(stars1, c(2,1))) 
            to avoid other hidden problems with this dataset')
    }
    
  }
  
  
  # can have any index column, but typically it will be 1 and named UID. Going
  # to rename it though in case it ever isn't, but throw a warning
  if(names(index1)[indexcol[1]] != 'UID' |
     names(index2)[indexcol[2]] != 'UID') {
    warning('indexcol is not UID, it is ', names(index2)[indexcol[2]])
  }
  
  origname1 <- names(index1)[indexcol[1]]
  origname2 <- names(index2)[indexcol[2]]
  
  names(index1)[indexcol[1]] <- 'INDEX'
  names(index2)[indexcol[2]] <- 'INDEX'
  
  # expected behaviour is that this will sort 2 to match 1, and so only returns
  # the sorted index and stars for 2
  
  # First, do a check, and if they already match, don't need to do any more
  if (all(index1$INDEX == index2$INDEX)) {
    names(index2)[indexcol[2]] <- origname2
    return(lst(index2, stars2))
  }
  

# Deal with potential duplicates ------------------------------------------

  # need to make a second index column to deal with potential duplicates
    # Do it out here so we can use that column name even when there aren't duplicates
  index1$INDUP <- index1$INDEX
  index2$INDUP <- index2$INDEX
  
  if (any(duplicated(index1$INDEX)) | any(duplicated(index2$INDEX))) {
    
    if (any(duplicated(index1)) != any(duplicated(index2))) {
      stop('Duplicated UIDs in only one of the stars. 
           This suggests you are comparing different sets of ANAEs,
           and this function may not yield expected results')
    }
    
    # identify where the duplicates are
    dup1 <- index1[which(duplicated(index1$INDEX)), ]
    bothdup1 <- which(index1$INDEX %in% dup1$INDEX)
    
    dup2 <- index2[which(duplicated(index2$INDEX)), ]
    bothdup2 <- which(index2$INDEX %in% dup2$INDEX)
    
    # looking just at the duplicates, get the indices of 2 that match 1 based on the geometry itself
    matchindex <- as.numeric(st_equals_exact(index1[bothdup1, ], 
                                             index2[bothdup2, ], 
                                             par = 1, sparse = TRUE))
    
    # Modify the relevant second index- for the reference set, just append 1:index
    index1$INDUP[bothdup1] <- paste0(index1[bothdup1, ]$INDUP, 1:length(bothdup1))
    # For the matching set, append the index match we just saved
    index2$INDUP[bothdup2] <- paste0(index2[bothdup2, ]$INDUP, matchindex)
  }
  

# Matching and sorting ----------------------------------------------------
  # I think this sorts the first argument to the second. So if I want to have
  # the first be the reference, need to reverse order here
  ordermatcher <- match(index2$INDUP, index1$INDUP)
  
  # Do the sort on the indices and check
  index2 <- index2[order(ordermatcher), ] 
  # Check
  if (!all(index1$INDUP == index2$INDUP)) {
    stop('the sorting is still failing')
  }
  
  # Now, finally, sort the stars

  # sort the geometry dimension in the second to match the first
  stars2 <- stars2[ ,order(ordermatcher), ]
  
  # Test the geometries at least intersect. This is't perfect because the
  # geometries may not be exact after a few transforms, but catches most thigns
  if (testfinal) {
    print('testfinal is on, might take a long time')
    # If there is a stars1, test on that for best safety, otherwise use the index
    if (is.null(stars1)) {
      inter3 <- diag(st_intersects(st_geometry(index1), 
                                   st_geometry(stars2), sparse = FALSE))
    } else {
      inter3 <- diag(st_intersects(st_geometry(stars1), 
                                   st_geometry(stars2), sparse = FALSE))
    }
    
    if (sum(!inter3) != 0) {
      warning(sum(!inter3), " geometries still unmatched")
    } else {
      print('stars match, at least at the level of st_intersects')
    }
  }
  
  # return the output.
    # I'm not going to return the dup-fixed index. It would be dangerous because
    # the matching is only correct for sets that reference the same index1,
    # which is likely to get lost as we move through analyses. Will be better to
    # avoid dups in the first place in processANAE. Basically, the dup issue
    # will be sorted here with the sorting that needs to be done to compare
    # anyway
    names(index2)[indexcol[2]] <- origname2
    index2$INDUP <- NULL
    
    # test <- 1
    
    return(lst(index2, stars2))
  
}


