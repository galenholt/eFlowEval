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
  if (length(index1$INDEX) == length(index2$INDEX) && 
      all(index1$INDEX == index2$INDEX)) {
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
    
    # sometimes duplicates happen because geometries have the same index, but
    # are not actually the same. We want to handle and retain those. But
    # sometimes there are true duplicates, in which case we should remove them.
    truedup2 <- unlist(st_equals(index2[bothdup2, ], retain_unique = TRUE))
    drop2 <- bothdup2[truedup2]
    
    # Suppress warnings because it can throw a warning when calculating the
    # moments to print(), and that screws up my warning/error catcher.
    if (length(drop2) > 0) {
      index2 <- index2[-drop2, ]
      stars2 <- stars2[,-drop2, ]
    }
    
    
    truedup1 <- unlist(st_equals(index1[bothdup1, ], retain_unique = TRUE))
    drop1 <- bothdup1[truedup1]
    
    if (length(drop1) > 0) {
      index1 <- index1[-drop1, ]
      if (!is.null(stars1)) {stars1 <- stars1[,-drop1, ]}
    }
    
    
    # and now we need to re-do the dup-finding. This is getting a bit silly, but I don't have time to refactor.
    # identify where the duplicates are
    dup1 <- index1[which(duplicated(index1$INDEX)), ]
    bothdup1 <- which(index1$INDEX %in% dup1$INDEX)
    
    dup2 <- index2[which(duplicated(index2$INDEX)), ]
    bothdup2 <- which(index2$INDEX %in% dup2$INDEX)
    
    if (!all(dup1$INDEX %in% dup2$INDEX)) {
      stop('duplicate indices present in both indices, 
      but they are not the same polygons. 
      This suggests you are comparing different sets of ANAEs,
           and this function may not yield expected results')
    }
    
    # looking just at the duplicates, get the indices of 2 that match 1 based on the geometry itself
    matchindex <- as.numeric(st_equals_exact(index1[bothdup1, ], 
                                             index2[bothdup2, ], 
                                             par = 1, sparse = TRUE))
    
    # There's a weird edge case sometimes where they don't `equal_exact`, but do
    # match, it just can't sort it out. Bypass and warn, I guess. I could
    # probably be more clever to find the closest, but at this point they're
    # functionally identical- I think it's likely an ANAE problem.
    if (all(is.na(matchindex)) & 
        all(st_intersects(index1[bothdup1, ],
                          index2[bothdup2, ], 
                          sparse = FALSE))) {
      warning('duplicates are failing to match, but seem to actually match well.
              Just assigning based on position, but should check')
      # just match on position to get past this.
      matchindex <- seq(1, length(bothdup1))
      # one way to look and see
      # plot(index1[bothdup1[1], 2])
      # plot(index2[bothdup2[1], 2])
    }
    
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


