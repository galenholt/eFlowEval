# Small helper functions


# Proportion function
propor <- function(x, na.rm = FALSE) {
  sum(x, na.rm = na.rm)/length(x)
}


# Aggregation and plotting in the catchment -------------------------------

catchAggW <- function(strict, strictWeights, FUN, summaryPoly) {
  # strict is the strictures stars object (T/F or 0/1)
  # strictWeights is a vector of the weights to give each stricture (typically area of relevant polygons)
  # summaryPoly is an sf with one or more polygons that get aggregated into
  
  sArea <- strict
  
  # get area-proportion of success (same as area-days, but divided by days in year)
  sArea[[1]] <- t(t(sArea[[1]])*strictWeights)
  
  # Now aggregate over space
  sCatch <- aggregate(sArea, by = summaryPoly, FUN = FUN, na.rm = TRUE)
  
  return(sCatch)
  
  # testing
  # germArea <- germLippiaYr
  # # get area-proportion of success (same as area-days, but divided by days in year)
  # germArea[[1]] <- t(t(germArea[[1]])*lachArea)
  # 
  # # Now aggregate over space
  # germCatch2 <- aggregate(germArea, by = filter(ltimCut, ValleyName == "Lachlan"), FUN = sum, na.rm = TRUE)
  # 
  # # Check
  # all(germCatch[[1]] == germCatch2[[1]])
  
  
}

catchAggPlot <- function(catchAgg, title = NULL) {
  # Plot. Color ramp traffic light, because we can
  catchPlot <- ggplot() +
    geom_stars(data = catchAgg) +
    coord_sf() +
    facet_wrap(~as.character(time)) +
    theme_void()  +
    scale_fill_gradient(low = 'firebrick', high = 'forestgreen' ) +
    ggtitle(title)
}


# Fixing the last time unit when feeding aggregate() a vector of d --------

# For some reason, feeding aggregate dates instead of something like 'years' leaves an NA sheet hanging on the end.
# and, rightmost.closed has to be used. So, setting up a wrapper function that fixes it

tempaggregate <- function(starObj, by_t, FUN, na.rm = TRUE) {
  
  if (is.character(by_t)) {
    # If character, aggregate handles the time intervals correctly
    aggObj <- aggregate(starObj, by = by_t, FUN = FUN, na.rm = na.rm)
  } else {
    # If fed a date vector, aggregate needs different settings, and returns a final NA sheet. 
    if (!(is.POSIXct(by_t) | is.Date(by_t))) {
      warning('aggregation fix only tested with POSIXct and Date objects, assuming other non-character objects work the same way')
    }
    # rightmost closed finishes the final day
    aggObj <- aggregate(starObj, by = by_t, FUN = FUN, na.rm = na.rm, rightmost.closed = TRUE)
    
    
    # check last sheet is really NA
    if (!all(is.na(slice(aggObj, time, length(by_t))[[1]]))) {
      stop('Data in the final aggregation sheet. Figure out what is happening')
    }
    
    # Discard the last sheet, it's just NA
    aggObj <- slice(aggObj, time, -length(by_t))
    
  }
  
  return(aggObj)
  
}
