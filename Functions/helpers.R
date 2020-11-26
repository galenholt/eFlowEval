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
