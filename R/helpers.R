# Small helper functions


# Proportion function
propor <- function(x, na.rm = FALSE) {
  sum(x, na.rm = na.rm)/length(x)
}

# when everything being summed (or otherwise aggregated) is NA, sum(na.rm =
# TRUE) gives 0, but I need it to be NA. So define a function on the backend,
# some of the code expects a na.rm so pass it I guess really, should make these
# generic and accept the FUN, but not now
sumna <- function(x, na.rm = TRUE) {
  ifelse(all(is.na(x)), NA, sum(x, na.rm = TRUE))
}

meanna <- function(x, na.rm = TRUE) {
  ifelse(all(is.na(x)), NA, mean(x, na.rm = TRUE))
}

maxna <- function(x, na.rm = TRUE) {
  ifelse(all(is.na(x)), NA, max(x, na.rm = TRUE))
}

minna <- function(x, na.rm = TRUE) {
  ifelse(all(is.na(x)), NA, min(x, na.rm = TRUE))
}


# Aggregation and plotting in the catchment -------------------------------

catchAggW <- function(strict, strictWeights, FUN, summaryPoly) {
  # strict is the strictures stars object (T/F or 0/1)
  # strictWeights is a vector of the weights to give each stricture (typically area of relevant polygons)
  # summaryPoly is an sf with one or more polygons that get aggregated into
  
  # sArea <- strict
  
  # get area-proportion of success (same as area-days, but divided by days in year)
  strict[[1]] <- t(t(strict[[1]])*strictWeights)
  
  # Now aggregate over space
  sCatch <- aggregate(strict, by = summaryPoly, FUN = FUN, na.rm = TRUE)
  
  return(sCatch)
  
  # testing
  # germArea <- germLippiaYr
  # # get area-proportion of success (same as area-days, but divided by days in year)
  # germArea[[1]] <- t(t(germArea[[1]])*lachArea)
  # 
  # # Now aggregate over space
  # germCatch2 <- aggregate(germArea, by = filter(ltimNoNorth, ValleyName == "Lachlan"), FUN = sum, na.rm = TRUE)
  # 
  # # Check
  # all(germCatch[[1]] == germCatch2[[1]])
  
  
}

catchAggPlot <- function(catchAgg, varname = NA, title = NULL, as_sf = FALSE) {
  # Plot. changing to viridis from red/green traffic light because traffic lights ugly and colorblind
  # The returned object is easier to modify as an sf, so default to that. It COULD blow up memory and time though
  if (as_sf) {
    
    catchAggsf <- st_as_sf(catchAgg, long = TRUE)
    
  if (is.na(varname)) {
    varname = names(catchAgg)
  }
    
   catchPlot <- ggplot() + 
      geom_sf(data = catchAggsf, aes_string(fill = varname)) + 
      facet_wrap(vars(as.character(time))) +
      scale_fill_viridis(option = 'plasma') +
      theme_bw() + ggtitle('Yearly Life Cycle Success') +
      theme_void() +
      scale_fill_viridis(option = 'plasma') +
      ggtitle(title)
   
  } else {
    catchPlot <- ggplot() +
      geom_stars(data = catchAgg) +
      coord_sf() +
      facet_wrap(~as.character(time)) +
      theme_void()  +
      # scale_fill_gradient(low = 'firebrick', high = 'forestgreen' ) +
      scale_fill_viridis(option = 'plasma') +
      ggtitle(title)
  }

  return(catchPlot)
  
}


# Fixing the last time unit when feeding aggregate() a vector of d --------

# For some reason, feeding aggregate dates instead of something like 'years' leaves an NA sheet hanging on the end.
# and, rightmost.closed has to be used. So, setting up a wrapper function that fixes it

### THIS RETURNS TIMES AT THE *START* OF THE INTERVAL ### 
# This is particularly an issue for inundation, which reports inundation for the
# *preceding* interval. dates_end_interval shifts to the end.
tempaggregate <- function(starObj, by_t, FUN, na.rm = TRUE, dates_end_interval = FALSE) {
  

  
  if (is.character(by_t)) {
    # If character, aggregate handles the time intervals correctly
    aggObj <- aggregate(starObj, by = by_t, FUN = FUN, na.rm = na.rm)
  } else { 
    # If fed a date vector, aggregate needs different settings, and returns a final NA sheet. 
    if (!(lubridate::is.POSIXct(by_t) | lubridate::is.Date(by_t))) {
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
    
    if (dates_end_interval) {
      # the first one gets dropped (just like the last one does if the dates are at the beginning- we can't aggregate into a period beyond the end)
      aggObj <- st_set_dimensions(aggObj, which = 'time', values = by_t[2:length(by_t)])
    }
    
  }
  
  # test <- 1
  
  return(aggObj)
  
}


# Time parsing functions --------------------------------------------------
# these are fairly specific, but I use them for regressions and predictions, so
# put them here so if anything changes they cascade

# Function to get the water year that a date is in
getWaterYear <- function(input.date) {
  wateryear <- ifelse((month(input.date) >= 7), year(input.date),
                      year(input.date-dyears()))
  return(wateryear)
}

# Assign 'seasons'- I dont like this one very much, but I guess look at it
# From https://stackoverflow.com/questions/36502140/determine-season-from-date-using-lubridate-in-r
# Modified for southern hemisphere
getSeason <- function(input.date){
  numeric.date <- 100*month(input.date)+day(input.date)
  ## input Seasons upper limits in the form MMDD in the "break =" option:
  cuts <- base::cut(numeric.date, breaks = c(0,319,0620,0921,1220,1231)) 
  # rename the resulting groups (could've been done within cut(...levels=) if "Winter" wasn't double
  levels(cuts) <- c("Summer","Autumn","Winter","Spring","Summer")
  return(cuts)
}

# Get the bimonthly group (this is specific to the inundation layer)
getBimonth <- function(input.date) {
  bimonth <- case_when(
    month(input.date) %in% c(1, 2) ~ 1, # Maps to 03-01
    month(input.date) %in% c(3, 4) ~ 2, # Map to 05-01
    month(input.date) %in% c(5, 6) ~ 3, # Map to 07-01
    month(input.date) %in% c(7, 8) ~ 4, # Map to 09-01
    month(input.date) %in% c(9, 10) ~ 5, # Map to 11-01  
    month(input.date) %in% c(11, 12) ~ 6, # Maps to Jan 01 because this is the preceding
  )
  return(bimonth)
}

# Get the number of days away from water year
daysfromWY <- function(input.date) {
  daysWY <- abs(yday(input.date)-
                  yday(dmy(paste0('0107', as.character(year(input.date)))))) # get the day of the year that is July 1
}



# Modeling and prediction -------------------------------------------------

# A function to return NA when trying to predict new factor levels for fixed effects
checklevels <- function(newdata, mod) {
  # Get the factor levels
  faclevs <- model.frame(mod) |> select_if(is.factor) |> 
    map(unique)
  # and the ones that are fixed factors (not random effects)
  fixedvars <- attributes(attributes(model.frame(mod))$terms)$varnames.fixed
  # throw out any potential random effects
  faclevs[!(names(faclevs) %in% fixedvars)] <- NULL
  
  # get the levels available in the data
  if ('sf' %in% class(newdata)) {
    datalevs <- newdata |> 
      st_drop_geometry() |> # only relevant if sf
      select(any_of(names(faclevs))) |> 
      map(unique)
  } else {
    datalevs <- newdata |> 
      select(any_of(names(faclevs))) |> 
      map(unique)
  }
  
  
  # if no factors are needed, just short-circuit
  if (length(faclevs) == 0 & length(datalevs) == 0) {
    return(TRUE)
  }
  
  # function to check if they are in
  checkin <- function(x,y) {
    tf <- (x %in% y)
  }
  
  # use map-reduce to ask if all levels are available for all factors across
  # what might be an uneven list.
  facsinmodel <- map2(datalevs, faclevs, checkin) |> reduce(all)
  return(all(facsinmodel))
}

# a new function to allow prediction over new levels of RANDOM effects
# add_predictions is pretty slick, but seems to also not be very full-featured.
# can I write my own?
add_preds <- function(newdata, mod, predname = NULL, 
                      interval = 'none', level = 0.9,
                      parTF = FALSE) {
  if (is.null(predname)) {
    predname <- deparse(substitute(mod))
  }
  test <- 1
  
  # first handle the situation where the data's not there
  if (!checklevels(newdata, mod)) {
    predf <- mutate(newdata, tempname = NA)
    # need the other terms if they'll be there for other catchments etc
    if ('prediction' %in% interval | interval %in% c('both', 'all')) {
      predf <- mutate(predf, 
                      tempname_pfit = NA, 
                      tempname_pupr = NA, 
                      tempname_plwr = NA)
    }
    
    if ('confidence' %in% interval | interval %in% c('both', 'all')) {
      predf <- mutate(predf, 
                      tempname_cfit = NA, 
                      tempname_cupr = NA, 
                      tempname_clwr = NA)
    }
      
      } else {
      preds <- predict(mod, newdata = newdata, allow.new.levels = TRUE)
      predf <- bind_cols(newdata, tempname = preds)
      
      # Easier to do the fit first, but I'll leave the $fit part of the predictIntervals on there too to check
      
      # Prediction intervals
        if ('prediction' %in% interval | interval %in% c('both', 'all')) {
          predsP <- merTools::predictInterval(merMod = mod, 
                                             newdata = newdata, 
                                             level = level, 
                                             include.resid.var = TRUE,
                                             .parallel = parTF)
        predsP <- rename(predsP,
                        tempname_pfit = fit, tempname_pupr = upr, tempname_plwr = lwr)
        predf <- bind_cols(predf, predsP)
        } 
      # Confidence intervals
      if ('confidence' %in% interval | interval %in% c('both', 'all')) {
        predsC <- merTools::predictInterval(merMod = mod, 
                                           newdata = newdata, 
                                           level = level, 
                                           include.resid.var = FALSE,
                                           .parallel = parTF)
        predsC <- rename(predsC,
                        tempname_cfit = fit, tempname_cupr = upr, tempname_clwr = lwr)
        predf <- bind_cols(predf, predsC)
      } 
    
  }
  
  # names(predf)[which(names(predf) == 'tempname')] <- predname # avoiding rlang to sort out the name programatically
  names(predf) <- str_replace(names(predf), 'tempname', predname)
  return(predf)
}

# function to check and change crs

crscheck <- function(obj, whichcrs) {
  if (st_crs(obj)$epsg != whichcrs) {
    obj <- st_transform(obj, whichcrs) |>
      st_make_valid()
  } 
  
  return(obj)
}


# turn a stars into a stacked sf with date col and add the catchme --------

sfandcatch <- function(starsObj, catches = NULL, newname) {
  starsObj <- starsObj |>
    st_as_sf() |> 
    pivot_longer(cols = -any_of(c('Shape', 'geometry')), names_to = 'date', values_to = {{newname}}) |>
    mutate(date = as.Date(date))
  
  if (!is.null(catches)) {
    starsObj <- starsObj |>
      st_join(catches, join = st_equals_exact, par = 1)
  }
  
  return(starsObj)
}


# rename objects on read-in with an appended name to avoid stomping
loadappend <- function(filename, append) {
  load(filename)
  objnames <- ls()[!ls() %in% c("filename", "append")]
  
  renamelist <- mget(objnames)
  
  names(renamelist) <- paste0(objnames, '_', append)

   return(renamelist)

}

# Similar to above, but for the situation where we know the names a priori, and
# want to package them into a list we can name whatever we want (and
# allows naming the list items whatever we want). Or just return one item.
load_rename <- function(filepath, knownnames, 
                       newnames = knownnames, returnOne = NULL) {
  load(filepath)
  
  # Short-circuit if we really just want to get one of them
  if (!is.null(returnOne)) {
    # if (length(knownnames) > 1) {rlang::abort("can't return one, not sure which")}
    
    return(get(returnOne))
  }
  
  renamelist <- mget(knownnames)
  names(renamelist) <- newnames
  
  return(renamelist)
  
  
}

# Make sure areas aren't larger than areas of polygons. There are cases where
# the rounding is an issue.
clean_area <- function(starsObj, anaes) {
  anaeareas <- as.numeric(st_area(anaes))
  polyareas <- matrix(rep(anaeareas, 
                          length(st_get_dimension_values(starsObj, which = 'time'))), 
                      ncol = length(st_get_dimension_values(starsObj, which = 'time')))
  repind <- which(starsObj[[1]] > polyareas)
  starsObj[[1]][repind] <- polyareas[repind]
  return(starsObj)
}

# Plot helpers (themes) ---------------------------------------------------
pubtheme <- ggplot2::theme_bw(base_size = 11) + 
  ggplot2::theme(strip.background = ggplot2::element_blank(),
        plot.background = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank())

theme_pub <- function(base_size = 8, ...) {
  ggplot2::theme_bw(base_size = base_size) +
    theme(strip.background = element_blank(),
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          ...)
}
