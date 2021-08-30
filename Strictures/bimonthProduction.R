# Script to aggregate bimonthly

# Header from the temperature file to retain all the directories,  --------
source('directorySet.R')

# Let's get libraries here, then sort out git then sort out making this a
# library so we don't have to deal with all the library crap
library(here)
library(tidyverse)
library(lubridate)
library(sf)
library(stars)
library(foreach)
library(doFuture)
registerDoFuture()
plan(multisession)



# Setup -------------------------------------------------------------------
scriptOut <- file.path(datOut, 'TempAndProduction', 'Predictions', 'bimonth')
if (!dir.exists(scriptOut)) {dir.create(scriptOut, recursive = TRUE)}

# and to save the predicted x volumes
totalOut <- file.path(scriptOut, 'predictxvol')
if (!dir.exists(totalOut)) {dir.create(totalOut, recursive = TRUE)}

# NOTE: this same approach should be easily extensible to bimonthing the temp itself if we want

# directory with metabolism predictions
predictIn <- file.path(datOut, 'TempAndProduction', 'Predictions')

# Directory with inundation
  # Use volume; it's most relevant
inunIn <- file.path(datOut, 'Inundationprocessed', 'volInun')


# Read in data ------------------------------------------------------------

# First, get the names of the catchments, so I can match them
catchFiles <- list.files(predictIn, pattern = '.rdata')
catchNames <- str_extract(catchFiles, pattern = '[A-z]*_') %>%
  str_remove('_') # I'm sure I could do this in one regex, but this is easier
catchNames

# I'm not building anything with chunks, just saving. So, I think I can probably
# run this locally, and use a standard for loop?
  # Though parallelizing would be nice. so maybe I will foreach it, but not return anything

# For testing
# ca <- 1

# trashOut <- foreach(ca = 4:5) %dopar% {
trashOut <- foreach(ca = 1:length(catchNames)) %dopar% {
  # Will need to loop over this
  thisCatch <- catchNames[ca]
  load(file.path(predictIn, paste0(thisCatch, '_predictedGPPER.rdata')))
  load(file.path(inunIn, paste0(thisCatch, '_volInun.rdata')))
  
  # Give them generic names
  # should have saved this with a different name. I COULD use starpreds, since
  # it's already generic, but keep consistent (and hopefully I'll fix it so it's
  # not generic later anyway)
  catchPredict <- get('starpreds') 
  rm(list = paste0('starpreds'))
  
  catchInun <- get(paste0(thisCatch, "_volInun"))
  rm(list = paste0(thisCatch, "_volInun"))
  rm(list = paste0(thisCatch, "_volInun_index"))
  
  
  
  # Get date breaks ---------------------------------------------------------
  
  inuntimes <- st_get_dimension_values(catchInun, which = 'time')
  predicttimes <- st_get_dimension_values(catchPredict, which = 'time')
  
  # Need to clip so we aren't doing anything beyond the overlapping times
  whichoverlap <- which(inuntimes >= min(predicttimes))
  overlaptimes <- inuntimes[inuntimes >= min(predicttimes)]
  
  # Get the relevant volumes
  overlapInun <- catchInun %>% slice('time', index = whichoverlap)
  
  
  # # Can I just throw that in yearsummary? NO. But maybe tempaggregate
  # # Stolen from yearsummary
  startpredict <- min(predicttimes)
  endpredict <- max(predicttimes)
  endinun <- max(inuntimes)
  # So, I need to set the intervals with the inuntimes
  # and I need to START the intervals with the previous time, I think (though
  # that is likely to yield NAs, because the inundation there is for the 2
  # months PRECEDING Jan 1, and the temps don't start until Jan 1)
  precedingInun <- max(inuntimes[inuntimes < startpredict])
  # # 
  # by_t <- c(startdate, overlaptimes,  enddate)
  startdate <- max(inuntimes < min(predicttimes))
  by_t <- c(precedingInun, overlaptimes)
  # # Now just go over to tempaggregate (NOT yearsummary- it expects more structure strictures) and step through
  
  # TAKE THE LOG OFF BEFORE AVERAGING
    # The problem is, this ends up throwing a lot of infs
  catchPredict <- exp(catchPredict)
  names(catchPredict) <- str_remove(names(catchPredict), 'log')
  # Double check the time naming is in fact assigning the FIRST time (ie the
  # START of the interval), while we want the END of the interval  names(catchPredict) <- str_remove(names(catchPredict), 'log')
  # by_test <- overlaptimes[39:41] # With 40:41 it just drops time
  # bimonthPredict <- tempaggregate(starObj = catchPredict, by = by_test, FUN = mean, na.rm = TRUE)
  
  bimonthPredict <- tempaggregate(starObj = catchPredict, by = by_t, FUN = mean, na.rm = TRUE)
  # Pretty quick for Avoca, anyway.
  
  # make the names obvious what's happened
  names(bimonthPredict) <- paste0('bimonth_', names(bimonthPredict))
  # shift the dates to be the END rather than START of the interval
  bimonthPredict <- st_set_dimensions(bimonthPredict, which = 'time', values = st_dimensions(overlapInun)[2])
  bimonthPredict
  

# Multiply predictions by volumes -----------------------------------------

  # This is probably not necessary
  bimonthPredict <- st_transform(bimonthPredict, st_crs(overlapInun))
  
  # Then just do the mult. the dims are swapped, but Ops.stars seems to handle that
  totalPredictVol <- bimonthPredict*overlapInun
  names(totalPredictVol) <- str_replace(names(totalPredictVol), pattern = 'bimonth', replacement = 'predict_x_vol')
  
  # Set up to save
  thisOutNameB <- paste0(thisCatch, '_PredictBimonthMean')
  assign(thisOutNameB, bimonthPredict)
  save(list = thisOutNameB, file = file.path(scriptOut, paste0(thisOutNameB, '.rdata')))
  
  thisOutNameT <- paste0(thisCatch, '_PredictxVol')
  assign(thisOutNameT, totalPredictVol)
  save(list = thisOutNameT, file = file.path(totalOut, paste0(thisOutNameT, '.rdata')))
  
  # Get aggressive about cleanup, since some things will have new names
  rm(list = c(thisOutNameB, thisOutNameT, 'bimonthPredict', 'by_t', 
              'startdate', 'overlaptimes', 'catchInun', 'catchPredict'))
  # Return a null dummy, since I really just want side effects
  dummy <- NULL
}

