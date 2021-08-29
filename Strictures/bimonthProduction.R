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
  overlaptimes <- inuntimes[inuntimes >= min(predicttimes)]
  
  # # Can I just throw that in yearsummary? NO. But maybe tempaggregate
  # # Stolen from yearsummary
  startdate <- min(predicttimes)
  enddate <- max(predicttimes)
  # 
  by_t <- c(startdate, overlaptimes,  enddate)
  # # Now just go over to tempaggregate (NOT yearsummary- it expects more structure strictures) and step through
  
  # TAKE THE LOG OFF BEFORE AVERAGING
  catchPredict <- exp(catchPredict)
  names(catchPredict) <- str_remove(names(catchPredict), 'log')
  bimonthPredict <- tempaggregate(starObj = catchPredict, by = by_t, FUN = mean, na.rm = TRUE)
  # Pretty quick for Avoca, anyway.
  
  names(bimonthPredict) <- paste0('bimonth_', names(bimonthPredict))
  
  bimonthPredict
  
  thisOutName <- paste0(thisCatch, '_PredictBimonthMean')
  assign(thisOutName, bimonthPredict)
  save(list = thisOutName, file = file.path(scriptOut, paste0(thisOutName, '.rdata')))
  
  # Get aggressive about cleanup, since some things will have new names
  rm(list = c(thisOutName, 'bimonthPredict', 'catchPredict', 'by_t', 
              'startdate', 'enddate', 'overlaptimes', 'catchInun', 'catchPredict'))
  # Return a null dummy, since I really just want side effects
  dummy <- NULL
}

