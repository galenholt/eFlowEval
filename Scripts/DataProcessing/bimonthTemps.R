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
scriptOut <- file.path(datOut, 'Tempprocessed', 'weightedMean', 'bimonth')
if (!dir.exists(scriptOut)) {dir.create(scriptOut, recursive = TRUE)}

# directory with metabolism predictions
tempIn <- file.path(datOut, 'Tempprocessed', 'weightedMean')

# Directory with inundation
# Use volume; it's most relevant
inunIn <- file.path(datOut, 'Inundationprocessed', 'volInun')


# Read in data ------------------------------------------------------------

# First, get the names of the catchments, so I can match them
catchFiles <- list.files(tempIn, pattern = '.rdata')
catchNames <- str_extract(catchFiles, pattern = '[A-z]*_') %>%
  str_remove('_') # I'm sure I could do this in one regex, but this is easier
catchNames
suffix <- str_extract(catchFiles[1], pattern = '_[A-z]*')
# I'm not building anything with chunks, just saving. So, I think I can probably
# run this locally, and use a standard for loop?
# Though parallelizing would be nice. so maybe I will foreach it, but not return anything

# For testing
# ca <- 1
startloop <- proc.time()
# trashOut <- foreach(ca = 4:5) %dopar% {
trashOut <- foreach(ca = 1:length(catchNames)) %dopar% {
  # Will need to loop over this
  thisCatch <- catchNames[ca]
  # suffix <- str_extract(catchFiles, pattern = '_[A-z]*')
  load(file.path(tempIn, paste0(thisCatch, suffix, '.rdata')))
  load(file.path(inunIn, paste0(thisCatch, '_volInun.rdata')))
  
  # Give them generic names
  # should have saved this with a different name. I COULD use starpreds, since
  # it's already generic, but keep consistent (and hopefully I'll fix it so it's
  # not generic later anyway)
  tempPolys <- get(paste0(thisCatch, suffix))
  rm(list = c(paste0(thisCatch, suffix))) 
  
  catchInun <- get(paste0(thisCatch, "_volInun"))
  rm(list = paste0(thisCatch, "_volInun"))
  rm(list = paste0(thisCatch, "_volInun_index"))
  
  # make the temp C
  tempPolys <- tempPolys - 272.15
  names(tempPolys) <- 'tempC'
  
  # Get date breaks ---------------------------------------------------------
  
  inuntimes <- st_get_dimension_values(catchInun, which = 'time')
  temptimes <- st_get_dimension_values(tempPolys, which = 'time')
  
  # Need to clip so we aren't doing anything beyond the overlapping times
  whichoverlap <- which(inuntimes >= min(temptimes))
  overlaptimes <- inuntimes[inuntimes >= min(temptimes)]
  
  # Get the relevant volumes
  overlapInun <- catchInun %>% slice('time', index = whichoverlap)
  
  
  # # Can I just throw that in yearsummary? NO. But maybe tempaggregate
  # # Stolen from yearsummary
  starttemp <- min(temptimes)
  endtemp <- max(temptimes)
  endinun <- max(inuntimes)
  # So, I need to set the intervals with the inuntimes
  # and I need to START the intervals with the previous time, I think (though
  # that is likely to yield NAs, because the inundation there is for the 2
  # months PRECEDING Jan 1, and the temps don't start until Jan 1)
  precedingInun <- max(inuntimes[inuntimes < starttemp])
  # # 
  # by_t <- c(startdate, overlaptimes,  enddate)
  startdate <- max(inuntimes < min(temptimes))
  by_t <- c(precedingInun, overlaptimes)
  # # Now just go over to tempaggregate (NOT yearsummary- it expects more structure strictures) and step through
  
  # Double check the time naming is in fact assigning the FIRST time (ie the
  # START of the interval), while we want the END of the interval  names(tempPolys) <- str_remove(names(tempPolys), 'log')
  # by_test <- overlaptimes[39:41] # With 40:41 it just drops time
  # bimonthtemp <- tempaggregate(starObj = tempPolys, by = by_test, FUN = mean, na.rm = TRUE)
  
  bimonthtemp <- tempaggregate(starObj = tempPolys, by = by_t, FUN = mean, na.rm = TRUE)
  # Pretty quick for Avoca, anyway.
  
  # make the names obvious what's happened
  names(bimonthtemp) <- paste0('bimonth_', names(bimonthtemp))
  # shift the dates to be the END rather than START of the interval
  bimonthtemp <- st_set_dimensions(bimonthtemp, which = 'time', values = st_dimensions(overlapInun)[2])
  bimonthtemp
  
  
  # Set up to save
  thisOutNameB <- paste0(thisCatch, '_TempBimonthMean')
  assign(thisOutNameB, bimonthtemp)
  save(list = thisOutNameB, file = file.path(scriptOut, paste0(thisOutNameB, '.rdata')))
  
  # Get aggressive about cleanup, since some things will have new names
  rm(list = c(thisOutNameB, 'bimonthtemp', 'by_t', 
              'startdate', 'overlaptimes', 'catchInun', 'tempPolys'))
  # Return a null dummy, since I really just want side effects
  dummy <- NULL
}

endloop <- proc.time()
looptime <- endloop - startloop
print('total time:')
print(looptime)
# 850 seconds local, 230 HPC