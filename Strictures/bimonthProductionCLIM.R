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

# new code to handle parallelisation
if (parSet == 'local') {
  plan(multisession)
} else if (parSet == 'hpc') {
  plan(multicore(workers = availableCores(methods = 'Slurm')))
  # plan(list(
  #   tweak(multicore, workers = availableCores(methods = 'Slurm')),
  #   tweak(multicore, workers = availableCores(methods = 'Slurm'))
  # ))
} else {
  plan(sequential)
}


# Setup -------------------------------------------------------------------
predicteds <- c('logERdays', 'logERdaysvalleys', 'logGPPdays', 'logGPPdaysvalleys')

# Testing
# predicteds <- 'logERdays'
# i <- 1

# if we're using arrays on the HPC, cut to just one predicted
# This is a bit different than I did for concatMetabolism, but allows me to
# leave the nested foreach below if I'm NOT arraying
predicteds <- predicteds[as.numeric(args[7])]

# To allow catchname in nested foreach, use the inundation directories because they're common to everything
# Directory with inundation
# Use volume; it's most relevant
inunIn <- file.path(datOut, 'Inundationprocessed', 'volInun')

# 10% volume inundations
inunIn10p <- file.path(datOut, 'Inundationprocessed', 'vol10p')

# First, get the names of the catchments, so I can match them
catchFiles <- list.files(inunIn, pattern = '.rdata')
catchNames <- str_extract(catchFiles, pattern = '[A-z]*_') %>%
  str_remove('_') # I'm sure I could do this in one regex, but this is easier
catchNames


# Kludgy, loop everything over teh predicteds. should probably nested foreach this.
# for (i in 1:length(predicteds)) {
startloop <- proc.time()

# trashout <- foreach(i = 1:2) %:%
#   foreach(ca = 6:7) %dopar% {
trashout <- foreach(i = 1:length(predicteds)) %:%
  foreach(ca = 1:length(catchNames)) %dopar% {
    scriptOut <- file.path(datOut, 'ClimateAndProduction', 'Predictions', predicteds[i], 'bimonth')
    if (!dir.exists(scriptOut)) {dir.create(scriptOut, recursive = TRUE)}
    
    # and to save the predicted x volumes
    totalOut <- file.path(scriptOut, 'predictxvol')
    if (!dir.exists(totalOut)) {dir.create(totalOut, recursive = TRUE)}
    
    # and to save the predicted x volume 10% scenario
    totalOut10p <- file.path(scriptOut, 'predictxvol10p')
    if (!dir.exists(totalOut10p)) {dir.create(totalOut10p, recursive = TRUE)}
    
    # NOTE: this same approach should be easily extensible to bimonthing the temp itself if we want
    
    # directory with metabolism predictions
    predictIn <- file.path(datOut, 'ClimateAndProduction', 'Predictions', predicteds[i])
    
    # # Directory with inundation
    # # Use volume; it's most relevant
    # inunIn <- file.path(datOut, 'Inundationprocessed', 'volInun')
    # 
    # # 10% volume inundations
    # inunIn10p <- file.path(datOut, 'Inundationprocessed', 'vol10p')
    # 
    # # directory with temperatures (needed for getting the relevant indices for predictions)
    # # Should be deprecated now
    # # tempIn <- file.path(datOut, 'Tempprocessed', 'weightedMean')
    # 
    # # Read in data ------------------------------------------------------------
    # 
    # # First, get the names of the catchments, so I can match them
    # catchFiles <- list.files(predictIn, pattern = '.rdata')
    # catchNames <- str_extract(catchFiles, pattern = '[A-z]*_') %>%
    #   str_remove('_') # I'm sure I could do this in one regex, but this is easier
    # catchNames
    
    # I'm not building anything with chunks, just saving. So, I think I can probably
    # run this locally, and use a standard for loop?
    # Though parallelizing would be nice. so maybe I will foreach it, but not return anything
    
    # For testing
    # ca <- 9 # 9 is Ed-Wak
    # startloop <- proc.time()
    # trashOut <- foreach(ca = 6) %dopar% {
    # trashOut <- foreach(ca = 1:length(catchNames)) %dopar% {
    # Will need to loop over this
    thisCatch <- catchNames[ca]
    load(file.path(predictIn, paste0(thisCatch, '_', predicteds[i], '.rdata')))
    load(file.path(inunIn, paste0(thisCatch, '_volInun.rdata')))
    load(file.path(inunIn10p, paste0(thisCatch, '_vol10p.rdata')))
    
    # load(file.path(tempIn, paste0(thisCatch, '_weightedMean.rdata')))
    
    # starpreds is in a different order than inundation because the processed
    # temps are in a different order and that's what the predictions depend on.
    # Ideally, would have saved the indices attached to starpreds. Next time. But
    # for now, read in temps and get the indices
    
    
    
    # Give them generic names
    # should have saved this with a different name. I COULD use starpreds, since
    # it's already generic, but keep consistent (and hopefully I'll fix it so it's
    # not generic later anyway)
    catchPredict <- get(paste0(thisCatch, '_', predicteds[i]))
    predictIndices <- get(paste0(thisCatch, '_', predicteds[i], "_index"))
    
    
    rm(list = c(paste0(thisCatch, '_', predicteds[i]), 
                paste0(thisCatch, '_', predicteds[i], "_index")))
    # rm(list = paste0(thisCatch, "_weightedMean_index"))
    # rm(list = paste0(thisCatch, "_weightedMean"))
    
    catchInun <- get(paste0(thisCatch, "_volInun"))
    inunIndices <- get(paste0(thisCatch, "_volInun_index"))
    
    rm(list = c(paste0(thisCatch, "_volInun"),
                paste0(thisCatch, "_volInun_index")))
    
    
    catchInun10p <- get(paste0(thisCatch, "_vol10p"))
    inunIndices10p <- get(paste0(thisCatch, "_vol10p_index"))
    
    rm(list = c(paste0(thisCatch, "_vol10p"),
                paste0(thisCatch, "_vol10p_index")))
    
    # Transform. Why on earth is predict back to WGS?
    # error catch crs issues. enfoirce albers
    catchPredict <- crscheck(catchPredict, 3577)
    predictIndices <- crscheck(predictIndices, 3577)
    catchInun <- crscheck(catchInun, 3577)
    inunIndices <- crscheck(inunIndices, 3577)
    catchInun10p <- crscheck(catchInun10p, 3577)
    inunIndices10p <- crscheck(inunIndices10p, 3577)
    
    # Make the ANAEs match in case they were sorted differently
    # Make the inundations match, since the predictions need to mutliplyh by both, and so everything matches
    inunmatch <- matchStarsIndex(index1 = inunIndices, stars1 = catchInun,
                                 index2 = inunIndices10p, stars2 = catchInun10p,
                                 testfinal = FALSE)
    # set the sorted versions as the names
    inunIndices10p <- inunmatch$index2
    catchInun10p <- inunmatch$stars2
    rm(inunmatch)
    
    # see Scripts/testingandscratch/mis_sortedANAEtesting.R for sorting this out
    resortpredict <- matchStarsIndex(index1 = inunIndices, stars1 = catchInun,
                                     index2 = predictIndices, stars2 = catchPredict,
                                     testfinal = FALSE)
    # set the sorted versions as the names
    predictIndices <- resortpredict$index2
    catchPredict <- resortpredict$stars2
    rm(resortpredict)
    
    
    
    
    # # Test
    # all(st_drop_geometry(inunIndices) == st_drop_geometry(predictIndices))
    # 
    
    # Get date breaks ---------------------------------------------------------
    
    inuntimes <- st_get_dimension_values(catchInun, which = 'time')
    predicttimes <- st_get_dimension_values(catchPredict, which = 'time')
    
    # Need to clip so we aren't doing anything beyond the overlapping times
    whichoverlap <- which(inuntimes >= min(predicttimes))
    overlaptimes <- inuntimes[inuntimes >= min(predicttimes)]
    
    # Get the relevant volumes
    overlapInun <- catchInun %>% slice('time', index = whichoverlap)
    overlapInun10p <- catchInun10p %>% slice('time', index = whichoverlap)
    
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
    
    # HAVE TO PUT THE DIMS BACK IN THE RIGHT ORDER
    if (attributes(st_dimensions(bimonthPredict))$name[1] != 'geometry') {
      bimonthPredict <- aperm(bimonthPredict, c(2,1))
    }
    
    # Multiply predictions by volumes -----------------------------------------
    
    # CHECK DIMS ARE IN THE RIGHT ORDER OR IT IS JUNK
    if (attributes(st_dimensions(overlapInun))$name[1] != 'geometry') {
      overlapInun <- aperm(overlapInun, c(2,1))
      warning("Inundation dimensions got swapped. This is unexpected. Investigate")
    }
    
    if (attributes(st_dimensions(overlapInun10p))$name[1] != 'geometry') {
      overlapInun10p <- aperm(overlapInun10p, c(2,1))
      warning("Inundation dimensions got swapped. This is unexpected. Investigate")
    }
    
    totalPredictVol <- bimonthPredict*overlapInun
    names(totalPredictVol) <- str_replace(names(totalPredictVol), pattern = 'bimonth', 
                                          replacement = 'predict_x_vol')
    
    totalPredictVol10p <- bimonthPredict*overlapInun10p
    names(totalPredictVol10p) <- str_replace(names(totalPredictVol10p), pattern = 'bimonth', 
                                             replacement = 'predict_x_vol10p')
    
    
    # Set up to save
    thisOutNameB <- paste0(thisCatch, '_', predicteds[i], '_PredictBimonthMean')
    assign(thisOutNameB, bimonthPredict)
    save(list = thisOutNameB, file = file.path(scriptOut, 
                                               paste0(thisOutNameB, '.rdata')))
    
    thisOutNameT <- paste0(thisCatch, '_', predicteds[i], '_PredictxVol')
    assign(thisOutNameT, totalPredictVol)
    save(list = thisOutNameT, file = file.path(totalOut, 
                                               paste0(thisOutNameT, '.rdata')))
    
    thisOutNameT10p <- paste0(thisCatch, '_', predicteds[i], '_PredictxVol10p')
    assign(thisOutNameT10p, totalPredictVol10p)
    save(list = thisOutNameT10p, file = file.path(totalOut10p, 
                                                  paste0(thisOutNameT10p, '.rdata')))
    
    # Get aggressive about cleanup, since some things will have new names
    rm(list = c(thisOutNameB, thisOutNameT, thisOutNameT10p, 
                'bimonthPredict', 'by_t', 
                'startdate', 'overlaptimes', 'catchInun', 'catchPredict'))
    # Return a null dummy, since I really just want side effects
    dummy <- NULL
  }


endloop <- proc.time()
looptime <- endloop - startloop
print('total time:')
print(looptime)
# 950 seconds local, 230+ HPC (for the whole basin, not just a test catchment)
# }

