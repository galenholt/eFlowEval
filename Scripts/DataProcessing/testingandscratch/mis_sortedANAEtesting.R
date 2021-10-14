# SCRIPT BASED ON STRICTURES/bimonthProduction.R, but ended up being about sorting out differently sorted ANAES.

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

# directory with temperatures (needed for getting the relevant indices for predictions)
tempIn <- file.path(datOut, 'Tempprocessed', 'weightedMean')

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
# ca <- 9 # 9 is Ed-Wak
startloop <- proc.time()
# trashOut <- foreach(ca = 4:5) %dopar% {
trashOut <- foreach(ca = 1:length(catchNames)) %dopar% {
  # Will need to loop over this
  thisCatch <- catchNames[ca]
  load(file.path(predictIn, paste0(thisCatch, '_predictedGPPER.rdata')))
  load(file.path(inunIn, paste0(thisCatch, '_volInun.rdata')))
  load(file.path(tempIn, paste0(thisCatch, '_weightedMean.rdata')))
  
  # starpreds is in a different order than inundation because the processed
  # temps are in a different order and that's what the predictions depend on.
  # Ideally, would have saved the indices attached to starpreds. Next time. But
  # for now, read in temps and get the indices
  
  
  
  # Give them generic names
  # should have saved this with a different name. I COULD use starpreds, since
  # it's already generic, but keep consistent (and hopefully I'll fix it so it's
  # not generic later anyway)
  catchPredict <- get('starpreds')
  predictIndices <- get(paste0(thisCatch, "_weightedMean_index"))
  
  # are those predictions right? quick look while I'm testing why the inun version isn't
  catchPredict
  EdwardWakool_weightedMean
  # AND, do they match? is using the index OK? 
  inter3 <- diag(st_intersects(st_as_sf(catchPredict[1, , 1]), 
                               st_as_sf(EdwardWakool_weightedMean[1, , 1]), sparse = FALSE))
  # inter3 is if they match, so sum(!inter3) should be 0
  sum(!inter3)
  
  # Pretty sure this should just be a linear relationship, so
  predcheck <- tibble(predictions = c(catchPredict[[1]][,2000:2010]), 
                      temps = c(EdwardWakool_weightedMean[[1]][,2000:2010]))
  ggplot(predcheck, aes(x = temps, y = predictions)) + geom_point()
  # yeah, basically, with the seasonality thrown in as well
  
  rm(list = paste0('starpreds'))
  rm(list = paste0(thisCatch, "_weightedMean_index"))
  rm(list = paste0(thisCatch, "_weightedMean"))
  
  catchInun <- get(paste0(thisCatch, "_volInun"))
  inunIndices <- get(paste0(thisCatch, "_volInun_index"))
  
  rm(list = paste0(thisCatch, "_volInun"))
  rm(list = paste0(thisCatch, "_volInun_index"))
  
  # Transform. Why on earth is predict back to WGS?
  catchPredict <- st_transform(catchPredict, st_crs(catchInun))
  predictIndices <- st_transform(predictIndices, st_crs(catchInun))
  
  # They don't match
  all(st_drop_geometry(inunIndices) == st_drop_geometry(predictIndices))
  # which(st_drop_geometry(inunIndices) != st_drop_geometry(predictIndices))
  # are they all there though? Yes
  all(pull(st_drop_geometry(inunIndices)) %in% pull(st_drop_geometry(predictIndices)))
  
  
  # Is the ANAE itself usorted?
    # And did it work to re-write mathStarsIndex to use it?
  load(file.path(datOut, 'ANAEprocessed', 'EdwardWakoolANAE.rdata'))
  which(duplicated(EdwardWakoolANAE$UID))
  all(inunIndices$UID == EdwardWakoolANAE$UID) # True
  all(predictIndices$UID == EdwardWakoolANAE$UID) # false
  # AAAAA THE CRSES
  EdwardWakoolANAE <- st_transform(EdwardWakoolANAE, st_crs(catchPredict))
  # TEST
  resortFromANAE <- matchStarsIndex(index1 = EdwardWakoolANAE, stars1 = NULL,
                  index2 = predictIndices, stars2 = catchPredict, indexcol = 1)
  
  # Test the check with something that should work
  inter3 <- diag(st_intersects(st_geometry(EdwardWakoolANAE), 
                               st_geometry(inunIndices), sparse = FALSE))
  
  sum(!inter3)
  ### THIS BIT OF CODE WRITTEN AFTER LOTS BELOW (now commented out so we can get
  ### to further checks). FUNCTION TESTING AND SETTING UP FOR REAL RUNNING
  resortpredict <- matchStarsIndex(index1 = inunIndices, stars1 = catchInun,
                                   index2 = predictIndices, stars2 = catchPredict)
  
  ### RESET THE STArS AND INDICeS
  predictIndices <- resortpredict$index2
  catchPredict <- resortpredict$stars2
  rm(resortpredict)
  # Test
  all(st_drop_geometry(inunIndices) == st_drop_geometry(predictIndices))
  
  
  # 
  
  # # How do the orders work?
  # ordermatcher <- match(pull(st_drop_geometry(inunIndices)), pull(st_drop_geometry(predictIndices)))
  # 
  # # Need to re-order the stars. BUt first, need to make sure I can check
  # catchPredict <- st_transform(catchPredict, st_crs(catchInun))
  # predictIndices <- st_transform(predictIndices, st_crs(catchInun))
  # inter <- diag(st_intersects(st_as_sf(catchPredict[1,1:200, 1]), st_as_sf(catchInun[1, 1:200, 1]), sparse = FALSE))
  # inter
  # 
  # # First, what are the fromtos?
  # testInun <- inunIndices
  # testInun <- testInun[order(ordermatcher), ] 
  # # Check
  # all(st_drop_geometry(testInun) == st_drop_geometry(predictIndices))
  # which(st_drop_geometry(testInun) != st_drop_geometry(predictIndices))
  # all(pull(st_drop_geometry(testInun)) %in% pull(st_drop_geometry(predictIndices)))
  # # huh?
  # testInun[2830:2840, ]
  # predictIndices[2830:2840, ]
  # plot(testInun[2830:2840, ])
  # plot(testInun[2832:2833, ])
  # # There are duplicates. And the sort takes the first, so this won't get fixed. WHY are there dups?
  # which(duplicated(st_drop_geometry(testInun)))
  # which(duplicated(st_drop_geometry(predictIndices)))
  # 
  # # what are those? and can we plot the data to see if it differs?
  # duppreds <- predictIndices[which(duplicated(st_drop_geometry(predictIndices))), ]
  # bothdups <- which(predictIndices$UID %in% duppreds$UID)
  # onlydups <- predictIndices[bothdups, ]
  # plot(predictIndices[bothdups, ])
  # plot(catchPredict[1, bothdups, 100:110])
  # 
  # # are these duplicates generated in the anae?
  # load(file.path(datOut, 'ANAEprocessed', 'EdwardWakoolANAE.rdata'))
  # which(duplicated(EdwardWakoolANAE$UID))
  # # YES. AAAARRGGHHGHGHG
  # # do they ACTUALLY get given the same UID? Sure looks like it in processANAE
  # # Nothing happens there excetp saving after mutate(UID = lwgeom::st_geohash(geometry, precision = 9)))
  # onlydups %>%
  #   mutate(UID2 = lwgeom::st_geohash(geometry, precision = 9)) %>% st_drop_geometry()
  # # SO MANY SWEARS
  # # what if I crank out the precision?
  # onlydups %>%
  #   mutate(UID2 = lwgeom::st_geohash(geometry, precision = 19)) %>% st_drop_geometry()
  # # That works
  # 
  # # How many are there total in the whole dataset?
  # rm(EdwardWakoolANAE)
  # load(file.path(datOut, 'ANAEprocessed', 'ANAEBasinClim.rdata'))
  # which(duplicated(ANAEbasinclim$UID))
  # # 16
  # dups <- ANAEbasinclim[which(duplicated(ANAEbasinclim$UID)), ]
  # bothdups <- which(ANAEbasinclim$UID %in% dups$UID)
  # onlydups <- ANAEbasinclim[bothdups, ]
  # onlydups
  # plot(onlydups[1:2, ])
  # plot(onlydups[3:4, ])
  # plot(onlydups[5:6, ])
  # plot(onlydups[7:8, ])
  # plot(onlydups[9:10, ])
  # plot(onlydups[11:12, ])
  # plot(onlydups[13:14, ])
  # plot(onlydups[15:16, ])
  # 
  # # OK, so there are two issues: 
  # # 1. can I just increase the resolution of the UID to get them to resolve?
  # # 2: can I handle this somehow NOW without having to reprocess everything?
  # onlydups %>%
  #   mutate(UID2 = lwgeom::st_geohash(geometry, precision = 100)) %>% 
  #   select(UID2) %>% st_drop_geometry() %>% duplicated() %>% sum()
  # # uhhh
  # # But 11 gets all but 1
  # onlydups %>%
  #   mutate(UID2 = lwgeom::st_geohash(geometry, precision = 11)) %>% 
  #   select(UID2) %>% st_drop_geometry() %>% duplicated() %>% sum()
  # 
  # # Ok, on to handling the issue as-is
  # # the question is, can we somehow figure out what the matching ANAEs are for those with duplicated UIDs?
  # duppreds <- predictIndices[which(duplicated(st_drop_geometry(predictIndices))), ]
  # bothduppreds <- which(predictIndices$UID %in% duppreds$UID)
  # onlyduppredss <- predictIndices[bothduppreds, ]
  # plot(predictIndices[bothduppreds, ])
  # plot(catchPredict[1, bothduppreds, 105:106])
  # 
  # dupinun <- inunIndices[which(duplicated(st_drop_geometry(inunIndices))), ]
  # bothdupinun <- which(inunIndices$UID %in% dupinun$UID)
  # onlydupinuns <- inunIndices[bothdupinun, ]
  # plot(inunIndices[bothdupinun, ])
  # plot(catchInun[1, bothdupinun, 105:106])
  # 
  # onlydupinuns
  # onlyduppredss
  # 
  # # all I need to do is match order, so I need to make them unique but match across datasets
  # inunIndices <- inunIndices %>%
  #   mutate(UID2 = ifelse(UID %in% dupinun$UID,
  #                        paste0(UID, st_area(geometry)), UID))
  # predictIndices <- predictIndices %>%
  #   mutate(UID2 = ifelse(UID %in% duppreds$UID,
  #                        paste0(UID, st_area(geometry)), UID))
  # 
  # inunIndices[bothdupinun, ]
  # predictIndices[bothduppreds, ]
  # # Would have thought this would work
  # st_nearest_feature(inunIndices[bothdupinun, ], predictIndices[bothduppreds, ])
  # # I think this does work, just need to check after shuffling
  # st_equals_exact(inunIndices[bothdupinun, ], predictIndices[bothduppreds, ], par = 1, sparse = FALSE)
  # bothdupshuffle <- bothduppreds[c(4,2,3,1)]
  # st_equals_exact(inunIndices[bothdupinun, ], predictIndices[bothdupshuffle, ], par = 1, sparse = TRUE)
  # 
  # # match up
  # matchindex <- as.numeric(st_equals_exact(inunIndices[bothdupinun, ], 
  #                                          predictIndices[bothdupshuffle, ], 
  #                                          par = 1, sparse = TRUE))
  # paste0(inunIndices[bothdupinun, ]$UID, 1:4)
  # paste0(predictIndices[bothdupshuffle, ]$UID, matchindex)
  # 
  # inunIndices$UID2[bothdupinun] <- paste0(inunIndices[bothdupinun, ]$UID, 1:4)
  # predictIndices$UID2[bothdupshuffle] <- paste0(predictIndices[bothdupshuffle, ]$UID, matchindex)
  # inunIndices[bothdupinun, ]$UID2
  # predictIndices[bothduppreds, ]$UID2
  # plot(inunIndices[bothdupinun, 'UID2'])
  # plot(predictIndices[bothduppreds, 'UID2'])
  # plot(inunIndices[inunIndices$UID2 == inunIndices$UID2[1], 'UID2'])
  # plot(predictIndices[predictIndices$UID2 == inunIndices$UID2[1], 'UID2'])
  # plot(inunIndices[inunIndices$UID2 == inunIndices$UID2[2], 'UID2'])
  # plot(predictIndices[predictIndices$UID2 == inunIndices$UID2[2], 'UID2'])
  # plot(inunIndices[inunIndices$UID2 == inunIndices$UID2[3], 'UID2'])
  # plot(predictIndices[predictIndices$UID2 == inunIndices$UID2[3], 'UID2'])
  # plot(inunIndices[inunIndices$UID2 == inunIndices$UID2[4], 'UID2'])
  # plot(predictIndices[predictIndices$UID2 == inunIndices$UID2[4], 'UID2'])
  # 
  # # Now does the sort work?
  # ordermatcher <- match(inunIndices$UID2, predictIndices$UID2)
  # # First, what are the fromtos?
  # testInun <- inunIndices
  # testInun <- testInun[order(ordermatcher), ] 
  # # Check
  # all(st_drop_geometry(testInun) == st_drop_geometry(predictIndices))
  # which(st_drop_geometry(testInun) != st_drop_geometry(predictIndices))
  # all(pull(st_drop_geometry(testInun)) %in% pull(st_drop_geometry(predictIndices)))
  # 
  # # TODO: CLEAN THAT UP INTO THE THIGNS WE ACTUALLY DO
  # # TODO: FIGURE OUT HOW TO SORT THE STARS
  # testIstars <- catchInun
  # testIstars <- testIstars[,order(ordermatcher), ]
  # inter2 <- diag(st_intersects(st_as_sf(catchPredict[1,1:200, 1]), st_as_sf(testIstars[1, 1:200, 1]), sparse = FALSE))
  # inter2
  # # Do they ALL match?
  # inter3 <- diag(st_intersects(st_as_sf(catchPredict[1, , 1]), 
  #                              st_as_sf(testIstars[1, , 1]), sparse = FALSE))
  # sum(!inter3)
  # # Yup. so the thing i thought was going to be hard was easy, and the easy part took all day.....
  # # Clean that up wiht the above, but that should work....
  # 
  # 
  
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
  # the geometry here is swapped#####
  
  # that should still be roughly linear with temps. But to get that sorted I
  # need to get the temps in here- they were done elsewhere bimonth temps too
  bimonthTemp <- tempaggregate(starObj = EdwardWakool_weightedMean, by = by_t, FUN = mean, na.rm = TRUE)
  bimonthTemp
  bimonthPredict
  ### THOSE ARE SHIFTED BY A BIMONTH####
  
  # If that worked, that should be roughl linear, with some noise
  predcheck <- tibble(predictions = c(bimonthPredict[[1]][20:25, ]), 
                      temps = c(bimonthTemp[[1]][20:25,]))
  ggplot(predcheck, aes(x = temps, y = predictions)) + geom_point()
  # that's actually quite a lot of noise. Do they line up better with shifted time?
  predcheck <- tibble(predictions = c(bimonthPredict[[1]][20:25, ]), 
                      temps = c(bimonthTemp[[1]][21:26,]))
  ggplot(predcheck, aes(x = temps, y = predictions)) + geom_point()
  # NOPE- so the time VALUES are shifted, but the indices are correct... They
  # may not be in the bimonth temp file, but that doesn't affect this- separate
  # issue
  
  # Multiply predictions by volumes -----------------------------------------
  
  # This is probably not necessary
  # bimonthPredict <- st_transform(bimonthPredict, st_crs(overlapInun))
  
  # Then just do the mult. the dims are swapped, but Ops.stars seems to handle that

  totalPredictVol <- bimonthPredict*overlapInun
  names(totalPredictVol) <- str_replace(names(totalPredictVol), pattern = 'bimonth', replacement = 'predict_x_vol')
  
  # TESTING
  totalPredictVol
  # If that worked, that should be roughl linear, with some noise
  predcheck <- tibble(predictions = c(totalPredictVol[[1]][20:25, ]), 
                      vols = c(overlapInun[[1]][,20:25]))
  ggplot(predcheck, aes(x = vols, y = predictions)) + geom_point()
  
  # Is it the flopped dims?
  # DOES IT THOUGH?
  bimonthSwap <- aperm(bimonthPredict, c(2,1))
  
  totalPredictVol <- bimonthSwap*overlapInun
  names(totalPredictVol) <- str_replace(names(totalPredictVol), pattern = 'bimonth', replacement = 'predict_x_vol')
  
  # TESTING
  totalPredictVol
  # If that worked, that should be roughl linear, with some noise
  predcheck <- tibble(predictions = c(totalPredictVol[[1]][20:30, ]), 
                      vols = c(overlapInun[[1]][20:30,]),
                      temps = c(aperm(bimonthTemp, c(2,1))[[1]][20:30,]))
  ggplot(predcheck, aes(x = vols, y = predictions, color = temps)) + geom_point()
  
  # Without the temps, so can use breakpoints in bimonthProdcution (where there aren't temps)
  predcheck <- tibble(predictions = c(totalPredictVol[[1]][20:30, ]), 
                      vols = c(overlapInun[[1]][20:30,]))
  ggplot(predcheck, aes(x = vols, y = predictions)) + geom_point()
  
  
  
  
  
  
  
  
  
  
  
  
  
  

# STOP TESTING SECTIOn ----------------------------------------------------

  
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

endloop <- proc.time()
looptime <- endloop - startloop
print('total time:')
print(looptime)
# 850 seconds local, 230 HPC