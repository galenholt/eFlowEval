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
plan(multicore)

# # For local testing
# plan(multisession)
# # we don't actually need args[7] because it specifies summaryFun in the
# # dataProcessing scripts. But easier to leave and throw away?
# args <- c('blah', 'b', 'c', 'g', '5', 't', 'a', '96', 'Broken')

# Setup -------------------------------------------------------------------

# This is a bit different than the dataprocessing scripts, because I don't use
# separate runs for summaryfuns, but do all the predicting in one go.

# Make a sub-directory for the subchunk

subOuts <- c('logGPPdaysvalleys', 'logGPPdays', 'logERdaysvalleys','logERdays')

scriptOut <- file.path(datOut, 'ClimateAndProduction', 'Predictions')
scriptOuts <- file.path(scriptOut,
                       subOuts, 
                       'chunked',
                       str_flatten(args[9:length(args)], collapse = '/sub_'))
# if (!dir.exists(scriptOut)) {dir.create(scriptOut, recursive = TRUE)}

# 
for (i in 1:length(scriptOuts)) {
  if (!dir.exists(scriptOuts[i])) {dir.create(scriptOuts[i], recursive = TRUE)}
}
# 
# 

# chunksize in n anae polygons
# NOT for the array chunking, but for the non-chunked version. not sure if we want to add this back in?
# chunksize <- 1000

# I htink rather than a slurm array, pass a catchment name?
# could do either, really. Sort out arg orders.
# arraynum <- as.numeric(args[8])
# Going to be easiest I think to base on names rather than arraynums and name indices.
thisCatch <- args[9] # "EdwardWakool" #  # 'Murrumbidgee' # For testing- needs to be grabbed from catchNames in a loop

print(thisCatch)

# Choose a size for the chunks. This is likely better elsewhere, but
nchunks <- 100
arraynum <- as.numeric(args[8])
chunkName <- args[8]

# this may be NA if there are no outer chunks
if (length(args) > 9) {
  outerchunks <- as.integer(args[10:length(args)])
  onelayer <- FALSE
} else {
  onelayer <- TRUE
}

# stop('testing end here to make sure passing catchment name')
# Read in data ------------------------------------------------------------

## TEMPS
# Need to wrap this over catchments
tempsIn <- file.path(datOut, 'Climateprocessed', 'weightedMeanCLIM')
# catchNames <- list.files(file.path(tempsIn, 'chunked')) # Get the catchment names from the folders
catchFiles <- list.files(tempsIn, pattern = '.rdata')
catchNames <- str_extract(catchFiles, pattern = '[A-z]*_') %>%
  str_remove('_') # I'm sure I could do this in one regex, but this is easier
catchNames
# And test with and without a catchment in the set of catchments from metabolism
thisWMname <- paste0(thisCatch, '_weightedMeanCLIM')
load(file = file.path(tempsIn, paste0(thisWMname, '.rdata')))

## Regressions
regIn <- file.path(datOut, 'TempAndProduction', 'tempMetabolismRegressions.rdata')
load(regIn)

# change its name to something generic
# This is annoying, but I guess not too bad
weightedMeanTemps <- get(thisWMname)
weightedMeanIndex <- get(paste0(thisWMname, '_index'))
rm(list = c(thisWMname, paste0(thisWMname, '_index')))

# Sort out the chunks -----------------------------------------------------

# FIRST, get the correct outer chunk that we want to drill into
# I *think* this wouldn't be TOO terrible to make recursive if needed, by
# looping over args[10]: length(args) and grabbing the desired bit each time
# as long as we weren't also chaning nchunks. Although that wouldn't be too
# bad either, really, just would need more args
# Get the row indices from the array number 

# For loop lets us drill down by feeding additional arguments to the shell script
if (!onelayer) { # handle the case where we're not subchunking
  for (chun in 1:length(outerchunks)) {
    
    # Handle the case where we've drilled down to where there are fewer rows than chunks
    # If nchunks > nrow(), it will break it up fine. Catch the case where that's not true
    if (nchunks >= nrow(weightedMeanIndex)) {
      # If we're trying to grab a chunk that is in the available rows, just pass out the row
      # Otherwise, we don't want to pass out anything.
      if (outerchunks[chun] <= nrow(weightedMeanIndex)) {
        weightedMeanIndex <- weightedMeanIndex[outerchunks[chun], ]
        weightedMeanTemps <- weightedMeanTemps[, outerchunks[chun], ]
      } else {
        stop('indexing beyond end of weightedMeanIndex')
      }
    } else { 
      # The usual case, define the edges of the chunk of weightedMeanIndex
      outersize <- ceiling(nrow(weightedMeanIndex)/nchunks)
      # arraynum <- 3
      prevoutertop <- outersize*(outerchunks[chun]-1)
      outerbottom <- prevoutertop+1
      
      if (outerchunks[chun] == nchunks) {
        outertop <- nrow(weightedMeanIndex) # make sure we finish
      } else {
        outertop <- prevoutertop + outersize
        # Can be too high even for chunks before the last one in weird edge
        # cases, so handle that
        if (outertop > nrow(weightedMeanIndex)) {
          outertop <- nrow(weightedMeanIndex) 
        }
      }
      
      # cut to this chunk of polygons
      weightedMeanIndex <- weightedMeanIndex[outerbottom:outertop, ]
      weightedMeanTemps <- weightedMeanTemps[, outerbottom:outertop, ]
    }
  }
}


# THEN, break up into the main chunks that need to be run here
# This is exactly the same, but uses arraynum to define the chunk instead of a
# predefined argument. Could easily put in the loop but not going to for
# clarity, and because the ordering is a bit backwards

# As above, pass a single anaePoly if we're indexing in too far, skip entirely
# if we're past the end, otherwise define the chunk
if (nchunks >= nrow(weightedMeanIndex)) {
  # If we're trying to grab a chunk that is in the available rows, just pass out the row
  # Otherwise, we don't want to pass out anything.
  if (arraynum <= nrow(weightedMeanIndex)) {
    weightedMeanIndex <- weightedMeanIndex[arraynum, ]
    weightedMeanTemps <- weightedMeanTemps[, arraynum, ]
  } else {
    stop('indexing beyond end of weightedMeanIndex')
  }
} else {
  chunksize <- ceiling(nrow(weightedMeanIndex)/nchunks)
  # arraynum <- 3
  prevtop <- chunksize*(arraynum-1)
  bottom <- prevtop+1
  
  if (arraynum == nchunks) {
    top <- nrow(weightedMeanIndex) # make sure we finish
  } else {
    top <- prevtop + chunksize
    # Can be too high even for chunks before the last one in weird edge cases,
    # so handle that
    if (top > nrow(weightedMeanIndex)) {
      top <- nrow(weightedMeanIndex) 
    }
  }
  
  # When running many chunks, the ceiling() call can make the chunks large
  # enough to finish in chunks before the end, and then grab NAs and the
  # endpoint. Need to bypass that, but I still want the script that checks what
  # has finished to work, as well as the script to concatenate, so need to
  # output a dummy- check those scripts,so get the file type and naming
  # convention right- ie if I export a text does it get picked up error checking
  # but not concat? Or can I spit out an sf with no rows?
  # They both expect a .rdata. So, can I return an empty one? the catch is
  # they expect an index version too, and will try to concat. So I actually
  # need to build a dummy dpList
  # Well, I could also put a flag in their name to keep them out of
  # catchfiles in concat. Not sure what is safest
  # Probably the dpList with no-row entries, but let's see what that does
  # Does not work to just cut to 0 row dataframe- rastpolyjoin fails
  if (bottom > nrow(weightedMeanIndex)) {
    bottom <- 0
    top <- 0
  }
  
  # cut to this chunk of polygons
  weightedMeanIndex <- weightedMeanIndex[bottom:top, ]
  weightedMeanTemps <- weightedMeanTemps[, bottom:top, ]
}



print(paste0('number of polygons processing is ', nrow(weightedMeanIndex)))

# Predictions -------------------------------------------------

# Somehow going for the sf creates something 524Gb. that's bad. will need to chunk. which means the question is how much to chunk
# subCatch <- weightedMeanTemps %>% slice('geometry', 1:10) %>% slice('time', 500:515)
# Turn the kelvins into tempC
weightedMeanTemps[[1]] <- as.numeric(weightedMeanTemps[[1]])-272.15
names(weightedMeanTemps) <- 'tempC'

# Let's ignore time and just loop over the anaes. doing it that way simplifies
# things and has worked elsewhere (the subchunk files and rastPolyjoin)
chunkpred <- function(bottom, top) {
  subCatch <- weightedMeanTemps %>% slice('geometry', bottom:top, drop = FALSE) # %>% slice('time', 500:515)
  
  
  # Make into an sf dataframe so we can do the predictions
  # THIS IS A BIT ABSURD- BEEN GOING 30 MINS AND NOT FINISHED. SO THINK I'LL NEED
  # TO WRAP ALL THIS IN A FOREACH WITH A CHUNKMAKER BIT ON IT.
  # Question is how big to make the chunks.
  # Probably should write the whole smash as a function, then do a test.
  # Suppose another question is whether the HPC can just do it quickly.
  # AND, do I want to chunk over space, time, or both? Would be nice if I only chunked one dimension
  catchSF <- st_as_sf(subCatch, long = TRUE) # THe long argument keeps times
  
  # create the columns we need
  catchSF <- catchSF %>% 
    mutate(daysAwayFromWaterYear = daysfromWY(time), 
           bimonthFactor = as.factor(getBimonth(time)), # Get both time variables, I guess
           ValleyName = thisCatch,
           wateryear = getWaterYear(time))
  
  # predict new values
  catchSF <- catchSF %>%
    add_preds(gppDaysValleys, predname = 'logGPPdaysvalleys', interval = 'both')  %>%
    add_preds(gppDays, predname = 'logGPPdays', interval = 'both') %>%
    add_preds(erDaysValleys, predname = 'logERdaysvalleys', interval = 'both') %>%
    add_preds(erDays, predname = 'logERdays', interval = 'both')
  
  # throw out all the extra variables, leaving just the predictions
  catchSF <- catchSF %>%
    select(time, starts_with('log'), -contains('pfit'), -contains('cfit'))
  # catchSF
  
  # Turn into stars
  # This version for all model predictions. might as well (unless the resulting object is too big)
  starpredictions <- catchSF %>% # select(time, logGPPdays) %>%
    st_as_stars() %>%
    # st_redimension is more flexible than merge.stars()
    st_redimension(new_dims = st_dimensions(subCatch))
  starpredictions[[1]] <- NULL
  return(starpredictions)
}
# # Sort out the dims
# a <- chunkpred(bottom = 1, top = 2)
# b <- chunkpred(bottom = 1, top = 2)
# c(a,b, along = 1)

# # Step 1: sort out an optimal chunksize
# benchChunk <- microbenchmark::microbenchmark("a2" = { b <- chunkpred(bottom = 1, top = 2)},
#                                              "a10" = { b <- chunkpred(bottom = 1, top = 10)},
#                                              "a100" = { b <- chunkpred(bottom = 1, top = 100)},
#                                              "a200" = { b <- chunkpred(bottom = 1, top = 200)},
#                                              "a1000" = { b <- chunkpred(bottom = 1, top = 1000)},
#                                            times = 1, unit = 's')
# # benchS_S
# print('chunk speeds')
# print(benchChunk)
# # benchChunk$mean
# 
# # on HPC, 2 was
# 245/2
# # 10 
# 1526/10
# # 100
# 15273/100
# # 200
# 29206/200
# # 1000 
# 142448/1000
# # I think I'll stop there

# so, the above are ms, in seconds that's 142/1000

# 142*27/60 minutes expected
# Try to fire one off for 4 hours and see how long it takes

# step 2: modify the function to allow a bottom and top, then with the optimal
# chunksize, run and c() together as in the subchunk scripts
# Stolen from rastPolyJoin, need to modify the chunksizes, and confirm this makes sense

startbig <- proc.time()

# just parallel over each anae when array-chunking. I'm sure there's optimization that could be done, but hoping not necessary
# ngeoms <- dim(weightedMeanTemps)['geometry']
# nbreaks <- ceiling(ngeoms/chunksize) + 1
# breaks <- round(seq(from = 0, to = ngeoms, length.out = nbreaks))
starpreds <- foreach(l = 1:nrow(weightedMeanIndex),
                     .combine=function(...) c(..., along = 1), # Pass dimension argument to c.stars
                     .multicombine=TRUE) %do% {
                       bottom <- l
                       top <- l
                       chunkpred(bottom, top) 
                     }

endbig <- proc.time()

print('Time taken for loop')
print(endbig-startbig)

## TODO IF COPYING FOR NEW VERSIONS:
# save the name of the outputs, instead of starpreds (see other sorts of scripts for the assign() code)
# SAVE THE INDICES sf- they aren't the same coming out of the processing script, which translates to here

### Break up into fit and intervals separately
gppdv <- starpreds %>%
  select(contains('logGPPdaysvalleys'))

gppd <- starpreds %>%
  select(contains('logGPPdays'), -contains('valleys'))

erdv <- starpreds %>%
  select(contains('logERdaysvalleys'))

erd <- starpreds %>%
  select(contains('logERdays'), -contains('valleys'))

# Name them
assign(paste0(thisCatch, '_logGPPdaysvalleys', '_', chunkName), gppdv)
assign(paste0(thisCatch, '_logGPPdays', '_', chunkName), gppd)
assign(paste0(thisCatch, '_logERdaysvalleys', '_', chunkName), erdv)
assign(paste0(thisCatch, '_logERdays', '_', chunkName), erd)

assign(paste0(thisCatch, '_index', '_', chunkName), weightedMeanIndex)

# 3578 seconds total for bidgee (27000 anaes). Ends up just under 1GB. So that's not terrible
# New version with predictInterval took 6621 for Avoca. That's wuite a bit worse. Going to try with more cores on petrichor
save(list = c(paste0(thisCatch, '_logGPPdaysvalleys', '_', chunkName), 
              paste0(thisCatch, '_index', '_', chunkName)), 
     file = file.path(scriptOut, 'logGPPdaysvalleys', 'chunked', thisCatch,
                      paste0(thisCatch, '_logGPPdaysvalleys_', chunkName, '.rdata')))

save(list = c(paste0(thisCatch, '_logGPPdays', '_', chunkName), 
              paste0(thisCatch, '_index', '_', chunkName)), 
     file = file.path(scriptOut, 'logGPPdays', 'chunked', thisCatch,
                      paste0(thisCatch, '_logGPPdays_', chunkName, '.rdata')))

save(list = c(paste0(thisCatch, '_logERdaysvalleys', '_', chunkName), 
              paste0(thisCatch, '_index', '_', chunkName)), 
     file = file.path(scriptOut, 'logERdaysvalleys',  'chunked', thisCatch,
                      paste0(thisCatch, '_logERdaysvalleys_', chunkName, '.rdata')))

save(list = c(paste0(thisCatch, '_logERdays', '_', chunkName), 
              paste0(thisCatch, '_index', '_', chunkName)), 
     file = file.path(scriptOut, 'logERdays', 'chunked', thisCatch,
                      paste0(thisCatch, '_logERdays_', chunkName, '.rdata')))

