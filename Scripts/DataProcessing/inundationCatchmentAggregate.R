# Plot inundation
# Libraries and system setup
source('directorySet.R')

# Let's get libraries here, then sort out git then sort out making this a library so we don't have to deal with all the library crap
# library(sp)
# library(rgeos)
library(here)
library(tidyverse)
library(sf)
library(stars)
library(foreach)
library(doFuture)

# Set up parallel backend
registerDoFuture()
# plan(multisession)
plan(sequential) # dopar doesn't work, so no sense setting up a multisession

# This is almost EXACTLY a library load at this point. Just need to actually wrap it up and split the git
basicfuns <- list.files(here('Functions'))
basicfuns <- file.path('Functions', basicfuns)
# read in those functions
sapply(basicfuns, source)

# Set the crs
whichcrs <- 3577
# directory
summaryFun <- 'areaInun'
# There are some that were NOT chunked- leave them alone, and just look in the chunked folder
inunIn <- paste0(datOut, '/Inundationprocessed/', summaryFun)
anaeIn <- file.path(datOut, 'ANAEprocessed')

scriptFigOut <- file.path('strictOut', 'inundation')
scriptDatOut <- file.path(inunIn, 'basinConcat')
# Make the out directory, in case it doesn't exist
if (!dir.exists(scriptFigOut)) {dir.create(scriptFigOut, recursive = TRUE)}
if (!dir.exists(scriptDatOut)) {dir.create(scriptDatOut, recursive = TRUE)}

# List the catchments
catchfiles <- list.files(inunIn, pattern = '*.rdata')
catchNames <-str_remove(catchfiles, pattern = paste0('_', summaryFun, '.rdata'))

# Loop over each catchment, since that's how the files are structured for memory purposes
# for (i in 1:length(catchNames)) {
# for (i in 1:2) {
loopstart <- proc.time()
inunBasin <- foreach(i = 1:length(catchNames), # length(catchNames)
        .combine=function(...) c(..., along = 1), # Pass dimension argument to c.stars
        .multicombine=TRUE,
        .inorder = TRUE) %do% { # dopar will take some work; the dplyr verbs don't carry into it very well
 
          # oneloopstart <- proc.time()
           # Set up loop iterations
  thisCatch <- catchNames[i] #13 is lachlan, to keep consistent with previous checking
  thisInunfile <- catchfiles[i]
  
  
  # Read in the data
  anfile <- file.path(anaeIn, paste0(thisCatch, 'ANAE.rdata'))
  inunfile <- file.path(inunIn, thisInunfile)
  
  load(anfile)
  load(inunfile)
  
  # get just this valley- would be nice to do this earlier, but unable
  # Get the right valley- same as in processANAE
  # Do it this way, NOT with the same index as above, because these are in a
  # different order for some reason
  valleys <-ltimNoNorth$ValleyName
  valleys <- str_remove_all(valleys, ' ')
  # Cut to just the valley we want
  thisvalley <- which(valleys == thisCatch)
  thisPoly <- ltimNoNorth %>% slice(thisvalley)
  
  # give standard names
  theseANAEs <- get(paste0(thisCatch, 'ANAE'))
  theseInuns <- get(paste0(thisCatch, '_', summaryFun))
  theseIndices <- get(paste0(thisCatch, '_', summaryFun, '_index'))
  rm(list = c(paste0(thisCatch, 'ANAE'), 
              paste0(thisCatch, '_', summaryFun),
              paste0(thisCatch, '_', summaryFun, '_index'))) 
  
  # make a smaller version of the inundation for testing
  # Smaller in the time dimension, NOT space, since we need all the anaes
    # TODO: make this settable with a timespan- I've done it somewhere else
  # theseInuns <- theseInuns[,,1:10] # %>% slice("time", 1:10) # Slice doesn't work with dopar
  
  # Set crs
  theseANAEs <- st_transform(theseANAEs, whichcrs)
  thisPoly <- st_transform(thisPoly, whichcrs)
  
  # Set up to plot ----------------------------------------------------------
  # Need to make this name-agnostic
  # There's some spillover if we use ltimNoNorth, so we need to cut it to the correct valley
  # Not sure if I want to drop_geometry or not? Not much reason to keep it, really
  areas <- theseIndices %>% mutate(area = as.numeric(st_area(.))) %>%
    st_drop_geometry() %>%
    select(area) %>%
    pull()
  
  # Let's just try my catchment aggregator
  # use sum for total area of ANAEs inundated
  inunAgg <- catchAggW(strict = theseInuns, strictWeights = areas,
                        FUN = sum, summaryPoly = thisPoly)
  names(inunAgg) <- 'totalareainundated'
  
  # oneloopend <- proc.time()
  # onelooptime <- oneloopend - oneloopstart
  # print(paste0('finished loop ', i, '(', thisCatch, ')', 'time = ', onelooptime))
  
  inunAgg
}
loopend <- proc.time()
looptime <- loopend-loopstart
looptime


# Let's save that so we don't have to re-do the loop calcs
  # Should we save this to the inundation folder?
save(inunBasin, 
     file = file.path(scriptDatOut, paste0('inunCatchConcat_', summaryFun, '.rdata')))




