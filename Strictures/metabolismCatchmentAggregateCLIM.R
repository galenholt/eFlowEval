# Plot at catchment aggregation
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

if (parSet == 'local') {
  plan(multisession)
} else if (parSet == 'hpc') {
  # plan(multicore(workers = availableCores(methods = 'Slurm')))
  plan(list(
    tweak(multicore, workers = availableCores(methods = 'Slurm')),
    tweak(multicore, workers = availableCores(methods = 'Slurm'))
  ))
} else {
  plan(sequential)
}

# Set the crs
whichcrs <- 3577
# directorys for the predictions themselves and the predictions x volume
fsd <- rep(c('logERdays/', 'logERdaysvalleys/', 'logGPPdays/', 'logGPPdaysvalleys/'), each = 3)
filesubdirs <- paste0(fsd, c('bimonth', 'bimonth/predictxvol', 'bimonth/predictxvol10p'))
# These are the data name suffixes
# suffixes <- c('PredictBimonthMean', 'PredictxVol')
trash <- foreach(sfun = 1:length(filesubdirs), .packages = c('sf', 'stars', 'dplyr', 'stringr')) %dopar% {
# for(sfun in 1:length(filesubdirs)) {
  filedir <- filesubdirs[sfun]
  # debug
  print(paste0('sfun is ', sfun))
  # suffix <- suffixes[sfun]
  # There are some that were NOT chunked- leave them alone, and just look in the chunked folder
  allIn <- file.path(datOut, 'ClimateAndProduction', 'Predictions', filedir)
  
  # I don't think I actually use the ANAEs, so no need to bring them in, now that I have ltimNoNorth alone
  # anaeIn <- file.path(datOut, 'ANAEprocessed')
  basinRef <- file.path(datOut, 'ANAEprocessed', 'ltimNoNorth.rdata')
  
  scriptFigOut <- file.path('strictOut', 'metabolism')
  scriptDatOut <- file.path(allIn, 'basinConcat')
  # Make the out directory, in case it doesn't exist
  if (!dir.exists(scriptFigOut)) {dir.create(scriptFigOut, recursive = TRUE)}
  if (!dir.exists(scriptDatOut)) {dir.create(scriptDatOut, recursive = TRUE)}
  
  # List the catchments
  catchFiles <- list.files(allIn, pattern = '*.rdata')
  catchNames <- str_split_fixed(catchFiles, pattern = '_', n = str_count(catchFiles[1], '_'))[,1]
  
  # str_remove_all('_') # I'm sure I could do this in one regex, but this is easier
  catchNames
  
  
  # Loop over each catchment, since that's how the files are structured for memory purposes
  # for (i in 1:length(catchNames)) {
  # for (i in 1:2) {
  loopstart <- proc.time()
  catchmentBasin <- foreach(i = 1:length(catchNames), # length(catchNames)
                            .combine=function(...) c(..., along = 1), # Pass dimension argument to c.stars
                            .multicombine=TRUE,
                            .inorder = TRUE, .packages = c('sf', 'stars', 'dplyr', 'stringr')) %dopar% { # I cannot sort out why I keep getting 'task 1 failed non numeric argument to mathematical function' when I use dopar
                              
                              # oneloopstart <- proc.time()
                              # Set up loop iterations
                              thisCatch <- catchNames[i] #13 is lachlan, to keep consistent with previous checking
                              thisFile <- catchFiles[i]
                              suffix <- str_extract(thisFile, pattern = '_[A-z 0-9]*')
                              
                              # Read in the data
                              # anfile <- file.path(anaeIn, paste0(thisCatch, 'ANAE.rdata'))
                              catchfile <- file.path(allIn, thisFile)
                              
                              
                              load(basinRef)
                              load(catchfile)
                              
                              
                              # get just this valley- would be nice to do this earlier, but unable
                              # Get the right valley- same as in processANAE
                              # Do it this way, NOT with the same index as above, because these are in a
                              # different order for some reason
                              valleys <-ltimNoNorth$ValleyName
                              valleys <- str_remove_all(valleys, ' ')
                              # Cut to just the valley we want
                              thisvalley <- which(valleys == thisCatch)
                              thisPoly <- ltimNoNorth[str_remove_all(ltimNoNorth$ValleyName, ' ') == thisCatch, ]
                              # thisPoly <- ltimNoNorth %>% slice(thisvalley)
                              
                              # give standard names
                              # theseANAEs <- get(paste0(thisCatch, 'ANAE'))
                              # thesePolys <- get(paste0(thisCatch, '_', summaryFun))
                              # TODO: make the predictions return catchment-specific
                              # names. otherwise this is silly. But want to be set
                              # up for that fix- see inundationCatchmentAggregate for an example
                              thesePolys <- get(paste0(thisCatch, suffix))
                              # get('starpreds')
                              # theseIndices <- get(paste0(thisCatch, '_', summaryFun, '_index'))
                              # This works, but throws warnings, and I'm not sure why
                              rm(list = c(paste0(thisCatch, suffix))) 
                              
                              # the predictions are logged, but the multiplied by volume are not
                              if (str_ends(filedir, 'bimonth')) {
                                thesePolys <- exp(thesePolys) 
                                names(thesePolys) <- str_remove(names(thesePolys), 'log')
                                
                              }
                              
                              # make a smaller version ofor testing
                              # Smaller in the time dimension, NOT space, since we need all the anaes
                              # TODO: make this settable with a timespan- I've done it somewhere else
                              # thesePolys <- thesePolys[,,1:10] # %>% slice("time", 1:10) # Slice doesn't work with dopar
                              
                              # Set crs
                              # Why (how???) are some not in the right crs?
                              # theseANAEs <- st_transform(theseANAEs, whichcrs)
                              thisPoly <- st_transform(thisPoly, whichcrs)
                              thesePolys <- st_transform(thesePolys, whichcrs)
                              # theseIndices <- st_transform(theseIndices, whichcrs)
                              
                              # Set up to plot ----------------------------------------------------------
                              # Need to make this name-agnostic
                              # There's some spillover if we use ltimNoNorth, so we need to cut it to the correct valley
                              # Not sure if I want to drop_geometry or not? Not much reason to keep it, really
                              # areas <- theseIndices %>% 
                              #   mutate(area = as.numeric(st_area(.))) %>%
                              #   st_drop_geometry() %>%
                              #   select(area) %>%
                              #   pull()
                              # No need for the indices, just pull them from the stars
                              areas <- thesePolys %>%
                                st_geometry() %>%
                                st_area() %>%
                                as.numeric()
                              
                              # Aggregate to the catchment
                              # the predictions are based on the average temp in the
                              # polygon. To add them up to the catchment, we should
                              # sum, but weight by area- this is a bit funny, since
                              # they're volumetric, but the idea is just to weight
                              # the POTENTIAL of each wetland
                              if (str_ends(filedir, 'bimonth')) {
                                # use sum for total metabolic activity across the catchment
                                catchAgg <- catchAggW(strict = thesePolys, strictWeights = areas,
                                                      FUN = sum, summaryPoly = thisPoly)
                              } else {
                                # For metabolism x volume, we do NOT want to area-weight,
                                # because we're just adding up total metabolism in the
                                # catchment, and volume already takes care of all
                                # relevant area effects
                                catchAgg <- aggregate(thesePolys, 
                                                      by = thisPoly, 
                                                      FUN = sum, na.rm = TRUE)
                              }
                              
                              # For some terrible reason, if everything is NA, it returns 0s instead of NAs. Fix
                              allNAS <- function(x) {
                                all(is.na(x))
                              }
                              
                              whichAllNAS <- which(purrr::map_lgl(thesePolys, allNAS))
                              
                              # indexing into the stars attributes is terrible
                              if (length(whichAllNAS) > 0) {
                                for (n in 1:length(whichAllNAS)) {
                                  catchAgg[[whichAllNAS[n]]] <- NA
                                }
                              }
                              
                              
                              
                              
                              # names(catchAgg) <- 'totalareainundated'
                              names(catchAgg) <- str_c('catchSum_', names(catchAgg))
                              
                              # oneloopend <- proc.time()
                              # onelooptime <- oneloopend - oneloopstart
                              # print(paste0('finished loop ', i, '(', thisCatch, ')', 'time = ', onelooptime))
                              
                              catchAgg
                            }
  loopend <- proc.time()
  looptime <- loopend-loopstart
  looptime
  # 212 on HPC, longer local
  
  # Let's save that so we don't have to re-do the loop calcs
  save(catchmentBasin, 
       file = file.path(scriptDatOut, paste0('catchmentAggregated.rdata')))
  
  # to keep the foreach small
  trashout <- NULL
}




