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
# plan(multisession)
plan(multisession) # dopar doesn't work, so no sense setting up a multisession

# Set the crs
whichcrs <- 3577
# directorys for the predictions themselves and the predictions x volume
filesubdirs <- c('bimonth')
# These are the data name suffixes
# suffixes <- c('PredictBimonthMean', 'PredictxVol')

for(sfun in 1:length(filesubdirs)) {
  filedir <- filesubdirs[sfun]
  # suffix <- suffixes[sfun]
  # There are some that were NOT chunked- leave them alone, and just look in the chunked folder
  allIn <- file.path(datOut, 'Climateprocessed', 'weightedMeanCLIM', filedir)
  
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
  catchNames <- str_extract(catchFiles, pattern = '[A-z]*_') %>%
    str_remove('_') # I'm sure I could do this in one regex, but this is easier
  catchNames
  
  
  
  # Loop over each catchment, since that's how the files are structured for memory purposes
  # for (i in 1:length(catchNames)) {
  # for (i in 1:2) {
  loopstart <- proc.time()
  catchmentBasin <- foreach(i = 1:length(catchNames), # length(catchNames)
                            .combine=function(...) c(..., along = 1), # Pass dimension argument to c.stars
                            .multicombine=TRUE,
                            .inorder = TRUE) %do% { # I cannot sort out why I keep getting 'task 1 failed non numeric argument to mathematical function' when I use dopar
                              
                              # oneloopstart <- proc.time()
                              # Set up loop iterations
                              thisCatch <- catchNames[i] #13 is lachlan, to keep consistent with previous checking
                              thisFile <- catchFiles[i]
                              suffix <- str_extract(thisFile, pattern = '_[A-z]*')
                              
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
                              
                              # Let's just try my catchment aggregator
                              # DO NOT USE catchAggW for weighted means. It is
                              # a workaround for functions WITHOUT built-in
                              # weighting
                              # # use sum for total metabolic activity across the catchment
                              # catchAgg <- catchAggW(strict = thesePolys, strictWeights = areas,
                              #                       FUN = mean, summaryPoly = thisPoly)
                              # 
                              
                              # Testing 
                              # wmeans <- foreach(r = 1:nrow(thesePolys[[1]]), .combine = c) %do% {
                              #   weighted.mean(thesePolys[[1]][r, ], w = areas, na.rm = TRUE)
                              # }
                              
                              # That said, we DO want to area-weight the temp averages
                              # For some super frustrating reason,
                              # weighted.mean and weights stops working for
                              # big stars objects. There is no obvious reason
                              # catchAgg <- aggregate(thesePolys, by = thisPoly, 
                              #                       FUN = weighted.mean, w = areas, na.rm = TRUE)
                              
                              # So, a horrible workaround is to use catchAggW to
                              # get the weighted SUM, and then divide to get the
                              # mean
                              catchAgg <- catchAggW(strict = thesePolys, strictWeights = areas,
                                                    FUN = sum, summaryPoly = thisPoly)
                              # And for some reason NA turns into 0 this way. argh
                              catchAgg[[1]][which(catchAgg[[1]] == 0)] <- NA
                              catchAgg <- catchAgg/sum(areas)
                              
                              # names(catchAgg) <- 'totalareainundated'
                              names(catchAgg) <- str_c('catchMean_', names(catchAgg))
                              
                              # oneloopend <- proc.time()
                              # onelooptime <- oneloopend - oneloopstart
                              # print(paste0('finished loop ', i, '(', thisCatch, ')', 'time = ', onelooptime))
                              
                              catchAgg
                            }
  loopend <- proc.time()
  looptime <- loopend-loopstart
  looptime
  
  # Let's save that so we don't have to re-do the loop calcs
  save(catchmentBasin, 
       file = file.path(scriptDatOut, paste0('catchmentAggregated.rdata')))
  
}




