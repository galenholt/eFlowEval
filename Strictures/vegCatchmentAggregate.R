# # Plot inundation
# # Libraries and system setup
# source('directorySet.R')
# 
# library(here)
# library(tidyverse)
# library(sf)
# library(stars)
# library(foreach)
# library(doFuture)
# library(doRNG)
# 
# # Set up parallel backend
# registerDoFuture()
# 
# if (parSet == 'local') {
#   plan(multisession)
# } else if (parSet == 'hpc') {
#   plan(multicore(workers = availableCores(methods = 'Slurm')))
# } else {
#   plan(sequential)
# }
# 
# # directory
# summaryFuns <- c('lippia', 'centipeda')


# plan(sequential) # 

veg_catch_agg <- function(datOut, summaryFuns, whichcrs = 3577) {
  for(sfun in 1:length(summaryFuns)) {
    summaryFun <- summaryFuns[sfun]
    # There are some that were NOT chunked- leave them alone, and just look in the chunked folder
    dataIn <- file.path(datOut, 'Strictures', summaryFun)
    anaeIn <- file.path(datOut, 'ANAEprocessed')
    
    scriptDatOut <- file.path(dataIn, 'basinConcat')
    # Make the out directory, in case it doesn't exist
    # if (!dir.exists(scriptFigOut)) {dir.create(scriptFigOut, recursive = TRUE)}
    if (!dir.exists(scriptDatOut)) {dir.create(scriptDatOut, recursive = TRUE)}
    
    # List the catchments
    catchfiles <- list.files(dataIn, pattern = '*.rdata')
    catchNames <- str_remove(catchfiles, pattern = paste0('_', summaryFun, '_strictures.rdata'))
    
    # Loop over each catchment, since that's how the files are structured for memory purposes
    # for (i in 1:length(catchNames)) {
    # for (i in 1:2) {
    loopstart <- proc.time()
    # don't .combine these strictures, because they are lists of stars, not single stars
    dataBasin <- foreach(i = 1:length(catchNames), # length(catchNames)
                         # .combine=function(...) c(..., along = 1), # Pass dimension argument to c.stars
                         # .multicombine=TRUE,
                         .inorder = TRUE, .packages = c('sf', 'stars', 'dplyr')) %dorng% { # dopar now seems to work with .packages
                           
                           # oneloopstart <- proc.time()
                           # Set up loop iterations
                           thisCatch <- catchNames[i] #13 is lachlan, to keep consistent with previous checking
                           thisdatafile <- catchfiles[i]
                           
                           
                           # Read in the data
                           anfile <- file.path(anaeIn, paste0(thisCatch, 'ANAE.rdata'))
                           datafile <- file.path(dataIn, thisdatafile)
                           
                           load(anfile)
                           load(datafile)
                           
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
                           thesedatas <- get(paste0(summaryFun, 'stricts'))
                           # indices are (should) in the stricture lists
                           # theseIndices <- get(paste0(thisCatch, '_', summaryFun, '_index'))
                           rm(list = c(paste0(thisCatch, 'ANAE'), 
                                       paste0(summaryFun, 'stricts'))) 
                           
                           # make a smaller version of for testing
                           # Smaller in the time dimension, NOT space, since we need all the anaes
                           # TODO: make this settable with a timespan- I've done it somewhere else
                           # thesedatas <- thesedatas[,,1:10] # %>% slice("time", 1:10) # Slice doesn't work with dopar
                           
                           # Set crs
                           # Why (how???) are some of the datas not in the right crs?
                           theseANAEs <- st_transform(theseANAEs, whichcrs)
                           thisPoly <- st_transform(thisPoly, whichcrs)
                           # thesedatas is a list of stars. deal with that
                           # and only transform if necessary?
                           thesedatas <- thesedatas %>% 
                             purrr::map(\(x) st_transform(x, whichcrs))
                           
                           # Aggregation setup
                           
                           # We do NOT want to area-weight here,
                           # because we're just adding up total area, and
                           # area already takes care of all relevant
                           # area effects
                           # It only makes sense to aggregate the stars, NOT the sf of ANAEs
                           # don't assume it's always last
                           flatanaes <- which(purrr::map_lgl(thesedatas, \(x) inherits(x, 'sf')))
                           dataAgg <- thesedatas[-flatanaes] %>% 
                             purrr::map(\(x) aggregate(x, 
                                                       by = thisPoly, 
                                                       FUN = sum, na.rm = TRUE))
                           
                           # oneloopend <- proc.time()
                           # onelooptime <- oneloopend - oneloopstart
                           # print(paste0('finished loop ', i, '(', thisCatch, ')', 'time = ', onelooptime))
                           
                           dataAgg
                         }
    
    # Because these strictures are lists of stars, concatenate them
    dataBasin <- purrr::pmap(dataBasin, ~c(..., along = 1))
    loopend <- proc.time()
    looptime <- loopend-loopstart
    looptime
    
    
    # Let's save that so we don't have to re-do the loop calcs
    thisobjname <- paste0(summaryFun, '_stricts_catchment')
    assign(thisobjname, dataBasin)
    
    # Save the list
    save(list = thisobjname, 
         file = file.path(scriptDatOut, paste0('catchmentAggregated.rdata')))
    
  }
  
}





