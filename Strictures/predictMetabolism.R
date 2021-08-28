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



# Setup -------------------------------------------------------------------

# Make a sub-directory for the subchunk
scriptOut <- file.path(datOut, 'TempAndProduction', 'Predictions')
if (!dir.exists(scriptOut)) {dir.create(scriptOut, recursive = TRUE)}

# chunksize in n anae polygons
chunksize <- 1000

# I htink rather than a slurm array, pass a catchment name?
  # could do either, really. Sort out arg orders.
# arraynum <- as.numeric(args[8])
# Going to be easiest I think to base on names rather than arraynums and name indices.
thisCatch <- args[7] # 'Murrumbidgee' # For testing- needs to be grabbed from catchNames in a loop

print(thisCatch)

stop('testing end here to make sure passing catchment name')
# Read in data ------------------------------------------------------------

## TEMPS
# Need to wrap this over catchments
tempsIn <- file.path(datOut, 'Tempprocessed', 'weightedMean')
catchNames <- list.files(file.path(tempsIn, 'chunked')) # Get the catchment names from the folders
catchFiles <- list.files(tempsIn, pattern = '.rdata')
# And test with and without a catchment in the set of catchments from metabolism
thisWMname <- paste0(thisCatch, '_weightedMean')
load(file = file.path(tempsIn, paste0(thisWMname, '.rdata')))

## Regressions
regIn <- file.path(datOut, 'TempAndProduction', 'tempMetabolismRegressions.rdata')
load(regIn)

# change its name to something generic
# This is annoying, but I guess not too bad
weightedMeanTemps <- get(thisWMname)
rm(list = thisWMname)


# Whole basin predictions -------------------------------------------------

# Somehow going for the sf creates something 524Gb. that's bad. will need to chunk. which means the question is how much to chunk
# subCatch <- weightedMeanTemps %>% slice('geometry', 1:10) %>% slice('time', 500:515)
# Turn the kelvins into tempC?
weightedMeanTemps[[1]] <- as.numeric(weightedMeanTemps[[1]])-272.15
names(weightedMeanTemps) <- 'tempC'

# Let's ignore time and just loop over the anaes. doing it that way simplifies
# things and has worked elsewhere (the subchunk files and rastPolyjoin)
chunkpred <- function(bottom, top) {
  subCatch <- weightedMeanTemps %>% slice('geometry', bottom:top) # %>% slice('time', 500:515)
  
  
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
    add_preds(gppDaysValleys, predname = 'logGPPdaysvalleys')  %>% 
    add_preds(gppDays, predname = 'logGPPdays') %>%
    add_preds(erDaysValleys, predname = 'logERdaysvalleys') %>%
    add_preds(erDays, predname = 'logERdays')
  
  # throw out all the extra variables, leaving just the predictions
  catchSF <- catchSF %>%
    select(time, starts_with('log'))
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
ngeoms <- dim(weightedMeanTemps)['geometry']
nbreaks <- ceiling(ngeoms/chunksize) + 1
breaks <- round(seq(from = 0, to = ngeoms, length.out = nbreaks))
starpreds <- foreach(l = 1:(length(breaks)-1),
                     .combine=function(...) c(..., along = 1), # Pass dimension argument to c.stars
                     .multicombine=TRUE) %do% {
                       bottom <- breaks[l]+1
                       top <- breaks[l+1]
                       chunkpred(bottom, top) # NEED TO MODIFY CHUNKPRED
                     }

endbig <- proc.time()

print('Time taken for loop')
print(endbig-startbig)
# 3578 seconds total for bidgee (27000 anaes). Ends up just under 1GB. So that's not terrible
save(starpreds, file = file.path(scriptOut, paste0(thisCatch, '_predictedGPPER.rdata')))