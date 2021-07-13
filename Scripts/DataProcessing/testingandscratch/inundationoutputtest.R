# scritp to check the inundation runs worked


# script to process inundation into ANAE polygons

# one of several statistics

# Very similar to inundationCleanSpeed, but focused just on speed testing the plans, not on also checking assorted things work
# The hpc_wrapper tends to source this too, but if I do it here I don't have to
# wrap the script to run locally. Sort out a cleaner way to do this
source('directorySet.R')

# Let's get libraries here, then sort out git then sort out making this a library so we don't have to deal with all the library crap
# library(sp)
# library(rgeos)
library(here)
library(tidyverse)
library(sf)
# library(RNetCDF) # Probably not, raster can handle geographic netCDF
# library(raster) # maybe?
library(stars)
library(foreach)
library(doFuture)

source('Functions/rastPolyJoin.R')




load(file.path(datOut, 'Inundationprocessed/areaInun', 'Avoca_areaInun.rdata'))

load(file.path(datOut, 'Inundationprocessed/areaInun', 'Lachlan_areaInun.rdata'))

load(file.path(datOut, 'Inundationprocessed/volInun', 'Avoca_volInun.rdata'))

load(file.path(datOut, 'Inundationprocessed/volInun', 'Lachlan_volInun.rdata'))

plot(Avoca_areaInun)
plot(Avoca_volInun)
plot(Lachlan_areaInun)
plot(Lachlan_volInun)

# does re-transforming work? This is entirely an aside
plot(st_transform(Lachlan_volInun, 4326))


# Checking which catchments didn't finish ---------------------------------

# Very similar to inundationCleanSpeed, but focused just on speed testing the plans, not on also checking assorted things work
# The hpc_wrapper tends to source this too, but if I do it here I don't have to
# wrap the script to run locally. Sort out a cleaner way to do this
source('directorySet.R')

scriptOut <- paste0(datOut, '/Inundationprocessed')

# Let's get libraries here, then sort out git then sort out making this a library so we don't have to deal with all the library crap
# library(sp)
# library(rgeos)
library(here)
library(tidyverse)
library(sf)
# library(RNetCDF) # Probably not, raster can handle geographic netCDF
# library(raster) # maybe?
library(stars)
library(foreach)
library(doFuture)
registerDoFuture()
source('Functions/rastPolyJoin.R')


# Read in all the data ----------------------------------------------------

## Inundation
# data location
inunDir <- file.path(datDir, 'Inundation', 'WaterDepth_TwoMonthly', 'geotiff')

# Get the file names
alltifs <- list.files(inunDir, pattern = '.tif$')
inunTifsALL <- file.path(inunDir, alltifs)

# get the crs from the first one (will read them all in later, but first need
# to deal with the corrupt file)
# As long as they're proxies, we can't change their crs, so have to shift
# everything else to them
starCRS <- st_crs(read_stars(inunTifsALL[1]))

## Read in all the anae polygons to see how many there are in each
load(file.path(datOut, 'ANAEprocessed', 'ANAEbasinclim.rdata'))

# how many in each?
  # drop geometry, we just want to add them up
  # area calc takes forever, split it off
numans1 <- ANAEbasinclim %>%
  st_transform(crs = 3577) %>%
  mutate(area = as.numeric(st_area(.))) 

numans <- numans1 %>%
  st_drop_geometry() %>%
  group_by(ValleyName) %>%
  summarise(numanaes = n(),
            totalArea = sum(area)) %>%
  mutate(predictedHours = (167/200)*numanaes/60/60) %>%
  mutate(valley = str_remove_all(ValleyName, pattern = ' '))
numans
View(numans)

# What are their IDs?
# The whole-basin version is ANAEbasinclim.rdata, so exclude it- will be more parallel to loop over catchments
allANAES <- list.files(file.path(datOut, 'ANAEprocessed'), pattern = 'ANAE.rdata$')
allANAES

# Which ones died after 12 hours?
# or, better yet, put in the times to do a better job estimating
voltimes <- c(15896, NA, 16128, 1132, 801, 4438, 16650, NA, 7930, 5165,
              11474, 658, NA, 2446, NA, 22597, NA, 1522, NA, 9306, 2795, 
              NA, 1605, 8572, 5520)
areatimes <- c(15943, NA, 16125, 1263, 873, 4708, 18887, NA, 8871, 5184, 
               11474, 645, NA, 2455, NA, NA, NA, 1872, NA, 9218, 
               2771, NA, 1590, 8843, 5865)

anaeTimings <- bind_cols(valley = str_remove(allANAES, pattern = 'ANAE.rdata'), volTimes = voltimes, areaTimes = areatimes)
timeAnalysis <- left_join(anaeTimings, numans, by = 'valley') %>%
  mutate(secondsPerVol = volTimes/numanaes,
         secondsPerArea = areaTimes/numanaes,
         newPredict = ((mean(secondsPerVol, na.rm = TRUE) + 
                          mean(secondsPerArea, na.rm = TRUE))/2) * numanaes,
         newPredictHours = newPredict/60/60,
         secondsPerANAEarea = mean(volTimes/totalArea, na.rm = TRUE),
         newPredictArea = secondsPerANAEarea*totalArea)
timeAnalysis
View(timeAnalysis)

# what is 12 hours in seconds?
12*60*60
# none of those are even close, are they?

ggplot(timeAnalysis, aes(x = numanaes)) + 
  geom_line(aes(y = secondsPerVol), color = 'dodgerblue') +
  geom_point(aes(y = secondsPerVol), color = 'dodgerblue') +
  geom_line(aes(y = secondsPerArea), color = 'firebrick') +
  geom_point(aes(y = secondsPerArea), color = 'firebrick')

# Except for that one weird one, they're all around 1 second
  # it looks like it didn't do the multicore?

# How about actual time
ggplot(timeAnalysis, aes(x = numanaes)) + 
  geom_line(aes(y = volTimes), color = 'dodgerblue') +
  geom_point(aes(y = volTimes), color = 'dodgerblue') +
  geom_line(aes(y = areaTimes), color = 'firebrick') +
  geom_point(aes(y = areaTimes), color = 'firebrick') +
  geom_line(aes(y = newPredict), color = 'firebrick') +
  geom_point(aes(y = newPredict), color = 'firebrick') +
  geom_hline(yintercept = 12*60*60)

# and vs area
ggplot(timeAnalysis, aes(x = totalArea)) + 
  geom_line(aes(y = volTimes), color = 'dodgerblue') +
  geom_point(aes(y = volTimes), color = 'dodgerblue') +
  geom_line(aes(y = newPredictArea), color = 'firebrick') +
  geom_point(aes(y = newPredictArea), color = 'firebrick') +
  geom_hline(yintercept = 12*60*60)


# OK, so, I should re-write the way I do this to chunk over evenly-sized chunks,
# and then post-combine. BUT, since a lot of them are already done, let's just
# get some started, and test the chunking in the background

# Which ones need to be run for which analysis?
which(is.na(voltimes))
which(is.na(areatimes))

# How long to put walltime? 36 hours? 40? 