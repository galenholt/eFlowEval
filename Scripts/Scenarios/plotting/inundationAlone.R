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
library(viridis)

# Set the crs
whichcrs <- 3577
# directory
summaryFun <- 'areaInun'
# There are some that were NOT chunked- leave them alone, and just look in the chunked folder
inunIn <- paste0(datOut, '/Inundationprocessed/', summaryFun)
anaeIn <- file.path(datOut, 'ANAEprocessed')

# List the catchments
catchfiles <- list.files(inunIn, pattern = '*.rdata')
catchNames <-str_remove(catchfiles, pattern = paste0('_', summaryFun, '.rdata'))

# Load some data
thisCatch <- catchNames[13] #13 is lachlan, to keep consistent with previous checking
thisInunfile <- catchfiles[13]
# Get the right valley- same as in processANAE
  # Do it this way, NOT with the same index as above, because these are in a
  # different order for some reason
valleys <-ltimNoNorth$ValleyName
valleys <- str_remove_all(valleys, ' ')
thisvalley <- which(valleys == thisCatch)
# Cut to just the valley we want
thisPoly <- ltimNoNorth %>% slice(thisvalley)

anfile <- file.path(anaeIn, paste0(thisCatch, 'ANAE.rdata'))
inunfile <- file.path(inunIn, thisInunfile)

load(anfile)
load(inunfile)

# make a smaller version of the inundation for testing
  # Smaller in the time dimension, NOT space, since we need all the anaes
lachtest_areaInun <- Lachlan_areaInun %>% slice("time", 1:10)

# Set up to plot ----------------------------------------------------------
  # Need to make this name-agnostic

# Set crs
LachlanANAE <- st_transform(LachlanANAE, whichcrs)
ltimNoNorth <- st_transform(ltimNoNorth, whichcrs)


# There's some spillover if we use ltimNoNorth, so we need to cut it to the correct valley
  # Not sure if I want to drop_geometry or not? Not much reason to keep it, really
areas <- Lachlan_areaInun_index %>% mutate(area = as.numeric(st_area(.))) %>%
  st_drop_geometry() %>%
  select(area) %>%
  pull()

# Let's just try my catchment aggregator
  # use sum for total area of ANAEs inundated
inunLach <- catchAggW(strict = lachtest_areaInun, strictWeights = areas,
                      FUN = sum, summaryPoly = thisPoly)
names(inunLach) <- 'totalareainundated'

# and plot
plotInunLach <- catchAggPlot(inunLach, title = 'Total Area Inundated')
plotInunLach

