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
