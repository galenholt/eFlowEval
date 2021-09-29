# concatenate inundation
# Script to concatenate temp files

# Header from the temperature file to retain all the directories,  --------
source('directorySet.R')

# Let's get libraries here, then sort out git then sort out making this a
# library so we don't have to deal with all the library crap
library(here)
library(tidyverse)
library(sf)
# library(RNetCDF) # Probably not, raster can handle geographic netCDF
library(stars)
library(foreach)
library(doFuture)

# source('Functions/rastPolyJoin.R')

# Set up parallel backend
registerDoFuture()
plan(sequential) # no need to parallelize, I don't think


# Concatenate -------------------------------------------------------------

# system.time(concatANAEchunks(outerDir = file.path(datOut, 'Inundationprocessed'),
#                  summaryFuns = c('areaInun', 'volInun')))

system.time(concatANAEchunks(outerDir = file.path(datOut, 'Inundationprocessed'),
                                              summaryFuns = c('areaSpoonbillBreed',
                                                              'areaSpoonbillForage')))
# and a quick look
# load(file.path(datOut, 'Tempprocessed', 'WeightedMean', 'Avoca_weightedMean.rdata'))
# plot(Avoca_weightedMean[,1:20, 1:9])