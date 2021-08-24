# script to check failures and make new shell scripts for temperature

# Rscript is easier than dealing with slurm for this
# Rscript hpc_wrap.R 'Scripts/DataProcessing/tempFails.R'


# Header from the temperature file to retain all the directories,  --------
source('directorySet.R')

# Let's get libraries here, then sort out git then sort out making this a
# library so we don't have to deal with all the library crap
library(here)
library(tidyverse)
# library(sf)
# library(RNetCDF) # Probably not, raster can handle geographic netCDF
# library(stars)
library(foreach)
library(doFuture)

# Set up parallel backend
registerDoFuture()
plan(sequential) # no need to parallelize, I don't think


# Find failures and make new sh script -------------------------------------------------------------

makeSHfails(outerDir = file.path(datOut, 'Tempprocessed'),
                        summaryFuns = 'weightedMean',
                        varName = 'Temp',
                        nchunks = 100,
                        lengthOrChunk = c('length', 'chunk'),
                        runImmediate = FALSE)
