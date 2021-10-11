# Royal Spoonbill Strictures.

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
scriptOut <- file.path(datOut, 'Spoonbill', 'Breed')
if (!dir.exists(scriptOut)) {dir.create(scriptOut, recursive = TRUE)}

# chunksize in n anae polygons
chunksize <- 1000

# I htink rather than a slurm array, pass a catchment name?
# could do either, really. Sort out arg orders.
# arraynum <- as.numeric(args[8])
# Going to be easiest I think to base on names rather than arraynums and name indices.
thisCatch <-  'Avoca' #args[7] # 'Murrumbidgee' # For testing- needs to be grabbed from catchNames in a loop

print(thisCatch)

# stop('testing end here to make sure passing catchment name')

# Read in data ------------------------------------------------------------



# ANAE 'preferences' ranked from most to least times observed
# McGinness, Langston and Brooks (2020) VEWH Prioritisation Project: Stage 2 Final Report
# Royal Spoonbill (Platalea regia) requirements, distribution and habitat mapping
breedANAE <- read.table(
  file.path(myhome, "Deakin University/QAEL - MER/Model/dataStrict/BreedingANAEcolonialBirds.txt"), 
  skip = 2, sep = ":", header = TRUE)
forageEtcANAE <- read.table(
  file.path(myhome, "Deakin University/QAEL - MER/Model/dataStrict/ForagingEtcANAEcolonialBirds.txt"), 
  skip = 2, sep = ":", header = TRUE)


## Areas of each ANAE meeting depth stricture
# Need to wrap this over catchments
depthIn <- file.path(datOut, 'Inundationprocessed', 'areaSpoonbillBreed')
catchNames <- list.files(file.path(tempsIn, 'chunked')) # Get the catchment names from the folders
catchFiles <- list.files(tempsIn, pattern = '.rdata')
# And test with and without a catchment in the set of catchments from metabolism
thisWMname <- paste0(thisCatch, '_weightedMean')
load(file = file.path(tempsIn, paste0(thisWMname, '.rdata')))


# Inundation for Breeding stricture ---------------------------------------

# Areas of each ANAE meeting Breeding depth stricture. Bimonthly timestep 
# No point having one anae meeting depth strictures is it is not big enough
# to support a colony.
#TODO identify  polygon boundaries of colonial bird breeding habitats in MDB
# C:\Users\amacq\Deakin University\QAEL - MER\Model\dataBase\RAMSARwetlandBoundaries
# OR code test for minumum contigous area of inunated anaes...

# 






