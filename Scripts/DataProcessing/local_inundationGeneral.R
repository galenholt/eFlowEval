# a local test of inundationGeneral for Lippia
source('directorySet.R') # deals with lots of things, including providing datOut

# Yeesh, pull these out
library(here)
library(tidyverse)
library(sf)
library(stars)
library(foreach)
library(doFuture)
# 
registerDoFuture()
plan(multisession)

source(file.path('Scripts/DataProcessing/processInundationGeneral.R'))

# Try it with just one catchment

# args order: 
# SLURM_ARRAY_TASK_ID (ie the chunk) is 8
# Catchment name is 9
# summaryFun is 7 
# the script to run is 6 (not necessary in this wrapper, at least not now)
# Subchunks are 10+

# The two foreaches are just doing what the sh per-catchment and SLURM chunking
# are doing. So those *should* be easily modifiable to call this same function

alltimes <- foreach(i = as.character(1:100), .combine = rbind) %dopar% {
  args <- c('blah', 'b', 'c', 'g', '5', 
            'Scripts/DataProcessing/processInundationGeneral.R', 
            'lippiaAdultSurvive', i, 'Avoca')
  timeeach <- system.time(processInundationGeneral(datOut, args))
  # both returns the time taken AND prevents it from trying to build a list from who knows what.
  timeeach <- c(timeeach, catchment = 'Avoca', chunk = i)
  timeach <- as.data.frame(t(timeeach))
}