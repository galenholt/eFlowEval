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


# NOTE --------------------------------------------------------------------

# This has been moved to `notebooks/lippia_inundation_data.qmd`. 

# USE THAT FOR PROCESSING


# -------------------------------------------------------------------------



# Try it with just one catchment

# args order: 
# SLURM_ARRAY_TASK_ID (ie the chunk) is 8
# Catchment name is 9
# summaryFun is 7 
# the script to run is 6 (not necessary in this wrapper, at least not now)
# Subchunks are 10+

# If I run this locally, does it make more sense to just loop over all polygons
# in a catchment, and let that inner foreach do all the work? The chunking here
# was to split array jobs on SLURM, but not clear that doesn't just booger
# things up locally and not let future/foreach pick the best options. And not
# clear whether to chunk by catchment even, though that's more enforced in the
# way the data and processData are set up

# One advantage of chunking is that it means a failure doesn't kill the whole
# thing

# This could really be cleaned up even quite a bit more- it really just only
# takes a couple args and could be a function. And possibly wrapped in a while
# (!is.null(runlist)) (though that's dangerous)- see the post-processing at the
# end.

# I've modified makeSHfails to give me a list
runlist <- makeSHfails(outerDir = file.path(datOut, 'Inundationprocessed'),
            summaryFuns = 'lippiaAdultSurvive',
            varName = 'LippiaSurvive',
            nchunks = 100,
            lengthOrChunk = c('short', 'long'), # , 'long', 'chunk'
            runImmediate = FALSE,
            forceAllCatchments = TRUE,
            returnForR = TRUE)

# still kind of a funny workaround for 'args' to work for both hpc and here.
# Unpacking them in the hpc version and calling them what they are makes more
# sense. What I really should do is figure out how to generate HPC runs *from*
# foreach/future, so it's all one thing

# Run over the catchments and chunks 
alltimes <- foreach(w = names(runlist), .combine = rbind, .errorhandling = 'remove') %:%
  foreach(i = as.character(runlist[[w]]), .combine = rbind, .errorhandling = 'remove') %dopar% {
  args <- c('blah', 'b', 'c', 'g', '5', 
            'Scripts/DataProcessing/processInundationGeneral.R', 
            'lippiaAdultSurvive', i, w)
  timeeach <- system.time(processInundationGeneral(datOut, args))
  # both returns the time taken AND prevents it from trying to build a list from who knows what.
  timeeach <- c(timeeach, catchment = w, chunk = i)
  timeach <- as.data.frame(t(timeeach))
  }

# TODO:: I should be able to run `makeSHfails` here, then if there are no fails,
# immediately run `concatANAEchunks`.

# Like so
runlist_end <- makeSHfails(outerDir = file.path(datOut, 'Inundationprocessed'),
                       summaryFuns = 'lippiaAdultSurvive',
                       varName = 'LippiaSurvive',
                       nchunks = 100,
                       lengthOrChunk = c('short', 'long'), # , 'long', 'chunk'
                       runImmediate = FALSE,
                       forceAllCatchments = TRUE,
                       returnForR = TRUE)

if (is.null(runlist_end)) {
  system.time(concatANAEchunks(outerDir = file.path(datOut, 'Inundationprocessed'),
                               summaryFuns = c('lippiaAdultSurvive')))
}