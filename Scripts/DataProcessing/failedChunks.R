# script to ID the missing files
# I think this would actually work better as a function, which then iterated
# into future.batchtools with updated resources or chunking. But that's for
# another day

# CURRENTLY, this sets up the missings for longer runs. It's entirely possible
# to instead set them up for sub-chunking, but that would take more work- would
# need to pass the chunk ids to allTempSLURM. It's possible here- would just
# need some more arguments in l3, just going to get this running first

# Can I just srun this instead of doing all the batch nonsense?
# Or do I need to slurm it at all? Can I just Rscript it?

# Rscript failedChunks.R should work, shouldn't it?'
# Not quite, but wrapping does
# Rscript hpc_wrap.R 'Scripts/DataProcessing/failedChunks.R'

# Header from the temperature file to retain all the directories,  --------


source('directorySet.R')

# Let's get libraries here, then sort out git then sort out making this a library so we don't have to deal with all the library crap
# library(sp)
# library(rgeos)
library(here)
library(tidyverse)
# library(sf)
# library(RNetCDF) # Probably not, raster can handle geographic netCDF
# library(raster) # maybe?
# library(stars)
library(foreach)
library(doFuture)

# source('Functions/rastPolyJoin.R')

# Set up parallel backend
registerDoFuture()
plan(sequential) # no need to parallelize, I don't think

# # For local testing
# plan(multisession)
summaryFun <- 'weightedMean'
# args <- c('blah', 'b', 'c', 'g', '3', 't', 'a', '3', 'Warrego', '8', '6', '8')
# args <- c('blah', 'b', 'c', 'g', '5', 't', 'a', '9', 'Warrego', '8', '6', '10')
# Does it break with one level of chunking?
args <- c('blah', 'b', 'c', 'g', '5', 't', 'a', '9', 'Warrego')

# ## The outerchunks need to start outer and go in, ie '8', '6' is the 6th subchunk of the 8th main chunk
# Need to handle the edge case wehre there aren't enough polys to do the array we're asking for


# Make a sub-directory for the subchunk
# Come back to this for iteration- we actually want one less level to check the missings
# scriptOut <- paste0(datOut, '/Tempprocessed/', summaryFun, '/chunked/', 
#                     str_flatten(args[9:length(args)], collapse = '/sub_'))
scriptOut <- paste0(datOut, '/Tempprocessed/', summaryFun, '/chunked')

# Set the projected CRS we want to use (australian Albers, typically)
# commonCRS <- 3577

# Choose a size for the chunks. This is likely better elsewhere, but
nchunks <- 100

summaryFuns <- c('weightedMean')
su <- 1 # Testing- so can loop over summary functions
# for (su in 1:length(summaryFuns)) {
  summaryFun <- summaryFuns[su]
  
  
  # get the names of the catchments
    # Could pull this out of the filenames, but this is easier
  catchNames <- list.files(scriptOut)
  
  # cn <- 1 # testing- put in foreach
  # thiscatch <- catchNames[cn]
  # catchfiles <- list.files(file.path(scriptOut, thiscatch), 
  #                          pattern = '.rdata', recursive = TRUE)
  # 
  # # Which chunks worked?
  # workedchunks <- as.numeric(str_extract(catchfiles, pattern = '[0-9]+'))
  # workedchunks
  # 
  # allchunks <- 1:nchunks
  # 
  # allchunks[!(allchunks %in% workedchunks)]
  # 
  # I think a list is actually better than a table here
  misslist <- foreach(cn = 1:length(catchNames), .inorder = TRUE) %do% {
    thiscatch <- catchNames[cn]
    catchfiles <- list.files(file.path(scriptOut, thiscatch), 
                             pattern = '.rdata', recursive = TRUE)
    
    # Which chunks worked?
    workedchunks <- as.numeric(str_extract(catchfiles, pattern = '[0-9]+'))
    workedchunks
    
    allchunks <- 1:nchunks
    
    missingchunks <- allchunks[!(allchunks %in% workedchunks)]
    missingchunks
  }
  names(misslist) <- catchNames
  
  # Get rid of catchments that finished (ie have empty list sections)
  misslist <- purrr::compact(misslist)
  
  finishers <- catchNames[!(catchNames %in% names(misslist))]
  cat(paste('These catchments finished:', 
            stringr::str_flatten(finishers, collapse = '\n'), sep = '\n'))
  print(" ")
  cat(paste('These catchments will run again:', 
            stringr::str_flatten(names(misslist), collapse = '\n'), sep = '\n'))
  print(" ")
  
  
  # NOW, do I just run this, or is there a way to create the shell script?
  missnames <- names(misslist)
  headline <- "#!/bin/bash"
  head2 <- "\n"
  
  # For each catchment, I need to set up the chunk in a list
  lineslist <- foreach(cn = 1:length(missnames), .inorder = TRUE, .combine = c) %do% {
    l1 <- stringr::str_c("thiscatch='", missnames[cn],"'")
    l2 <- stringr::str_c("echo 'start' $thiscatch")
    misschars <- str_flatten(misslist[[cn]], collapse = ",")
    l3 <- stringr::str_c("sbatch -J $thiscatch --array=", misschars, " allTempSLURMLong.sh $thiscatch")
    l4 <- "sleep 2"
    l5 <- "\n"
    thislist <- list(l1, l2, l3, l4, l5)
  }
  
  filechars <- c(headline, head2, unlist(lineslist))
  
  # Hpc says file isn't an option
  # readr::write_lines(filechars, file = 'missingTemps.sh')
  
  writeLines(filechars, con = 'missingTemps.sh')
  
  # Do I want to have R actually run this too?
  # IE I could do 
  # system2(command = 'bash', args = 'missingTemps.sh')
  # That should work, but I'm going to hold off for now so I can check on things
  