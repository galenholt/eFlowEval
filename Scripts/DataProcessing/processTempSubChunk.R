# Script for chunk-processing the catchments, following inundation sub-chunks
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

# source('Functions/rastPolyJoin.R') # Shouldn't be needed anymore

# Set up parallel backend
registerDoFuture()
plan(multicore) # multicore on HPC

# # For local testing
# plan(multisession)
# summaryFun <- 'weightedMean'
# args <- c('blah', 'b', 'c', 'g', '3', 't', 'a', '3', 'Warrego', '8', '6', '8')
# args <- c('blah', 'b', 'c', 'g', '5', 't', 'a', '9', 'Warrego', '8', '6', '10')
# Does it break with one level of chunking?
# args <- c('blah', 'b', 'c', 'g', '5', 't', 'a', '9', 'Warrego')

# ## The outerchunks need to start outer and go in, ie '8', '6' is the 6th subchunk of the 8th main chunk
# Need to handle the edge case wehre there aren't enough polys to do the array we're asking for


# Make a sub-directory for the subchunk
scriptOut <- paste0(datOut, '/Tempprocessed/', summaryFun, '/chunked/', 
                    str_flatten(args[9:length(args)], collapse = '/sub_'))

# Set the projected CRS we want to use (australian Albers, typically)
commonCRS <- 3577

# Choose a size for the chunks. This is likely better elsewhere, but
nchunks <- 100
arraynum <- as.numeric(args[8])
chunkName <- args[8]

# this may be NA if there are no outer chunks
if (length(args) > 9) {
  outerchunks <- as.integer(args[10:length(args)])
} else {
  onelayer <- TRUE
}


# and the maximum-ish (there's a ceiling() involved, so this is approximate)
# number of pixels to read in for any one ANAE
maxPix <- 100000 # Seems likely to be a good balance based on local testing, but we'll see


# Set up the function -----------------------------------------------------

if (summaryFun == 'weightedMean') {
  # Weighted mean, ignoring NA by default
  chosenSummary <- function(x, area) {
    weighted.mean(x, area, na.rm = TRUE)
  }
} else {
  stop('need to choose a summary function')
}

# Read in all the data ----------------------------------------------------

## Temp data
# data location
# Set up where the soil temp data is
tempfile <- list.files(file.path(datDir, 'soilTemp14_20'), pattern = '.nc')
temppath <- file.path(datDir, 'soilTemp14_20', tempfile)

# First, use a stars object to get the overall dimensions without reading data
soilTstars <-  read_stars(temppath, sub = "LST_Day_1km", proxy = TRUE) # Force a proxy for testing the subsetting

# As long as they're proxies, we can't change their crs, so have to shift
# everything else to them
# The new version now comes in as 4326
starCRS <- 4326 # st_crs(soilTstars) 

# This is dangerous, but I am telling NASA to give me this CRS. For some reason
# it comes in as something else sometimes
if(st_crs(soilTstars)$epsg != starCRS) {
  st_crs(soilTstars) <- starCRS 
  warning('changed the soil CRS, but it should be correct')
}

# Get pixel size
rasterio = list(nXOff = 1, nYOff = 1, nXSize = 10, nYSize = 10)
pixarea <- temppath %>% # filenames
  read_stars(sub = "LST_Day_1km", RasterIO = rasterio) %>% # cut
  st_as_sf(as_points = FALSE, merge = FALSE, na.rm = FALSE) %>% # Read in as sf polygons
  # transform not necessary - tested in warrego8_6_3.R
  mutate(area = as.numeric(st_area(.))) %>%
  st_drop_geometry() %>% 
  summarize(area = mean(area, na.rm = TRUE)) %>% 
  pull()




# Read in all ANAE ----------------------------------
# The whole-basin version is ANAEbasinclim.rdata
# the particular file we want here is passed as args[9]
thisAName <- paste0(args[9], 'ANAE') 
allANAES <- list.files(file.path(datOut, 'ANAEprocessed'), pattern = paste0('^', thisAName))
## Read in all ANAE polygons
load(file.path(datOut, 'ANAEprocessed', allANAES))
# datOut is location-aware, based on directorySet.R, so this should work
# locally or on HPC

# change its name to something generic so can be looped over
# This is annoying, but I guess not too bad
anaePolys <- get(thisAName)
rm(list = thisAName)

# Somehow some are invalid, despite the st_make_valid in processANAE
anaePolys <- st_make_valid(anaePolys)

# change its name to something generic so can be looped over
# This is annoying, but I guess not too bad


# Sort out the chunks -----------------------------------------------------

# FIRST, get the correct outer chunk that we want to drill into
# I *think* this wouldn't be TOO terrible to make recursive if needed, by
# looping over args[10]: length(args) and grabbing the desired bit each time
# as long as we weren't also chaning nchunks. Although that wouldn't be too
# bad either, really, just would need more args
# Get the row indices from the array number 

# For loop lets us drill down by feeding additional arguments to the shell script
if (!onelayer) { # handle the case where we're not subchunking
  for (chun in 1:length(outerchunks)) {
    
    # Handle the case where we've drilled down to where there are fewer rows than chunks
    # If nchunks > nrow(), it will break it up fine. Catch the case where that's not true
    if (nchunks >= nrow(anaePolys)) {
      # If we're trying to grab a chunk that is in the available rows, just pass out the row
      # Otherwise, we don't want to pass out anything.
      if (outerchunks[chun] <= nrow(anaePolys)) {
        anaePolys <- anaePolys[outerchunks[chun], ]
      } else {
        stop('indexing beyond end of anaePolys')
      }
    } else { 
      # The usual case, define the edges of the chunk of anaePolys
      outersize <- ceiling(nrow(anaePolys)/nchunks)
      # arraynum <- 3
      prevoutertop <- outersize*(outerchunks[chun]-1)
      outerbottom <- prevoutertop+1
      
      if (outerchunks[chun] == nchunks) {
        outertop <- nrow(anaePolys) # make sure we finish
      } else {
        outertop <- prevoutertop + outersize
      }
      
      # cut to this chunk of polygons
      anaePolys <- anaePolys[outerbottom:outertop, ]
    }
  }
}


# THEN, break up into the main chunks that need to be run here
# This is exactly the same, but uses arraynum to define the chunk instead of a
# predefined argument. Could easily put in the loop but not going to for
# clarity, and because the ordering is a bit backwards

# As above, pass a single anaePoly if we're indexing in too far, skip entirely
# if we're past the end, otherwise define the chunk
if (nchunks >= nrow(anaePolys)) {
  # If we're trying to grab a chunk that is in the available rows, just pass out the row
  # Otherwise, we don't want to pass out anything.
  if (arraynum <= nrow(anaePolys)) {
    anaePolys <- anaePolys[arraynum, ]
  } else {
    stop('indexing beyond end of anaePolys')
  }
} else {
  chunksize <- ceiling(nrow(anaePolys)/nchunks)
  # arraynum <- 3
  prevtop <- chunksize*(arraynum-1)
  bottom <- prevtop+1
  
  if (arraynum == nchunks) {
    top <- nrow(anaePolys) # make sure we finish
  } else {
    top <- prevtop + chunksize
  }
  
  # cut to this chunk of polygons
  anaePolys <- anaePolys[bottom:top, ]
}



print(paste0('number of polygons processing is ', nrow(anaePolys)))

# Transform to stars crs
anaePolys <- st_transform(anaePolys, starCRS)


# Do the aggregation ------------------------------------------------------


# dpList <- foreach(s = 8:12) %dopar% {
dpList <- foreach(s = 1:nrow(anaePolys)) %dopar% {
  thiscrop <- st_crop(soilTstars, anaePolys[s,], as_points = FALSE)
  thistemp <- rastPolyJoin(polysf = anaePolys[s,], rastst = thiscrop, FUN = chosenSummary,
                            grouper = 'UID', maintainPolys = TRUE,
                            na.replace = NA, whichcrs = commonCRS, 
                            maxPixels = maxPix,
                            pixelsize = pixarea)
} # end foreach

# Then, unpack the lists also using foreach
tempAns <- foreach(l = 1:length(dpList),
                    .combine=function(...) c(..., along = 1), # Pass dimension argument to c.stars
                    .multicombine=TRUE) %dopar% {
                      dpList[[l]][[1]]
                    }

tempIndex <- foreach(l = 1:length(dpList),
                      .combine=bind_rows,
                      .multicombine=TRUE) %dopar% {
                        dpList[[l]][[2]]
                      }

# If we did this over all of ANAEbasinclim it'd be cleaner, but all the catchments would be mixed up
# I could do something like matching UIDS to ValleyName, and then splitting the output up into separate folders
# For now, I'm actually going to change some things to just run this within-catchment though
# ultimately, this should use a future.batchapply to spawn chunks within catchments

# Make the out directory, in case it doesn't exist
if (!dir.exists(scriptOut)) {dir.create(scriptOut, recursive = TRUE)}

thisInunName <- str_remove_all(thisAName, 'ANAE')
if (!onelayer) {
  thistemp <- paste0(thisInunName, '_', summaryFun, '_', 
                     str_flatten(args[10:length(args)], collapse = '_'),
                     '_', chunkName)
  thisIndex <- paste0(thisInunName, '_', summaryFun, '_index', '_',
                      str_flatten(args[10:length(args)], collapse = '_'),
                      '_', chunkName)
} else {
  thistemp <- paste0(thisInunName, '_', summaryFun, '_', chunkName)
  thisIndex <- paste0(thisInunName, '_', summaryFun, '_index', '_', chunkName)
}

assign(thistemp, tempAns)
assign(thisIndex, tempIndex)

# Could just use thisInunName for the rdata, since there's a folder structure,
# but this is more explicit
# This does not actually save a list; to save with character vectors need to
# use the list of characters
save(list = c(thistemp, thisIndex), file = file.path(scriptOut, paste0(thistemp, '.rdata')))

