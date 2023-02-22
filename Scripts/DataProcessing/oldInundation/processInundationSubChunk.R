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


# Set up parallel backend
registerDoFuture()
plan(multicore) # multicore on HPC

# # For local testing
# plan(multisession)
# summaryFun <- 'areaInun'
# args <- c('blah', 'b', 'c', 'g', '3', 't', 'a', '3', 'Warrego', '8', '6', '8')
# args <- c('blah', 'b', 'c', 'g', '5', 't', 'a', '9', 'Warrego', '8', '6', '10')
# ## The outerchunks need to start outer and go in, ie '8', '6' is the 6th subchunk of the 8th main chunk
  # Need to handle the edge case wehre there aren't enough polys to do the array we're asking for

# Make a sub-directory for the subchunk
scriptOut <- paste0(datOut, '/Inundationprocessed/', summaryFun, '/chunked/', args[9], '/sub', 
                    str_flatten(args[10:length(args)], collapse = '/sub_'))

# Set the projected CRS we want to use (australian Albers, typically)
commonCRS <- 3577

# Choose a size for the chunks. This is likely better elsewhere, but
nchunks <- 10
arraynum <- as.numeric(args[8])
chunkName <- args[8]

outerchunks <- as.integer(args[10:length(args)])
# chunksize <- 6000

# and the maximum-ish (there's a ceiling() involved, so this is approximate)
# number of pixels to read in for any one ANAE
maxPix <- 100000 # Seems likely to be a good balance based on local testing, but we'll see


# Which function ----------------------------------------------------------

if (summaryFun == 'areaInun') {
  # Area of inundation
  chosenSummary <- function(x, area) {
    sum(ifelse(x > 0, area, 0))
  }
} else if (summaryFun == 'volInun') {
  # area*depth for each raster, so sum(depth, area)
  chosenSummary <- function(z, area) {
    sum(z*area)
  }
} else if (summaryFun == 'volLimit') {
  # Volume of water <= 10cm (or arbitrary photic limit)
  # depth up to 10cm * area, then summed
  # So, get the minimum of depth or photic limit, multiply by area, and
  # summarise with sum to get all depths
  # Have to sort of write a min() out of ifelse or it does min() over the column, not the rows
  chosenSummary <- function(x, area, limit = 0.1) {
    sum(ifelse(x > limit, limit*area, x * area))
  }
} else if (summaryFun == 'avgInunDepth') {
  # mean depth of the inundated area- this potentially much different than mean
  # depth across the polygon
  # give 0 weights to those that aren't inundated
  chosenSummary <- function(x, area) {
    areaifinun <- ifelse(x > 0, area, 0)
    weighted.mean(x, areaifinun)
  }
} else {
  stop('need to choose a summary function')
}
  
# Read in inundation data --------------------------------------------------

## Inundation
# data location
inunDir <- file.path(datDir, 'Inundation', 'WaterDepth_TwoMonthly', 'geotiff')

# Get the file names
alltifs <- list.files(inunDir, pattern = '.tif$')
inunTifs <- file.path(inunDir, alltifs)

# get the crs from the first one (will read them all in later, but first need
# to deal with the corrupt file)
# As long as they're proxies, we can't change their crs, so have to shift
# everything else to them
starCRS <- st_crs(read_stars(inunTifs[1]))

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

# FIRST, get the correct outer chunk that we want to drill into
  # I *think* this wouldn't be TOO terrible to make recursive if needed, by
  # looping over args[10]: length(args) and grabbing the desired bit each time
    # as long as we weren't also chaning nchunks. Although that wouldn't be too
    # bad either, really, just would need more args
# Get the row indices from the array number 

# For loop lets us drill down by feeding additional arguments to the shell script
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


# THEN, break up into the sub-chunk-
# This is exactly the same, but uses arraynum to define the chunk instead of a
# predefined argument. Could easily put in the loop but not going to for clarity

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

# handle corrupt tif ------------------------------------------------------
  # Not needed (was a corrupt download)
  # Keeping but commented out for a bit longer in case I'm wrong
# Need to do this after the ANAE because I have to read it in
# There's a corrupt tif, so cut it out
# Have to try to read in, but can't read in the whole thing. So do a little
# crop and call it good
# passer <- vector(mode = 'logical', length(inunTifsALL))
# for (tif in 1:length(inunTifsALL)) {
#   checkTif <- read_stars(inunTifsALL[tif])
#   cropTif <- st_crop(checkTif, anaePolys[1,], as_points = FALSE)
#   testsf <- NA
#   try(testsf <- st_as_sf(cropTif, as_points = FALSE, merge = FALSE, na.rm = FALSE),
#       silent = TRUE)
#   if (class(testsf) == 'logical') {
#     passer[tif] <- FALSE
#   } else if ('sf' %in% class(testsf)) {
#     passer[tif] <- TRUE
#   }
#   rm(testsf)
# }

# # and now the list of functional tifs is
# inunTifs <- inunTifsALL[passer]



# Get the stars in as a proxy ---------------------------------------------


# Get the dates from the tif names so we can set dimensions
tifdates <- inunTifs %>% # Set of filenames
  str_extract_all("[1-2][0-9][0-9][0-9]_[0-9][0-9]_WaterDepth.tif") %>%
  # now delete the safety
  str_remove("_WaterDepth.tif") %>%
  # add the first of the month on there
  str_c('_01') %>%
  # turn into dates
  lubridate::ymd() %>%
  as.POSIXct() # soilMstars uses posix instead of the Date class that lubridate returns, so be consistent


# Make the proxy
tifTimes <- inunTifs %>% # filenames
  read_stars() %>% # read in
  merge() %>% # make a brick 
  setNames('depth') %>% # name the attribute
  # Make dates
  # soilMstars uses an offset and delta, while this has 'values'.
  # I seem to remember values might cause an issue somewhere down the track, so
  # might need to revisit
  st_set_dimensions(3, values = tifdates) %>% 
  st_set_dimensions(names = c("x", "y", "time"))

# Get pixel area (for reference, but also to pass to rastPolyJoin as a check if the read-in is too large)
# Read in 100 pixels of the first sheet
rasterio = list(nXOff = 1, nYOff = 1, nXSize = 10, nYSize = 10)
pixarea <- inunTifs[1] %>% # filenames
  read_stars(RasterIO = rasterio) %>% # cut
  st_as_sf(as_points = FALSE, merge = FALSE, na.rm = FALSE) %>% # Read in as sf polygons
  # transform not necessary - tested in warrego8_6_3.R
  mutate(area = as.numeric(st_area(.))) %>%
  st_drop_geometry() %>% 
  summarize(area = mean(area, na.rm = TRUE)) %>% 
  pull()


# Since we want to combine the two list bits differently, just return the list
# and let foreach make a list of lists for now


# dpList <- foreach(s = 8:12) %dopar% {
dpList <- foreach(s = 1:nrow(anaePolys)) %dopar% {
  thiscrop <- st_crop(tifTimes, anaePolys[s,], as_points = FALSE)
  thisdepth <- rastPolyJoin(polysf = anaePolys[s,], rastst = thiscrop, FUN = chosenSummary,
                            grouper = 'UID', maintainPolys = TRUE,
                            na.replace = 0, whichcrs = commonCRS, 
                            maxPixels = maxPix,
                            pixelsize = pixarea)
} # end foreach

# Then, unpack the lists also using foreach
depthAns <- foreach(l = 1:length(dpList),
                    .combine=function(...) c(..., along = 1), # Pass dimension argument to c.stars
                    .multicombine=TRUE) %dopar% {
                      dpList[[l]][[1]]
                    }

depthIndex <- foreach(l = 1:length(dpList),
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
thisDepth <- paste0(thisInunName, '_', summaryFun, '_', 
                    str_flatten(args[10:length(args)], collapse = '_'),
                    '_', chunkName)
thisIndex <- paste0(thisInunName, '_', summaryFun, '_index', '_',
                    str_flatten(args[10:length(args)], collapse = '_'),
                    '_', chunkName)
assign(thisDepth, depthAns)
assign(thisIndex, depthIndex)

# Could just use thisInunName for the rdata, since there's a folder structure,
# but this is more explicit
  # This does not actually save a list; to save with character vectors need to
  # use the list of characters
save(list = c(thisDepth, thisIndex), file = file.path(scriptOut, paste0(thisDepth, '.rdata')))
