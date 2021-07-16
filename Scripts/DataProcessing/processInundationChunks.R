# script to process inundation into ANAE polygons

# one of several statistics

# Very similar to inundationCleanSpeed, but focused just on speed testing the plans, not on also checking assorted things work
# The hpc_wrapper tends to source this too, but if I do it here I don't have to
# wrap the script to run locally. Sort out a cleaner way to do this
source('directorySet.R')

scriptOut <- paste0(datOut, '/Inundationprocessed/', summaryFun, '/chunked/', args[9])

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

# Set up parallel backend
registerDoFuture()
plan(multicore) # Likely multicore on HPC?

# Choose a size for the chunks. This is likely better elsewhere, but
nchunks <- 10
arraynum <- as.numeric(args[8])
chunkName <- args[8]

# chunksize <- 6000

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
inunTifsALL <- file.path(inunDir, alltifs)

# get the crs from the first one (will read them all in later, but first need
# to deal with the corrupt file)
# As long as they're proxies, we can't change their crs, so have to shift
# everything else to them
starCRS <- st_crs(read_stars(inunTifsALL[1]))

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
# Get the row indices from the array number 
chunksize <- ceiling(nrow(anaePolys)/nchunks)
# arraynum <- 3
prevtop <- chunksize*(arraynum-1)
bottom <- prevtop+1

if (arraynum == nchunks) {
  top <- nrow(ANAEbasinclim) # make sure we finish
} else {
  top <- prevtop + chunksize
}

# cut to this chunk of polygons
anaePolys <- anaePolys[bottom:top, ]

# Transform to stars crs
anaePolys <- st_transform(anaePolys, starCRS)

# handle corrupt tif ------------------------------------------------------

# Need to do this after the ANAE because I have to read it in
# There's a corrupt tif, so cut it out
# Have to try to read in, but can't read in the whole thing. So do a little
# crop and call it good
passer <- vector(mode = 'logical', length(inunTifsALL))
for (tif in 1:length(inunTifsALL)) {
  checkTif <- read_stars(inunTifsALL[tif])
  cropTif <- st_crop(checkTif, anaePolys[1,], as_points = FALSE)
  testsf <- NA
  try(testsf <- st_as_sf(cropTif, as_points = FALSE, merge = FALSE, na.rm = FALSE),
      silent = TRUE)
  if (class(testsf) == 'logical') {
    passer[tif] <- FALSE
  } else if ('sf' %in% class(testsf)) {
    passer[tif] <- TRUE
  }
  rm(testsf)
}

# and now the list of functional tifs is
inunTifs <- inunTifsALL[passer]



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


# Since we want to combine the two list bits differently, just return the list
# and let foreach make a list of lists for now
# TESTING
# dpList <- foreach(s = 1:10) %dopar% {
dpList <- foreach(s = 1:nrow(anaePolys)) %dopar% {
  thiscrop <- st_crop(tifTimes, anaePolys[s,], as_points = FALSE)
  thisdepth <- rastPolyJoin(polysf = anaePolys[s,], rastst = thiscrop, FUN = chosenSummary,
                            grouper = 'UID', maintainPolys = TRUE,
                            na.replace = 0, whichcrs = 3577)
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
thisDepth <- paste0(thisInunName, '_', summaryFun, '_', chunkName)
thisIndex <- paste0(thisInunName, '_', summaryFun, '_index', '_', chunkName)
assign(thisDepth, depthAns)
assign(thisIndex, depthIndex)

# Could just use thisInunName for the rdata, since there's a folder structure, but this is more explicit
save(list = c(thisDepth, thisIndex), file = file.path(scriptOut, paste0(thisDepth, '.rdata')))
