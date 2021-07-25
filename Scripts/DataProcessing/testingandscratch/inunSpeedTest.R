# Script to cleanly test inundation speed of different plans, across nodes, etc

# Very similar to inundationCleanSpeed, but focused just on speed testing the plans, not on also checking assorted things work
# The hpc_wrapper tends to source this too, but if I do it here I don't have to
# wrap the script to run locally. Sort out a cleaner way to do this
source('directorySet.R')

scriptOut <- paste0(datOut, '/Inundationprocessed')

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
registerDoFuture()
source('Functions/rastPolyJoin.R')


# Read in all the data ----------------------------------------------------

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
starCRS <- st_crs(read_stars(inunTifsALL[1]))

## Read in some ANAE polygons from the Lachlan
# datOut is location-aware, based on directorySet.R, so this should work
# locally or on HPC
load(file.path(datOut, 'ANAEprocessed', 'LachlanANAE.rdata'))
# set the crs
LachlanANAE <- st_transform(LachlanANAE, starCRS)

## Do some minor data wrangling to get set up for testing

# # There's a corrupt tif, so cut it out
# # Have to try to read in, but can't read in the whole thing. So do a little
# # crop and call it good
# passer <- vector(mode = 'logical', length(inunTifsALL))
# for (tif in 1:length(inunTifsALL)) {
#   checkTif <- read_stars(inunTifsALL[tif])
#   cropTif <- st_crop(checkTif, LachlanANAE[1,], as_points = FALSE)
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

# and now the list of functional tifs is
# inunTifs <- inunTifsALL[passer]

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


# Clip the polygons to a tiny box for testing (115 polygons)
# bb <- st_bbox(c(xmin = 147.4, ymin = -33.7, xmax = 147.6, ymax = -33.6), 
#               crs = starCRS)

# First thing to do is to clip the wetlands to the box so I can see what I'm doing
# and deal with transforms. 
LachlanANAE <- LachlanANAE %>%
  st_transform(st_crs(starCRS))



# -------------------------------------------------------------------------
# I think we're pretty much set up for the looping. If we want to JUST check the
# anae looping, and always use all times, could find tifTimes here. Otherwise,
# do it in the benchmark fnction. I suppose for benchmarking might as well do
# that.

print('past the read-in, about to write functions')

# Parallel function -------------------------------------------------------

timeinunP <- function(ntimes, nanaes, FUN = weighted.mean) {
  # Fun allows passing function for simultaneously testing that
  
  # Read in a few stars proxies
  # Is there any harm here in reading them all in as PROXIES?
  tifTimes <- inunTifs[1:ntimes] %>% # filenames
    read_stars() %>% # read in
    merge() %>% # make a brick 
    setNames('depth') %>% # name the attribute
    # Make dates
    # soilMstars uses an offset and delta, while this has 'values'.
    # I seem to remember values might cause an issue somewhere down the track, so
    # might need to revisit
    st_set_dimensions(3, values = tifdates[1:ntimes]) %>% 
    st_set_dimensions(names = c("x", "y", "time"))
  
  # For looping at first, use a subset. Come back to test time looping
  # a subset for testing
  cutwet <- LachlanANAE[1:nanaes, ]
  
  # Since we want to combine the two list bits differently, just return the list and let foreach make a list of lists for now
  dpList <- foreach(s = 1:nrow(cutwet)) %dopar% {
    thiscrop <- st_crop(tifTimes, cutwet[s,], as_points = FALSE)
    thisdepth <- rastPolyJoin(polysf = cutwet[s,], rastst = thiscrop, FUN = FUN,
                              grouper = 'UID', maintainPolys = TRUE,
                              na.replace = 0, whichcrs = 3577)
  } # end foreach
  
  # Then, unpack the lists
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
  
  return(list(avgPRStars = depthAns, avgPRindex = depthIndex))
}


# Sequential
plan(sequential)
sequentialBench <- microbenchmark::microbenchmark("sequential" = { b <- timeinunP(ntimes = length(inunTifs), nanaes = 200, FUN = weighted.mean)},
                                                  times = 1)
print('sequential worked')
print(sequentialBench)

# Multisession
plan(multisession)
multisessionBench <- microbenchmark::microbenchmark("multisession" = { b <- timeinunP(ntimes = length(inunTifs), nanaes = 200, FUN = weighted.mean)},
                                                    times = 1)
print('multisession worked')
print(multisessionBench)

# multicore # Pretty sure this just fails silently on Windows
plan(multicore)
multicoreBench <- microbenchmark::microbenchmark("multicore" = { b <- timeinunP(ntimes = length(inunTifs), nanaes = 200, FUN = weighted.mean)},
                                                 times = 1)
print('multicore worked')
print(multicoreBench)
