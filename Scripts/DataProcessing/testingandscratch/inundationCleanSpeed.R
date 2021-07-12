# clean inundation test, hopefully becoming a speed test for the HPC

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

source('Functions/rastPolyJoin.R')


# Read in all the data ----------------------------------------------------

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

## Read in some ANAE polygons from the Lachlan
  # datOut is location-aware, based on directorySet.R, so this should work
  # locally or on HPC
load(file.path(datOut, 'ANAEprocessed', 'LachlanANAE.rdata'))
# set the crs
LachlanANAE <- st_transform(LachlanANAE, starCRS)

## Do some minor data wrangling to get set up for testing

# There's a corrupt tif, so cut it out
  # Have to try to read in, but can't read in the whole thing. So do a little
  # crop and call it good
passer <- vector(mode = 'logical', length(inunTifsALL))
for (tif in 1:length(inunTifsALL)) {
  checkTif <- read_stars(inunTifsALL[tif])
  cropTif <- st_crop(checkTif, LachlanANAE[1,], as_points = FALSE)
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
bb <- st_bbox(c(xmin = 147.4, ymin = -33.7, xmax = 147.6, ymax = -33.6), 
              crs = starCRS)

# First thing to do is to clip the wetlands to the box so I can see what I'm doing
# and deal with transforms. 
boxwet <- LachlanANAE %>%
  st_transform(st_crs(starCRS)) %>%
  st_crop(st_as_sfc(bb))



# -------------------------------------------------------------------------
# I think we're pretty much set up for the looping. If we want to JUST check the
# anae looping, and always use all times, could find tifTimes here. Otherwise,
# do it in the benchmark fnction. I suppose for benchmarking might as well do
# that.


# Serial function ---------------------------------------------------------

timeinun <- function(ntimes, nanaes) {
  
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
  cutwet <- boxwet[1:nanaes, ]
  
  thiscrop <- st_crop(tifTimes, cutwet[1,], as_points = FALSE)
  
  thisdepth <- rastPolyJoin(polysf = cutwet[1,], rastst = thiscrop, 
                            grouper = 'UID', maintainPolys = TRUE,
                            na.replace = 0, whichcrs = 3577)
  
  depthAns <- thisdepth[[1]]
  depthIndex <- thisdepth[[2]]
  
  # Crop the raster to the ANAE
  for (s in 2:nrow(cutwet)) {
    thiscrop <- st_crop(tifTimes, cutwet[s,], as_points = FALSE)
    
    thisdepth <- rastPolyJoin(polysf = cutwet[s,], rastst = thiscrop, FUN = weighted.mean,
                              grouper = 'UID', maintainPolys = TRUE,
                              na.replace = 0, whichcrs = 3577)
    depthAns <-  c(depthAns, thisdepth[[1]], along = 1)
    depthIndex <- bind_rows(depthIndex, thisdepth[[2]])
  }
  return(list(avgPRStars = depthAns, avgPRindex = depthIndex))
}


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
  cutwet <- boxwet[1:nanaes, ]
  
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

# Check each function with a first go
timeinun(10,10)
timeinunP(10,10)

# NEED TO TEST THE DIFFERENT RASTPOLY FUNCTIONS TOO
  # Passing arguments to the functions using ... doesnt work with the parallel
  # structure. Instead, we'll have to build the photic limit as a hardcode into
  # the function

# Set up the benchmark runs -----------------------------------------------

registerDoFuture()
plan(multisession) # On windows

# Can i loop over plans in the benchmarking somehow? One of the plans should jut collapse to serial, right?


# benchmark the usual for loop vs the parallel with multisession
# How about how long this is going to take to get to 20k ANAES? That might make the decision
benchP_S <- microbenchmark::microbenchmark("t10a10" = { b <- timeinun(ntimes = 10, nanaes = 10)},
                          "t100a10" = { b <- timeinun(ntimes = 100, nanaes = 10) },
                          "t100a50" = { b <- timeinun(ntimes = 100, nanaes = 50) },
                          "t100a100" = { b <- timeinun(ntimes = 100, nanaes = 100) },
                          "p_t10a10" = { b <- timeinunP(ntimes = 10, nanaes = 10)},
                          "p_t100a10" = { b <- timeinunP(ntimes = 100, nanaes = 10) },
                          "p_t100a50" = { b <- timeinunP(ntimes = 100, nanaes = 50) },
                          "p_t100a100" = { b <- timeinunP(ntimes = 100, nanaes = 100) },
                          times = 1)
benchP_S

# for HPC, useful to 
print(benchP_S)

# Not sure how to change the plan between benches, so 
registerDoFuture()
plan(sequential) # ie does it in serial
benchS_S <- microbenchmark::microbenchmark("t10a10" = { b <- timeinun(ntimes = 10, nanaes = 10)},
                                          "t100a10" = { b <- timeinun(ntimes = 100, nanaes = 10) },
                                          "t100a50" = { b <- timeinun(ntimes = 100, nanaes = 50) },
                                          "t100a100" = { b <- timeinun(ntimes = 100, nanaes = 100) },
                                          "p_t10a10" = { b <- timeinunP(ntimes = 10, nanaes = 10)},
                                          "p_t100a10" = { b <- timeinunP(ntimes = 100, nanaes = 10) },
                                          "p_t100a50" = { b <- timeinunP(ntimes = 100, nanaes = 50) },
                                          "p_t100a100" = { b <- timeinunP(ntimes = 100, nanaes = 100) },
                                          times = 1)
benchS_S
print(benchS_S)

# now, what else to test on HPC?
  # A few more plan()s



# Checking the rastPolyJoinFUNs work on HPC -------------------------------
  # This is a bit sideways, but this way we can test all at once
# volume
# area*depth for each raster, so sum(depth, area)
volSummary <- function(z, area) {
  sum(z*area)
}


# Volume of water <= 10cm (or arbitrary photic limit)
# depth up to 10cm * area, then summed
# So, get the minimum of depth or photic limit, multiply by area, and
# summarise with sum to get all depths
# Have to sort of write a min() out of ifelse or it does min() over the column, not the rows
volLimit <- function(x, area, limit = 0.1) {
  sum(ifelse(x > limit, limit*area, x * area))
}


# Area of inundation
areaInun <- function(x, area) {
  sum(ifelse(x > 0, area, 0))
}

# mean depth of the inundated area- this potentially much different than mean
# depth across the polygon
# give 0 weights to those that aren't inundated
avgInunDepth <- function(x, area) {
  areaifinun <- ifelse(x > 0, area, 0)
  weighted.mean(x, areaifinun)
}

# Fraction inundated
fracInun <- function(x, area) {
  areaInun(x, area) / sum(area)
}

# Test each function in a benchloop
rastPolyFunBench <- microbenchmark::microbenchmark("weightedmean" = { b <- timeinunP(ntimes = 100, nanaes = 10, FUN = weighted.mean)},
                               "areainundated" = { b <- timeinunP(ntimes = 100, nanaes = 10, FUN = areaInun) },
                               "volumeinundated" = { b <- timeinunP(ntimes = 100, nanaes = 10, FUN = volSummary) },
                               "volPhotic" = { b <- timeinunP(ntimes = 100, nanaes = 10, FUN = volLimit) },
                               "avgDepthInundation" = { b <- timeinunP(ntimes = 100, nanaes = 10, FUN = avgInunDepth) },
                               "fractionInundated" = { b <- timeinunP(ntimes = 100, nanaes = 10, FUN = fracInun) },
                               times = 1)

rastPolyFunBench

# Cool, all work


# Now, to benchmark for the HPC, what I really want to do is change the plan()s 
# There are a couple options. Maybe easiest, especially while we're testing is just to do each separately

# an alternative is to try to pass the plan changes to bench. But i think that
# will be an issue if some break. Whereas I can wrap plan with try() otherwise? Nah, that doesn't work

plan(sequential)
plan(multisession)
plan(multicore) # Seems like this shouldn't work on windows?
plan(future.batchtools::batchtools_slurm) # Spits out each future into slurm nodes
# Probably better to do 
plan(list(future.batchtools::batchtools_slurm, multisession)) # and include an outer layer of futures around catchment or something. 
# basically, according to 
# https://future.batchtools.futureverse.org/, it looks like the
# future.batchtools plans are about evaluating futures on NODES of a cluster,
# while the mutli... plans are about evaluating across cores. Which is super
# useful, but I think I could do something very similar with array jobs
# On the other hand, it might be SUPER useful to to the batchtools thing for multiple rastPolyJoin functions?
# So, should test, I guess. BUt will need to do that on the HPC, I think. In an interactive session?

# Let's set up those benchmarks as separate things, and then put together
  # bump this up to at least nanaes = 100 for the HPC i think

# Sequential
plan(sequential)
sequentialBench <- microbenchmark::microbenchmark("sequential" = { b <- timeinunP(ntimes = 100, nanaes = 10, FUN = weighted.mean)},
                                                   times = 1)

sequentialBench

# Multisession
plan(multisession)
multisessionBench <- microbenchmark::microbenchmark("multisession" = { b <- timeinunP(ntimes = 100, nanaes = 10, FUN = weighted.mean)},
                                                  times = 1)

multisessionBench

# multicore # Pretty sure this just fails silently on Windows
plan(multicore)
multicoreBench <- microbenchmark::microbenchmark("multicore" = { b <- timeinunP(ntimes = 100, nanaes = 10, FUN = weighted.mean)},
                                                    times = 1)

multicoreBench

# batchtools_slurm
plan(future.batchtools::batchtools_slurm)
batchtools_slurmBench <- microbenchmark::microbenchmark("batchtools_slurm" = { b <- timeinunP(ntimes = 100, nanaes = 10, FUN = weighted.mean)},
                                                 times = 1)

batchtools_slurmBench

# two levels: batchtools_slurm, multisession
# I think this is potentially super powerful, but likely a whole project to
# change the workflow. So skip for now, maybe work on sorting it out as a
# longer-term thing
plan(list(future.batchtools::batchtools_slurm, multisession))
batchtools_slurmMultiBench <- microbenchmark::microbenchmark("batchtools_slurmMulti" = { b <- timeinunP(ntimes = 100, nanaes = 10, FUN = weighted.mean)},
                                                        times = 1)

batchtools_slurmMultiBench