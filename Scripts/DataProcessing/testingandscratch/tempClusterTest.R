# Script to test shifting temp (and all raster data) processing to the same system as developed for inundation

# Based largely around inundationCleanSpeed, although I'm sure there'll be bugs to sort out.

# The hpc_wrapper tends to source this too, but if I do it here I don't have to
# wrap the script to run locally. Sort out a cleaner way to do this
source('directorySet.R')
options(error = traceback)
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
registerDoFuture
print('plan at the top')
plan(cluster) 
print('plan started')
source('Functions/rastPolyJoin.R')

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
}

## Read in ANAE polygons from the Lachlan
# datOut is location-aware, based on directorySet.R, so this should work
# locally or on HPC
load(file.path(datOut, 'ANAEprocessed', 'LachlanANAE.rdata'))
# set the crs- need to transform to starCRS because can't transform a proxy
LachlanANAE <- st_transform(LachlanANAE, starCRS)


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


# For the inundation testing I cut to a box (I think so i could plot). Not really sure that is necessary
# Parallel function -------------------------------------------------------

# if we want to have na.rm = TRUE, we can't actually pass that argument easily ( I don't think> need to test- can I just pass na.rm after FUN?)
# Will need to find somewhere with NAs
# weighted.mean.narm <- function(x, area, na.rm = TRUE){
#   weighted.mean(x, area, na.rm = TRUE)
# }

# Testing
# nanaes <- 2
# ntimes <- 1000

# timeinun <- function(ntimes, nanaes, FUN = weighted.mean) {
#   # Fun allows passing function for simultaneously testing that
#   
#   # subset the number of anaes
#   cutwet <- LachlanANAE[1:nanaes, ]
#   
#   # Subset the raster in time
#     # This is very silly- it won't set the time dimension without also setting the x and y, so just put them in for their boundaries
#   cutrast <- soilTstars[,1:1933,1:1865,1:ntimes]
#   # # As long as they're proxies, we can't change their crs, so have to shift
#   # # everything else to them
#   # # I've told NASA to give me this (4326 = WGS84) for soil temp, but st_crs() says
#   # # it's 9122 (NAD83) for british columbia. Force it to be what NASA says.
#   # starCRS <- 4326 # st_crs(soilTstars) 
#   # st_crs(soilTstars) <- starCRS
#   #   read_stars(sub = "LST_Day_1km", RasterIO = rasterio) 
#   # cutrast <- soilTstars[,,,1:ntimes]
#   # Because that only sets up a call list, the dimensions we get in rastPolyJoin are wrong
#   
#   
#   # Since we want to combine the two list bits differently, just return the list and let foreach make a list of lists for now
#   dpList <- foreach(s = 1:nrow(cutwet)) %dopar% {
#     thiscrop <- st_crop(cutrast, cutwet[s,], as_points = FALSE)
#     thisdepth <- rastPolyJoin(polysf = cutwet[s,], rastst = thiscrop,
#                               grouper = 'UID', maintainPolys = TRUE,
#                               na.replace = NA, whichcrs = 3577,
#                               pixelsize = pixarea)
#   } # end foreach
#   
#   # Then, unpack the lists
#   depthAns <- foreach(l = 1:length(dpList),
#                       .combine=function(...) c(..., along = 1), # Pass dimension argument to c.stars
#                       .multicombine=TRUE) %dopar% {
#                         dpList[[l]][[1]]
#                       }
#   
#   depthIndex <- foreach(l = 1:length(dpList),
#                         .combine=bind_rows,
#                         .multicombine=TRUE) %dopar% {
#                           dpList[[l]][[2]]
#                         }  
#   
#   return(list(avgPRStars = depthAns, avgPRindex = depthIndex))
# }
# 
# # Set up the benchmark runs -----------------------------------------------
# 
# registerDoFuture()
# plan(multisession) # On windows
# 
# # Can i loop over plans in the benchmarking somehow? One of the plans should jut collapse to serial, right?
# 
# 
# # benchmark the usual for loop vs the parallel with multisession
# # How about how long this is going to take to get to 20k ANAES? That might make the decision
# benchP_S <- microbenchmark::microbenchmark("t100a5" = { b <- timeinun(nanaes = 5)},
#                                            # "t100a10" = { b <- timeinun(nanaes = 10) },
#                                            "t1000a5" = { b <- timeinun(ntimes = 1000, nanaes = 5) },
#                                            "t2000a5" = { b <- timeinun(ntimes = 2000, nanaes = 5) },
#                                            "t2557a5" = { b <- timeinun(ntimes = 2557, nanaes = 5)},
#                                            times = 1)
# # benchP_S
# 
# # for HPC, useful to 
# print('time and anaes')
# print(benchP_S)

# Not too terrible, really. # For reference, was 21 seconds for inundation (but different set of anaes)

# Benchmark with more anaes on the cluster --------------------------------
print('write function')
# Make a simpler function- don't bother with times
timeinun <- function(nanaes, FUN = weighted.mean) {
  # Fun allows passing function for simultaneously testing that
  
  # subset the number of anaes
  cutwet <- LachlanANAE[1:nanaes, ]
  
  # Subset the raster in time
  # This is very silly- it won't set the time dimension without also setting the x and y, so just put them in for their boundaries
  # cutrast <- soilTstars[,1:1933,1:1865,1:ntimes]
  # # As long as they're proxies, we can't change their crs, so have to shift
  # # everything else to them
  # # I've told NASA to give me this (4326 = WGS84) for soil temp, but st_crs() says
  # # it's 9122 (NAD83) for british columbia. Force it to be what NASA says.
  # starCRS <- 4326 # st_crs(soilTstars) 
  # st_crs(soilTstars) <- starCRS
  #   read_stars(sub = "LST_Day_1km", RasterIO = rasterio) 
  # cutrast <- soilTstars[,,,1:ntimes]
  # Because that only sets up a call list, the dimensions we get in rastPolyJoin are wrong
  
  
  # Since we want to combine the two list bits differently, just return the list and let foreach make a list of lists for now
  dpList <- foreach(s = 1:nrow(cutwet), 
                    .packages = c("doFuture", "sf", "stars", "tidyverse")) %dopar% {
    thiscrop <- st_crop(soilTstars, cutwet[s,], as_points = FALSE)
    thisdepth <- rastPolyJoin(polysf = cutwet[s,], rastst = thiscrop,
                              grouper = 'UID', maintainPolys = TRUE,
                              na.replace = NA, whichcrs = 3577,
                              pixelsize = pixarea)
  } # end foreach
  
  # Then, unpack the lists
  depthAns <- foreach(l = 1:length(dpList),
                      .packages = c("doFuture", "tidyverse"),
                      .combine=function(...) c(..., along = 1), # Pass dimension argument to c.stars
                      .multicombine=TRUE) %dopar% {
                        dpList[[l]][[1]]
                      }
  
  depthIndex <- foreach(l = 1:length(dpList),
                        .packages = c("doFuture", "tidyverse"),
                        .combine=bind_rows,
                        .multicombine=TRUE) %dopar% {
                          dpList[[l]][[2]]
                        }  
  
  return(list(avgPRStars = depthAns, avgPRindex = depthIndex))
}

print('function written')
# I think split these up into separate runs
# multisession
# registerDoFuture()
# plan(multisession) # On windows
# benchmultisession <- microbenchmark::microbenchmark("t2557a10" = { b <- timeinun(nanaes = 10)},
#                                            "t2557a100" = { b <- timeinun(nanaes = 100)},
#                                            # "t2557a1000" = { b <- timeinun(nanaes = 1000)},
#                                            times = 1)
# 
# # for HPC, useful to 
# print('multisession')
# print(benchmultisession)


# #Multicore
# registerDoFuture()
# plan(multicore) # On windows
# benchmulticore <- microbenchmark::microbenchmark("t2557a10" = { b <- timeinun(nanaes = 10)},
#                                                     "t2557a100" = { b <- timeinun(nanaes = 100)},
#                                                     # "t2557a1000" = { b <- timeinun(nanaes = 1000)},
#                                                     times = 1)
# 
# # for HPC, useful to 
# print('multicore')
# print(benchmulticore)
# 
# Cluster
# print('sessionInfo')
# print(sessionInfo())
# 
# registerDoFuture()
# print('available workers: ')
# print(availableWorkers())

# Commenting out to try to just get the plan started
# print('makeNodePSOCK.setup.strategy is ')
# print(parallelly:::getOption2("parallelly.makeNodePSOCK.setup_strategy", "parallel"))

# print('trying to set up the cluster dry run')
# print(parallelly::makeClusterPSOCK(workers = availableWorkers(), dryrun = TRUE))


# print('now try the plan inside withCalling')
# write.to.log <- function(x) {
#   print(x) # Let's try this- it should just print directly?
# }
# withCallingHandlers(plan(cluster), error = function(e) { write.to.log(sys.calls()) })

# # This and the above both fail

# print('try to set up the cluster for real without setting plan()')
# print(parallelly::makeClusterPSOCK(workers = availableWorkers()))



# print('now try the plan alone')
# plan(cluster) 
# print('plan started')
print('about to bench')
benchcluster <- microbenchmark::microbenchmark("t2557a10" = { b <- timeinun(nanaes = 10)},
                                                 "t2557a100" = { b <- timeinun(nanaes = 100)},
                                                 # "t2557a1000" = { b <- timeinun(nanaes = 1000)},
                                                 times = 1)

# for HPC, useful to
print('cluster plan')
print(benchcluster)
""
# # Future.batchtools?
# registerDoFuture()
# # using template from
# # https://github.com/mllg/batchtools/blob/master/inst/templates/slurm-lido3.tmpl
# 
# ## ncpus [integer(1)]:        Number of required cpus per task,
# ##                            Set larger than 1 if you want to further parallelize
# ##                            with multicore/parallel within each task.
# ## walltime [integer(1)]:     Walltime for this job, in seconds.
# ##                            Must be at least 1 minute.
# ## memory   [integer(1)]:     Memory in megabytes for each cpu.
# ##                            Must be at least 100 (when I tried lower values my
# ##                            jobs did not start at all).
# 
# # Trying to set up resources according to
# # https://github.com/mllg/batchtools/issues/201
# 
# resources = list(ncpus =  1L,
#                  walltime = 200L, # Probably too long for most, but some will fail at this, I think
#                  memory = 10000L) # seems excessive, but this is 120GB/10cpus, where the node has 128GB. Then convert to Mb
# 
# plan(future.batchtools::batchtools_slurm, resources = resources)
# benchbatch <- microbenchmark::microbenchmark("t2557a10" = { b <- timeinun(nanaes = 10)},
#                                                  "t2557a100" = { b <- timeinun(nanaes = 100)},
#                                                  # "t2557a1000" = { b <- timeinun(nanaes = 1000)},
#                                                  times = 1)
# 
# # for HPC, useful to
# print('cluster plan')
# print(benchbatch)
# 
# # Then, I think if I can get batchtools to work, try to multilayer: plan(list(future.batchtools::batchtools_slurm, multisession))
# # probably makes most difference for lots of anaes (ie 1000+, where I can nest
# # somehow). Would need to write a different function to test it
# 
# # TODO: split these up into scripts, throw on cluster to test speeds
