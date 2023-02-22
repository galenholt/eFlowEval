# sizeTiming OUtputs

# First, read in all the data, so we can actually look at the ANAES
  # This is the same as the first part of sizeTiming, but DOESN"t cut to the
  # arraynum

source('directorySet.R')

scriptOut <- paste0(datOut, '/Inundationprocessed/', summaryFun, '/Timetesting/', args[9])

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
# plan(multicore) # multicore on HPC

# # For local testing
plan(multisession)
summaryFun <- 'areaInun'
args <- c('blah', 'b', 'c', 'g', '5', 't', 'a', '8', 'CondamineBalonne')

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



# DIVERGE FROM PROCESSINUNDATIONCHUNKS HERE -------------------------------
testsizes <- c(4e8, 2.5e8, 2e8, 1e8, 5e7, 1e7, 5e6, 1e6, 1e5, 1e4)

# Get areas
# First, get the areas
anaePolys <- anaePolys %>%
  mutate(area = as.numeric(st_area(.)))

# For each value, get the closest 
sizeindex <- 0*testsizes
for (i in 1:length(testsizes)) {
  sizeindex[i] <- which(abs(anaePolys$area-testsizes[i]) == min(abs(anaePolys$area - testsizes[i])))
}

# cut to this chunk of polygons
anaePolys <- anaePolys[sizeindex, ]

# Now we have ten polys at approximately the sizes I want to test
# Now split each value into its own run (whihc is super dumb, but we can't do
# more than 1 of the big ones)
# anaePolys <- anaePolys[arraynum, ]

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
  cropTif <- st_crop(checkTif, anaePolys[9,], as_points = FALSE) # CHANGED HERE BECAUSE FIRST ONE IS HUGE
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

# The above takes FOREVER
# I think I need to just comment it out and exclude the one I know fails



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



# Now, what are the outcomes? ---------------------------------------------

sizetimes <- c(12554, 55615, 26278, 16111, 3329, 869, 101, 80, 49, 35)
memtimes <- c(12533, 55745, 26378, 15997, NA, NA, NA, NA, NA, NA) # NA because still running
# Bigmem doesn't seem to matter, so cancel the rest of those runs

anaePolys <- bind_cols(anaePolys, sizetimes = sizetimes)

ggplot(anaePolys, aes(x = area, y = sizetimes)) + geom_point() + geom_line()

# Why is that big one so weirdly fast?
plot(ltimNoNorth[7, 'ValleyName'], reset = FALSE)
plot(anaePolys['UID'], add = TRUE)

plot(anaePolys[1, 'UID'])
plot(anaePolys[2, 'UID'])
plot(anaePolys[3, 'UID'])
# Nothing obvious in the plots. If anythignI'd expect all the holes in 1 to make
# it slower, because it needs more raster cells to get to its area (becasue it's
# 'less dense')

# What are the bbox sizes that it's cropping to?
st_area(st_as_sfc(st_bbox(anaePolys[1, ])))
st_area(st_as_sfc(st_bbox(anaePolys[2, ])))
st_area(st_as_sfc(st_bbox(anaePolys[3, ])))

# lets do the crops and see what happens?

system.time(crop1 <- st_crop(tifTimes, anaePolys[1,], as_points = FALSE))
system.time(crop2 <- st_crop(tifTimes, anaePolys[2,], as_points = FALSE))
system.time(crop3 <- st_crop(tifTimes, anaePolys[3,], as_points = FALSE))

# And the read-in
  # This itself might take too long?
# system.time(rastSF1 <- st_as_sf(crop1, as_points = FALSE, merge = FALSE, na.rm = FALSE))
# system.time(rastSF2 <- st_as_sf(crop1, as_points = FALSE, merge = FALSE, na.rm = FALSE))



# No idea what's up with that biggest one. But if we assume that the others
# basically set the scale, can we figure out how many hours we *think* this
# should take? (not quite clear if I should fit an exponential or not, but given
# that the big one is the ONLY big one, maybe doesn't matter?

areahours <- anaePolys %>% select(area, sizetimes = sizetimes) %>% 
  mutate(sizehours = sizetimes/60/60, slope = sizetimes/area) %>% st_drop_geometry()
areahours

# Logging variables always makes it worse, so linear is reasonable.
ggplot(anaePolys, aes(x = area, y = log(sizetimes))) + geom_point() + geom_line()
ggplot(anaePolys, aes(x = log(area), y = sizetimes)) + geom_point() + geom_line()
ggplot(anaePolys, aes(x = log(area), y = sizetimes)) + geom_point() + geom_line()

# a super simple linear extrapolation
mutate(areahours, slope = sizetimes/area)

# If we just use the one set by the second (ie the highest time), we get
slopeest <- areahours$slope[2]


# Now, can we use that to try to sort out the times needed for each chunk?

# REad in the whole catchment: most of this stolen from testingInundation
load(file.path(datOut, 'ANAEprocessed', allANAES))
anaePolys <- get(thisAName)
# Somehow some are invalid, despite the st_make_valid in processANAE
anaePolys <- st_make_valid(anaePolys)


# First, get the areas
anaePolys <- anaePolys %>%
  mutate(area = as.numeric(st_area(.))) 

chunksize <- ceiling(nrow(anaePolys)/nchunks)

# Now, make each topbottom (well, really, want to categorize)
chunknum <- rep(1:10, each = chunksize) 
chunknum <- chunknum[1:nrow(anaePolys)] # Because there might be a few less in the last one
anaePolys$chunknum <- chunknum


# Now, a crude estiate of time
anaePolys <- anaePolys %>%
  mutate(timePredict = slopeest*area)

# And how much time for each chunk?

chunktimes <- anaePolys %>%
  st_drop_geometry() %>%
  group_by(chunknum) %>%
  summarize(timeseconds = sum(timePredict), timehours = timeseconds/60/60,
            timeParHours = timehours/10) # because parallel

actualtimes <- tibble(actualtimes = c(10349, 6992, 32405, NA, NA, 37843, NA, 7471, 3458, 3116))

chunktimes <- bind_cols(chunktimes, actualtimes) %>% mutate(actualhours = actualtimes/60/60)
chunktimes
ggplot(chunktimes, aes(x = timeParHours, y = actualhours)) + geom_point() + geom_line() + geom_abline(slope = 1, intercept= 0)

# all but one are below the line. I guess that's good?

# So, that implies I'll get pretty close if I just extend the timing out above 19 hours. Will doubling it help? Or is it actually exponential?
# I think I'll try saying 48 hours and re-running the failed arraynums. That's WAY above the 19 suggested here

# Now, let's check each of the failed catchments
failed <- c('CondamineBalonne', 'Lachlan', 'LowerDarling', 'Paroo', 'Warrego')

registerDoFuture()
plan(multisession)
chunklist <- foreach (i = 1:length(failed), .combine=bind_rows) %dopar% {
  thisAName <- paste0(failed[i], 'ANAE') 
  allANAES <- list.files(file.path(datOut, 'ANAEprocessed'), pattern = paste0('^', thisAName))
  load(file.path(datOut, 'ANAEprocessed', allANAES))
  anaePolys <- get(thisAName)
  rm(list = thisAName)
  
  # Somehow some are invalid, despite the st_make_valid in processANAE
  anaePolys <- st_make_valid(anaePolys) %>%
    mutate(area = as.numeric(st_area(.))) 
  
  chunksize <- ceiling(nrow(anaePolys)/nchunks)
  
  # Now, make each topbottom (well, really, want to categorize)
  chunknum <- rep(1:10, each = chunksize) 
  chunknum <- chunknum[1:nrow(anaePolys)] # Because there might be a few less in the last one
  anaePolys$chunknum <- chunknum
  
  
  # Now, a crude estiate of time
  anaePolys <- anaePolys %>%
    mutate(timePredict = slopeest*area)
  
  # And how much time for each chunk?
  
  chunktimes <- anaePolys %>%
    st_drop_geometry() %>%
    group_by(chunknum) %>%
    summarize(timeseconds = sum(timePredict), timehours = timeseconds/60/60,
              timeParHours = timehours/10) %>%
    mutate(basin = thisAName)
  chunktimes
}

max(chunklist$timeParHours)
