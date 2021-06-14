# cleaned up script to get soil temp into the anae polygons

# Unlike the moisture, I think I'm going to split this into the polygon averaging (which takes forever) and the time rolling, whcih shoudl be as fast as everything else

# Script to build on the ANAE without having to reprocess it
library(here)
library(tidyverse)
library(sf)
library(stars)

# Argh. sort all this directory crap out later
# Trying to at least separate scripts and functions, looking towards library
source(here::here('Functions', 'rastPolyJoin.R'))
source(here::here('Functions', 'timeRoll.R'))
source(here::here('Functions', 'timeChunkPoly.R'))

myhome <- str_remove(path.expand("~"), "/Documents")
datDir <- file.path(myhome, "Deakin University/QAEL - MER/Model/dataBase") # "C:/Users/Galen/Deakin University/QAEL - MER/Model/dataBase"

datOut <- "datOut"

# load the processed anae files, cut to lachlan
load(file.path(datOut, 'lachAll.rdata'))
# load(file.path(datOut, 'bothANAE.rdata'))

# To allow plotting the ltim zones (otherwise their polygons get lost)
# And get the LTIM_Valleys to use to subset for toy models at scale, but not enormous scale
LTIM_Valleys <- read_sf(dsn = file.path(datDir, 'ANAE/MDB_ANAE.gdb'), layer = 'LTIM_Valleys') %>%
  st_cast("MULTIPOLYGON") # cleans up an issue with multisurfaces

# LTIM areas, useful for plotting
ltimCut <- LTIM_Valleys %>%
  select(ValleyName, ValleyID, ValleyCode) # Three different ways to reference, basically


# Read in the soil data
# soilMstarsOrig <- read_ncdf(file.path(datDir, 'soilmoisture/sm_pct_2010_Actual_day.nc'))

# Arh. the temp doesn't read in the same way as the soil moist. back to the drawing board
# Set up where the soil temp data is
tempfile <- list.files(file.path(datDir, 'soilTemp1419'), pattern = '.nc')
temppath <- file.path(datDir, 'soilTemp1419', tempfile)

# Previous work -----------------------------------------------------------

# and the basin boundary, for clipping rasters; though likely will start with lachlan
basin <- read_sf(dsn = file.path(datDir, 'ANAE/MDB_ANAE.gdb'), layer = 'MDB_Boundary') %>%
  st_cast("MULTIPOLYGON")  %>% # cleans up an issue with multisurfaces
  dplyr::select(LEVEL2NAME) # no need for other info

# Set the crs.
# Doing this here because it's not yet clear which is best, so we want
# everything in before we choose one to get the donor CRS from
# using a regular (not curvilinear) grid is necessary to avoid retaining the
# full extent and size of the raster
# soilMstars comes in as regular, so use that. Though st_warp() likely would
# do it on a different projection (see help and vignettes)
whichcrs <- 4326 # st_crs(soilTstars) # this is WGS84, and is what I've told NASA to give me for soil temp (and matches soil moisture as well)
# TODO:: Check this is right on PC, I didn't need to set the CRS there for some reason
st_crs(lachAll) <- 4283 # This is what's there when I look at st_crs, but it's not registering for some reason. 
lachAll <- st_transform(lachAll, whichcrs)
ltimCut <- st_transform(ltimCut, whichcrs)
basin <- st_transform(basin, whichcrs)

# Do the soil moist read-in and processing in a loop ----------------------

# First, use a stars object to get the overall dimensions without reading data
soilTstars <-  read_stars(temppath, sub = "LST_Day_1km", proxy = TRUE) # Force a proxy for testing the subsetting
st_crs(soilTstars) <- 4326

soilDims <- st_dimensions(soilTstars)
soilTstars

# New formulation
days <- soilDims$time$to
chunksize = 100 # Not really sure, needs to be tested better
nchunks <- ceiling(days/chunksize) # 2 or 4 or something for testing

lachcutter <- filter(ltimCut, ValleyName == 'Lachlan')

savechunks <- 1:nchunks # If the chunksize is big, worth saving all of them, probably

# does the whole loop (or, over the first nchunks, I suppose. Useful to do 2 or something to test.)
# savepoints are CHUNK numbers, not SHEET numbers
system.time(dailyTemppolyavg <- chunklooper(nchunks = nchunks, rastpath = temppath, rastvar = "LST_Day_1km", 
                                            totaltime = days, starttime = 1, chunksize = chunksize, 
                                            catchCrop = lachcutter, polyAvg = lachAll, saverast = TRUE,
                                            savepoints = savechunks, savepath = file.path(datOut, 'tempIntermediate')))

# user    system   elapsed 
# 150367.89   4726.53 155519.14 
155520/60/60

# Save that NOW. 
save(lachAll, 
     dailyTemppolyavg, 
     file = file.path(datOut, 'lachTempPolyAvg.rdata'))



# Process and test --------------------------------------------------------

# unpack the list
dailyPolyindex <- dailyTemppolyavg[[2]]
# at least for testing, don't overwrite, or have to run the whole thing again
dailyPolyTempavg <- dailyTemppolyavg[[1]]

# Get rid of the list to save space
# rm(dailyTemppolyavg)
# Why not keep the list and not do the above? I dunno. There was a reason.

# Check ordering
all(dailyPolyindex$SYS2 == lachAll$SYS2)

# Test plot
# Let's set up a bbox for subsampled plotting without taking 8 million years
bb <- st_bbox(c(xmin = 144.4, ymin = -33.8, xmax = 144.6, ymax = -33.6), crs = whichcrs)
dailyPolySub <- dailyPolyTempavg[st_as_sfc(bb)]
plot(dailyPolySub[,,10:13])

# Do the processing on the newly-filled ANAE polys ------------------------

# Likely will split this off into its own script, that just reads in
# lachTempPolyAvg.rdat, so we can update in future sessions after the above runs
# for days

# See addSoilTempTesting for a start.

# TODO: figure out what the strictures are.

# Let's say the 60 degree condition can't have been met in the last month (28 days)? No idea why I'm choosing that, but just because
# To meet the "dead if temp > 60 in last month" condition, we need to check if max(last 28 days) is above 10
soilTempMax28 <- dailyPolyTempavg 

range(dailyPolyTempavg[[1]], na.rm = T)-273
hist(dailyPolyTempavg[[1]])
sum((dailyPolyTempavg[[1]]-273) > 60, na.rm = TRUE)

# Get a 42-day rolling min for each polygon
system.time(soilTempMax28[[1]] <- timeRoll(soilTempMax28[[1]], 
                                            FUN = RcppRoll::roll_max, 
                                            rolln = 28, 
                                            align = 'right',
                                            na.rm = TRUE))

# 14 seconds. Why so long?
# Test plot
max28polysub <- soilTempMax28[st_as_sfc(bb)]
plot(max28polysub[,,10:13])

# Save a lot of stuff
save(lachAll,
     dailyPolyTempavg,
     soilTempMax28,
     file = file.path(datOut, 'lachSoilTempprocessedAllOut.rdata'))

# Save just the outputs, separately (for memory management on the strictures side)

save(lachAll,
     soilTempMax28,
     file = file.path(datOut, 'lachSoilTempMax28.rdata'))

# A check that we saved the things we want to
# load(file.path(datOut, 'tempIntermediate_22.rdata'))
# chunk1[[1]]
# dailyTemppolyavg[[1]]


# Careful with below; do checks in new session ----------------------------


# Open a new session so don't overwrite
# load(file.path(datOut, 'tempIntermediate_22.rdata'))
# load(file.path(datOut, 'lachTempPolyAvg.rdata'))

# For the raster, the x,y have turned into polygons (because that's how we
# processed it), and time has turned into "band", but that's fixable, but the
# data's there.
# testrast <- read_stars(file.path(datOut, 'catchCrop_LST_Day_1km_101.tif'))
# testrast
# plot(testrast[,,,1:6])

# 'band' does reset for each one, so be careful with that.
# testrast2 <- read_stars(file.path(datOut, 'catchCrop_LST_Day_1km_201.tif'))
# testrast2
# plot(testrast2[,,,1:6])
