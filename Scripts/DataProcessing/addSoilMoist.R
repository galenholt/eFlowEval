# Script to build on the ANAE without having to reprocess it
library(here)
library(tidyverse)
library(sf)
library(stars)

# Argh. sort all this directory crap out later
  # Trying to at least separate scripts and functions, looking towards library
source(here('Functions', 'rastPolyJoin.R'))
source(here('Functions', 'timeRoll.R'))


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
soilMstars <- read_ncdf(file.path(datDir, 'soilmoisture/sm_pct_2020_Actual_day.nc'))

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
whichcrs <- st_crs(soilMstars)
# TODO:: Check this is right on PC, I didn't need to set the CRS there for some reason
st_crs(lachAll) <- 4283 # This is what's there when I look at st_crs, but it's not registering for some reason. 
lachAll <- st_transform(lachAll, whichcrs)
ltimCut <- st_transform(ltimCut, whichcrs)
basin <- st_transform(basin, whichcrs)

# Crop. Have to use as_points = FALSE or it crops to the raster centers, and misses stuff around the edges.
lachSoil <- st_crop(soilMstars, filter(ltimCut, ValleyName == 'Lachlan'), as_points = FALSE)
# check it worked
plot(lachSoil[,,,1:4])


# Let's set up a bbox for subsampled plotting without taking 8 million years
bb = st_bbox(c(xmin = 144.4, ymin = -33.8, xmax = 144.6, ymax = -33.6), crs = whichcrs)

# And quick plots of the ANAE and rasters
anaeSub <- st_crop(lachAll, st_as_sfc(bb))
plot(anaeSub[,'ANAE_DESC'])

soilSub <- lachSoil[st_as_sfc(bb)]
plot(soilSub[,,,10:13])

# 1. Spatial average of raster into the ANAE polys at each timestep ---------------------------

# Test the new function
system.time(dailySMpolyavg <- rastPolyJoin(polysf = lachAll, rastst = lachSoil, grouper = 'SYS2', maintainPolys = TRUE))
# 290 seconds with 280 timesteps in Lachlan
  # There has GOT to be a way to speed this up...
  # It's the grouped summarize that kills it, NOT the intersection. i think it
  # might be the work sf is doing to put the polygons back together?
    # TODO: see if it's faster to st_drop_geometry, do the summarize, and then join back to lachAll with SYS2?

# unpack the list
dailyPolyindex <- dailySMpolyavg[[2]]
# at least for testing, don't overwrite, or have to run the whole thing again
dailyPolySMavg <- dailySMpolyavg[[1]]

# Check ordering
all(dailyPolyindex$SYS2 == lachAll$SYS2)

# Test plot
dailyPolySub <- dailyPolySMavg[st_as_sfc(bb)]
plot(dailyPolySub[,,10:13])

# ggplot is a bigger pain, skip for now, we just want to look at things
# testDaily <- ggplot() + 
#   geom_stars(data = dailyPolySub[,,10:13], aes(fill = sm_pct)) +
#   facet_grid(cols = as.character)
# testDaily

# Rolling through time over the spatial polygons ---------------------

# Set up a new stars object from the daily soil moistures in the polygons
polyTimeSmax_10 <- dailyPolySMavg 

# Get a 10-day rolling max for each polygon
system.time(polyTimeSmax_10[[1]] <- timeRoll(polyTimeSmax_10[[1]], 
                                             FUN = RcppRoll::roll_max, 
                                             rolln = 10, 
                                             align = 'right',
                                             na.rm = TRUE))

# Half a second?? That's great!

# Test plot
max10PolySub <- polyTimeSmax_10[st_as_sfc(bb)]
plot(max10PolySub[,,10:13])

# Rolling through time on the raster, then putting into polygons --------

# Could probably streamline with the dailies in the first section, but for now
rastTimeSmean_7 <- lachSoil
system.time(rastTimeSmean_7[[1]] <- timeRoll(rastTimeSmean_7[[1]],
                                             FUN = RcppRoll::roll_mean, 
                                             rolln = 7, 
                                             align = 'right',
                                             na.rm = TRUE))
# 0.7 seconds
# Test plot the raster itself
rastweekSub <- rastTimeSmean_7[st_as_sfc(bb)]
plot(rastweekSub[,,,10:13])


# Now, put those time-means into polygons
system.time(weeklySMpoly <- rastPolyJoin(polysf = lachAll, rastst = rastTimeSmean_7, grouper = 'SYS2', maintainPolys = TRUE))


# unpack the list
weeklyPolyindex <- weeklySMpoly[[2]]
# at least for testing, don't overwrite, or have to run the whole thing again
weeklyRastavgSMpoly <- weeklySMpoly[[1]]

# Check ordering
all(weeklyPolyindex$SYS2 == lachAll$SYS2)

# Test plot
weekmeanSub <- weeklyRastavgSMpoly[st_as_sfc(bb)]
plot(weekmeanSub[,,10:13])



# Split the polygons by the rasters to get smaller scale ------------------

# Let's do this with the dailies, because then it would be easy to, for example, get a rolling min or something.

# Test the new function
system.time(dailySMpolysplit <- rastPolyJoin(polysf = lachAll, 
                                             rastst = lachSoil, 
                                             grouper = 'SYS2', 
                                             maintainPolys = FALSE))
# 290 seconds with 280 timesteps in Lachlan
# There has GOT to be a way to speed this up...
# It's the grouped summarize that kills it, NOT the intersection. i think it
# might be the work sf is doing to put the polygons back together?
# TODO: see if it's faster to st_drop_geometry, do the summarize, and then join back to lachAll with SYS2?

# unpack the list
dailyPolySplitindex <- dailySMpolysplit[[2]]
# at least for testing, don't overwrite, or have to run the whole thing again
dailyPolySMsplit <- dailySMpolysplit[[1]]

# Check ordering
  # This SHOULD be false
all(dailyPolySplitindex$SYS2 == lachAll$SYS2)

# Test plot
dailyPolySplitSub <- dailyPolySMsplit[st_as_sfc(bb)]
plot(dailyPolySplitSub[,,10:13])


# all the test plots in one place -----------------------------------------
  #including making the subsets and bbox, so we can use something different here, or comment out above

# it's a little annoying that st_crop is a hard crop, but the [] cropping lets
# the polys hang over, and so we get different extents. So doing the polygon
# crops a bit different than above, so we DO get the same extents

# Let's set up a bbox for subsampled plotting without taking 8 million years
bb = st_bbox(c(xmin = 144.4, ymin = -33.8, xmax = 144.6, ymax = -33.6), crs = whichcrs)

# ANAE
anaeSub <- st_crop(lachAll, st_as_sfc(bb))
plot(anaeSub[,'ANAE_DESC'])

# Soil moisture raster
soilSub <- lachSoil[st_as_sfc(bb)]
plot(soilSub[,,,10:13])

# Daily sm in the polygons
dailyPolySub <- st_crop(st_as_sf(dailyPolySMavg[,,10:13]), st_as_sfc(bb))
plot(dailyPolySub)

# 10-day max in the polygons directly
max10PolySub <- st_crop(st_as_sf(polyTimeSmax_10[,,10:13]), st_as_sfc(bb))
plot(max10PolySub)

# Weekly mean in the raster (as a raster still)
rastweekSub <- rastTimeSmean_7[st_as_sfc(bb)]
plot(rastweekSub[,,,10:13])

# Weekly mean put into polygons
weekmeanSub <- st_crop(st_as_sf(weeklyRastavgSMpoly[,,10:13]), st_as_sfc(bb))
plot(weekmeanSub)

# and the split by raster units
dailyPolySplitSub <- st_crop(st_as_sf(dailyPolySMsplit[,,10:13]), st_as_sfc(bb))
plot(dailyPolySplitSub)

save(lachAll, 
     lachSoil, 
     dailyPolySMavg, 
     polyTimeSmax_10, 
     weeklyRastavgSMpoly, 
     file = file.path(datOut, 'kanDemo.rdata'))



