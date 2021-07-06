# Script to test rastPolyJoin
# Script to build on the ANAE without having to reprocess it
library(here)
library(tidyverse)
library(sf)
library(stars)

# Argh. sort all this directory crap out later
# Trying to at least separate scripts and functions, looking towards library
source('directorySet.R')
source(here('Functions', 'rastPolyJoin.R'))

# load the processed anae files, cut to lachlan
load(file.path(datOut, 'ANAEprocessed', 'LachlanANAE.rdata'))
# load(file.path(datOut, 'bothANAE.rdata'))

# # To allow plotting the ltim zones (otherwise their polygons get lost)
# # And get the LTIM_Valleys to use to subset for toy models at scale, but not enormous scale
# LTIM_Valleys <- read_sf(dsn = file.path(datDir, 'ANAE/MDB_ANAE_Aug2017/MDB_ANAE.gdb'), layer = 'LTIM_Valleys') %>%
#   st_cast("MULTIPOLYGON") # cleans up an issue with multisurfaces
# 
# # LTIM areas, useful for plotting
# ltimNoNorth <- LTIM_Valleys %>%
#   select(ValleyName, ValleyID, ValleyCode) # Three different ways to reference, basically

# Read in the soil data
soilMstars <- read_ncdf(file.path(datDir, 'soilmoisture/sm_pct_2020_Actual_day.nc'))

# and the basin boundary, for clipping rasters; though likely will start with lachlan
basin <- read_sf(dsn = file.path(datDir, 'ANAE/MDB_ANAE_Aug2017/MDB_ANAE.gdb'), layer = 'MDB_Boundary') %>%
  st_cast("MULTIPOLYGON")  %>% # cleans up an issue with multisurfaces
  dplyr::select(LEVEL2NAME) # no need for other info

# Set the crs.
# use the projected Australian Albers (3577) because the spherical geometry of
# unprojected (e.g. WGS84 lat/long) makes intersections break
whichcrs <- 3577
# TODO:: Check this is right on PC, I didn't need to set the CRS there for some reason
# st_crs(LachlanANAE) <- 4283 # This is what's there when I look at st_crs, but it's not registering for some reason. 
LachlanANAE <- st_transform(LachlanANAE, whichcrs)
ltimNoNorth <- st_transform(ltimNoNorth, whichcrs)
basin <- st_transform(basin, whichcrs)
# Have to st_warp soilMstars, NOT st_transform. Because it comes in as curvilinear
soilMstars <- st_warp(soilMstars, crs = whichcrs)

# Let's set up a bbox for subsampled plotting without taking 8 million years
bb <- st_bbox(c(xmin = 147.4, ymin = -33.7, xmax = 147.6, ymax = -33.6), crs = 4326)
bb <- st_transform(st_as_sfc(bb), whichcrs) # This is silly, but beats re-figuring where the box is
# Crop. Have to use as_points = FALSE or it crops to the raster centers, and misses stuff around the edges.
lachSoil <- st_crop(soilMstars, bb, as_points = FALSE)

# And quick plots of the ANAE and rasters
anaeSub <- st_crop(LachlanANAE, bb)

# check it worked
plot(lachSoil[,,,1], reset = FALSE)
plot(anaeSub['ANAE_DESC'], add = TRUE)

# # Testing inside rastPolyJoin
# notin <- which(!(polysf$UID %in% intPR$UID))
# sfin <- which((polysf$UID %in% intPR$UID))
# plot(rastSF[1], reset = FALSE)
# plot(polysf[notin, 'ANAE_DESC'], add = TRUE)
# plot(polysf[sfin, 'ANAE_DESC'], add = TRUE)
# plot(st_as_sfc(bb), add = TRUE)
# dev.off()
# plot(intPR[,'ANAE_DESC'])
# r60b6pv2p
# 
# intPR <- st_intersection(polysf[15:20, c('ANAE_DESC', 'UID')], rastSF[,1])
# 
# unionPR <- st_union(polysf[15:20, c('ANAE_DESC', 'UID')], rastSF[,1])
# 
# intPR2 <- st_intersection(rastSF[,1], polysf[15:20, c('ANAE_DESC', 'UID')])
# 
# polysf <- polysf %>% mutate(SYS2 = paste0(SYSID, row_number()))
# intPR <- st_intersection(st_make_valid(polysf[15:20, c('ANAE_DESC', 'UID', 'SYS2')]), st_make_valid(rastSF[,1]))
# plot(intPR)
# 1. Spatial average of raster into the ANAE polys at each timestep ---------------------------

# Test the new function
system.time(dailySMpolyavg <- rastPolyJoin(polysf = anaeSub, rastst = lachSoil, grouper = 'UID', maintainPolys = TRUE))
# TODO: see if it's faster to st_drop_geometry, do the summarize, and then join back to LachlanANAE with SYS2?

# dailySMpolyavg[[1]] is a stars object with 2 dims (geometry x 140 and time x 301)
# dailySMpolyavg[[2]] is an sf object wit the UID (or other grouper) and geometry (times 140, matching [[1]])


# Testing inside rastPolyJoin ---------------------------------------------

# Option 1: Drop geometry, then put back
avgPR2 <- intPR %>%
  mutate(area = st_area(.)) %>%
  st_drop_geometry() %>%
  group_by(across(all_of(grouper))) %>%
  summarize(across(starts_with("X"), ~weighted.mean(.x, as.numeric(area)))) %>% # averages across the cols with x in their name and gets weighted mean
  # TODO:: is it faster to pre-allocate these cols? IE just cut to
  # only those cols and then summarize everything?
  # Tried it and it crashed, but maybe moving out of dplyr syntax
  # entirely would speed it up, but be a pain. across() finding
  # the indices shouldn't be that slow, really, though. so the
  # only issue is if it can't parallelize or something
  # Same as above, but keeps all the other columns. Way slower
  # summarize(across(starts_with("X"), ~weighted.mean(.x, as.numeric(area))), 
  #           across(-c(starts_with("X"), Shape), first)) %>% # st_area returns a units object, which is good, but breaks weighted.mean
  ungroup()

# Need to put the geometry back on
avgPRindex <- avgPR[ ,grouper]
avgPR2j <- left_join(avgPRindex, avgPR2, by = grouper)

# Option 2: do it as vectors, glue it back on?
avgPR3 <- intPR %>%
  mutate(area = st_area(.))
# Nah, this is going to fall apart with lots of times
wm <- weighted.mean(avgPR3$X2020.01.01, as.numeric(avgPR3$area))
# etc. give up on this, I think



# Check the outputs: is the new way actually working? ---------------------

plot(avgPR['X2020.09.03'])
plot(avgPR2j['X2020.09.03'])

# Not sure how this is false, btu the below is true. They clearly DO match
all(st_drop_geometry(avgPR['X2020.09.03']) %in% st_drop_geometry(avgPR2j['X2020.09.03']))
# Are UIDs the same?
all(polysf$UID %in% avgPR$UID)
all(avgPR$UID %in% polysf$UID)
# do the values match?
all(avgPR2j$X2020.09.03 %in% avgPR$X2020.09.03)
all(avgPR$X2020.09.03 %in% avgPR2j$X2020.09.03)
# Does the ordering match? No. Doesn't really matter though
all(avgPR2j$X2020.09.03 == avgPR$X2020.09.03)

# Timings -----------------------------------------------------------------
# Time for both to return the summaries and the indexing

# Original
origfn <- function() {
  avgPR <- intPR %>%
    mutate(area = st_area(.)) %>%  
    group_by(across(all_of(grouper))) %>%
    summarize(across(starts_with("X"), ~weighted.mean(.x, as.numeric(area)))) %>% # averages across the cols with x in their name and gets weighted mean
    # TODO:: is it faster to pre-allocate these cols? IE just cut to
    # only those cols and then summarize everything?
    # Tried it and it crashed, but maybe moving out of dplyr syntax
    # entirely would speed it up, but be a pain. across() finding
    # the indices shouldn't be that slow, really, though. so the
    # only issue is if it can't parallelize or something
    # Same as above, but keeps all the other columns. Way slower
    # summarize(across(starts_with("X"), ~weighted.mean(.x, as.numeric(area))), 
    #           across(-c(starts_with("X"), Shape), first)) %>% # st_area returns a units object, which is good, but breaks weighted.mean
    ungroup()
  
  avgPRindex <- avgPR[ ,grouper]
}

opt2fn <- function() {
  avgPR2 <- intPR %>%
    mutate(area = st_area(.)) %>%
    st_drop_geometry() %>%
    group_by(across(all_of(grouper))) %>%
    summarize(across(starts_with("X"), ~weighted.mean(.x, as.numeric(area)))) %>% # averages across the cols with x in their name and gets weighted mean
    # TODO:: is it faster to pre-allocate these cols? IE just cut to
    # only those cols and then summarize everything?
    # Tried it and it crashed, but maybe moving out of dplyr syntax
    # entirely would speed it up, but be a pain. across() finding
    # the indices shouldn't be that slow, really, though. so the
    # only issue is if it can't parallelize or something
    # Same as above, but keeps all the other columns. Way slower
    # summarize(across(starts_with("X"), ~weighted.mean(.x, as.numeric(area))), 
    #           across(-c(starts_with("X"), Shape), first)) %>% # st_area returns a units object, which is good, but breaks weighted.mean
    ungroup()
  
  # Need to put the geometry back on
  sfindex <- polysf[ ,grouper]
  avgPR2j <- left_join(sfindex, avgPR2, by = grouper)
}

system.time(origfn()) # 3.7
system.time(opt2fn()) # 0.5




#





#











#