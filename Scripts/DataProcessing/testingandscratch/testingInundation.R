# Script to read in ANAE and process it into the standard format

# There's a ton of useful layers in there, for now let's focus on getting the main ANAE classifications in, and then maybe joining to one other thing to practice

# The main ANAE are in MDB_ANAE.gdb/Features/
  # Let's start with Wetlands_ANAE_20171015 and Watercourses_ANAE
print(getwd())
# source('directorySet.R')

# Data is in QAEL - MER/Model/Data/ANAE
# tempted to go with a Here, but should really have a library structure
# use here for now
# library(here)

# Argh. sort all this directory crap out later
# Need to have a shared data folder, without tracking in git and without
# having to setwd(). And that works on HPC and locally

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




# print(paste0('starting processInundate, time is ', Sys.time(), ', run is ', dataWhere))

# Get the basin in, because it's useful to see
# and the basin boundary, might be useful, especially for clipping rasters
basin <- read_sf(dsn = file.path(datDir, 'ANAE/MDB_ANAE_Aug2017/MDB_ANAE.gdb'), layer = 'MDB_Boundary') %>%
  st_cast("MULTIPOLYGON") %>% # cleans up an issue with multisurfaces
  st_make_valid() %>%
  select(LEVEL2NAME) # no need for other info

# Get catchments
ltimNoNorth <- read_sf(dsn = file.path(datDir, 'ANAE/MDB_ANAE_Aug2017/MDB_ANAE.gdb'), layer = 'LTIM_Valleys') %>%
  st_cast("MULTIPOLYGON") %>% # cleans up an issue with multisurfaces
  st_make_valid() %>%
  select(ValleyName, ValleyID, ValleyCode) %>%
  filter(ValleyName != 'Northern Unregulated')

# Read in the wetlands as well, at least for lachlan
load('C:/Users/Galen/Dropbox/Australia/MER/GalenGits/CC2/datOut/ANAEprocessed/LachlanANAE.rdata')


# # Set the CRS
# whichcrs <- 3577
# # TODO:: Check this is right on PC, I didn't need to set the CRS there for some reason
# # st_crs(LachlanANAE) <- 4283 # This is what's there when I look at st_crs, but it's not registering for some reason. 
# # LachlanANAE <- st_transform(LachlanANAE, whichcrs)
# ltimNoNorth <- st_transform(ltimNoNorth, whichcrs)
# basin <- st_transform(basin, whichcrs)


# Read in the data ----------------------------------------

# data location
inunDir <- file.path(datDir, 'Inundation', 'WaterDepth_TwoMonthly', 'geotiff')

# Get the file names
alltifs <- list.files(inunDir, pattern = '.tif$')
inunTifs <- file.path(inunDir, alltifs)

# Read in one for testing
testtif <- read_stars(inunTifs[1])
str(testtif)

# Can't transform here- too big
# testtif <- st_transform(testtif, crs = whichcrs)

plot(testtif, reset = FALSE)
plot(basin, col = NA, border = 'red', add = TRUE)

# Can I crop to actually get in?
  # I can't set the stars proxy transform without reading it in
# testtif <- st_transform(testtif, st_crs(ltimNoNorth))
st_crs(testtif)
# But, can I transform the ltim?
ltimNoNorth <- st_transform(ltimNoNorth, st_crs(testtif))

# Can I crop to bring in?
lachInun <- st_crop(testtif, filter(ltimNoNorth, ValleyName == 'Lachlan'), as_points = FALSE)
plot(lachInun, reset = FALSE)
plot(filter(ltimNoNorth, ValleyName == 'Lachlan'), col = NA, border = 'red', add = TRUE)
# Can I read that in?
  # Still no. reads in 12gb, but then throws an error claiming it's only trying
  # to read 3.6. and doesn't give the 12 back
# lachin <- st_as_stars(lachInun)
# WHY? do I need to? Or can I do what I need as a proxy? wouldn't that be better?
  # For testing, useful to see what we actually are dealin gwith.

# How about a bbox?
# Let's set up a bbox for subsampled plotting without taking 8 million years
# bb = st_bbox(c(xmin = 144.4, ymin = -33.8, xmax = 144.6, ymax = -33.6), crs = st_crs(testtif))

# # Where is the box?
# ggplot() + 
#   geom_sf(data = ltimNoNorth, aes(fill = ValleyName)) + 
#   geom_sf(data = st_as_sfc(bb)) +
#   # # geom_sf(data = roads) +
#   # coord_sf(xlim = c(144, 145),
#   #          ylim = c(-35.5, -36)) +
#   # scale_fill_brewer(palette = 'SeltimNoNortht1') +
#   theme_bw() + theme(legend.position = 'top')

# Can I crop to bring in?
# boxInun <- st_crop(testtif, st_as_sfc(bb), as_points = FALSE)
plot(select(filter(ltimNoNorth, ValleyName == 'Lachlan'), ValleyName), col = NA, border = 'red', reset = FALSE)
# plot(boxInun, add = TRUE)
# plot(st_as_sfc(bb), col = NA, border = 'red', add = TRUE)
plot(lachInun, add = TRUE)
# Find a box that has some water in it
bb = st_bbox(c(xmin = 147.2, ymin = -33.8, xmax = 147.6, ymax = -33.6), crs = st_crs(testtif))
# plot(st_as_sfc(bb), col = NA, border = 'red', add = TRUE)


boxInun <- st_crop(testtif, st_as_sfc(bb), as_points = FALSE)
plot(st_as_sfc(bb), col = NA, border = 'red', reset = FALSE)
plot(boxInun, add = TRUE)

# boxin <- st_as_stars(boxInun)
# plot(st_as_sfc(bb), col = NA, border = 'red', reset = FALSE)
# plot(boxin, add = TRUE)
# I think I need to sort out whether I have a bunch of 0 cells or what. Then
# sort out the aggregations, and hope I can do that with proxy object
  # What might we want to do? Area? Volume? Volume Photic? Mean Depth? %inundated (area would be better, I think)? TimeSinceWet? Time
  # since some % area wet or depth wet? The last two would be calculated from
  # the area or volume or depth
# Now that the data's in, hopefully I can do that
# boxin
# sum(is.na(boxin[[1]]))
# sum(!is.na(boxin[[1]]))
# So, this has 1,101,128 raster grids JUST within boxin
  # Can we figure that out from just the proxy? from-tos (+ 1 because inclusive) are
  # (33508-32024)*(34212-33470) = 1,101,128
# No wonder it's blowing up

# So yeah, massive NA's (so WHY are these so huge to read in?)
# str(boxin[[1]])

# change the name to depth from the filename
# boxin <- setNames(boxin, 'depth')
# boxin

# Instead of st_as_stars, can I st_as_sf to make polygons?
# st_as_sf(boxin) # works
# boxsf <- st_as_sf(boxInun)
# 70926 polygons (as we'd expect, turning each that isn't an NA into a polygon)
# Can I just rastPolyJoin this? Maybe not with the bbox, might need an actual sf dataframe. But can test that
  # If so, will need to update rastPolyJoin to take other functions
# What about st_as_sf on the rasters? Make each a polygon. Does that get rid of all the NAs?

# Try with whle lcahlan
lachInun <- setNames(lachInun, 'depth')
lachInun
# lin <- st_as_sf(lachInun)
# NOPE. has to put it in memory at that point
# The calculation of size is a bit funny because it's not square, but
# (42285-18994)*(38625-28357)=239,151,988 oof. No wonder

# addSoilMoistLach and addSoilTempLach demo has some interesting proxy--> chunking things going on.
# But I think it's reading time chunks, not space chunks. But same thing could
# work here

# It seems like there's two approaches that might work: 
  # chunk the Inundation raster over space
    # Ie read it in in little rectangles and process
  # loop over ANAEs, subset raster to them, then process.
    # Probably possible to parallelize this somehow or at least read in a few at a time

# If I chunk the raster in in rectangles, will probably immediately need to
# st_as_sf it, because that will yield a polygon sf dataset I can use to join
# AFTER reading the whole thing in. Otherwise, will end up wiht issues with the
# rectangle splitting wetlands.
  # I think, even though it will be annoyingly loopy, that reading in over each
  # wetland will likely be safest, most efficient, and easiest. NOW, can I test
  # it?

# First thing to do is to clip the wetlands to the box so I can see what I'm doing
# and deal with transforms. 
boxwet <- LachlanANAE %>%
  st_transform(st_crs(boxInun)) %>%
  st_crop(st_as_sfc(bb))

plot(st_as_sfc(bb), col = NA, border = 'red', reset = FALSE)
plot(boxwet['ANAE_DESC'], add = TRUE)
plot(boxInun, add = TRUE)

# OK, that's working

# NOW, what do I want to do???
# just try a rastpolyjoin
# inunraster <- rastPolyJoin(polysf = boxwet, rastst = boxInun,
#                            grouper = 'UID', maintainPolys = TRUE)
# Yikes. that takes FOREVER. probably should have looped instead of gone for it.
# First, let's shrink the box
bb = st_bbox(c(xmin = 147.4, ymin = -33.7, xmax = 147.6, ymax = -33.6), crs = st_crs(testtif))
# plot(st_as_sfc(bb), col = NA, border = 'red', add = TRUE)

boxInun <- st_crop(testtif, st_as_sfc(bb), as_points = FALSE)
plot(st_as_sfc(bb), col = NA, border = 'red', reset = FALSE)
plot(boxInun, add = TRUE)

boxwet <- LachlanANAE %>%
  st_transform(st_crs(boxInun)) %>%
  st_crop(st_as_sfc(bb))

plot(st_as_sfc(bb), col = NA, border = 'red', reset = FALSE)
plot(boxwet['ANAE_DESC'], add = TRUE)
plot(boxInun, add = TRUE)

# cut the set of sfs down to test
cutwet <- boxwet[15:20, ]
plot(st_as_sfc(bb), col = NA, border = 'red', reset = FALSE)
plot(cutwet['ANAE_DESC'], add = TRUE)
plot(boxInun, add = TRUE)

# Need to Crop the raster first. Otherwise this will read in the whole thing
rastcrop <- st_crop(testtif, cutwet, as_points = FALSE)
plot(cutwet['ANAE_DESC'], reset = FALSE)
plot(rastcrop, add = TRUE)
# Cool, so they do overhang a bit with as_points = FALSE

# STILL takes forever with only 5 polys. where is the issue?
# system.time(cutjoin <- rastPolyJoin(polysf = cutwet, rastst = rastcrop,
#                                                 grouper = 'UID', maintainPolys = TRUE))

# TRY WITH JUST ONE TO SORT OUT WHY SO SLOW
# cut the set of sfs down to test
cutwet <- boxwet[20, ]
plot(st_as_sfc(bb), col = NA, border = 'red', reset = FALSE)
plot(cutwet['ANAE_DESC'], add = TRUE)
# plot(boxInun, add = TRUE)

# Need to Crop the raster first. Otherwise this will read in the whole thing
rastcrop <- st_crop(testtif, cutwet, as_points = FALSE)
plot(cutwet['ANAE_DESC'], reset = FALSE)
plot(rastcrop, add = TRUE)

# Now, debug in rastPolyJoin
rastcrop <- setNames(rastcrop, 'depth')

# It's been cropped. Why do I need to st_as_sf?
  # Because the st_intersection doesn't work
# intPR <- st_intersection(cutwet, rastcrop)
system.time(rastSF <- st_as_sf(rastcrop, as_points = FALSE, merge = FALSE, na.rm = FALSE)) # Why do we na.rm = FALSE? needed for zeros?
# 0.6 seconds for single example
sum(is.na(rastSF$depth))
all(st_is_valid(rastSF)) # Check validity- often an issue with NA according to help

system.time(intPR <- st_intersection(cutwet, rastSF))
# 3 seconds
sum(is.na(intPR$depth)) 
plot(rastSF) # NA in the corners- crop makes a rectangle- that's super important
# So, if I make them zeros at this point, I'll add processsing, I think

sum(is.na(intPR$depth)) # where did the other 6642 go? - in the corners. see above
plot(intPR$depth)

# The NA need to be 0, NOT na here
intPR$depth[is.na(intPR$depth)] <- 0

# there's no time dimension, currently, so will need to edit the dplyr relative
# to rastPolyJoin fbit for now
system.time(avgPR <- intPR %>%
              group_by(UID) %>%
  mutate(area = st_area(.)) %>%
  summarise(depth = weighted.mean(depth, as.numeric(area))) %>%
    ungroup())
# How does that POSSIBLY take 40 seconds?

# Can I aggregate()? I stopped using it before because it screwd up dimensions
# (particularly time). But if I'm looping, maybe less of an issue

# We  can't aggregate.sf after the sf but before the intersection, or we lose the ability to
# accurately area-weight, because it doesn't capture the partial pixels
# correctly
  # So, exact = true uses coverage_fraction. But does it weight the mean? Unclear
# But we haven't turned the raster NA to 0 yet
remna <- function(x) {ifelse(is.na(x), 0, x)}
rast0 <- st_apply(rastcrop, MARGIN = 1:2, remna)
# Not working for some reason
rast0s <- st_as_stars(rast0)
rastcrops <- st_as_stars(rastcrop)  
# I think this will work if we read into memory?: rastcrop[[is.na(rastcrop)]] <- 0
system.time(aggPR <- aggregate(rast0s, st_geometry(cutwet), FUN = mean, as_points = FALSE, exact = TRUE))

# Why is this returning warnings and NA? sum(is.na(rast0s[[1]])) is 0
# It needed the st_geometry. Otherwise it tries to aggregate to all the coluns
plot(cutwet['ANAE_DESC'], reset = FALSE)
plot(rastcrops, add= TRUE)

# Does that work for the proxy, now that I've found the issue?
  # no
system.time(aggPRp <- aggregate(rast0, st_geometry(cutwet),
                                FUN = mean, as_points = FALSE, exact = TRUE))

# so, will unfortunately need to read the whole thing in, looks like if I go this way. 
# AND, this returns a stars thing, but what I really want is an sf. (though I
# then turn it into stars)


# So, is it actually easiest to use the dplyr method but unpacked?

system.time(wm <- intPR %>%
              mutate(area = st_area(.)))
meand <- weighted.mean(wm$depth, as.numeric(wm$area))

# That's reasonably fast, and works larely how rastPolyJoin expects (sort of)
# well, maybe not. rastPolyJoin returns a stars that I think might be JUST the
# value and time dimension, with a separate indexing list item. So maybe the aggregate() is exactly what we want?
aggPR
meand # Looks like the aggregation ISN'T doing the area-weighting correctly. So that must be why I went with the other method. 

# Is it the weighting of the mean that's so slow?
  # Nope
system.time(avgPR <- intPR %>%
              group_by(UID) %>%
              mutate(area = st_area(.)) %>%
              summarise(depth = mean(depth)) %>%
              ungroup())

# What if I drop the geometry and it's just a df?
system.time(avgPR <- intPR %>%
              group_by(UID) %>%
              mutate(area = st_area(.)) %>%
              st_drop_geometry() %>%
              summarise(depth = weighted.mean(depth, as.numeric(area))) %>%
              ungroup())
# Can I write this up as a question on git?
# SO, to do now: can I re-write rastPolyJoin to do the weighted mean OUTSIDE dplyr, and return the correct output?
# AND, make it work for previous stuff still
# AND, try to do the stuff above with time sheets (maybe do this FIRST, so the
# rastPolyJoin doesn't have to get re-written twice)
# 
# TODO: 
  # write a test script for rastPolyJoin so I can cehck that it is returning what it should
  # Change it to strip off geometry and maybe add back in
  # Did that. the issue was with crs. Going to be an issue here, because I can't transform until I read in. So will have to do the whole loopy thing and THEN transform.
  

# Do the intersection and looping -----------------------------------------


# SO: 
  # Read in some subset of inundation and ANAE timeslices likely a single or a couple ANAEs
  # Will need to do this on whatever crs the inundation is, because can't transform  
  # transform to 3577
  # Feed to rastPolyJoin
  # figure out how to do it programatically

# cut the set of sfs down to just a couple
cutwet <- boxwet[18:20, ]

# Read in a few stars proxies
# Read in a few- need to make a raster brick, I think
testtif <- read_stars(inunTifs[1:10])
testtif
ttif <- merge(testtif)
ttif

ttif <- merge(testtif) %>% 
  setNames('depth') %>%
  st_set_dimensions(3, values = 1:10) %>% # The 1:10 should actually be dates
  st_set_dimensions(names = c("x", "y", "time"))
ttif

# TODO: come back and deal with the time in the dimension. First, is this going to work to crop?
# NOW, can we crop those together?
rastcrop <- st_crop(ttif, cutwet, as_points = FALSE)

plot(st_as_sfc(bb), col = NA, border = 'red', reset = FALSE)
plot(cutwet['ANAE_DESC'], add = TRUE)
plot(rastcrop[,,,1], add = TRUE)

plot(rastcrop[,,,1])
plot(rastcrop[,,,2]) # Error?
plot(rastcrop[,,,8])

rcheck <- st_as_stars(rastcrop[,,,2])
sum(is.na(rcheck$depth))
sum(!is.na(rcheck$depth))
# So, we'll need to deal with it if it is entirely NA
# But, basically, that worked to do the clip

st_crs(rastcrop)
st_crs(cutwet)

# Let's see if I can walk through rastPolyJoin here
# change name so can copy/paste
polysf <- cutwet
rastst <- rastcrop
# read in raster
rastSF <- st_as_sf(rastst, as_points = FALSE, merge = FALSE, na.rm = FALSE)
st_crs(rastSF)
st_crs(polysf)

# Do the transform
whichcrs <- 3577
rastSF <- st_transform(rastSF, whichcrs)
polysf <- st_transform(polysf, whichcrs)
grouper <- 'UID'

# Replace the NAs in the raster with 0's
rastSF[is.na(rastSF)] <- 0

# TODO: When I actually build this, I should do the transform of the polysf all
# at once, even if I have to do it for the raster for each subunit
  # I did check again, and still can't transform pre-bringing-in
intPR <- st_intersection(polysf, rastSF)
# at this point those variables are depth.V#, not X.Date. so to check things are working, change the loop
avgPR <- intPR %>%
  mutate(area = st_area(.)) %>%
  st_drop_geometry() %>%
  group_by(across(all_of(grouper))) %>%
  summarize(across(starts_with("depth."), ~weighted.mean(.x, as.numeric(area)))) %>% # averages across the cols with x in their name and gets weighted mean
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

# Save the indexer column (is it possible to glue this back on as a dimension? Probably not, because it's parallel to shape)
avgPRindex <- polysf[ ,grouper] # leave the geometry ON this one, maybe?

# Put the geometry back on the summarised data
avgPR <- left_join(avgPRindex, avgPR, by = grouper)
# Look at it
plot(avgPR['depth.V1'])

# Turn back into a stars
avgPRStars <- avgPR %>% 
  select(-grouper) %>%
  st_as_stars() %>%
  merge()

# change the time dimension
st_dimensions(avgPRStars)[2] <- st_dimensions(rastst)[3]
# st_dimensions(avgPRtars)[2] # yup, though it's still called X1?
names(st_dimensions(avgPRStars))[2] <- names(st_dimensions(rastst))[3]
# and change the name of the attribute
names(avgPRStars) <- names(rastst)
avgPRStars


# TIME --------------------------------------------------------------------

# Start as above
# cut the set of sfs down to just a couple
cutwet <- boxwet[18:20, ]

# Read in a few stars proxies
# Read in a few- need to make a raster brick, I think
testtif <- read_stars(inunTifs[1:10])
testtif
ttif <- merge(testtif)
ttif

# Sort out the time-setting
  # Include the WaterDepth.tif to avoid corner cases with dates in the path somewhere else
tifdates <- inunTifs[1:10] %>% # Set of filenames
  str_extract_all("[1-2][0-9][0-9][0-9]_[0-9][0-9]_WaterDepth.tif") %>%
# now delete the safety
  str_remove("_WaterDepth.tif") %>%
  # add the first of the month on there
  str_c('_01') %>%
  # turn into dates
  lubridate::ymd() %>%
  as.POSIXct()
tifdates

# Now, can we put those on the data?
ttif <- merge(testtif) %>% 
  setNames('depth') %>%
  st_set_dimensions(3, values = tifdates) %>% # use as.POSIXct(tifdates) if need to match soilMstars. This is a bit funny though because soilMstars uses an offset and delta, while this has 'values'. I seem to remember values might cause an issue?
  st_set_dimensions(names = c("x", "y", "time"))
ttif

# NOW, can we crop those together?
rastcrop <- st_crop(ttif, cutwet, as_points = FALSE)

plot(st_as_sfc(bb), col = NA, border = 'red', reset = FALSE)
plot(cutwet['ANAE_DESC'], add = TRUE)
plot(rastcrop[,,,1], add = TRUE)

plot(rastcrop[,,,1])
plot(rastcrop[,,,2]) # Error?
plot(rastcrop[,,,8])

rcheck <- st_as_stars(rastcrop[,,,2])
sum(is.na(rcheck$depth))
sum(!is.na(rcheck$depth))
# So, we'll need to deal with it if it is entirely NA
# But, basically, that worked to do the clip

st_crs(rastcrop)
st_crs(cutwet)

# Let's see if I can walk through rastPolyJoin here
# change name so can copy/paste
polysf <- cutwet
rastst <- rastcrop
# read in raster
rastSF <- st_as_sf(rastst, as_points = FALSE, merge = FALSE, na.rm = FALSE)
st_crs(rastSF)
st_crs(polysf)

# Do the transform
whichcrs <- 3577
rastSF <- st_transform(rastSF, whichcrs)
polysf <- st_transform(polysf, whichcrs)
grouper <- 'UID'

# Replace the NAs in the raster with 0's
rastSF[is.na(rastSF)] <- 0

# TODO: When I actually build this, I should do the transform of the polysf all
# at once, even if I have to do it for the raster for each subunit
# I did check again, and still can't transform pre-bringing-in
intPR <- st_intersection(polysf, rastSF)
# plot(intPR['ANAE_DESC']) # Don't do this, it takes forever because it doesn't subsamply polygons
# at this point those variables are depth.V#, not X.Date. so to check things are working, change the loop
avgPR <- intPR %>%
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

# Save the indexer column (is it possible to glue this back on as a dimension? Probably not, because it's parallel to shape)
avgPRindex <- polysf[ ,grouper] # leave the geometry ON this one, maybe?

# Put the geometry back on the summarised data
avgPR <- left_join(avgPRindex, avgPR, by = grouper)
# Look at it
plot(avgPR['X1988.01.01.11.00.00'])

# Turn back into a stars
avgPRStars <- avgPR %>% 
  select(-grouper) %>%
  st_as_stars() %>%
  merge()

# change the time dimension
st_dimensions(avgPRStars)[2] <- st_dimensions(rastst)[3]
# st_dimensions(avgPRtars)[2] # yup, though it's still called X1?
names(st_dimensions(avgPRStars))[2] <- names(st_dimensions(rastst))[3]
# and change the name of the attribute
names(avgPRStars) <- names(rastst)
avgPRStars

plot(avgPRStars[,,1])



# feed to rastPolyJoin ----------------------------------------------------

# Slowly closing in on a sequence here, but easing up to it slowly
# Quite a bit of streamlining happening here vs above

# cut the set of sfs down to just a couple
cutwet <- boxwet[18:20, ]

# Sort out the time-setting
# Include the WaterDepth.tif to avoid corner cases with dates in the path
# somewhere else
tifdates <- inunTifs[1:10] %>% # Set of filenames
  str_extract_all("[1-2][0-9][0-9][0-9]_[0-9][0-9]_WaterDepth.tif") %>%
  # now delete the safety
  str_remove("_WaterDepth.tif") %>%
  # add the first of the month on there
  str_c('_01') %>%
  # turn into dates
  lubridate::ymd() %>%
  as.POSIXct() # soilMstars uses posix instead of the Date class that lubridate returns, so be consistent


# Read in a few stars proxies
testtif <- inunTifs[1:10] %>% # filenames
  read_stars() %>% # read in
  merge() %>% # make a brick 
  setNames('depth') %>% # name the attribute
  # Make dates
  # soilMstars uses an offset and delta, while this has 'values'.
  # I seem to remember values might cause an issue somewhere down the track, so
  # might need to revisit
  st_set_dimensions(3, values = tifdates) %>% 
  st_set_dimensions(names = c("x", "y", "time"))

# Crop the raster to the ANAE
  # TODO: does this crop separately, or one big rectangle?
  # if one big rect, we almost certainly don't want to do more than one ANAE when we loop
# at THIS point, the polygons need to be on whatever crs the raster is.
# I suppose we could THEN transform the whole thing and feed to rastPolyJoin,
# but not sure how much time that'd save. Will test, I suppose
rastcrop <- st_crop(testtif, cutwet, as_points = FALSE)

testDepth <- rastPolyJoin(polysf = cutwet, rastst = rastcrop, 
                          grouper = 'UID', maintainPolys = TRUE,
                          na.replace = 0, whichcrs = 3577)
# Replace NA with 0, transform to Albers needs to happen AFTER it's not a proxy
testDepth
plot(testDepth[[2]]) # The index
plot(testDepth[[1]]) # Depths x time


# Test cropping disconnected ANAE -----------------------------------------

# st-crop will always yield a rectangular raster around the bbox of the sf. But
# does it do it on a per-polygon or per-all-plygons?
# cut the set of sfs down to just a couple
cutwet <- boxwet[18:20, ]
rastcrop <- st_crop(testtif[,,,1], cutwet, as_points = FALSE)
plot(rastcrop) # but does that tell us anything?
rc0 <- st_as_stars(rastcrop)
rc0[is.na(rc0)] <- 0
plot(rc0, reset = FALSE) # OK, can see what's happening there
plot(cutwet, add = TRUE)

cutwet <- boxwet[20:30, ]
plot(cutwet['ANAE_DESC'])
rastcrop <- st_crop(testtif[,,,1], cutwet, as_points = FALSE)
plot(rastcrop) # but does that tell us anything?
rc0 <- st_as_stars(rastcrop)
rc0[is.na(rc0)] <- 0
plot(rc0, reset = FALSE) # OK, can see what's happening there
plot(cutwet['ANAE_DESC'], add = TRUE)

# so yeah, grabs th bb around ALL included shapefiles


# Test looping over ANAE (and time?) --------------------------------------

# I Think this will run if I run everything to line 75 and then skip to here

# Sort out the time-setting for ALL the tifdates
# Include the WaterDepth.tif to avoid corner cases with dates in the path
# somewhere else
tifdates <- inunTifs %>% # Set of filenames
  str_extract_all("[1-2][0-9][0-9][0-9]_[0-9][0-9]_WaterDepth.tif") %>%
  # now delete the safety
  str_remove("_WaterDepth.tif") %>%
  # add the first of the month on there
  str_c('_01') %>%
  # turn into dates
  lubridate::ymd() %>%
  as.POSIXct() # soilMstars uses posix instead of the Date class that lubridate returns, so be consistent


# Read in a few stars proxies
  # Is there any harm here in reading them all in as PROXIES?
test10 <- inunTifs[1:10] %>% # filenames
  read_stars() %>% # read in
  merge() %>% # make a brick 
  setNames('depth') %>% # name the attribute
  # Make dates
  # soilMstars uses an offset and delta, while this has 'values'.
  # I seem to remember values might cause an issue somewhere down the track, so
  # might need to revisit
  st_set_dimensions(3, values = tifdates[1:10]) %>% 
  st_set_dimensions(names = c("x", "y", "time"))

# For looping at first, use a subset. Come back to test time looping

# still just set up for a subset of ANAE where I know where they are
# cut the set of sfs down to just a couple
bb = st_bbox(c(xmin = 147.4, ymin = -33.7, xmax = 147.6, ymax = -33.6), 
             crs = st_crs(test10))
# plot(st_as_sfc(bb), col = NA, border = 'red', add = TRUE)

# Test it's working
boxInun <- st_crop(test10, st_as_sfc(bb), as_points = FALSE)

# why is this taking so long?
boxwet <- LachlanANAE %>%
  st_transform(st_crs(boxInun)) %>%
  st_crop(st_as_sfc(bb))

plot(st_as_sfc(bb), col = NA, border = 'red', reset = FALSE)
plot(boxInun[,,,1], add = TRUE)
plot(boxwet['ANAE_DESC'], add = TRUE)


# a subset for testing
cutwet <- boxwet[15:20, ]


thiscrop <- st_crop(test10, cutwet[1,], as_points = FALSE)

thisdepth <- rastPolyJoin(polysf = cutwet[1,], rastst = thiscrop, 
                          grouper = 'UID', maintainPolys = TRUE,
                          na.replace = 0, whichcrs = 3577)

depthAns <- thisdepth[[1]]
depthIndex <- thisdepth[[2]]

# Crop the raster to the ANAE
for (s in 2:nrow(cutwet)) {
  thiscrop <- st_crop(test10, cutwet[s,], as_points = FALSE)
  
  thisdepth <- rastPolyJoin(polysf = cutwet[s,], rastst = thiscrop, 
                            grouper = 'UID', maintainPolys = TRUE,
                            na.replace = 0, whichcrs = 3577)
  depthAns <-  c(depthAns, thisdepth[[1]], along = 1)
  depthIndex <- bind_rows(depthIndex, thisdepth[[2]])
}

depthAns
plot(depthAns)
# Thought there would be more ANAEs?
plot(cutwet['ANAE_DESC']) # Guess not
# plot(depthAns[,,1:5])

# # Scratch for above pasting
# # esting pasting toegher
# s <- 1
# thatDepth <- thisDepth[[1]]
# theDepth <- thatDepth
# s <- 2
# newDepth <- thisDepth[[1]]
# dim(theDepth)
# plot(theDepth[,,])
# 
# theDepth[2,] <- newDepth
# bothDepth <- c(theDepth, newDepth, along = 1)
# plot(bothDepth[,1,1:5])
# plot(bothDepth[,2,1:5])
# plot(bothDepth[,,1:5])
# ### The c() seems to be working, but the plotting isn't. NEed to come back to this.
# 
# 
# # Replace NA with 0, transform to Albers needs to happen AFTER it's not a proxy
# testDepth
# plot(testDepth[[2]]) # The index
# plot(testDepth[[1]]) # Depths x time



# TIMING ------------------------------------------------------------------

# Same as above, but setting up a function to adjust the number of time slices and ANAEs
# Let's get the ANAE polys set out here

# Don't need to run this preamble stuff unless haven't run it above
# still just set up for a subset of ANAE where I know where they are
# cut the set of sfs down to just a couple
bb = st_bbox(c(xmin = 147.4, ymin = -33.7, xmax = 147.6, ymax = -33.6), 
             crs = st_crs(tifTimes))
# plot(st_as_sfc(bb), col = NA, border = 'red', add = TRUE)

# All I need is the crs from boxInun, so don't worry about the fact that it's
# made elsewhere for now
# why is this taking so long?
boxwet <- LachlanANAE %>%
  st_transform(st_crs(boxInun)) %>%
  st_crop(st_as_sfc(bb))


timeinun <- function(ntimes, nanaes) {
  # Sort out the time-setting for ALL the tifdates
  # Include the WaterDepth.tif to avoid corner cases with dates in the path
  # somewhere else
  tifdates <- inunTifs %>% # Set of filenames
    str_extract_all("[1-2][0-9][0-9][0-9]_[0-9][0-9]_WaterDepth.tif") %>%
    # now delete the safety
    str_remove("_WaterDepth.tif") %>%
    # add the first of the month on there
    str_c('_01') %>%
    # turn into dates
    lubridate::ymd() %>%
    as.POSIXct() # soilMstars uses posix instead of the Date class that lubridate returns, so be consistent
  
  
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
    
    thisdepth <- rastPolyJoin(polysf = cutwet[s,], rastst = thiscrop, 
                              grouper = 'UID', maintainPolys = TRUE,
                              na.replace = 0, whichcrs = 3577)
    depthAns <-  c(depthAns, thisdepth[[1]], along = 1)
    depthIndex <- bind_rows(depthIndex, thisdepth[[2]])
  }
  return(list(avgPRStars = depthAns, avgPRindex = depthIndex))
}

benchInSmall <- microbenchmark("t2a2" = { b <- timeinun(ntimes = 2, nanaes = 2) },
                          "t5a5" = {b <- timeinun(ntimes = 5, nanaes = 5)},
                          "t10a10" = { b <- timeinun(ntimes = 10, nanaes = 10)},
                          times = 1)
benchInSmall
benchIn100 <- microbenchmark("t10a10" = { b <- timeinun(ntimes = 10, nanaes = 10)},
                               "t5a20" = { b <- timeinun(ntimes = 5, nanaes = 20) },
                               "t20a5" = { b <- timeinun(ntimes = 20, nanaes = 5) },
                               times = 1)
benchIn100
# So, more ANAEs is a killer, but more times is almost free?
# Check that
benchInT <- microbenchmark("t10a10" = { b <- timeinun(ntimes = 10, nanaes = 10)},
                             "t50a10" = { b <- timeinun(ntimes = 50, nanaes = 10) },
                             "t100a10" = { b <- timeinun(ntimes = 100, nanaes = 10) },
                             times = 1)
benchInT

# Argh. why is that failing to read?

# Going on a hunt to figure out why it's breaking
timeinunTT <- function(tstart, tend, nanaes) {
  # Sort out the time-setting for ALL the tifdates
  # Include the WaterDepth.tif to avoid corner cases with dates in the path
  # somewhere else
  tifdates <- inunTifs %>% # Set of filenames
    str_extract_all("[1-2][0-9][0-9][0-9]_[0-9][0-9]_WaterDepth.tif") %>%
    # now delete the safety
    str_remove("_WaterDepth.tif") %>%
    # add the first of the month on there
    str_c('_01') %>%
    # turn into dates
    lubridate::ymd() %>%
    as.POSIXct() # soilMstars uses posix instead of the Date class that lubridate returns, so be consistent
  
  
  # Read in a few stars proxies
  # Is there any harm here in reading them all in as PROXIES?
  tifTimes <- inunTifs[tstart:tend] %>% # filenames
    read_stars() %>% # read in
    merge() %>% # make a brick 
    setNames('depth') %>% # name the attribute
    # Make dates
    # soilMstars uses an offset and delta, while this has 'values'.
    # I seem to remember values might cause an issue somewhere down the track, so
    # might need to revisit
    st_set_dimensions(3, values = tifdates[tstart:tend]) %>% 
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
    
    thisdepth <- rastPolyJoin(polysf = cutwet[s,], rastst = thiscrop, 
                              grouper = 'UID', maintainPolys = TRUE,
                              na.replace = 0, whichcrs = 3577)
    depthAns <-  c(depthAns, thisdepth[[1]], along = 1)
    depthIndex <- bind_rows(depthIndex, thisdepth[[2]])
  }
  return(list(avgPRStars = depthAns, avgPRindex = depthIndex))
}

section <- 0
for (t in seq(from = 11, to = 101, by = 10)) {
  print(paste0('starting ', as.character(t)))
  every10 <- timeinunTT(tstart = (t-10), tend = t, nanaes = 2)
  section <- c(section, t)
}
section
# Interesting. Gets through the 91 section
section2 <- 79
for (t in 80:101) {
  print(paste0('starting ', as.character(t)))
  every2 <- timeinunTT(tstart = (t-1), tend = t, nanaes = 2)
  section2 <- c(section2, t)
}
section2

# Pull them out of the loop
every10 <- timeinunTT(tstart = 81, tend = 91, nanaes = 2)

test86 <- timeinunTT(tstart = 85, tend = 86, nanaes = 2)

# I just want to figure out WHY it's crashing so I can hopefully do a check and skip
tifCheck <- inunTifs[85:86] %>% # filenames
  read_stars() %>% # read in
  merge() %>% # make a brick 
  setNames('depth') %>% # name the attribute
  # Make dates
  # soilMstars uses an offset and delta, while this has 'values'.
  # I seem to remember values might cause an issue somewhere down the track, so
  # might need to revisit
  st_set_dimensions(3, values = tifdates[85:86]) %>% 
  st_set_dimensions(names = c("x", "y", "time"))

thiscrop <- st_crop(tifTimes, cutwet[1,], as_points = FALSE)

testsf <- st_as_sf(thiscrop, as_points = FALSE, merge = FALSE, na.rm = FALSE)
# I think that error code implies it's number 86
   # Error 1: C:\Users\Galen\Deakin University\QAEL -
   # MER\Model\dataBase\Inundation\WaterDepth_TwoMonthly\geotiff\WOFSwater_MaxTwoMonthly_4326_MDB_2002_03_WaterDepth.tif,
   # band 1: IReadBlock failed at X offset 0, Y offset 33660:
   # TIFFReadEncodedStrip() failed.
check86 <- read_stars(inunTifs[86])
check86
crop86 <- st_crop(check86, cutwet[1,], as_points = FALSE)
sf86 <- st_as_sf(crop86, as_points = FALSE, merge = FALSE, na.rm = FALSE)

# does anything else look different
check85 <- read_stars(inunTifs[85])
check85
check86

crop85 <- st_crop(check85, cutwet[1,], as_points = FALSE)
crop85
crop86
# Nothing obvious. SO if it only becomes an issue on read-in, and ther's no way to know it's corrupted, what do we do?

# Can I set up a tryCatch?

result = tryCatch({
  sf86 <- st_as_sf(crop86, as_points = FALSE, merge = FALSE, na.rm = FALSE)
}, warning = function(w) {
  print(paste("sf warning:  ", w))
  sf86 <- NA
  return(sf86)
}, error = function(err) {
  print(paste("sf_error:  ",err))
  sf86 <- NA
  return(sf86)
}, finally = {
  print('finally')
})
result
sf86

# Maybe Hadley's idea is better 
# http://adv-r.had.co.nz/Exceptions-Debugging.html
sf86 <- NA
try(sf86 <- st_as_sf(crop85, as_points = FALSE, merge = FALSE, na.rm = FALSE))
sf86

# I guess. But I do like getting the messages
# So maybe a combo
sf86 <- NA
result = tryCatch({
  sf86 <- st_as_sf(crop86, as_points = FALSE, merge = FALSE, na.rm = FALSE)
}, warning = function(w) {
  print(paste("sf warning:  ", w))
  sf86 <- NA
  return(sf86)
}, error = function(err) {
  print(paste("sf_error:  ",err))
  sf86 <- NA
  return(sf86)
}, finally = {
  # print('finally')
})
result
sf86


# Argh. but this will can the whole thing, huh? not just the specific corrupted
# TIF
check80s <- read_stars(inunTifs[80:90])
check80s
crop80s <- st_crop(check80s, cutwet[1,], as_points = FALSE)
sf80s <- NA
result = tryCatch({
  sf80s <- st_as_sf(crop80s, as_points = FALSE, merge = FALSE, na.rm = FALSE)
}, warning = function(w) {
  print(paste("sf warning:  ", w))
  sf86 <- NA
  return(sf86)
}, error = function(err) {
  print(paste("sf_error:  ",err))
  sf86 <- NA
  return(sf86)
}, finally = {
  # print('finally')
})
result
sf86

# Damn. So I can't auto-skip
# Instead, I'm going to need to write a looper to check for problems (and hope they apply to the file, and not the ANAE)

# BUT, maybe I can use a trycatch to do that? Or hadley's idea of changing if it works
passer <- vector(mode = 'logical', length(inunTifs))
for (tif in 1:length(inunTifs)) {
  checkTif <- read_stars(inunTifs[tif])
  cropTif <- st_crop(checkTif, cutwet[1,], as_points = FALSE)
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
sum(passer)
length(inunTifs)
# ARGH. It's just the one


# START OVER WITH TIMING WIHTOUT CORRUPTED FILE ---------------------------
# use passer to delete it
inunTifs <- inunTifs[passer]

# Same as above, but setting up a function to adjust the number of time slices and ANAEs
# Let's get the ANAE polys set out here

# Don't need to run this preamble stuff unless haven't run it above
# still just set up for a subset of ANAE where I know where they are
# cut the set of sfs down to just a couple
bb = st_bbox(c(xmin = 147.4, ymin = -33.7, xmax = 147.6, ymax = -33.6), 
             crs = st_crs(tifTimes))
# plot(st_as_sfc(bb), col = NA, border = 'red', add = TRUE)

# All I need is the crs from boxInun, so don't worry about the fact that it's
# made elsewhere for now
# why is this taking so long?
boxwet <- LachlanANAE %>%
  st_transform(st_crs(boxInun)) %>%
  st_crop(st_as_sfc(bb))


timeinun <- function(ntimes, nanaes) {
  # Sort out the time-setting for ALL the tifdates
  # Include the WaterDepth.tif to avoid corner cases with dates in the path
  # somewhere else
  tifdates <- inunTifs %>% # Set of filenames
    str_extract_all("[1-2][0-9][0-9][0-9]_[0-9][0-9]_WaterDepth.tif") %>%
    # now delete the safety
    str_remove("_WaterDepth.tif") %>%
    # add the first of the month on there
    str_c('_01') %>%
    # turn into dates
    lubridate::ymd() %>%
    as.POSIXct() # soilMstars uses posix instead of the Date class that lubridate returns, so be consistent
  
  
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
    
    thisdepth <- rastPolyJoin(polysf = cutwet[s,], rastst = thiscrop, 
                              grouper = 'UID', maintainPolys = TRUE,
                              na.replace = 0, whichcrs = 3577)
    depthAns <-  c(depthAns, thisdepth[[1]], along = 1)
    depthIndex <- bind_rows(depthIndex, thisdepth[[2]])
  }
  return(list(avgPRStars = depthAns, avgPRindex = depthIndex))
}

benchInSmall <- microbenchmark("t2a2" = { b <- timeinun(ntimes = 2, nanaes = 2) },
                               "t5a5" = {b <- timeinun(ntimes = 5, nanaes = 5)},
                               "t10a10" = { b <- timeinun(ntimes = 10, nanaes = 10)},
                               times = 1)
benchInSmall
benchIn100 <- microbenchmark("t10a10" = { b <- timeinun(ntimes = 10, nanaes = 10)},
                             "t5a20" = { b <- timeinun(ntimes = 5, nanaes = 20) },
                             "t20a5" = { b <- timeinun(ntimes = 20, nanaes = 5) },
                             times = 1)
benchIn100
# So, more ANAEs is a killer, but more times is almost free?
# Check that
benchInT <- microbenchmark("t10a10" = { b <- timeinun(ntimes = 10, nanaes = 10)},
                           "t50a10" = { b <- timeinun(ntimes = 50, nanaes = 10) },
                           "t100a10" = { b <- timeinun(ntimes = 100, nanaes = 10) },
                           "tAlla10" = { b <- timeinun(ntimes = length(inunTifs), nanaes = 10) },
                           times = 1)
benchInT

# So, what am I getting at here? 
  # we have to do all the anaes, so one question is just how long that's going to take? How slow does this go as we add ANAEs
  # The other question is whether there's an advantage to not doing all the times.
  # It seems the answer there is no, although I'm not sure there's any reason TO
  # do them, if nothing else goes back past 2014, why do we need inundation back
  # to 1988?

# My inclination is to only take time back to ~2000 or 2010 or something, even though
# the penalty is lo?
sum(tifdates >= lubridate::ymd('20000101')) # Approx 16-18 seconds (with 10 ANAE)
sum(tifdates >= lubridate::ymd('20100101')) # Approx 8 seconds
# And all is approx 27. So about 1/3 the time if we only go back to 2010


# How about how long this is going to take to get to 20k ANAES? That might make the decision
benchInA <- microbenchmark("t50a5" = { b <- timeinun(ntimes = 50, nanaes = 5)},
                           "t50a10" = { b <- timeinun(ntimes = 50, nanaes = 10) },
                           "t50a20" = { b <- timeinun(ntimes = 50, nanaes = 20) },
                           "t50a50" = { b <- timeinun(ntimes = 50, nanaes = 50) },
                           "t50a100" = { b <- timeinun(ntimes = 50, nanaes = 100) },
                           times = 1)
benchInA
# So, roughly linear after about 50.
# if 100 takes 137 seconds, 
(nrow(LachlanANAE)/100)*137 # Seconds
# seconds
((nrow(LachlanANAE)/100)*137)/60/60 # hours.
# 8 hours is actually not that bad. I guess I'll try to parallelize, but chunking into catchments at running for 8 hours might be fine
# NEXT STEP: CAN I GET SPEEDUP FROM PARALLELING THE LOOP OVER ANAES?
  # Likely copy the approach in EWKR. looks like surprisingly little movement in R in the last few years


# Parallel loop -----------------------------------------------------------
   # Just get the loop working first, then put in function
# These are the function inputs so I can comment out the function bit
ntimes <- 10
nanaes <- 10
# timeinun <- function(ntimes, nanaes) {
  # Sort out the time-setting for ALL the tifdates
  # Include the WaterDepth.tif to avoid corner cases with dates in the path
  # somewhere else
  tifdates <- inunTifs %>% # Set of filenames
    str_extract_all("[1-2][0-9][0-9][0-9]_[0-9][0-9]_WaterDepth.tif") %>%
    # now delete the safety
    str_remove("_WaterDepth.tif") %>%
    # add the first of the month on there
    str_c('_01') %>%
    # turn into dates
    lubridate::ymd() %>%
    as.POSIXct() # soilMstars uses posix instead of the Date class that lubridate returns, so be consistent
  
  
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
    
    thisdepth <- rastPolyJoin(polysf = cutwet[s,], rastst = thiscrop, 
                              grouper = 'UID', maintainPolys = TRUE,
                              na.replace = 0, whichcrs = 3577)
    depthAns <-  c(depthAns, thisdepth[[1]], along = 1)
    depthIndex <- bind_rows(depthIndex, thisdepth[[2]])
  }
  
  
  # OK, let's re-write that as a foreach loop
  library(foreach)
  
  # Since we want to combine the two list bits differently, just return the list and let foreach make a list of lists for now
  dpList <- foreach(s = 1:nrow(cutwet)) %do% {
    thiscrop <- st_crop(tifTimes, cutwet[s,], as_points = FALSE)
    thisdepth <- rastPolyJoin(polysf = cutwet[s,], rastst = thiscrop, 
                              grouper = 'UID', maintainPolys = TRUE,
                              na.replace = 0, whichcrs = 3577)
  } # end foreach
  
  # Then, unpack the lists
depthAnsP <- foreach(l = 1:length(dpList),
                    .combine=function(...) c(..., along = 1), # Pass dimension argument to c.stars
                    .multicombine=TRUE) %do% {
  dpList[[l]][[1]]
}

depthIndexP <- foreach(l = 1:length(dpList),
                       .combine=bind_rows,
                       .multicombine=TRUE) %do% {
                         dpList[[l]][[2]]
                       }  
#   return(list(avgPRStars = depthAns, avgPRindex = depthIndex))
# }


# doParallel --------------------------------------------------------------
# library(doParallel)
# # use ALL the cores. Most people use all-1
# allCores <- detectCores()
# cl <- makeCluster(allCores)
# registerDoParallel(cl)
# 
# # ARGH. Gonna have to pass packages and functions. PITA. maybe doFuture just works?
# # Since we want to combine the two list bits differently, just return the list and let foreach make a list of lists for now
# dpList <- foreach(s = 1:nrow(cutwet)) %dopar% {
#   thiscrop <- st_crop(tifTimes, cutwet[s,], as_points = FALSE)
#   thisdepth <- rastPolyJoin(polysf = cutwet[s,], rastst = thiscrop, 
#                             grouper = 'UID', maintainPolys = TRUE,
#                             na.replace = 0, whichcrs = 3577)
# } # end foreach
# 
# # Then, unpack the lists
# depthAnsP <- foreach(l = 1:length(dpList),
#                      .combine=function(...) c(..., along = 1), # Pass dimension argument to c.stars
#                      .multicombine=TRUE) %dopar% {
#                        dpList[[l]][[1]]
#                      }
# 
# depthIndexP <- foreach(l = 1:length(dpList),
#                        .combine=bind_rows,
#                        .multicombine=TRUE) %dopar% {
#                          dpList[[l]][[2]]
#                        }  
# 
# # Turn off the cluster
# stopCluster(cl)


# doFuture ----------------------------------------------------------------

library(doFuture)
# use ALL the cores. Most people use all-1
registerDoFuture()
plan(multisession) # On windows
# plan(multicore) # Forked, works on Unix
# plan(future.batchtools::...) # Maybe needed for cluster? Or can we just use multicore?
cl

# Since we want to combine the two list bits differently, just return the list and let foreach make a list of lists for now
dpList <- foreach(s = 1:nrow(cutwet)) %dopar% {
  thiscrop <- st_crop(tifTimes, cutwet[s,], as_points = FALSE)
  thisdepth <- rastPolyJoin(polysf = cutwet[s,], rastst = thiscrop, 
                            grouper = 'UID', maintainPolys = TRUE,
                            na.replace = 0, whichcrs = 3577)
} # end foreach

# Then, unpack the lists
depthAnsP <- foreach(l = 1:length(dpList),
                     .combine=function(...) c(..., along = 1), # Pass dimension argument to c.stars
                     .multicombine=TRUE) %do% {
                       dpList[[l]][[1]]
                     }

depthIndexP <- foreach(l = 1:length(dpList),
                       .combine=bind_rows,
                       .multicombine=TRUE) %do% {
                         dpList[[l]][[2]]
                       }  

# Turn off the cluster
# stopCluster(cl) # Unclear if we do this with doFuture?

# There's another way of using futures that I can't wrap my head around, where it just wraps a function with future()
  # I think for now, this is the way to go. He implies the future foreach is how
  # to do for loops (or future_lapply or furrr)


# futures function to benchmark -------------------------------------------

timeinunP <- function(ntimes, nanaes) {
  # Sort out the time-setting for ALL the tifdates
  # Include the WaterDepth.tif to avoid corner cases with dates in the path
  # somewhere else
  tifdates <- inunTifs %>% # Set of filenames
    str_extract_all("[1-2][0-9][0-9][0-9]_[0-9][0-9]_WaterDepth.tif") %>%
    # now delete the safety
    str_remove("_WaterDepth.tif") %>%
    # add the first of the month on there
    str_c('_01') %>%
    # turn into dates
    lubridate::ymd() %>%
    as.POSIXct() # soilMstars uses posix instead of the Date class that lubridate returns, so be consistent
  
  
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
  
  # Since we want to combine the two list bits differently, just return the list and let foreach make a list of lists for now
  dpList <- foreach(s = 1:nrow(cutwet)) %dopar% {
    thiscrop <- st_crop(tifTimes, cutwet[s,], as_points = FALSE)
    thisdepth <- rastPolyJoin(polysf = cutwet[s,], rastst = thiscrop, 
                              grouper = 'UID', maintainPolys = TRUE,
                              na.replace = 0, whichcrs = 3577)
  } # end foreach
  
  # Then, unpack the lists
  depthAns <- foreach(l = 1:length(dpList),
                       .combine=function(...) c(..., along = 1), # Pass dimension argument to c.stars
                       .multicombine=TRUE) %do% {
                         dpList[[l]][[1]]
                       }
  
  depthIndex <- foreach(l = 1:length(dpList),
                         .combine=bind_rows,
                         .multicombine=TRUE) %do% {
                           dpList[[l]][[2]]
                         }  
  
  return(list(avgPRStars = depthAns, avgPRindex = depthIndex))
}

# Test
timeinunP(10,10)

# bench
# How about how long this is going to take to get to 20k ANAES? That might make the decision
benchPS <- microbenchmark("t10a10" = { b <- timeinun(ntimes = 10, nanaes = 10)},
                           "t100a10" = { b <- timeinun(ntimes = 100, nanaes = 10) },
                           "t100a50" = { b <- timeinun(ntimes = 100, nanaes = 50) },
                           "t100a100" = { b <- timeinun(ntimes = 100, nanaes = 100) },
                          "p_t10a10" = { b <- timeinunP(ntimes = 10, nanaes = 10)},
                          "p_t100a10" = { b <- timeinunP(ntimes = 100, nanaes = 10) },
                          "p_t100a50" = { b <- timeinunP(ntimes = 100, nanaes = 50) },
                          "p_t100a100" = { b <- timeinunP(ntimes = 100, nanaes = 100) },
                           times = 1)
benchPS

# Will need to test agian on cluster, using the different plans
# I *Think* this is set up so that I can choose the plan in the outermost script
# (ie whereever I am doing the other unix/windows swithing)

# Still a question about whether to have a parallel (or, more accurately, a loop
# and maybe parallel if a plan is set) switch in rastPolyJoin, or to do it in
# the script.

# How would I do it in rastPolyJoin?
  # send the raster as a proxy and the full sf
  # I think don't even loop over times, so can skip all the tifTimes stuff in the function
  # Then just loop over the sfs
  # I actually think this would be confusing in rastPolyjoin, so if we did it,
  # we should probably do it as a wrapper that is almost identical to the set of
  # loops in that parallel function above

# I think it probably makes the most sense then to just do the loop in th
# escript rather than a wrapper funciton, at least for now. It's a very short
# couple of loops, and avoids needing pass-through arguments, etc



# -------------------------------------------------------------------------






# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


# Below from processANAE for ref ------------------------------------------




# The st_cast and st_make_valid clean things up so the intersects work. 
# https://www.r-spatial.org/r/2017/03/19/invalid.html says we should make valid,
# and then cast, but that didn't work for me
# Takes a while. Didn't time, but > 10 mins
wetlands <- read_sf(dsn = file.path(datDir, 
                                    'ANAE/ANAE_Wetlands_v3_24mar2021/Wetlands_ANAE_v3_24mar2021/Wetlands_ANAE_v3_24mar2021.shp')) %>%
  st_cast("MULTIPOLYGON") %>% # cleans up an issue with multisurfaces
  st_make_valid()
 
# # And the interim NSW data
# wetlandsNSW <- read_sf(dsn = file.path(datDir, 'ANAE/MDB_ANAE_Aug2017/MDB_ANAE.gdb'), layer = 'Interim_Western_NSW_Floodplain_ANAE') %>%
#   st_cast("MULTIPOLYGON") %>% # cleans up an issue with multisurfaces
#   st_make_valid()
#   
# The rest of this still needs to come from the V2 for now, since V3 is just the shapefiles
# Get koppen climate region as a test of the joining of data
kopSub <- read_sf(dsn = file.path(datDir, 'ANAE/MDB_ANAE_Aug2017/MDB_ANAE.gdb'), layer = 'BoM_Koppen_subregions') %>%
  st_cast("MULTIPOLYGON") %>% # cleans up an issue with multisurfaces
  st_make_valid()

# And get the LTIM_Valleys to use to subset for toy models at scale, but not enormous scale
LTIM_Valleys <- read_sf(dsn = file.path(datDir, 'ANAE/MDB_ANAE_Aug2017/MDB_ANAE.gdb'), layer = 'LTIM_Valleys') %>%
  st_cast("MULTIPOLYGON") %>% # cleans up an issue with multisurfaces
  st_make_valid()

# and the basin boundary, might be useful, especially for clipping rasters
basin <- read_sf(dsn = file.path(datDir, 'ANAE/MDB_ANAE_Aug2017/MDB_ANAE.gdb'), layer = 'MDB_Boundary') %>%
  st_cast("MULTIPOLYGON") %>% # cleans up an issue with multisurfaces
  st_make_valid() %>%
  select(LEVEL2NAME) # no need for other info

# Bring watercourses in in another script, I think
  

# Simplify to carry less data around --------------------------------------

# Wetlands
# Suppose I'll keep SYSID for now, but really we want to shift over to the
# geohash UID (see Shane's discussion in the metadata)
# 'WaterRegim' seems to be a typo- was water regime last time
# There are others here we might want to bring along, but for now, not
wetCut <- wetlands %>% 
  select(UID, SYSID, ANAE_DESC, ANAE_CODE, WaterType, WaterRegim, SystemType, Confidence)

# # NSW wetlands
# nswCut <- wetlandsNSW %>% 
#   select(SYSID, WaterType, WaterRegime, ANAE_DESC, ANAE_CODE)

# Kind of wanted to include landform in the above, but it's called different
# things (MeanLandform vs Landform, easy enough to fix), but also has different
# names (e.g. Lowland vs Lowlands). Not worth cleaning data for a toy model

# Koppen subregions
kopCut <- kopSub %>%
  select(ANAEField, CODE, Zone)

# LTIM areas
ltimCut <- LTIM_Valleys %>%
  select(ValleyName, ValleyID, ValleyCode) # Three different ways to reference, basically

# Sort out overlaying and joining raster and vector --------------------------------------



# combine the regular ANAE and NSW.  --------------------------------------

# # Some error checking. Something has broken
# maybeOverlapCheck <- st_intersects(ltimNoNorth)
# notsure <- st_intersection(ltimNoNorth)
# notsure <- st_difference(ltimNoNorth)

# Northern unregulated is a compound of others and so causes issues 
ltimNoNorth <- ltimCut %>% filter(ValleyName != 'Northern Unregulated')

# # CUT OUT LATER:
# # ARE DUPLICATE SYSIDS coming in from this join?
# length(unique(wetCut$SYSID))
# length(unique(nswCut$SYSID))
# sum(duplicated(wetCut$SYSID)) 
# sum(duplicated(nswCut$SYSID))
# 
# wtsys <- wetCut$SYSID
# nswsys <- nswCut$SYSID
# 
# sum(nswsys %in% wtsys)
# inboth <- nswsys[nswsys %in% wtsys]
# 
# # NEARLY ALL OF THEM???
# # I can find nothing particularly useful in the metadata addressing why these are massively overlapped
#   # They seem to just be entirely different
# dev.off()
# plot(nswCut[nswCut$SYSID %in% inboth[1:1000], 'ANAE_DESC'])
# plot(wetCut[wetCut$SYSID %in% inboth[1:1000], 'ANAE_DESC'])

# # Let's just make a new ID column
# NO LONGER NEEDED WITH V3
# # not overwriting SYSID, so it is there for a reference.
# nswCut$SYS2 <- paste0(as.character(nswCut$SYSID), 'nsw')
# wetCut$SYS2 <- paste0(as.character(wetCut$SYSID), 'wet')


print(paste0('starting polygon split, time is ', Sys.time(), ', run is ', dataWhere))

# Test the re-hashing
  # First, without splitting anything
# These are NOT identical to Shane's, likely because the polygons are specified
# very slightly differently (maybe as a result of the multipolygons and make
# # valids)
# wettest <- wetCut %>%
#   slice(1:10) %>%
#   mutate(UID2 = st_geohash(geometry, precision = 9)) %>%
#   select(UID, UID2)
# wettest

# # This takes forever (11700 seconds, ~3.25 hours)
  # AND, because it splits polygons, it creates duplicate SYSIDs
  # In updating to V3, we can LEAVE the SYSIDs in case we ever want to know they
  # were originally the same polygon, and re-hash the new polygons
# This still needs to happen as of V3 because it is pasting the climate and
# basin info onto the ANAEs (and cutting them if they overlap)
system.time(ANAEbasinclim <- wetCut %>%
  st_intersection(kopCut) %>% # intersect with Koppen
  st_intersection(ltimNoNorth) %>% # and add the ltim catchment ## Not sure this is the best way to to this, ie, could probably do it as a selection somehow
  mutate(UID = lwgeom::st_geohash(geometry, precision = 9))) # Re-set the UID. This is the way to keep every polygon unique, and still retains the original SYSIDs from before.


print(paste0('finished polygon split, time is ', Sys.time(), ', run is ', dataWhere))


# # And, just as an extra check, throw some flags on there. Not sure why this
# # happens, but it does. Just brute force fix it.
# while (any(duplicated(bothANAE$SYS2))) {
#   bothANAE$SYS2[which(duplicated(bothANAE$SYS2))] <- paste0(bothANAE$SYS2[which(duplicated(bothANAE$SYS2))], '_DUP')
# }
# 
# # And, to make sorting easier when we break things up
# bothANAE <- arrange(bothANAE, SYS2)

# # Projecting doesn't fix the self-intersect, but should we do it anyway for the intersects? I kind of think not
# # 3577 doesn't fix it
# transcode <- 3577 # 3577 is albers equal area, 3112 is lambert conformal, 3395 is worldwide mercator (no zones, etc. Would be shit but maybe a good test)
# kopCutT <- st_transform(kopCut, crs = transcode)
# ltimNoNorthT <- st_transform(ltimNoNorth, crs = transcode)
# 
# system.time(bothANAE <- bind_rows(wetCut, nswCut) %>%
#               sf::st_transform(crs = transcode) %>%
#               # st_difference() %>% # removes overlaps
#               st_intersection(kopCutT) %>% # intersect with Koppen
#               st_intersection(ltimNoNorthT)) # and add the ltim catchment ## Not sure this is the best way to to this, ie, could probably do it as a selection somehow


# Make the out directory, in case it doesn't exist
if (!dir.exists(scriptOut)) {dir.create(scriptOut, recursive = TRUE)}

save(ANAEbasinclim, ltimNoNorth, file = file.path(scriptOut, 'ANAEbasinclim.rdata'))
print(paste0('saved the full data, time is ', Sys.time(), ', run is ', dataWhere))

# Let's spit these out for ALL the basins, not just lachlan
valleys <-ltimNoNorth$ValleyName

for (b in 1:nrow(ltimNoNorth)) {
  thisbasin <- filter(ANAEbasinclim, ValleyName == valleys[b])
  # thisbasin <- filter(ltimNoNorth, ValleyName == valleys[b]) # For testing
  # Usually a bad idea to assign names, but doing it here so when we read them
  # in we know what they are, and if read in multiple they have different names
  thisname <- str_remove_all(valleys[b], ' ')
  thisname <- paste0(thisname, 'ANAE')
  assign(thisname, thisbasin)
  save(list = c(thisname, 'ltimNoNorth'), file = file.path(scriptOut, paste0(thisname, '.rdata')))
}

# # Testing
# load(file.path(scriptOut, 'BarwonDarlingANAE.rdata'))
# load(file.path(scriptOut, 'GoulburnANAE.rdata'))
# ggplot(BarwonDarlingANAE, aes(color = ValleyName)) + geom_sf()
# ggplot(GoulburnANAE, aes(color = ValleyName)) + geom_sf()
# ggplot(ltimNoNorth, aes(color = ValleyName)) + geom_sf()

# LachlanANAE <- filter(ANAEbasinclim, ValleyName == 'Lachlan')

# Check
# ggplot() +
#   geom_sf(data = filter(ltimNoNorth, ValleyName %in% c("Lachlan")),
#           aes(fill = ValleyName), alpha = 0.5) +
#   geom_sf(data = LachlanANAE, aes(fill = ANAE_DESC)) +
#    # Fill doesn't work without closed shape, as happens with the coord_sf call below
#   # coord_sf(xlim = c(145.65, 145.71),
#   #          ylim = c(-35.94, -35.89)) +
#   theme_bw()

# ggplot() +
#   geom_sf(data = filter(ltimNoNorth, ValleyName %in% c("Lachlan", "Goulburn")), aes(fill = ValleyName))

# 
# save(LachlanANAE, ltimNoNorth, file = file.path(scriptOut, 'LachlanANAE.rdata'))

print(paste0('finished processANAE, time is ', Sys.time(), ', run is ', dataWhere))

