# Script to read in temp data and sensor data, match it up and do a regression

source('directorySet.R')

# Let's get libraries here, then sort out git then sort out making this a
# library so we don't have to deal with all the library crap
library(here)
library(tidyverse)
library(sf)
library(stars)
library(foreach)
library(doFuture)

# Read in all the data ----------------------------------------------------

## Temp data
# data location
# Set up where the soil temp data is
tempfile <- list.files(file.path(datDir, 'soilTemp14_20'), pattern = '.nc')
temppath <- file.path(datDir, 'soilTemp14_20', tempfile)

# First, use a stars object to get the overall dimensions without reading data
soilTstars <-  read_stars(temppath, sub = "LST_Day_1km", proxy = TRUE) # Force a proxy for testing the subsetting
names(soilTstars) <- 'tempK'
# As long as they're proxies, we can't change their crs, so have to shift
# everything else to them
# The new version now comes in as 4326 (WGS84 lat/long)
starCRS <- 4326 # st_crs(soilTstars) 

# This is dangerous, but I am telling NASA to give me this CRS. For some reason
# it comes in as something else sometimes
if(st_crs(soilTstars)$epsg != starCRS) {
  st_crs(soilTstars) <- starCRS 
  warning('changed the soil CRS, but it should be correct')
}

## Get the basin in for a quick lok
LTIM_Valleys <- read_sf(dsn = file.path(datDir, 'ANAE/MDB_ANAE_Aug2017/MDB_ANAE.gdb'), layer = 'LTIM_Valleys') %>%
  st_cast("MULTIPOLYGON") # cleans up an issue with multisurfaces
# 
ltimNoNorth <- LTIM_Valleys %>%
  select(ValleyName) %>% # Three different ways to reference, basically
  filter(ValleyName != 'Northern Unregulated') # Deal with the northern unregulated issue

ltimNoNorth <- st_transform(ltimNoNorth, starCRS)


## Metabolism data
metfile <- file.path(datDir, 'metabolism', 'StreamMetabolismBASEModelData_2014_2020.xlsx')
metdata <- readxl::read_xlsx(path = metfile, 
                             sheet = 'Data', skip = 3)
# poke the data a bit
any(!is.na(metdata$Easting)) # no easting (or northing)
sum(is.na(metdata$Latitude))
sum(is.na(metdata$Longitude))

# The names are a mess to program with. clean them up
names(metdata) <- names(metdata) %>%
  str_to_lower() %>%
  str_remove_all(' ') %>%
  str_remove_all('\\(.*\\)')

metdatasf <- metdata %>% 
  select(-easting, -northing) %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  st_as_sf(coords = c('longitude', 'latitude'), crs = 4326)

# Can I quickly plot all the points?
singlepoints <- metdata %>% 
  select(-easting, -northing) %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  group_by(`samplepoint`) %>%
  summarise_all(first)  %>%
  st_as_sf(coords = c('longitude', 'latitude'), crs = 4326)

plot(ltimNoNorth, reset = FALSE)
plot(singlepoints[,'gpp'], add = TRUE)

plot(singlepoints[,'gpp'], reset = FALSE)
plot(ltimNoNorth, add = TRUE)

# This is from mixing up lat and long, but useful for later plotting
# let's get a basemap
library(tmap)
# Not sure how tmap works tho
qtm(ltimNoNorth)
qtm(singlepoints)

tmap_mode("view")
tm_shape(ltimNoNorth) + tm_fill('ValleyName') +
tm_shape(singlepoints) + tm_symbols('gpp')

tmap_mode("plot")
tm_shape(ltimNoNorth) + tm_fill('ValleyName') +
  tm_shape(singlepoints) + tm_symbols('gpp')

# hey, the tmap is pretty cool

# But need to move on- how do we extract temps from the rasters that match the gpp points?
  # at the right times?
ntimes <- 1
cutsoil <- soilTstars %>% slice("time", 1)# this should work, but breaks: soilTstars[,1:1933,1:1865,1:ntimes]
# Will this work, or do I need to loop over the points?


# try with only a couple
cutpoints <- singlepoints[30:35,]
bbpoints <- st_as_sfc(st_bbox(cutpoints))
plot(ltimNoNorth, reset = FALSE)
plot(bbpoints, add = TRUE)
# Can I read that in NOT as a proxy?
croprast <- st_crop(cutsoil, bbpoints)
cropin <- st_as_stars(croprast)
testextract <- st_extract(cropin, at = singlepoints)
testextract

plot(testextract)

# OK, that worked. NOW try it with a couple TIMES
  # First with just a couple, then with time_column to match up-- maybe with a single oint?
# Will ALSO need to sort it out without the bbox read-in- otherwise will be super annoying
cutsoil <- soilTstars %>% slice("time", 1:2)
croprast <- st_crop(cutsoil, bbpoints)
cropin <- st_as_stars(croprast)
testextract <- st_extract(cropin, at = singlepoints)
testextract
plot(testextract)

# Cool. Now, does it work without the st_as_stars?
   # NOPE, crashes
# cutsoil <- soilTstars %>% slice("time", 1:2)
# croprast <- st_crop(cutsoil, bbpoints)
# # cropin <- st_as_stars(croprast)
# testextract <- st_extract(croprast, at = singlepoints)
# testextract
# plot(testextract)

# So, how to proceed? break the points up into nearby points, read in the
# raster, and use time_column? Does that even work?
cutsoil <- soilTstars %>% slice("time", 1:10)
croprast <- st_crop(cutsoil, bbpoints)
cropin <- st_as_stars(croprast)
testextract <- st_extract(cropin, at = metdatasf, time_column = 'sampledate')
testextract
plot(testextract)
# Not actually sure if that worked or not. I think not.
  # Need to do some cutting of the metdatasf so I can see what I'm working with
# metcut <- st_crop(metdatasf, bbpoints) # Fails. But WHY?

# Can I st_crop the raster to a single point?
onecrop <- st_crop(cutsoil, metdatasf[1, ])
cropin <- st_as_stars(onecrop)
testextract <- st_extract(cropin, at = metdatasf[1, ])
testextract
# OK, so that grabbed a single raster, across all raster layers. I was sure
# hoping this would be automatic, but it's hanging on something

# can I use the time_column? Or do I need to do that piecemeal too?
  # Ideally, sites would be constant lat/long, and so I could do each SITE
  # individually, and split them up by time

# BUT, for now, let's just see if the time_column can find the right thing
# what time is 
metdatasf[1,]
# And what dates are in the cutraster
cropin

# OK, that won't overlap. maybe since it's just one point, I can do it with soilTstars directly
onecrop <- st_crop(soilTstars, metdatasf[1, ])
cropin <- st_as_stars(onecrop)
testextract <- st_extract(cropin, at = metdatasf[1, ])
testextract
# this seems redundant at this point. can I just
# testextract <- st_extract(soilTstars, at = metdatasf[1, ])
# testextract
# NOPE, extracting over the whole thing is a killer for some reason

# Can I get that to work with the times automatically?
testtimes <- st_extract(cropin, at = metdatasf[1, ], time_column = 'sampledate')
testtimes
# well, that's an NA, but it might actually BE an NA- there are lots of NAs in the temp data I think
# I could try to read in buffers around the points and average, but that's
# probably a pain. Let's see if there's enough data just going for it before
# overthinking/doing it

# SO, that'll work, but I'd have to loop 30k times
# Are the sites all at the same lat/long?
metdatasf # lat/long are gone
metdata
length(unique(metdata$samplepoint))
length(unique(metdata$latitude))
length(unique(metdata$longitude))
table(metdata$samplepoint, metdata$latitude)

# does this work?
length(unique(metdatasf$geometry))
# duh
# So, 37 total different places. If I can do the extract at that scale and then deal with time, that'll be easy

# Can I subset
# Filter is killing me. But I *think* straight up logical seems to be fast
metdatasf[1:10, ]$geometry == metdatasf$geometry[5]

# singlegeoms <- unique(st_geometry(metdatasf)) # But this is a list and not geometries anymore
## Rgh. 
# metdatasf %>% group_by(geometry) %>% summarise_all(first)

# Takes forever, but does give distinct geoms
singlegeoms <- metdatasf %>% select(samplepoint) %>% distinct()


# Try to filter- the dplyr is usually really slow, so might need to do something else- indexing?
  # takes ~ 30 seconds. SO not insignificant
system.time(onesite <- filter(metdatasf, geometry == singlegeoms$geometry[1]))

# Not appreciably faster to do it with indexing, and then we'd need to do the
# cutting, so stick with filter()
# system.time(singleindex <- which(metdatasf$geometry == singlegeoms$geometry[1]))
# that seems really short, but it's correct;
table(metdatasf$samplepoint[which(metdatasf$samplepoint == metdatasf$samplepoint[1])])

# NOW, back up to the extraction
onecrop <- st_crop(soilTstars, onesite)
cropin <- st_as_stars(onecrop)
# testextract <- st_extract(cropin, at = metdatasf[1, ])
# testextract
# this seems redundant at this point. can I just
# testextract <- st_extract(soilTstars, at = metdatasf[1, ])
# testextract
# NOPE, extracting over the whole thing is a killer for some reason

# Can I get that to work with the times automatically?
testtimes <- st_extract(cropin, at = onesite, time_column = 'sampledate')
testtimes
# So, that worked, but now we've lost the rest of onesite
# onewithtemp <- st_join(onesite, testtimes, join = st_equals)
# uhhhhh. that is nrow*nrow because of the date issue
onesite
testtimes

# maybe
testflat <- st_drop_geometry(testtimes) %>% select(-time)
testflat
onewithtemp <- left_join(onesite, testflat, by = 'sampledate')

# ok, package that up into a loop.

# Try to write the actual loop --------------------------------------------

# Takes forever, but does give distinct geoms
singlegeoms <- metdatasf %>% select(samplepoint) %>% distinct()

# make sure that didn't get some with the same geom and differen names
sum(duplicated(unlist(singlegeoms$geometry)))

# FOR LOOP STARTS HERE

# Filter to a single site- takes a while
system.time(onesite <- filter(metdatasf, geometry == singlegeoms$geometry[1]))
# Crop raster to that site and de-proxy (I thought this is what st_extract DID,
# but it doesn't work directly)
onecrop <- st_crop(soilTstars, onesite)
cropin <- st_as_stars(onecrop)

# NOW, extract the values for each time and strip off the geometry so the joining works
testtimes <- st_extract(cropin, at = onesite, time_column = 'sampledate') %>%
  st_drop_geometry(testtimes) %>%
  select(-time)
# Join back to the original sf
onewithtemp <- left_join(onesite, testflat, by = 'sampledate')
onewithtemp
