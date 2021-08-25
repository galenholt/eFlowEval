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
metcut <- st_crop(metdatasf, bbpoints)
