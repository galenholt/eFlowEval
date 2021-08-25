# Clean script to join the temperature and production data

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
registerDoFuture()
plan(multisession)


# Set up directory --------------------------------------------------------

scriptOut <- file.path(datOut, 'TempAndProduction')
if (!dir.exists(scriptOut)) {dir.create(scriptOut, recursive = TRUE)}

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


# Joining loop ------------------------------------------------------------
  # see tempProdRespScratch for how we got here (and some plotting)


# Takes forever, but does give distinct geoms
singlegeoms <- metdatasf %>% select(samplepoint) %>% distinct()

# FOR LOOP STARTS HERE
loopstart <- proc.time()
joinedTempProd <- foreach(i = 1:nrow(singlegeoms), 
                          .combine = bind_rows) %dopar% {
  # Filter to a single site- takes a while
  onesite <- filter(metdatasf, geometry == singlegeoms$geometry[i])
  # Crop raster to that site and de-proxy (I thought this is what st_extract DID,
  # but it doesn't work directly)
  onecrop <- st_crop(soilTstars, onesite)
  cropin <- st_as_stars(onecrop)
  
  # NOW, extract the values for each time and strip off the geometry so the joining works
  flattimetemps <- st_extract(cropin, at = onesite, time_column = 'sampledate') %>%
    st_drop_geometry() %>%
    select(-time, flatdate = sampledate)
  # Join back to the original sf
  if (nrow(onesite) != nrow(flattimetemps)) {
    stop("something went wrong with the extraction")
  }
  onewithtemp <- bind_cols(onesite, flattimetemps) # should be shuffle-safe, but check
  if (any(onewithtemp$sampledate != onewithtemp$flatdate)) {
    stop("something went wrong with data joining")
  }
  # onewithtemp <- left_join(onesite, flattimetemps, by = 'sampledate') # This duplicates data if there are multiple samplings in a day
  onewithtemp
}
loopend <- proc.time()
looptime <- loopend-loopstart
looptime #570 seconds locally with multisession- almost worth HPC

# loses 10 lines because they have NA lat/long (see tempProdResp)

save(joinedTempProd, file = file.path(scriptOut, 'joinedTempProd.rdata'))
