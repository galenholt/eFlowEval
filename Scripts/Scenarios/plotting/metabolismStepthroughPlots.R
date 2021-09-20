# Plots of the INTERMEDIATE steps for the metabolism demo
# all metabinuntempplots.R makes the final outcome plots
# Libraries and system setup
source('directorySet.R')

# Let's get libraries here, then sort out git then sort out making this a library so we don't have to deal with all the library crap
# library(sp)
# library(rgeos)
library(here)
library(tidyverse)
library(sf)
library(stars)
library(tmap)
library(transformr)
library(gganimate)
library(viridis)
library(colorspace)


# Set the crs
whichcrs <- 3577
# directory


# This is all based on inundationCatchmentPlots, but modified to focus on the outcomes (GPP and ER)

# Setup -------------------------------------------------------------------
# Directory to export TO
scriptOut <- file.path('strictOut', 'inundation')
if (!dir.exists(scriptOut)) {dir.create(scriptOut, recursive = TRUE)}

# Basin boundary, for clipping rasters; though likely will start with lachlan
basin <- read_sf(dsn = file.path(datDir, 'ANAE/MDB_ANAE_Aug2017/MDB_ANAE.gdb'), layer = 'MDB_Boundary') %>%
  st_cast("MULTIPOLYGON")  %>% # cleans up an issue with multisurfaces
  dplyr::select(LEVEL2NAME) # no need for other info

# Catchments
basinRef <- file.path(datOut, 'ANAEprocessed', 'ltimNoNorth.rdata')

## Raw inundation
# data location
inunDir <- file.path(datDir, 'Inundation', 'WaterDepth_TwoMonthly', 'geotiff')

# Get the file names
alltifs <- list.files(inunDir, pattern = '.tif$')
inunTifs <- file.path(inunDir, alltifs)


## Temperature
## Temp data
# data location
# Set up where the soil temp data is
tempfile <- list.files(file.path(datDir, 'soilTemp14_20'), pattern = '.nc')
temppath <- file.path(datDir, 'soilTemp14_20', tempfile)

# First, use a stars object to get the overall dimensions without reading data
soilTstars <-  read_stars(temppath, sub = "LST_Day_1km", proxy = TRUE) # Force a proxy for testing the subsetting



# get the crs from the first one (will read them all in later, but first need
# to deal with the corrupt file)
# As long as they're proxies, we can't change their crs, so have to shift
# everything else to them
starCRS <- st_crs(read_stars(inunTifs[1]))

basin <- st_transform(basin, starCRS)

# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


# PLOTS -------------------------------------------------------------------


# -------------------------------------------------------------------------


# -------------------------------------------------------------------------



# Raw Inundation --------------------------------------------------------------

# Taken mostly from processInundation
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
timesub <- 190:193
# rasterio <- list(nXOff = 0, nYOff = 0, # nXSize = 100, nYSize = 100,
# nBufXSize = 1079, nBufYSize = 1079) # , bands = c(1, 3, 4)

tifTimes <- inunTifs[timesub] %>% # filenames
  read_stars() %>% # read in # RasterIO = rasterio
  merge() %>% # make a brick 
  setNames('depth') %>% # name the attribute
  # Make dates
  # soilMstars uses an offset and delta, while this has 'values'.
  # I seem to remember values might cause an issue somewhere down the track, so
  # might need to revisit
  st_set_dimensions(3, values = tifdates[timesub]) %>% 
  st_set_dimensions(names = c("x", "y", "time"))

# Try to just plot it?
# inunNonProxy <- st_as_stars(slice(tifTimes, along = 'time', 1:4))
ltimNoNorth <- st_transform(ltimNoNorth, starCRS)
# plot(ltimNoNorth['ValleyName'], reset = FALSE, col = FALSE)
titletime <- as.Date(st_get_dimension_values(tifTimes, which = 'time')[1])
plot(basin, reset = FALSE, col = FALSE, main = '')
plot(tifTimes[,,,1], add = TRUE, main = paste0("Depth ", titletime))

png(file.path(scriptOut, 'inunPlot1.png'), height = 6/2.54, width = 6/2.54, units = 'in', res = 300)
titletime <- as.Date(st_get_dimension_values(tifTimes, which = 'time')[1])
plot(basin, reset = FALSE, col = FALSE, main = '')
plot(tifTimes[,,,1], add = TRUE, main = paste0("Depth ", titletime))
dev.off()

png(file.path(scriptOut, 'inunPlot2.png'), height = 6/2.54, width = 6/2.54, units = 'in', res = 300)
titletime <- as.Date(st_get_dimension_values(tifTimes, which = 'time')[2])
plot(basin, reset = FALSE, col = FALSE, main = '')
plot(tifTimes[,,,2], add = TRUE, main = paste0("Depth ", titletime))
dev.off()

png(file.path(scriptOut, 'inunPlot3.png'), height = 6/2.54, width = 6/2.54, units = 'in', res = 300)
titletime <- as.Date(st_get_dimension_values(tifTimes, which = 'time')[3])
plot(basin, reset = FALSE, col = FALSE, main = '')
plot(tifTimes[,,,3], add = TRUE, main = paste0("Depth ", titletime))
dev.off()

png(file.path(scriptOut, 'inunPlot4.png'), height = 6/2.54, width = 6/2.54, units = 'in', res = 300)
titletime <- as.Date(st_get_dimension_values(tifTimes, which = 'time')[4])
plot(basin, reset = FALSE, col = FALSE, main = '')
plot(tifTimes[,,,4], add = TRUE, main = paste0("Depth ", titletime))
dev.off()


# plot(ltimNoNorth['ValleyName'], add = TRUE, col = FALSE)
plot(ltimNoNorth['ValleyName'], col = FALSE)

# # downsample and read in?
# 
# # rasterio <- list(nXOff = 0, nYOff = 0, # nXSize = 100, nYSize = 100,
# #                 nBufXSize = 1079, nBufYSize = 1079) # , bands = c(1, 3, 4)
# # (x <- read_stars(inunTifs[timesub], RasterIO = rasterio))
# 
# ggplot() +
#   geom_sf(data = ltimNoNorth, aes(color = ValleyName)) +
#   geom_stars(data = tifTimes)



# Temperature -------------------------------------------------------------
soilTend <- st_crop(soilTstars, basin)

# Only works with read_ncdf for some reason
dimtestS <- st_dimensions(soilTend)

# 
allspace1time <- cbind(start = c(1,1,2150), count = c(dimtestS$x$to, dimtestS$y$to, 1))
soilTnc1 <- read_ncdf(temppath, var = "LST_Day_1km", ncsub = allspace1time)

titletime <- as.Date(st_get_dimension_values(soilTnc1, which = 'time')[1])
titletime
plot(soilTnc1, reset = FALSE, main = paste0("Temp ", titletime))
plot(basin, add = TRUE, col = NA, main = '')

# 
# plot(basin, reset = FALSE, col = FALSE, main = '')
# plot(soilTnc1, add = TRUE, main = paste0("Temp ", titletime))
png(file.path(scriptOut, 'tempPlot1.png'), height = 6/2.54, width = 6/2.54, units = 'in', res = 300)
allspace1time <- cbind(start = c(1,1,2150), count = c(dimtestS$x$to, dimtestS$y$to, 1))
soilTnc1 <- read_ncdf(temppath, var = "LST_Day_1km", ncsub = allspace1time)

titletime <- as.Date(st_get_dimension_values(soilTnc1, which = 'time')[1])
titletime
plot(soilTnc1, reset = FALSE, main = paste0("Temp ", titletime), key.pos = NULL)
plot(basin, add = TRUE, col = NA, main = '')
dev.off()

png(file.path(scriptOut, 'tempPlot2.png'), height = 6/2.54, width = 6/2.54, units = 'in', res = 300)
allspace1time <- cbind(start = c(1,1,2151), count = c(dimtestS$x$to, dimtestS$y$to, 1))
soilTnc1 <- read_ncdf(temppath, var = "LST_Day_1km", ncsub = allspace1time)

titletime <- as.Date(st_get_dimension_values(soilTnc1, which = 'time')[1])
titletime
plot(soilTnc1, reset = FALSE, main = paste0("Temp ", titletime), key.pos = NULL)
plot(basin, add = TRUE, col = NA, main = '')
dev.off()

png(file.path(scriptOut, 'tempPlot3.png'), height = 6/2.54, width = 6/2.54, units = 'in', res = 300)
allspace1time <- cbind(start = c(1,1,2152), count = c(dimtestS$x$to, dimtestS$y$to, 1))
soilTnc1 <- read_ncdf(temppath, var = "LST_Day_1km", ncsub = allspace1time)

titletime <- as.Date(st_get_dimension_values(soilTnc1, which = 'time')[1])
titletime
plot(soilTnc1, reset = FALSE, main = paste0("Temp ", titletime), key.pos = NULL)
plot(basin, add = TRUE, col = NA, main = '')
dev.off()



# REGRESSIONS MADE DIRECTLY IN TEMPPRODRESPREGRESSIONS --------------------


# Now, find some predicitons and plot them --------------------------------

# Based loosely on metabolismCatchmentAggregate, but really just want a couple examples
filesubdirs <- c('bimonth', 'bimonth/predictxvol')
# These are the data name suffixes
# suffixes <- c('PredictBimonthMean', 'PredictxVol')
basinRef <- file.path(datOut, 'ANAEprocessed', 'ltimNoNorth.rdata')

# for(sfun in 1:length(filesubdirs)) {
  filedir <- filesubdirs[2] # filesubdirs[sfun]
  # suffix <- suffixes[sfun]
  # There are some that were NOT chunked- leave them alone, and just look in the chunked folder
  allIn <- file.path(datOut, 'TempAndProduction', 'Predictions', filedir)
  # List the catchments
  catchFiles <- list.files(allIn, pattern = '*.rdata')
  catchNames <- str_extract(catchFiles, pattern = '[A-z]*_') %>%
    str_remove('_') # I'm sure I could do this in one regex, but this is easier
  catchNames

  thisCatch <- 'Lachlan'  
  thisFile <- catchFiles[13] # matches lachlan
  suffix <- str_extract(thisFile, pattern = '_[A-z]*')
  
  # Read in the data
  # anfile <- file.path(anaeIn, paste0(thisCatch, 'ANAE.rdata'))
  catchfile <- file.path(allIn, thisFile)
  
  load(basinRef)
  load(catchfile)
  
  
  # get just this valley- would be nice to do this earlier, but unable
  # Get the right valley- same as in processANAE
  # Do it this way, NOT with the same index as above, because these are in a
  # different order for some reason
  valleys <-ltimNoNorth$ValleyName
  valleys <- str_remove_all(valleys, ' ')
  # Cut to just the valley we want
  thisvalley <- which(valleys == thisCatch)
  thisPoly <- ltimNoNorth[str_remove_all(ltimNoNorth$ValleyName, ' ') == thisCatch, ]
  # thisPoly <- ltimNoNorth %>% slice(thisvalley)
  
  # What am I dealing wiht
  Lachlan_PredictxVol

GPPlast <- ggplot() +
    geom_stars(data = log(Lachlan_PredictxVol[1,15,])) +
    coord_sf() +
    geom_sf(data = thisPoly, fill = NA) +
      theme_void() +
    scale_fill_continuous_sequential(palette = 'Emrld') + # Mint and Green-Yellow probabyl work too
    labs(fill = 'log(GPP)')
# GPPlast

GPP2last <- ggplot() +
  geom_stars(data = log(Lachlan_PredictxVol[1,13,])) +
  coord_sf() +
  geom_sf(data = thisPoly, fill = NA) +
  theme_void() +
  scale_fill_continuous_sequential(palette = 'Emrld') + # Mint and Green-Yellow probabyl work too
  labs(fill = 'log(GPP)')
# GPP2last

ERlast <- ggplot() +
  geom_stars(data = log(Lachlan_PredictxVol[3,15,])) +
  coord_sf() +
  geom_sf(data = thisPoly, fill = NA) +
  theme_void() +
  scale_fill_continuous_sequential(palette = 'Purples') +
  labs(fill = 'log(ER)')
# ERlast
ER2last <- ggplot() +
  geom_stars(data = log(Lachlan_PredictxVol[3,13,])) +
  coord_sf() +
  geom_sf(data = thisPoly, fill = NA) +
  theme_void() +
  scale_fill_continuous_sequential(palette = 'Purples') +
  labs(fill = 'log(ER)')
# ER2last


bothgpp <- ggpubr::ggarrange(GPP2last, GPPlast, common.legend = TRUE, nrow = 1)
bother <- ggpubr::ggarrange(ER2last, ERlast, common.legend = TRUE, nrow = 1)
gpper <- ggpubr::ggarrange(bothgpp, bother, nrow = 2)

png(file.path(scriptOut, 'GPPplot.png'), height = 12/2.54, width = 16/2.54, units = 'in', res = 300)
gpper
dev.off()
