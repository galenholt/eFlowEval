# Script to build on the ANAE without having to reprocess it

library(tidyverse)
library(sf)
library(stars)

# Argh. sort all this directory crap out later

myhome <- str_remove(path.expand("~"), "/Documents")
datDir <- file.path(myhome, "Deakin University/QAEL - MER/Model/dataBase") # "C:/Users/Galen/Deakin University/QAEL - MER/Model/dataBase"

datOut <- "datOut"

# load the processed anae files, cut to lachlan
load(file.path(datOut, 'lachAll.rdata'))

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
lachAll <- st_transform(lachAll, whichcrs)
ltimCut <- st_transform(ltimCut, whichcrs)
basin <- st_transform(basin, whichcrs)

lachSoil <- st_crop(soilMstars, filter(ltimCut, ValleyName == 'Lachlan'))
# check it worked
plot(lachSoil[,,,1:4]) 


# Split the polygons by the rasters to make unique polygons -------------
  # May be what we want sometimes, AND is the precursor to averages across polygons

# First, make the stars into polygons
soilSF <- st_as_sf(lachSoil, as_points = FALSE)
soilSF
# AK:JSFLHJKDSfk
# st_as_sf works great for 1 timestep, but apparently not for many. Argh. and I REALLY want time as sheets. It'll just work so much better

# Can I just get the stupid while loop to work?
  # AM I CONVINCED THIS IS WEIGHTING CORRECTLY?
ag1 <- aggregate(lachSoil, by = lachAll, as_points = FALSE, FUN = mean, na.rm = TRUE)

notna <- ag1[!is.na(ag1[1])]

# Damnit. back to the drawing board.
