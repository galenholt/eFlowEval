# Script to read in ANAE and process it into the standard format

# There's a ton of useful layers in there, for now let's focus on getting the main ANAE classifications in, and then maybe joining to one other thing to practice

# The main ANAE are in MDB_ANAE.gdb/Features/
  # Let's start with Wetlands_ANAE_20171015 and Watercourses_ANAE

# Let's get libraries here, then sort out git then sort out making this a library so we don't have to deal with all the library crap
# library(sp)
# library(rgeos)
library(here)
library(tidyverse)
library(sf)
# library(RNetCDF) # Probably not, raster can handle geographic netCDF
# library(raster) # maybe?
library(stars)



# Data is in QAEL - MER/Model/Data/ANAE
  # tempted to go with a Here, but should really have a library structure
# use here for now
# library(here)

# Argh. sort all this directory crap out later
  # Need to have a shared data folder, without tracking in git and without having to do damn setwd()

myhome <- str_remove(path.expand("~"), "/Documents")
datDir <- file.path(myhome, "Deakin University/QAEL - MER/Model/dataBase") # "C:/Users/Galen/Deakin University/QAEL - MER/Model/dataBase"

datOut <- "datOut"



# Read in the ANAE classifications and other data ----------------------------------------
  # takes ~1-2 minutes
wetlands <- read_sf(dsn = file.path(datDir, 'ANAE/MDB_ANAE.gdb'), layer = 'Wetlands_ANAE_20171025') %>%
  st_cast("MULTIPOLYGON") # cleans up an issue with multisurfaces
 
# And the interim NSW data
wetlandsNSW <- read_sf(dsn = file.path(datDir, 'ANAE/MDB_ANAE.gdb'), layer = 'Interim_Western_NSW_Floodplain_ANAE') %>%
  st_cast("MULTIPOLYGON") # cleans up an issue with multisurfaces

# Get koppen climate region as a test of the joining of data
kopSub <- read_sf(dsn = file.path(datDir, 'ANAE/MDB_ANAE.gdb'), layer = 'BoM_Koppen_subregions') %>%
  st_cast("MULTIPOLYGON") # cleans up an issue with multisurfaces

# And get the LTIM_Valleys to use to subset for toy models at scale, but not enormous scale
LTIM_Valleys <- read_sf(dsn = file.path(datDir, 'ANAE/MDB_ANAE.gdb'), layer = 'LTIM_Valleys') %>%
  st_cast("MULTIPOLYGON") # cleans up an issue with multisurfaces

# and the basin boundary, might be useful, especially for clipping rasters
basin <- read_sf(dsn = file.path(datDir, 'ANAE/MDB_ANAE.gdb'), layer = 'MDB_Boundary') %>%
  st_cast("MULTIPOLYGON")  %>% # cleans up an issue with multisurfaces
  select(LEVEL2NAME) # no need for other info

# Skipping the watercourses for now, come back if doing channel stuff, but as a toy based on veg, probably not now
  

# Simplify to carry less data around --------------------------------------

# Wetlands
wetCut <- wetlands %>% 
  select(SYSID, WaterType, WaterRegime, ANAE_DESC, ANAE_CODE, WOfS)

# NSW wetlands
nswCut <- wetlandsNSW %>% 
  select(SYSID, WaterType, WaterRegime, ANAE_DESC, ANAE_CODE)

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

# This takes forever
bothANAE <- bind_rows(wetCut, nswCut) %>%
  st_intersection(kopCut) %>% # intersect with Koppen
  st_intersection(ltimCut) # and add the ltim catchment ## Not sure this is the best way to to this, ie, could probably do it as a selection somehow

lachAll <- filter(bothANAE, ValleyName == 'Lachlan')

# Check
# ggplot() + 
#   geom_sf(data = filter(ltimCut, ValleyName %in% c("Lachlan")), 
#           aes(fill = ValleyName), alpha = 0.5) +
#   geom_sf(data = lachAll, aes(fill = ANAE_DESC)) +
#    # Fill doesn't work without closed shape, as happens with the coord_sf call below
#   # coord_sf(xlim = c(145.65, 145.71),
#   #          ylim = c(-35.94, -35.89)) +
#   theme_bw()

# ggplot() +
#   geom_sf(data = filter(ltimCut, ValleyName %in% c("Lachlan", "Goulburn")), aes(fill = ValleyName))

if (!dir.exists(datOut)) {dir.create(datOut)}

save(bothANAE, file = file.path(datOut, 'bothANAE.rdata'))
save(lachAll, ltimCut, file = file.path(datOut, 'lachAll.rdata'))
