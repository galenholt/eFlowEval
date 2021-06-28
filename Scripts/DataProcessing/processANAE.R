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


# Let's get libraries here, then sort out git then sort out making this a library so we don't have to deal with all the library crap
# library(sp)
# library(rgeos)
library(here)
library(tidyverse)
library(sf)
# library(RNetCDF) # Probably not, raster can handle geographic netCDF
# library(raster) # maybe?
library(stars)




print(paste0('starting processANAE, time is ', Sys.time(), ', run is ', dataWhere))


# Read in the ANAE classifications and other data ----------------------------------------
  # takes ~1-2 minutes

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


# Save the processed data
if (!dir.exists(datOut)) {dir.create(datOut)}

save(ANAEbasinclim, ltimNoNorth, file = file.path(datOut, 'ANAEbasinclim.rdata'))
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
  save(list = c(thisname, 'ltimNoNorth'), file = file.path(datOut, paste0(thisname, '.rdata')))
}

# # Testing
# load(file.path(datOut, 'BarwonDarlingANAE.rdata'))
# load(file.path(datOut, 'GoulburnANAE.rdata'))
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
# save(LachlanANAE, ltimNoNorth, file = file.path(datOut, 'LachlanANAE.rdata'))

print(paste0('finished processANAE, time is ', Sys.time(), ', run is ', dataWhere))

