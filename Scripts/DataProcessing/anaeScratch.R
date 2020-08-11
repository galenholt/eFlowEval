# Script to read in ANAE and process it into the standard format

# There's a ton of useful layers in there, for now let's focus on getting the main ANAE classifications in, and then maybe joining to one other thing to practice

# The main ANAE are in MDB_ANAE.gdb/Features/
  # Let's start with Wetlands_ANAE_20171015 and Watercourses_ANAE

# Let's get libraries here, then sort out git then sort out making this a library so we don't have to deal with all the library crap
# library(sp)
# library(rgeos)
library(tidyverse)
library(sf)
# Data is in QAEL - MER/Model/Data/ANAE
  # tempted to go with a Here, but should really have a library structure
# use here for now
# library(here)

# Argh. sort all this crap out later

datDir <- "C:/Users/Galen/Deakin University/QAEL - MER/Model/Data"

# Let's go for it, using the LTIM valleys first, since it's way smaller but has same structure
LTIM_Valleys <- read_sf(dsn = paste0(datDir, '/ANAE/MDB_ANAE.gdb'), layer = 'LTIM_Valleys')

ggplot(LTIM_Valleys, aes(color= ValleyName, fill = ValleyName)) + geom_sf() + theme_bw() 
  # scale_fill_brewer(type = 'qual') + scale_color_brewer(type = 'qual') # Too many values for brewer

# Try a smaller line file to test before jumping into rivers
roads <- read_sf(dsn = paste0(datDir, '/ANAE/MDB_ANAE.gdb'), layer = 'MajorRoads')

ggplot() + 
  geom_sf(data = LTIM_Valleys, aes(color= ValleyName, fill = ValleyName)) + 
  geom_sf(data = roads) +
  theme_bw() 


# Read in the ANAE classifications ----------------------------------------
  # takes ~1-2 minutes
wetlands <- read_sf(dsn = paste0(datDir, '/ANAE/MDB_ANAE.gdb'), layer = 'Wetlands_ANAE_20171025')

# Plot check
  # Takes a REALLY long time, so try trimming.

# Not sure what's up here, but having trouble filtering (and likely intersecting
# down the track), so reading some github help that sugggests the multisurfaces are the issue
# unique(st_geometry_type(st_geometry(wetlands)))
# https://github.com/r-spatial/sf/issues/427
  # Seems to work
wetlands <- st_cast(wetlands,  "MULTIPOLYGON")

# Trim to the Kow swamp/gunbower area
trimKow <- st_crop(wetlands, xmin = 144, xmax = 144.5, ymin = -36, ymax = -35.75)

ggplot() + 
  geom_sf(data = trimKow, aes(fill = ANAE_DESC)) + 
  # # geom_sf(data = roads) +
  # coord_sf(xlim = c(144, 145),
  #          ylim = c(-35.5, -36)) +
  theme_bw() + theme(legend.position = 'top')

# what's the difference with?
  # Unclear. This is a data.frame, the other is a tibble. But that's not really much different...
wet2 <- st_read(dsn = paste0(datDir, '/ANAE/MDB_ANAE.gdb'), layer = 'Wetlands_ANAE_20171025')

# OK, that seems to work. Now, can I work on something to demo intersections, etc
  # Wondering if I should continue with the cut down data at first? Probably to sort things out.
# probably worth cutting OUT stuff we don't need too, though for now let's leave in

# There's a koppen subregion break near barmah. Use that as an intersection
trimBar <- st_crop(wetlands, xmin = 144.5, xmax = 146, ymin = -36.5, ymax = -35.6)

# Get the koppen subregion in there
kopSub <- read_sf(dsn = paste0(datDir, '/ANAE/MDB_ANAE.gdb'), layer = 'BoM_Koppen_subregions')
trimKop <- st_crop(kopSub, xmin = 144.5, xmax = 146, ymin = -36.5, ymax = -35.6)

# Quick plot
ggplot() + 
  geom_sf(data = trimBar, aes(fill = ANAE_DESC)) + 
  geom_sf(data = trimKop, aes(fill = Zone, alpha = 0.25)) +
  # coord_sf(xlim = c(144, 145),
  #          ylim = c(-35.5, -36)) +
  theme_bw() + theme(legend.position = 'top')

# Let's simplify so I can tell what I'm doing. Probably will want more later, but for now
trimBarCut <- trimBar %>% 
  select(SYSID, MeanLandform, WaterSource, WaterType, WaterRegime, ANAE_DESC, WOfS)

# Dunno what ANAEField is, but let's keep a couple attributes
trimKopCut <- trimKop %>%
  select(ANAEField, Zone)

# Now, an intersect should give a new table with added attibutes, and a few new rows where features got cut in half
wetKop <- st_intersection(trimBarCut, trimKopCut)
str(wetKop)
# Wow, I think that just worked?

# Crop, otherwise a million labels with same colors
doubletrimBar <- st_crop(trimBarCut, xmin = 145.65, xmax = 145.71, ymin = -35.94, ymax = -35.89)
doubletrimKop <- st_crop(trimKopCut, xmin = 145.65, xmax = 145.71, ymin = -35.94, ymax = -35.89)
trimWetKop <- st_crop(wetKop, xmin = 145.65, xmax = 145.71, ymin = -35.94, ymax = -35.89)

# Does coord_sf work now that I fixed the multisurface?, that'd sure be useful
  # It does, but still confusing
ggplot() + 
  geom_sf(data = doubletrimBar, aes(fill = ANAE_DESC)) + 
  geom_sf(data = doubletrimKop, aes(fill = Zone, alpha = 0.5)) + # Fill doesn't work without closed shape, as happens with the coord_sf call below
  # coord_sf(xlim = c(145.65, 145.71),
  #          ylim = c(-35.94, -35.89)) +
  theme_bw() + theme(legend.position = 'top')

# The question is, has it split those shapes at the koppen border?
ggplot() + 
  geom_sf(data = trimWetKop, aes(fill = interaction(ANAE_DESC, Zone))) + 
  # geom_sf(data = trimKopCut, aes(fill = Zone, alpha = 0.5)) + # Dunno why alpha isn't working, but whatever
  # coord_sf(xlim = c(145.65, 145.71),
  #          ylim = c(-35.94, -35.89)) +
  theme_bw() # + theme(legend.position = 'top')

# Cool. That actually worked.

# Do I want to do something with raster averaging? Probably. But I don't know
# that it's necessary now, it can happen as further development inside this
# module

# how to handle time? 
  # will likely need an isWet at each timestep. and maybe then from that build a "time since last wet" and "time since this wet"
  # But that will depend on the strictures. What do they need?
    # including more complex things, like distance to nearest ANAE = x that is wet and has been for a week and has fish...
# but for now, should be able to build the toy static, and add those either as arrays or columns, depending on specifics

# So for now, let's kick out a toy dataset

# Need to check that I'm handling adding nsw correctly
# And the interim NSW data
wetlandsNSW <- read_sf(dsn = paste0(datDir, '/ANAE/MDB_ANAE.gdb'), layer = 'Interim_Western_NSW_Floodplain_ANAE') %>%
  st_cast("MULTIPOLYGON") # cleans up an issue with multisurfaces

# Cut
# Wetlands
wetCut <- wetlands %>% 
  select(SYSID, WaterType, WaterRegime, ANAE_DESC, ANAE_CODE, WOfS)

# NSW wetlands
nswCut <- wetlandsNSW %>% 
  select(SYSID, WaterType, WaterRegime, ANAE_DESC, ANAE_CODE)

wetTrim <- st_crop(wetCut, xmin = 146.75, xmax = 147.20, ymin = -30.89, ymax = -30.52)
nswTrim <- st_crop(nswCut, xmin = 146.75, xmax = 147.20, ymin = -30.89, ymax = -30.52)

ggplot() + 
  geom_sf(data = wetTrim, aes(fill = ANAE_DESC)) +
  geom_sf(data = nswTrim, aes(fill = ANAE_DESC)) + # Fill doesn't work without closed shape, as happens with the coord_sf call below
  # coord_sf(xlim = c(145.65, 145.71),
  #          ylim = c(-35.94, -35.89)) +
  theme_bw() # + theme(legend.position = 'top')

# now, can I union?
# joinWets <- st_combine(wetTrim, nswTrim) # This is NOT what we want
# joinWets <- st_join(wetTrim, nswTrim) # Nor is this, I don't think, it does some sort of merge with shared features
# joinWets <- rbind(wetTrim, nswTrim) # Breaks because columns not identical

# dplyr handles missing columns better
joinWets <- bind_rows(wetTrim, nswTrim) # Phew. looks like this only recently started working.

# Check
ggplot() + 
  geom_sf(data = joinWets, aes(fill = ANAE_DESC)) +
  # geom_sf(data = nswTrim, aes(fill = ANAE_DESC)) + # Fill doesn't work without closed shape, as happens with the coord_sf call below
  # coord_sf(xlim = c(145.65, 145.71),
  #          ylim = c(-35.94, -35.89)) +
  theme_bw()
