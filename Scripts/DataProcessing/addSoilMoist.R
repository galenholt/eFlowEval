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
# load(file.path(datOut, 'bothANAE.rdata'))

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
# TODO:: Check this is right on PC, I didn't need to set the CRS there for some reason
st_crs(lachAll) <- 4283 # This is what's there when I look at st_crs, but it's not registering for some reason. 
lachAll <- st_transform(lachAll, whichcrs)
ltimCut <- st_transform(ltimCut, whichcrs)
basin <- st_transform(basin, whichcrs)

# Crop. Have to use as_points = FALSE or it crops to the raster centers, and misses stuff around the edges.
lachSoil <- st_crop(soilMstars, filter(ltimCut, ValleyName == 'Lachlan'), as_points = FALSE)
# check it worked
plot(lachSoil[,,,1:4]) 


# 1. Spatial average of raster into the ANAE polys at each timestep ---------------------------

# First, make the stars into sf polygons
# TODO: likely going to need to start overwriting objects to keep size down at some point. 
# Though, this sf is going to be used for a few things, as is the raster
soilSF <- st_as_sf(lachSoil, as_points = FALSE, merge = FALSE)
str(soilSF)

# Have to intersect with the ANAE polygons to get average.
# Less fiddly (because it's one-to-one), and it ensures the averages area area
# weighted (they're not with aggregate; see timePolyRastScratch for testing)
intA_S <- st_intersection(lachAll, soilSF)
# intF leads with the ANAE cols, and then has date cols all preceded by 'X'

# Get the averages into each ANAE ID at each time
# This strips off the other ANAE cols, which is fine. Could do it earlier, but don't see a need?
# TODO:: Again, rename to reduce size later on
# 255 seconds with 221 cols
system.time(avgA_S <- intA_S %>%
              mutate(area = st_area(.)) %>%  
              group_by(SYS2) %>%
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
              ungroup())

# So, that's basically all the data processing for this one, but for format
# consistency and letting time be time, let's put it back into a `stars` array

# TODO:: do the checks here that the new SYS2 variable fixes the duplicated SYSIDs. 
# then, make the stars with linking vector for confirming indexing
# Then, on to the different rollers

# It has been re-sorted for some unknown stupid dplyr reason. Make sure it is sorted to match lachAll
avgA_S <- avgA_S %>% arrange(SYS2)
lachAll <- lachAll %>% arrange(SYS2)
# Check
all(avgA_S$SYS2 == lachAll$SYS2)

# And, still, save the indexer column (is it possible to glue this back on as a dimension? Probably not, because it's parallel to shape)
avgA_Sindex <- avgA_S$SYS2
# at the least, it'll allow me to see if things have gotten jumbled between shape and sys2

# Turn back into a stars
avgA_Stars <- avgA_S %>% 
  select(-SYS2) %>%
  st_as_stars() %>%
  merge()
# avgA_Stars

# change the time dimension
st_dimensions(avgA_Stars)[2] <- st_dimensions(lachSoil)[3]
st_dimensions(avgA_Stars)[2] # yup, though it's still called X1?
names(st_dimensions(avgA_Stars))[2] <- names(st_dimensions(lachSoil))[3]
# and change the name of the attribute
names(avgA_Stars) <- names(lachSoil)

# avgA_Stars
# plot(avgA_Stars[,,50:51])


# Rolling through time over the spatial polygons ---------------------

# Set up a stars object from the daily soil moistures in the polygons
polyTimeSmax_10 <- avgA_Stars 

# Get a 10-day rolling max for each polygon
polyTimeSmax_10[[1]] <- timeRoll(polyTimeSmax_10[[1]], FUN = RcppRoll::roll_max, rolln = 10, align = 'right')



# Rolling through time on the raster, then putting into polygons --------

# Could probably streamline with the dailies in the first section, but for now
rastTimeSmean_7 <- lachSoil
rastTimeSmean_7[[1]] <- timeRoll(rastTimeSmean_7[[1]], FUN = RcppRoll::roll_mean, rolln = 7, align = 'right')
