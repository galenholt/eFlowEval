# code to add time and rasters. may be integrated into makeTestData, or maybe
# not, since it will be nice to separate the different heavy processing steps

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

# Can I clip the whole stars obj?
  # Commented out bits are for the whole basin, do that later after we see how long the lachlan takes
# basinMatchA <- st_transform(basin, st_crs(soilMstars)) # datum transformation. same as above, but in case that gets commented
# clipSoil <- soilMstars[basinMatchA] # This syntax makes a lot of sense for a single shape, but can also 

# Set the crs.
  # Doing this here because it's not yet clear which is best, so we want
  # everything in before we choose one to get the donor CRS from
# using a regular (not curvilinear) grid is necessary to avoid retaining the
# full extent and size of the raster
  # soilMstars comes in as regular, so use that. Though st_warp() likely would
  # do it on a different projection (see help and vignettes)
whichcrs <- st_crs(soilMstars)
# Try to fix on mac
st_crs(lachAll) <- 4283 # This is what's there when I look at st_crs, but it's not registering for some reason. 
  # TODO:: But check it's right on PC
lachAll <- st_transform(lachAll, whichcrs) # Why doesnt this work on Mac? AAAAAAAAA!!!!
ltimCut <- st_transform(ltimCut, whichcrs)
basin <- st_transform(basin, whichcrs)

lachSoil <- st_crop(soilMstars, filter(ltimCut, ValleyName == 'Lachlan'))
# check it worked
plot(lachSoil[,,,1:4]) 

# Also works
# lachSoil <- soilMstars[filter(ltimCut, ValleyName == 'Lachlan')]
# # check it worked
# plot(lachSoil[,,,1:4])

# Now, what do I want to do?
  # and here's a question: what happens if I do the crop with the ANAE
  # and either way, if this is based on centroids, and an anae is small enough
  # to fit outside all centroids, what happens?
  # I think I'll just have to check, the help doesn't say clearly: ?aggregate.stars
    # perhaps as_points = FALSE, or exact = TRUE?

# 1: get the soil moisture averaged over the anae region at each timestep
  # i.e. an instantaneous variable
# 2: Roll the time in the raster, maybe using max over 10 days or something, then average into polygons
# 3: average into the polygons (see 1), and then roll the time in a different way, maybe 3-day smoother or something. doesn't matter, it's a demo

# aggregate each timestep into the polygons

# Pull the instantaneous values in averaged over each polygon
agSoil = aggregate(lachSoil, by = lachAll, FUN = mean, na.rm = TRUE)
  # Yields a LOT of NAs
      # 99387. NOPE, that's from the first 1e5 cells, so there's a LOT more than that.
        # Though there are 36340*208 objects * times, so that's 7558720, and 99387 is actually small in that sense
      # Why are they almost all in the first thousand cells and 100 days?
agSoil
plot(agSoil[,1:10,1:10])
sum(is.na(agSoil[[1]]))

# Does as_points = FALSE fix it by turning them into polygons?
agSoil2 = aggregate(lachSoil, by = lachAll, as_points = FALSE, FUN = mean, na.rm = TRUE)
  # Some of them
  agSoil2
  plot(agSoil2[,1:10, 1:10])
  sum(is.na(agSoil2[[1]]))
  
# # Does exact == TRUE help? Doesn't seem to work for rasters
#   agSoil3 = aggregate(lachSoil, by = lachAll, as_points = FALSE, exact = TRUE, FUN = mean, na.rm = TRUE)
#   # Some of them
#   agSoil3
#   plot(agSoil3[,1:10, 1:10])
#   sum(is.na(agSoil3[[1]]))
  
# where are the NA's? I'm wondering if this is stuff around the edge because the lachlan crop cropped too much
 # Grab a single timeslice for error chasing
  a50 <- agSoil2[,,50]
  sum(is.na(a50[[1]]))
  # well, 34610 na out of 36340 is a LOT of NA
 lookforna <- ggplot() + 
  geom_sf(data = filter(ltimCut, ValleyName %in% c("Lachlan")), 
          aes(color = ValleyName), alpha = 0.5) +
  geom_stars(data = a50, aes(fill = sm_pct, color = is.na(sm_pct)))
lookforna

# That didn't show up well
lookforna2 <- ggplot() + 
  # geom_sf(data = filter(ltimCut, ValleyName %in% c("Lachlan")), 
  #         aes(color = ValleyName), alpha = 0.5) +
  geom_stars(data = a50, aes(fill = is.na(sm_pct), color = is.na(sm_pct)))
lookforna2
# Well, it's not edges. what's going on?

# I'd sure like to zoom in... can I set that up with a bbox rather than a defined polygon?
st_bbox(a50)
bb = st_bbox(c(xmin = 144.4, ymin = -33.8, xmax = 144.6, ymax = -33.6), crs = whichcrs)
# bb = st_bbox(c(xmin = 144, ymin = -34, xmax = 145, ymax = -33), crs = whichcrs)
# aSub = st_crop(a50, st_as_sfc(bb))
asub <- a50[st_as_sfc(bb)]
# Replot
lookforna3 <- ggplot() + 
  # geom_sf(data = filter(ltimCut, ValleyName %in% c("Lachlan")), 
  #         aes(color = ValleyName), alpha = 0.5) +
  geom_stars(data = asub, aes(fill = is.na(sm_pct)))
lookforna3

# What about the soil data?
soilsub <- lachSoil[st_as_sfc(bb)]
soilsub <- soilsub[,,,50]
lookforna4 <- ggplot() + 
  # geom_sf(data = filter(ltimCut, ValleyName %in% c("Lachlan")), 
  #         aes(color = ValleyName), alpha = 0.5) +
  geom_stars(data = soilsub, aes(x = longitude, y = latitude, fill = is.na(sm_pct)))
lookforna4

# huh.so the raster has some nas
lookforna5 <- ggplot() + 
  # geom_sf(data = filter(ltimCut, ValleyName %in% c("Lachlan")), 
  #         aes(color = ValleyName), alpha = 0.5) +
  geom_stars(data = soilsub, aes(x = longitude, y = latitude, fill = sm_pct)) +
  geom_stars(data = asub, aes(fill = sm_pct))
lookforna5
  # Are there some in there hiding? must be

# AAAAAAA!!!!!! 

# I can do
lookforna6 <- ggplot() + 
  # geom_sf(data = filter(ltimCut, ValleyName %in% c("Lachlan")), 
  #         aes(color = ValleyName), alpha = 0.5) +
  geom_stars(data = soilsub, aes(x = longitude, y = latitude, fill = sm_pct)) +
  geom_sf(data = st_as_sf(asub), aes(color = is.na(`2020-02-18 12:00:00`)))
lookforna6

# Are any of the polgons bad?
sum(!st_is_valid(lachAll))


# Can I zoom way in to try to find out what's happening? ------------------
bb = st_bbox(c(xmin = 144.4, ymin = -33.8, xmax = 144.6, ymax = -33.6), crs = whichcrs)
soilCrop <- soilMstars[st_as_sfc(bb)]
lachCrop <- st_crop(lachAll, st_as_sfc(bb))
agCrop <- aggregate(soilCrop, by = lachCrop, FUN = mean, na.rm = TRUE)
ac50 <- agCrop[,,50]
sc50 <- soilCrop[,,,50]

na7 <- ggplot() + 
  # geom_sf(data = filter(ltimCut, ValleyName %in% c("Lachlan")), 
  #         aes(color = ValleyName), alpha = 0.5) +
  geom_stars(data = sc50, aes(x = longitude, y = latitude, fill = sm_pct)) +
  geom_sf(data = st_as_sf(ac50), aes(color = is.na(`2020-02-18 12:00:00`)))
na7

# That's definitely a DIFFERENT set of NA's than 
lookforna6 + xlim(c(144.35, 144.65)) + ylim(c(-33.85, -33.55))
# BUt neither makes much sense.
  # Argh. super hard to get the coords right, but whatever

# dunno if this works
# ggplotly(na7) # only works for raster, not sfc_geometry

# WHAT IF THE BBox is way bigger for the raster?
bbP <- st_bbox(c(xmin = 144.4, ymin = -33.8, xmax = 144.6, ymax = -33.6), crs = whichcrs)
bbR <- st_bbox(c(xmin = 144, ymin = -34.2, xmax = 145, ymax = -33.2), crs = whichcrs)
soilCropR <- soilMstars[st_as_sfc(bbR)]
lachCropP <- st_crop(lachAll, st_as_sfc(bbP))
agCrop2 <- aggregate(soilCropR, by = lachCropP, FUN = mean, na.rm = TRUE)
ac502 <- agCrop2[,,50]
sc502 <- soilCropR[,,,50]

na8 <- ggplot() + 
  # geom_sf(data = filter(ltimCut, ValleyName %in% c("Lachlan")), 
  #         aes(color = ValleyName), alpha = 0.5) +
  geom_stars(data = sc502, aes(x = longitude, y = latitude, fill = sm_pct)) +
  geom_sf(data = st_as_sf(ac502), aes(color = is.na(`2020-02-18 12:00:00`)))
na8 + xlim(c(144.35, 144.65)) + ylim(c(-33.85, -33.55))
na7

# Argh. haven't been using as_points = FALSe
# WHAT IF THE BBox is way bigger for the raster?
bbP <- st_bbox(c(xmin = 144.4, ymin = -33.8, xmax = 144.6, ymax = -33.6), crs = whichcrs)
bbR <- st_bbox(c(xmin = 144, ymin = -34.2, xmax = 145, ymax = -33.2), crs = whichcrs)
soilCropR <- soilMstars[st_as_sfc(bbR)]
lachCropP <- st_crop(lachAll, st_as_sfc(bbP))
agCrop3 <- aggregate(soilCropR, by = lachCropP, as_points = FALSE, FUN = mean, na.rm = TRUE)
ac503 <- agCrop3[,,50]
sc503 <- soilCropR[,,,50]

na9 <- ggplot() + 
  # geom_sf(data = filter(ltimCut, ValleyName %in% c("Lachlan")), 
  #         aes(color = ValleyName), alpha = 0.5) +
  geom_stars(data = sc503, aes(x = longitude, y = latitude, fill = sm_pct)) +
  geom_sf(data = st_as_sf(ac503), aes(color = is.na(`2020-02-18 12:00:00`)))
na9 + xlim(c(144.35, 144.65)) + ylim(c(-33.85, -33.55))
na8 + xlim(c(144.35, 144.65)) + ylim(c(-33.85, -33.55))

# hmm. Do I only get one polygon per raster?
# This has 5x5 raster cells, and 
sum(!is.na(ac503[[1]]))
# Hmmm. 21. But, I can see some of those are spanning multiple raster cells. so likely about right.

# Rather than jump in and try to fix it, let's double check that IS the problem
ac503
ac503_1 <- ac503 %>% st_as_sf() %>% rename(sm_pct = `2020-02-18 12:00:00`)
# Did that work?

na10 <- ggplot() + 
  # geom_sf(data = filter(ltimCut, ValleyName %in% c("Lachlan")), 
  #         aes(color = ValleyName), alpha = 0.5) +
  geom_stars(data = sc503, aes(x = longitude, y = latitude, fill = sm_pct)) +
  geom_sf(data = st_as_sf(ac503_1), aes(color = is.na(sm_pct)))
na10 + xlim(c(144.35, 144.65)) + ylim(c(-33.85, -33.55))

# Yes.

# Now, let's do the st_as_sf earlier, so we can do a series of peelings.
# WHAT IF THE BBox is way bigger for the raster?
bbP <- st_bbox(c(xmin = 144.4, ymin = -33.8, xmax = 144.6, ymax = -33.6), crs = whichcrs)
bbR <- st_bbox(c(xmin = 144, ymin = -34.2, xmax = 145, ymax = -33.2), crs = whichcrs)
soilCropR <- soilMstars[st_as_sfc(bbR)]
lachCropP <- st_crop(lachAll, st_as_sfc(bbP))
soilC50 <- soilCropR[,,,50] %>% adrop()

agC50 <- aggregate(soilC50, by = lachCropP, as_points = FALSE, FUN = mean, na.rm = TRUE) %>%
  st_as_sf()

na11 <- ggplot() + 
  # geom_sf(data = filter(ltimCut, ValleyName %in% c("Lachlan")), 
  #         aes(color = ValleyName), alpha = 0.5) +
  geom_stars(data = soilC50, aes(x = longitude, y = latitude, fill = sm_pct)) +
  geom_sf(data =agC50, aes(color = is.na(sm_pct)))
na11 + xlim(c(144.35, 144.65)) + ylim(c(-33.85, -33.55))
# OK, same thing.

# Now, can I do it again with just the nas?
firstna <- filter(agC50, is.na(sm_pct))
agC50_2 <- aggregate(soilC50, by = firstna, as_points = FALSE, FUN = mean, na.rm = TRUE) %>%
  st_as_sf()

# plot
na12 <- ggplot() + 
  # geom_sf(data = filter(ltimCut, ValleyName %in% c("Lachlan")), 
  #         aes(color = ValleyName), alpha = 0.5) +
  geom_stars(data = soilC50, aes(x = longitude, y = latitude, fill = sm_pct)) +
  geom_sf(data =agC50_2, aes(color = is.na(sm_pct)))
na12 + xlim(c(144.35, 144.65)) + ylim(c(-33.85, -33.55))
# OK, that works.

# Can I fill it all in?
agC50 <- aggregate(soilC50, by = lachCropP, as_points = FALSE, FUN = mean, na.rm = TRUE) %>%
  st_as_sf()

notna <- filter(agC50, !is.na(sm_pct))
napoly <- filter(agC50, is.na(sm_pct))

while(nrow(napoly) > 0) {
  newag <- aggregate(soilC50, by = napoly, as_points = FALSE, FUN = mean, na.rm = TRUE) %>%
    st_as_sf()
  newfit <- filter(newag, !is.na(sm_pct))
  notna <- bind_rows(notna, newfit)
  napoly <- filter(newag, is.na(sm_pct))
  
}

notna
agC50
# plot
na13 <- ggplot() + 
  # geom_sf(data = filter(ltimCut, ValleyName %in% c("Lachlan")), 
  #         aes(color = ValleyName), alpha = 0.5) +
  geom_stars(data = soilC50, aes(x = longitude, y = latitude, fill = sm_pct)) +
  geom_sf(data = notna, aes(color = is.na(sm_pct)))
na13 + xlim(c(144.35, 144.65)) + ylim(c(-33.85, -33.55))

# and the actual data inside them
na14 <- ggplot() + 
  # geom_sf(data = filter(ltimCut, ValleyName %in% c("Lachlan")), 
  #         aes(color = ValleyName), alpha = 0.5) +
  geom_stars(data = soilC50, aes(x = longitude, y = latitude, fill = sm_pct)) +
  geom_sf(data = notna, aes(fill = sm_pct))
na14 + xlim(c(144.35, 144.65)) + ylim(c(-33.85, -33.55))

# adn just the shapes
na15 <- ggplot() + 
  # geom_sf(data = filter(ltimCut, ValleyName %in% c("Lachlan")), 
  #         aes(color = ValleyName), alpha = 0.5) +
  # geom_stars(data = soilC50, aes(x = longitude, y = latitude, fill = sm_pct)) +
  geom_sf(data = notna, aes(fill = sm_pct))
na15 + xlim(c(144.35, 144.65)) + ylim(c(-33.85, -33.55))


# Try with st_join and then aggregate()... --------------------------------
# st_join with examples here https://r-spatial.github.io/sf/reference/st_join.html

# So, trying to average the soilC50 raster into the lachCropP polygons
# I guess a question is whether I'll need to polygonize the rasters. seems likely

jointestL <- st_join(soilC50, lachCropP, as_points = FALSE)
# Uses default what = 'left1'
jointestL
plot(jointestL)

# Throws Warning: In st_join.stars(soilC50, agC50, as_points = FALSE) :
# st_join found 25 1-to-n matches, taking the first match for each of those

# See ?st_join.stars: it takes te first y that matches an x: so this is runnign into the same issue as aggregate

# So, that's not what I want: it returns a raster grid with only the first match. It's basically wholly backwards

# So, this splits the polygons by the raster. That's one option. 
jointestI <- st_join(soilC50, lachCropP, as_points = FALSE, what = "inner") # what = 'right' is unsupported?
jointestI
plot(jointestI)

# If we DON'T want to split them (typically), can I put them back together? Or is my while loop better?

length(unique(jointestI$SYSID))
isdup <- duplicated(jointestI$SYSID)
sum(isdup)

# Would be easy to group_by(SYSID), but two issues: 
# one is that that is specific to the ANAE, and so would rely on other datasets
# having a unique ID (though we can always create one, so that's not a huge
# issue)
# Second is whether the resulting mean is area-weighted. It SHOULD be easy to
# do, since each polygon is different size, but not sure it happens automatically
  # AND, would it return a combined polygon??
# Let's test
iddup <- jointestI$SYSID[isdup]
alldup <- jointestI$SYSID %in% iddup

jointestD <- jointestI[alldup, ]

# what are the naive means as a comparison?
unweightRef <- jointestD %>% 
  select(sm_pct, SYSID) %>% 
  st_drop_geometry() %>%
  group_by(SYSID) %>%
  summarize(naiveMean = mean(sm_pct)) %>%
  ungroup()
unweightRef

# Now, the exact same thing but WITH the geometry
withGeom <- jointestD %>%
  group_by(SYSID) %>%
  summarize(geoMean = mean(sm_pct)) %>%
  ungroup() %>%
  select(geoMean, SYSID)
withGeom

# Now, do the means match, or were they area-weighted?
withGeom$geoMean == unweightRef$naiveMean
# NOT area-weighted. Suppose I could put in an area column to weight by. BUTAND,
# what happened to the polygons?

# WAIt: DID THIS SPLIT THE POLYGONS? OR DUPLICATE THEM?
# Plotting the three first ones, all look exactly the same
plot(slice(jointestD, 1:3)['sm_pct'])
plot(slice(jointestD, 1)['sm_pct'])
plot(slice(jointestD, 3)['sm_pct'])

# 6 and 7 have more different values
plot(slice(jointestD, 6:7)['sm_pct'])
plot(slice(jointestD, 6)['sm_pct'])
plot(slice(jointestD, 7)['sm_pct'])

# Yeah, that's the same damn polygon. SO... it's been duplicated, not split.
# Which means I CAN'T mean-weight

# SO, we can try to actually DO the intersection splitting explicitly, and then
# do what I was about to do. Or, we can use the while loop.


# The while loop exists. Let's see if we can do the intersection splitting?
  # Fails on stars object, so making the raster a polygon
soilSF <- st_as_sf(soilC50)
plot(soilSF)
plot(soilC50)
intC50 <- st_intersection(lachCropP, soilSF)
intC50
plot(intC50['sm_pct'])

# Now, as we were before, let's look at the dups and make sure they worked right.
# This time, let's START with the dup'ed polygon plots to check
length(unique(intC50$SYSID))
isdup <- duplicated(intC50$SYSID)
sum(isdup)

# Would be easy to group_by(SYSID), but two issues: 
# one is that that is specific to the ANAE, and so would rely on other datasets
# having a unique ID (though we can always create one, so that's not a huge
# issue)
# Second is whether the resulting mean is area-weighted. It SHOULD be easy to
# do, since each polygon is different size, but not sure it happens automatically
# AND, would it return a combined polygon??
# Let's test
iddup <- intC50$SYSID[isdup]
alldup <- intC50$SYSID %in% iddup

jointestD <- intC50[alldup, ]
jointestD

# Look at the polygons
# Crap. Can't slice because out of order
jointestD <- arrange(jointestD, SYSID)
plot(slice(jointestD, 1:3)['sm_pct'])
plot(slice(jointestD, 1)['sm_pct'])
plot(slice(jointestD, 2)['sm_pct'])
plot(slice(jointestD, 3)['sm_pct'])

# 6 and 7 have more different values
plot(slice(jointestD, 6:7)['sm_pct'])
plot(slice(jointestD, 6)['sm_pct'])
plot(slice(jointestD, 7)['sm_pct'])

# Cool. So, that has actually worked!

# Now, let's see how we can combine them. Need to look at both aggregate and group_by() %>% summarize()

# what are the naive means as a comparison?
unweightRef <- jointestD %>% 
  select(sm_pct, SYSID) %>% 
  st_drop_geometry() %>%
  group_by(SYSID) %>%
  summarize(naiveMean = mean(sm_pct)) %>%
  ungroup()
unweightRef

# Now, the exact same thing but WITH the geometry
withGeom <- jointestD %>%
  group_by(SYSID) %>%
  summarize(geoMean = mean(sm_pct)) %>%
  ungroup() %>%
  select(geoMean, SYSID)
withGeom

# Now, do the means match, or were they area-weighted?
withGeom$geoMean == unweightRef$naiveMean

# NOT AREA-WEIGHTED

# How about an aggregate?
aggCheck <- aggregate(jointestD, by = list(jointestD$SYSID), FUN = mean)
# Well, that lost everything non-numeric. So we'd have to cut it down and then
# re-join. But that's not unreasonable, I suppose. OR, could do the original
# join the other way to only have the sm_pct

aggCheck$sm_pct == withGeom$geoMean
# STILL not area-weighted

# SO, let's try to make an area column and do a weighted mean using dplyr approacjh
# Now, the exact same thing but WITH the geometry
withArea <- jointestD %>%
  mutate(area = st_area(.)) %>%  
  select(sm_pct, SYSID, area)
withArea

# breaking here to plot again, could link with magrittr below easily, (and maybe not even do this select)
plot(slice(withArea, 1:3))
plot(slice(withArea, 1))
plot(slice(withArea, 2))
plot(slice(withArea, 3))

# 6 and 7 have more different values
plot(slice(withArea, 6:7))
plot(slice(withArea, 6))
plot(slice(withArea, 7))


withAreaM <- withArea %>%
  group_by(SYSID) %>%
  summarize(geoMean = weighted.mean(sm_pct, as.numeric(area))) %>% # st_area returns a units object, which is good, but breaks weighted.mean
  ungroup() %>%
  select(geoMean, SYSID)
withAreaM

# did the weighting work?
withAreaM$geoMean == unweightRef$naiveMean
# Seems to

# what happened to the polygons?
plot(slice(withArea, 1:3)) # before agg
plot(slice(withArea, 6:7))

# need to get those sysids
slice(withArea, 1:3)
plot(filter(withAreaM, SYSID == 6308))
slice(withArea, 6:7)
plot(filter(withAreaM, SYSID == 6936))

# Cool. It puts them back together. SO, THIS HAS NOW GIVEN THE OPTION TO EITHER INTERSECT/SPLIT THE POLY BY RASTER, OR AVERAGE IT.


# Do it for the whole cut area --------------------------------------------
newC50 <- intC50 %>%
  mutate(area = st_area(.)) %>%  
  select(sm_pct, SYSID, area) %>%
  group_by(SYSID) %>%
  summarize(geoMean = weighted.mean(sm_pct, as.numeric(area))) %>% # st_area returns a units object, which is good, but breaks weighted.mean
  ungroup() %>%
  select(geoMean, SYSID)
newC50
plot(newC50['geoMean'])

# Cool. But can I not lose the other ANAE info?
  # Or, not have it in the first place, then join?

# Keep ANAE
  # usually I'd do a bunch of first() calls in the summarize, but distinct() might be good
# https://stackoverflow.com/questions/39092110/r-dplyr-summarize-and-retain-other-columns

# The across() in here seems to work, but it is really slow
system.time(newC50 <- intC50 %>%
  mutate(area = st_area(.)) %>%  
  # select(sm_pct, SYSID, area) %>%
  group_by(SYSID) %>%
  summarize(geoMean = weighted.mean(sm_pct, as.numeric(area)), 
            across(-c(geoMean, Shape), first)) %>% # st_area returns a units object, which is good, but breaks weighted.mean
  ungroup())
newC50
plot(newC50[c('geoMean', 'ANAE_DESC')])

# Is it faster to do them separate and rejoin?
lachCropP
timetest <- function() {newC50j <- intC50 %>%
  mutate(area = st_area(.)) %>%  
  select(sm_pct, SYSID, area) %>% # If go this way, no need to ever have the other attributes
  group_by(SYSID) %>%
  summarize(geoMean = weighted.mean(sm_pct, as.numeric(area))) %>% # st_area returns a units object, which is good, but breaks weighted.mean
  ungroup() %>%
  # st_drop_geometry() # This strikes me as dangerous, because now we're relying on the summarize working right (which it should, but there's no check)
newj <- left_join(lachCropP, newC50j, by = 'SYSID')
}
system.time(timetest)


# Well, that's certianly faster
# TODO:: put all these options together; e.g. write up an example of each, so we can pick and choose from a men

# is the while() approach correct? ----------------------------------------
notna == withAreaM$geoMean 
notna == unweightRef$naiveMean
# SORT OUT TIME. DAMN -----------------------------------------------------

# Soil, cut but with time still on it
soilT <- soilCropR[,,,1:5]
soilT
plot(soilT)

# How to do this with the time on it?

# can we turn the raster into an sf?
asSF <- st_as_sf(soilT, as_points = FALSE, merge = FALSE) # including some defaults, to be really explicit
# Turns it into polygons (good), but loses the sheets for columns (bad)
  # Might be able to work with that tho. OR... re-sheet them?
# Could use long = TRUE to make long, but that will take up more memory. I really just want it to be sheets. Why can't I do that?

# Sounds right from fn name, but isn't
polySt <- st_polygonize(soilT)

# Can we use merge() and then st_set_dimension etc as at 
# https://r-spatial.github.io/stars/articles/stars1.html

asfM <- merge(asSF) # ah, but it's an sf now
# make it a stars again
asfst <- st_as_stars(asSF)
asfst
# phew, kept the polygon

# merge() to get back to the right dims and attributes
asfstm <- merge(asfst)
asfstm

# But now the dims don't know they're time, and the attribute has lost its name
st_dimensions(asfstm)
st_dimensions(soilT)

names(asfstm)
names(soilT)

st_dimensions(soilT)[3]
st_dimensions(asfstm)[2]

# Can I just change it?
st_dimensions(asfstm)[2] <- st_dimensions(soilT)[3]
st_dimensions(asfstm)[2] # yup, though it's still called X1?
names(st_dimensions(asfstm))[2] <- names(st_dimensions(soilT))[3]

names(asfstm) <- names(soilT)

asfstm
soilT

plot(asfstm)

# the split and weighted-average ------------------------------------------

# First step is to get the intersection
intT <- st_intersection(lachCropP, asfstm)
# asldkfjasd;fl Doesn't keep the slices OR the attributes; it JUST is the shape;
# ie it splits the polygons, but doesn't put the numbers in them OR make them
# time slces
plot(intT)

# Tempted to develop the 'while' approach, but it's not area-weighted in the aggregation. 

# ah, but intT is now at least cut up, rihgt? so can I aggregate()? 
# or do the st_intersect in a loop over layers?
intT
length(unique(intT$SYSID))
isdup <- duplicated(intT$SYSID)
sum(isdup)
# Same as previous

# Try to aggregate(): hopefully this works without having to while() it
tsoilag = aggregate(asfstm, by = intT, FUN = mean, na.rm = TRUE)
tsoilag
plot(tsoilag)
# Swears
  # because the intT is chopped up, but the asfstm ISN'T, it still yields a ton of NAs
# so, we could while() it.

# OR, we could do the intersect on the flat thing with a million columns

# Let's try to loop over the time slices
# First, does it work for a single one? This SHOULD be the same damn thing as before
asfstm[,,2]

intT1 <- st_intersection(lachCropP, asfstm[,,1])
intT1
# swears some more. 
# have to turn it into sf, not a vector cube
intT1sf <- st_intersection(lachCropP, st_as_sf(asfstm[,,1]))
intT1sf

# Try to loop. THis could very well be worse than the while, (or the flat then stack), but ;et's see
intTest <- asfstm # set it up with dims we want
for (i in 1:5){
  intTest[,,i] <- st_intersection(lachCropP, st_as_sf(asfstm[,,i]))
}
# It doesn't like to write it. 

# The 'while' approach ----------------------------------------------------
# 

# Can I fill it all in?
agC50t <- aggregate(asfstm, by = intT, FUN = mean, na.rm = TRUE) # %>%
  # st_as_sf() # did this before to filter, but it screws up the whole thing with layers

# Now filtering fails. How do we subset out the NA?
# notna <- filter(agC50t, !is.na(sm_pct))
# napoly <- filter(agC50t, is.na(sm_pct))
is.na(agC50t[1,,])
is.na(agC50t[[1]])
which(is.na(agC50t[[1]]))
agC50t[[1]][4]
agC50t[[1]][which(is.na(agC50t[[1]]))]

na50t <- agC50t
na50t[[1]] <- agC50t[[1]][which(is.na(agC50t[[1]]))] # breaks dimensions. Not surprising

# can I just do it again? No, because it would need to be the y, which has to be the sf
agC50t2 <- aggregate(asfstm, by = intT, FUN = mean, na.rm = TRUE)
agC50t3 <- aggregate(asfstm, by = intT, FUN = mean, na.rm = TRUE)

# This is a pain. go back to the flat approach (below), it basically worked
# while(nrow(napoly) > 0) {
#   newag <- aggregate(soilC50, by = napoly, as_points = FALSE, FUN = mean, na.rm = TRUE) %>%
#     st_as_sf()
#   newfit <- filter(newag, !is.na(sm_pct))
#   notna <- bind_rows(notna, newfit)
#   napoly <- filter(newag, is.na(sm_pct))
#   
# }
# 
# notna
# agC50



# The flat approach: wide -------------------------------------------------
# can we turn the raster into an sf?
asSF <- st_as_sf(soilT, as_points = FALSE, merge = FALSE) # including some defaults, to be really explicit
# Turns it into polygons (good), but loses the sheets for columns (bad)

# Get the intersect
intF <- st_intersection(lachCropP, asSF) # Will get unweildy
intF
# maybe could take off all the unchanging variables, and JUST use the SYSID and geometry?
  # Might work even better if we did that Tall. Not doing it for wide, because
  # it will eat memory for duplicated values

# Now, get the averages
# The across() in here seems to work, but it is really slow
system.time(avgT <- intF %>%
              mutate(area = st_area(.)) %>%  
              # select(sm_pct, SYSID, area) %>%
              group_by(SYSID) %>%
              summarize(across(starts_with("X"), ~weighted.mean(.x, as.numeric(area)))) %>%
              # Same as above, but keeps all the other columns. Way slower
              # summarize(across(starts_with("X"), ~weighted.mean(.x, as.numeric(area))), 
              #           across(-c(starts_with("X"), Shape), first)) %>% # st_area returns a units object, which is good, but breaks weighted.mean
              ungroup())
avgT # That works, and without the additional , 
# across(-c(starts_with("X"), Shape), first)
# it loses everything but sysid (and goes WAY faster)
plot(avgT[2:6])
# Well, that works. THey're not indexable by actual time, but that's still pretty ok..
# And, if it's JUST time, stacking wouldn't be too much more memory-expensive
# (would cost one more col of sysid and one more shape/geom col. So would be best to avoid, I guess)
# I'm starting to think flat might be the way to go, because i don't really see
# how to effiicintly carry the other info around
  # or, ideally, a cube with a variable AND a sysid as attributes (or sysid as a dimension too???)
  # I do think that's likely possible, but maybe not worth it now?


# Quickly, what happens if I merge?
meravt <- merge(st_as_stars(avgT))
meravt 
# so, we'd have to cut off the sysid, and just use indices. blech.
# Though Shape should work as a ref. Let's ignore it for the moment. Flat and
# wide should be approximately as OK as sheets, and we COULD move between them i
# think, though we'd want to be careful with attributes

# The "do them separate and rejoin" version earlier works out to be what I've
# done above with the across() other attributes commented out
  # though there'd need to be a final left_join(). 
# ignore that for now then

# can I join by the shape column??
  # and can I do it as sheets?

nosys <- avgT
nosys[,2:6] # god, the indexing is unintuitive, especially because it changes between sf and stars
nosys <- select(nosys, starts_with('X'))
nosysS <- merge(st_as_stars(nosys))
nosysS
# Then could do time as above. But not really sure what this gets me. 

# COULD I MABE DO THE SAME WIT SYSID and then join?

# What about xy2sfc? ------------------------------------------------------

# What does this do?
asxy2 <- st_xy2sfc(soilT, as_points = FALSE, merge = FALSE)
# makes it a sheeted sf

#What does the intersect do?
# Get the intersect
intFxy <- st_intersection(lachCropP, asxy2) # Will get unweildy
intFxy

# So, same as before, it chops it up, but doesn't include the sm_pct

# What about tall? More memory, but easier to see -------------------------
# SLOWER TOO, stick with the above.

# can we turn the raster into an sf?
asSFt <- st_as_sf(soilT, as_points = FALSE, merge = FALSE, long = TRUE) # including some defaults, to be really explicit
# Turns it into polygons (good), but loses the sheets for columns (bad)

# Get the intersect
intFt <- st_intersection(lachCropP, asSFt) # Will get unweildy
intFt
  # time is now time, so that's good

# maybe could take off all the unchanging variables, and JUST use the SYSID and geometry?
# Might work even better if we did that Tall. Not doing it for wide, because
# it will eat memory for duplicated values

# Is it faster to do them separate and rejoin?
lachCropP
timetest <- function() {
  newC50t <- intFt %>%
  mutate(area = st_area(.)) %>%  
  select(sm_pct, SYSID, area, time) %>% # If go this way, no need to ever have the other attributes
  group_by(SYSID, time) %>%
  summarize(geoMean = weighted.mean(sm_pct, as.numeric(area))) %>% # st_area returns a units object, which is good, but breaks weighted.mean
  ungroup()
  # st_drop_geometry() # This strikes me as dangerous, because now we're relying on the summarize working right (which it should, but there's no check)
  newj <- st_join(lachCropP, newC50t, by = 'SYSID')
  return(newj)
}

system.time(timelong <- timetest())
timelong

# Huh. that's NOT actually faster when i do it right...
  # maybe

# ignore other vars to make comparable to wide, ie do the same thing as wide, but without the across(X)
system.time(avgTl <- intFt %>%
              mutate(area = st_area(.)) %>%  
              select(sm_pct, SYSID, area, time) %>% # If go this way, no need to ever have the other attributes
              group_by(SYSID, time) %>%
              summarize(geoMean = weighted.mean(sm_pct, as.numeric(area))) %>% # st_area returns a units object, which is good, but breaks weighted.mean
              ungroup())
avgTl
# that's gonna be a bear to plot
# AND it takes ~4x as long