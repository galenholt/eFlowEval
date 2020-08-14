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
lachAll <- st_transform(lachAll, whichcrs)
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
bbP = st_bbox(c(xmin = 144.4, ymin = -33.8, xmax = 144.6, ymax = -33.6), crs = whichcrs)
bbR = st_bbox(c(xmin = 144, ymin = -34.2, xmax = 145, ymax = -33.2), crs = whichcrs)
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
bbP = st_bbox(c(xmin = 144.4, ymin = -33.8, xmax = 144.6, ymax = -33.6), crs = whichcrs)
bbR = st_bbox(c(xmin = 144, ymin = -34.2, xmax = 145, ymax = -33.2), crs = whichcrs)
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
bbP = st_bbox(c(xmin = 144.4, ymin = -33.8, xmax = 144.6, ymax = -33.6), crs = whichcrs)
bbR = st_bbox(c(xmin = 144, ymin = -34.2, xmax = 145, ymax = -33.2), crs = whichcrs)
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
