# local plots of metabolism (and maybe a few other static plots)

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


# based on metabolismStepthroughPlots and allMetabolismInunTemp
  # Too many changes to just modify those files though

# Setup -------------------------------------------------------------------
# Directory to export TO
scriptOut <- file.path('strictOut', 'inundation')
if (!dir.exists(scriptOut)) {dir.create(scriptOut, recursive = TRUE)}

# Basin boundary, for clipping rasters; though likely will start with lachlan
basin <- read_sf(dsn = file.path(datDir, 'ANAE/MDB_ANAE_Aug2017/MDB_ANAE.gdb'), layer = 'MDB_Boundary') %>%
  st_cast("MULTIPOLYGON")  %>% # cleans up an issue with multisurfaces
  dplyr::select(LEVEL2NAME)# no need for other info

# Catchments
basinRef <- file.path(datOut, 'ANAEprocessed', 'ltimNoNorth.rdata')
load(basinRef)

# Temperature data in ANAEs JUST FOR THIS CATCHMENT
tempdir <- file.path(datOut, 'Tempprocessed', 'weightedMean')
load(file.path(tempdir, 'EdwardWakool_weightedMean.rdata'))
# Though to match, we probably actually want the bimonthly to match the predicting
  # Leaving the above though in case we do want to show something changing on a daily basis
tempdir <- file.path(datOut, 'Tempprocessed', 'weightedMean', 'bimonth')
load(file.path(tempdir, 'EdwardWakool_TempBimonthMean.rdata'))

# Inundation data in ANAEs JUST for this catchment
inundir <- file.path(datOut, 'inundationprocessed', 'volInun')
load(file.path(inundir, 'EdwardWakool_volInun.rdata'))

# Do those line up?
EdwardWakool_weightedMean <- st_transform(EdwardWakool_weightedMean, whichcrs)
EdwardWakool_volInun <- st_transform(EdwardWakool_volInun, whichcrs)

which(!diag(st_intersects(st_as_sf(EdwardWakool_volInun[1,1:200,1]), 
                          st_as_sf(EdwardWakool_weightedMean[1,1:200,1]), sparse = FALSE)))
which(st_drop_geometry(EdwardWakool_volInun_index[1:200, 1]) != st_drop_geometry(EdwardWakool_weightedMean_index[1:200, 1]))
# GPP and ER predictions in ANAEs JUST for this catchment 
  # also just grab the bimonthly
preddir <- file.path(datOut, 'TempAndProduction', 'Predictions', 'bimonth', 'predictxvol')
load(file.path(preddir, 'EdwardWakool_PredictxVol.rdata'))

# Read in Ramsar sites as an example
ramin <- file.path(datOut, 'WetlandBoundaries', 'ramsarMDB.rdata')
load(ramin)

# Define a bbox for Werai forest as an example
werai <- st_as_sfc(st_bbox(c(xmin = 144.4, ymin = -35.4, 
                             xmax = 144.6, ymax = -35.2), 
                           crs = 4283))


# Transform everything spatial to the right crs
allobj <- ls()
for (i in 1:length(allobj)) {
  if ('sf' %in% class(get(allobj[i])) | 
      'stars' %in% class(get(allobj[i]))) {
    assign(allobj[i], st_transform(get(allobj[i]), whichcrs))
  }
}



# cumbung <- st_bbox(c(xmin = 143.85, ymin = -34.35, xmax = 144.4, ymax = -34.05), crs = whichcrs)

# where are we?
# plot(st_as_sfc(werai))
tmap_mode("view")
# tm_shape(werai) + tm_borders()

# does that match ramsar?

ramsarW <- filter(ramsarMDB, WNAME == 'Werai Forest')
# tm_shape(ramsarW) + tm_borders()  + tm_shape(werai) + tm_borders()

# yeah, roughly. It's a subset.

# Single ramsar polygon
# TURN ON ESRI WORLD TOPO MAP TO SEE WHERE WE ARE IN THE LAYERS MENU THING.
ramsarW1 <- summarise(ramsarW)
tm_shape(ramsarW1) + tm_borders() + tm_shape(werai) + tm_borders()
# Cool. and the box does a better job hitting the state forest, Ramsar is bigger

# crop down to the ramsar polygon- should I crop or intersect? Not entirely
# sure, since I don't know how much wetland is outside ramsar. So I guess crop
# and look

# fails because the stars is a geom stars, not raster
# weraiCropTemp <- st_crop(EdwardWakool_TempBimonthMean, ramsarW1)
weraiCropTemp <- EdwardWakool_TempBimonthMean[ramsarW1]
# interesting. that actually DID crop it to the polygon. Maybe because they're polygons?
# plot(weraiCropTemp[,,10])

# keep looking at what's outside ramsar
werbb <- st_as_sfc(st_bbox(ramsarW1))
weraiCropTempBox <- EdwardWakool_TempBimonthMean[werbb]
# plot(weraiCropTempBox[,,10])
# # There's some, but it's mostly in the ramsar, and the ramsar view is sure cleaner
# 
# # Sure starting to look like a dashboard...
# # Will need to apply these with a general mapping
# temppal <- divergingx_hcl(9, palette = 'Spectral',
#                           rev = TRUE)
# midtemp <- median(weraiCropTemp[[1]], na.rm = TRUE)
# tm_shape(weraiCropTemp[,,10]) + tm_fill(palette = temppal, midpoint = midtemp) # tm_fill(palette = 'RdYlBu', rev = TRUE)

# Let's do the same for the inundation and the outputs, and we might be almost
# there. Will need to do something temporal too, but a "last two months (or x period) summary dashboard" might actually be really good
weraiCropInun <- EdwardWakool_volInun[ramsarW1]

# # MAKE INUNDATION IN something other than liters
# # weraiCropInun <- weraiCropInun/1000000
# # names(weraiCropInun) <- 'megalitersInundation'
# 
# 
# inunpal <- divergingx_hcl(9, palette = 'Earth',
#                       rev = FALSE)
# # 10th slice won't match in time, sort out using actual times
# midinun <- median(weraiCropInun[[1]], na.rm = TRUE)
# # I used log(inundation) for the basin-scale stuff, but not sure that's so useful here.
# # tm_shape(log(weraiCropInun[,,100])) + tm_fill(palette = inunpal, midpoint = 0)
# # tm_shape(weraiCropInun[,,100]) + 
# #   tm_fill(col = 'depth', palette = inunpal, midpoint = midinun)
# # The log is better, but is confusing. Can we do even better?
# 
# # Can't use col for stars, but
# tm_shape(st_as_sf(1+weraiCropInun[,,100])) + 
#   tm_fill(col = '2004-07-01 10:00:00', palette = inunpal, 
#           midpoint = midinun, style = 'log10_pretty')

# That's pretty good. Now the relevant predictions
# Let's do the same for the inundation and the outputs, and we might be almost
# there. Will need to do something temporal too, but a "last two months (or x period) summary dashboard" might actually be really good
weraiCropPred <- EdwardWakool_PredictxVol[ramsarW1]
# gpppal <- sequential_hcl(9, palette = 'Emrld', rev = TRUE)
# erpal <- sequential_hcl(9, palette = 'Purples', rev = TRUE)
# # there might be an advantage of some transformations here, but need to sort out
# # a consistent color scale- right now, it smears out whatever it has, instead of
# # knowing what the full range should be for comparing across days
#   # ggplot could do it, but I'm kind of liking tmap. We'll see
# tm_shape(weraiCropPred[1,,10]) +
#   tm_fill(palette = gpppal) # tm_fill(palette = 'RdYlBu', rev = TRUE)
# 
# tm_shape(weraiCropPred[3,,10]) +
#   tm_fill(palette = erpal)

# Then what? 

# a few timeslices grabbed by time, 
# to make this easier, cut the hours off the times
weraiCropInun <- st_set_dimensions(weraiCropInun, which = 'time', 
                                values = as.Date(st_get_dimension_values(weraiCropInun, which = 'time')))
weraiCropTemp <- st_set_dimensions(weraiCropTemp, which = 'time', 
                                   values = as.Date(st_get_dimension_values(weraiCropTemp, which = 'time')))
weraiCropPred <- st_set_dimensions(weraiCropPred, which = 'time', 
                                   values = as.Date(st_get_dimension_values(weraiCropPred, which = 'time')))


# Now let's have a consistent way to make the plots
  # Does setting the breaks allow me to establish breaks from the whole dataset,
  # and avoid rescaling figs?


# midinun <- median(weraiCropInun[[1]], na.rm = TRUE)
# 
# gpppal <- sequential_hcl(9, palette = 'Emrld', rev = TRUE)


# Set breaks and labels
  # breaks up to 10, because 9 seems to be the standard for how long a palette
  # can be and stay discernible

# Temp
tempbreaks <- labeling::extended(m = 10,
                                 dmin = min(weraiCropTemp[[1]], na.rm = TRUE),
                                 dmax = max(weraiCropTemp[[1]], na.rm = TRUE))
temppal <- divergingx_hcl(length(tempbreaks)-1, palette = 'Spectral',
                          rev = TRUE)
midtemp <- median(weraiCropTemp[[1]], na.rm = TRUE)

# inundation
inunbreaks <- labeling::extended(m = 10,
                                 dmin = min(weraiCropInun[[1]], na.rm = TRUE),
                                 dmax = max(weraiCropInun[[1]], na.rm = TRUE))
inunpal <- divergingx_hcl(length(inunbreaks)-1, palette = 'Earth',
                          rev = FALSE)
midinun <- median(weraiCropInun[[1]], na.rm = TRUE)

inunbreaks_log <- labeling::extended(m = 10,
                                 dmin = min(log10(1+weraiCropInun[[1]]), na.rm = TRUE),
                                 dmax = ceiling(max(log10(1+weraiCropInun[[1]]), na.rm = TRUE)))

inunpal_log <- divergingx_hcl(length(inunbreaks_log)-1, palette = 'Earth',
                          rev = FALSE)
midinun_log <- median(log10(1+weraiCropInun[[1]]), na.rm = TRUE)

# Make pretty labels. Breaks CONTAIN the endpoints
inunlabels_log <- 10^inunbreaks_log
inunlabels_log <- format(inunlabels_log, big.mark=",", scientific=FALSE, trim = TRUE)
inunstart <- inunlabels_log[1:(length(inunlabels_log)-1)]
inunstart[1] <- "0" # instead of 1
inunlabels_log <- paste0(inunstart, ' to ', inunlabels_log[2:length(inunlabels_log)])
inunlabels_log

# ER
erbreaks <- labeling::extended(m = 10,
                               dmin = min(weraiCropPred[[3]], na.rm = TRUE),
                               dmax = max(weraiCropPred[[3]], na.rm = TRUE))
erbreaks_log <- labeling::extended(m = 10, 
                                   dmin = min(log10(1+weraiCropPred[[3]]), na.rm = TRUE), 
                                   dmax = ceiling(max(log10(1 + weraiCropPred[[3]]), na.rm = TRUE)))

# and those breaks might not quite yield 10, so maximise the palette differences
erpal_log <- sequential_hcl(length(erbreaks_log)-1, palette = 'Purples', rev = TRUE)

# Make pretty labels. Breaks CONTAIN the endpoints
erlabels_log <- 10^erbreaks_log
erlabels_log <- format(erlabels_log, big.mark=",", scientific=FALSE, trim = TRUE)
erstart <- erlabels_log[1:(length(erlabels_log)-1)]
erstart[1] <- "0" # instead of 1
erlabels_log <- paste0(erstart, ' to ', erlabels_log[2:length(erlabels_log)])
erlabels_log



# GPP
gppbreaks <- labeling::extended(m = 10,
                               dmin = min(weraiCropPred[[1]], na.rm = TRUE),
                               dmax = max(weraiCropPred[[1]], na.rm = TRUE))
gppbreaks_log <- labeling::extended(m = 10, 
                                   dmin = min(log10(1+weraiCropPred[[1]]), na.rm = TRUE), 
                                   # adding ceiling because this misses the actual max
                                   dmax = ceiling(max(log10(1 + weraiCropPred[[1]]), na.rm = TRUE)))

# and those breaks might not quite yield 10, so maximise the palette differences
gpppal_log <- sequential_hcl(length(gppbreaks_log)-1, palette = 'Emrld', rev = TRUE)

# Make pretty labels. Breaks CONTAIN the endpoints
gpplabels_log <- 10^gppbreaks_log
gpplabels_log <- format(gpplabels_log, big.mark=",", scientific=FALSE, trim = TRUE)
gppstart <- gpplabels_log[1:(length(gpplabels_log)-1)]
gppstart[1] <- "0" # instead of 1
gpplabels_log <- paste0(gppstart, ' to ', gpplabels_log[2:length(gpplabels_log)])
gpplabels_log



# Plots- let's make these consistently

# make a list of the potential times- don't use inundation, it goes back too far
availDays <- st_get_dimension_values(weraiCropTemp, which = 'time')
availDays


datewanted <- as.character(availDays[18]) # just pick something for now

# Temp plot
temp_sf <- weraiCropTemp %>% 
  st_as_sf() %>% 
  select(all_of(datewanted)) %>%
  rename(Temp = 1) 
temp_tm <- temp_sf %>%
  tm_shape() + 
  tm_fill(col = 'Temp', palette = temppal,
          midpoint = midtemp,
          breaks = tempbreaks) +
  tm_layout(title = paste0('Two months preceding ', datewanted))
temp_tm 

# Inundation 
inun_sf <- weraiCropInun %>% 
  st_as_sf() %>% 
  select(all_of(datewanted)) %>%  
  rename(InundationVolume = 1) %>%
  mutate(logVolume = log10(1+InundationVolume)) # don't actually log, that happens in the plot
inun_tm <- inun_sf %>%
  tm_shape() + 
  tm_fill(col = 'logVolume', palette = inunpal_log,
          midpoint = midinun_log, 
          breaks = inunbreaks_log,
          labels = inunlabels_log,
          title = 'Volume Inundation') +
  tm_layout(title = paste0('Two months preceding ', datewanted))
inun_tm  

# GPP
gpp_sf <- weraiCropPred[1,,] %>% 
  st_as_sf() %>% 
  select(all_of(datewanted)) %>%
  rename(GPP = 1) %>%
  mutate(logGPP = log10(1+GPP))
gpp_tm <- gpp_sf %>%
  tm_shape() +
  tm_fill(col = 'logGPP', palette = gpppal_log,
          breaks = gppbreaks_log,
          labels = gpplabels_log,
          title = 'GPP') +
  tm_layout(title = paste0('Two months preceding ', datewanted))
gpp_tm

# ER plot
er_sf <- weraiCropPred[3,,] %>% 
  st_as_sf() %>% 
  select(all_of(datewanted)) %>%
  rename(ER = 1) %>%
  mutate(logER = log10(1+ER))
er_tm <- er_sf %>%
  tm_shape() +
  tm_fill(col = 'logER', palette = erpal_log,
          breaks = erbreaks_log,
          labels = erlabels_log,
          title = 'ER') +
  tm_layout(title = paste0('Two months preceding ', datewanted))
er_tm

# Can I make a big facet thing
tmap_arrange(temp_tm, inun_tm,
             gpp_tm, er_tm)
# Yep. Though two might make more sense- one for inputs, one for predictions


# write it up
# Sure is close to a dashboard


