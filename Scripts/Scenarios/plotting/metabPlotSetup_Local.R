# local plots of metabolism (and maybe a few other static plots)

# Libraries and system setup
source('directorySet.R')

# Let's get libraries here, then sort out git then sort out making this a library so we don't have to deal with all the library crap
# library(sp)
# library(rgeos)
# library(here)
library(tidyverse)
library(sf)
library(stars)
library(doFuture)
library(foreach)
plan(sequential)
# library(tmap)
# library(transformr)
# library(gganimate)
# library(viridis)
# library(colorspace)
# library(doFuture)


# Set the crs
whichcrs <- 3577
# directory


# based on metabolismStepthroughPlots and allMetabolismInunTemp
  # Too many changes to just modify those files though

# Setup -------------------------------------------------------------------
# Directory to export TO
scriptOut <- file.path('strictOut', 'metabolism', 'local')
if (!dir.exists(scriptOut)) {dir.create(scriptOut, recursive = TRUE)}

# Basin boundary, for clipping rasters; though likely will start with lachlan
basin <- read_sf(dsn = file.path(datDir, 'ANAE/MDB_ANAE_Aug2017/MDB_ANAE.gdb'), layer = 'MDB_Boundary') %>%
  st_cast("MULTIPOLYGON")  %>% # cleans up an issue with multisurfaces
  dplyr::select(LEVEL2NAME)# no need for other info

# Catchments
basinRef <- file.path(datOut, 'ANAEprocessed', 'ltimNoNorth.rdata')
load(basinRef)

# Temperature data in ANAEs JUST FOR THIS CATCHMENT
# tempdir <- file.path(datOut, 'Tempprocessed', 'weightedMean')
# load(file.path(tempdir, 'EdwardWakool_weightedMean.rdata'))

# To match, we actually want the bimonthly to match the predicting
  # Leaving the above though in case we do want to show something changing on a daily basis
tempdir <- file.path(datOut, 'Tempprocessed', 'weightedMean', 'bimonth')
load(file.path(tempdir, 'EdwardWakool_TempBimonthMean.rdata'))

# Inundation data in ANAEs JUST for this catchment
inundir <- file.path(datOut, 'inundationprocessed', 'volInun')
load(file.path(inundir, 'EdwardWakool_volInun.rdata'))

# Do those line up?
# EdwardWakool_weightedMean <- st_transform(EdwardWakool_weightedMean, whichcrs)
EdwardWakool_volInun <- st_transform(EdwardWakool_volInun, whichcrs)
# 
# which(!diag(st_intersects(st_as_sf(EdwardWakool_volInun[1,1:200,1]), 
#                           st_as_sf(EdwardWakool_weightedMean[1,1:200,1]), sparse = FALSE)))
# which(st_drop_geometry(EdwardWakool_volInun_index[1:200, 1]) != st_drop_geometry(EdwardWakool_weightedMean_index[1:200, 1]))
# GPP and ER predictions in ANAEs JUST for this catchment 
  # also just grab the bimonthly
preddirER <- file.path(datOut, 'TempAndProduction', 'Predictions', 'logERdaysvalleys',
                       'bimonth', 'predictxvol')
preddirGPP <- file.path(datOut, 'TempAndProduction', 'Predictions', 'logGPPdaysvalleys',
                        'bimonth', 'predictxvol')

load(file.path(preddirER, 'EdwardWakool_logERdaysvalleys_PredictxVol.rdata'))
load(file.path(preddirGPP, 'EdwardWakool_logGPPdaysvalleys_PredictxVol.rdata'))

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


# UNITS NOTE --------------------------------------------------------------

# Temps are in C

# Volumes are in m^3.
# GPP and ER have been multiplied by volume. So any shifts in
# vol units should be mirrored in those units
#
# GPP is in mg O_2 L^{-1} day^{-1} . but the predictions have been multiplied by vol, but at the per liter rate, yielding mg02/D but 1,000x too little
# ER is in mg O_2 L^{-1} day^{-1} 

# That per day part is tricky. I could multiply by days in the period, but the
# inundation is a max extent. SO maybe I'll just say it's the daily rate at the
# max extent. Without knowing filling patterns that'll work out to just be
# exactly the same as 1/60th the value of multiplying it out

## TODO PROPAGATE THESE UNITS THROUGH EVERYTHING AND CHECK
# a m3 is 0.001 of a megaliter (1,000 vs 1,000,000 liters)
EdwardWakool_volInunML <- EdwardWakool_volInun * 0.001
names(EdwardWakool_volInunML) <- 'volumeML'

EdwardWakool_logERdaysvalleys_PredictxVolUnits <- EdwardWakool_logERdaysvalleys_PredictxVol *
  1000 * # because it had multiplied per L estimates by m3, and there are 1,000 L. Now we're in mg02/day
  0.000001 # convert from mg to kg. If convert to g would be 0.001 and effectively reverse the above
  
names(EdwardWakool_logERdaysvalleys_PredictxVolUnits) <- 
  str_replace(names(EdwardWakool_logERdaysvalleys_PredictxVolUnits),
              pattern = 'predict_x_vol_ERdaysvalleys', 
              replacement = 'predicted_ER_kg02perday_at_max_inun')

EdwardWakool_logGPPdaysvalleys_PredictxVolUnits <- EdwardWakool_logGPPdaysvalleys_PredictxVol *
  1000 * # because it had multiplied per L estimates by m3, and there are 1,000 L. Now we're in mg02/day
  0.000001 # convert from mg to kg. If convert to g would be 0.001 and effectively reverse the above

names(EdwardWakool_logGPPdaysvalleys_PredictxVolUnits) <- 
  str_replace(names(EdwardWakool_logGPPdaysvalleys_PredictxVolUnits),
              pattern = 'predict_x_vol_GPPdaysvalleys', 
              replacement = 'predicted_GPP_kg02perday_at_max_inun')

# remove the old units versions to avoid confustion
rm(EdwardWakool_volInun, EdwardWakool_logGPPdaysvalleys_PredictxVol, EdwardWakool_logERdaysvalleys_PredictxVol)

# cumbung <- st_bbox(c(xmin = 143.85, ymin = -34.35, xmax = 144.4, ymax = -34.05), crs = whichcrs)

# where are we?
# plot(st_as_sfc(werai))
# tmap_mode("view")
# tm_shape(werai) + tm_borders()

# does that match ramsar?

ramsarW <- filter(ramsarMDB, WNAME == 'Werai Forest')
# tm_shape(ramsarW) + tm_borders()  + tm_shape(werai) + tm_borders()

# yeah, roughly. It's a subset.

# Single ramsar polygon
# TURN ON ESRI WORLD TOPO MAP TO SEE WHERE WE ARE IN THE LAYERS MENU THING.
ramsarW1 <- summarise(ramsarW)
# tm_shape(ramsarW1) + tm_borders() + tm_shape(werai) + tm_borders()
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
weraiCropInun <- EdwardWakool_volInunML[ramsarW1]

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
weraiCropPredER <- EdwardWakool_logERdaysvalleys_PredictxVolUnits[ramsarW1]
weraiCropPredGPP <- EdwardWakool_logGPPdaysvalleys_PredictxVolUnits[ramsarW1]

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
weraiCropPredER <- st_set_dimensions(weraiCropPredER, which = 'time',
                                   values = as.Date(st_get_dimension_values(weraiCropPredER, which = 'time')))
weraiCropPredGPP <- st_set_dimensions(weraiCropPredGPP, which = 'time',
                                   values = as.Date(st_get_dimension_values(weraiCropPredGPP, which = 'time')))


# # Now let's have a consistent way to make the plots
#   # Does setting the breaks allow me to establish breaks from the whole dataset,
#   # and avoid rescaling figs?
#
#
# # midinun <- median(weraiCropInun[[1]], na.rm = TRUE)
# #
# # gpppal <- sequential_hcl(9, palette = 'Emrld', rev = TRUE)
#
#
# # Set breaks and labels
#   # breaks up to 10, because 9 seems to be the standard for how long a palette
#   # can be and stay discernible
#
# # Temp
# tempbreaks <- labeling::extended(m = 10,
#                                  dmin = min(weraiCropTemp[[1]], na.rm = TRUE),
#                                  dmax = max(weraiCropTemp[[1]], na.rm = TRUE))
# temppal <- divergingx_hcl(length(tempbreaks)-1, palette = 'Spectral',
#                           rev = TRUE)
# midtemp <- median(weraiCropTemp[[1]], na.rm = TRUE)
#
# tempbreaktext <- format(tempbreaks)
# tempstart <- tempbreaktext[1:(length(tempbreaktext)-1)]
# templabels <- paste0(tempstart, ' to ', tempbreaktext[2:length(tempbreaktext)])
#
# # inundation
# inunbreaks <- labeling::extended(m = 10,
#                                  dmin = min(weraiCropInun[[1]], na.rm = TRUE),
#                                  dmax = max(weraiCropInun[[1]], na.rm = TRUE))
# inunpal <- divergingx_hcl(length(inunbreaks)-1, palette = 'Earth',
#                           rev = FALSE)
# midinun <- median(weraiCropInun[[1]], na.rm = TRUE)
#
# inunbreaks_log <- labeling::extended(m = 10,
#                                  dmin = min(log10(1+weraiCropInun[[1]]), na.rm = TRUE),
#                                  dmax = ceiling(max(log10(1+weraiCropInun[[1]]), na.rm = TRUE)))
#
# inunpal_log <- divergingx_hcl(length(inunbreaks_log)-1, palette = 'Earth',
#                           rev = FALSE)
# midinun_log <- median(log10(1+weraiCropInun[[1]]), na.rm = TRUE)
#
# # Make pretty labels. Breaks CONTAIN the endpoints
# inunlabels_log <- 10^inunbreaks_log
# inunlabels_log <- format(inunlabels_log, big.mark=",",
#                          scientific=FALSE, trim = TRUE, digits = 0)
# inunstart <- inunlabels_log[1:(length(inunlabels_log)-1)]
# inunstart[1] <- "0" # instead of 1
# inunlabels_log <- paste0(inunstart, ' to ', inunlabels_log[2:length(inunlabels_log)])
# inunlabels_log
#
# # ER Use the mean estimate to set the values? Or should I use the min and max?
# # Might depend on what I want to show. If min and max, will need to change to
# # [[2]] for the dmin and [[3]] for the dmax
# erbreaks <- labeling::extended(m = 10,
#                                dmin = min(weraiCropPredER[[1]], na.rm = TRUE),
#                                dmax = max(weraiCropPredER[[1]], na.rm = TRUE))
# erbreaks_log <- labeling::extended(m = 10,
#                                    dmin = min(log10(1+weraiCropPredER[[1]]), na.rm = TRUE),
#                                    dmax = ceiling(max(log10(1 + weraiCropPredER[[1]]), na.rm = TRUE)))
#
# # and those breaks might not quite yield 10, so maximise the palette differences
# erpal_log <- sequential_hcl(length(erbreaks_log)-1, palette = 'Purples', rev = TRUE)
#
# # Make pretty labels. Breaks CONTAIN the endpoints
# erlabels_log <- 10^erbreaks_log
# erlabels_log <- format(erlabels_log, big.mark=",",
#                        scientific=FALSE, trim = TRUE, digits = 0)
# erstart <- erlabels_log[1:(length(erlabels_log)-1)]
# erstart[1] <- "0" # instead of 1
# erlabels_log <- paste0(erstart, ' to ', erlabels_log[2:length(erlabels_log)])
# erlabels_log
#
#
#
# # GPP
# # as above, might need to extend the top and bottom if I use the uncertainty
# gppbreaks <- labeling::extended(m = 10,
#                                dmin = min(weraiCropPredGPP[[1]], na.rm = TRUE),
#                                dmax = max(weraiCropPredGPP[[1]], na.rm = TRUE))
# gppbreaks_log <- labeling::extended(m = 10,
#                                    dmin = min(log10(1+weraiCropPredGPP[[1]]), na.rm = TRUE),
#                                    # adding ceiling because this misses the actual max
#                                    dmax = ceiling(max(log10(1 + weraiCropPredGPP[[1]]), na.rm = TRUE)))
#
# # and those breaks might not quite yield 10, so maximise the palette differences
# gpppal_log <- sequential_hcl(length(gppbreaks_log)-1, palette = 'Emrld', rev = TRUE)
#
# # Make pretty labels. Breaks CONTAIN the endpoints
# gpplabels_log <- 10^gppbreaks_log
# gpplabels_log <- format(gpplabels_log, big.mark=",",
#                         scientific=FALSE, trim = TRUE, digits = 0)
# gppstart <- gpplabels_log[1:(length(gpplabels_log)-1)]
# gppstart[1] <- "0" # instead of 1
# gpplabels_log <- paste0(gppstart, ' to ', gpplabels_log[2:length(gpplabels_log)])
# gpplabels_log
#
#
#
# # Plots- let's make these consistently
#
# # make a list of the potential times- don't use inundation, it goes back too far
# availDays <- st_get_dimension_values(weraiCropTemp, which = 'time')
# availDays <- availDays[2:length(availDays)] # since the first temp is NA
#
#
# # datewanted <- as.character(availDays[28]) # just pick something for now
#
# # Do these as functions
# tempfun <- function(datewanted, titled = TRUE, plotPkg = 'tmap') {
#   # Temp plot
#   temp_sf <- weraiCropTemp %>%
#     st_as_sf() %>%
#     select(all_of(datewanted)) %>%
#     rename(Temp = 1)
#
#   legendlabel <- 'Temp C'
#
#   if (plotPkg == 'tmap') {
#     temp_tm <- temp_sf %>%
#       tm_shape() +
#       tm_fill(col = 'Temp', palette = temppal,
#               midpoint = midtemp,
#               breaks = tempbreaks,
#               title = legendlabel)
#
#
#     if (titled) {
#       temp_tm <- temp_tm +
#         tm_layout(title = paste0('Two months preceding ', datewanted))
#     }
#
#     return(temp_tm)
#   }
#
#   if (plotPkg == 'ggplot') {
#     temp_gg <- ggplot() +
#       geom_sf(data = temp_sf, mapping = aes(fill = Temp), color = NA) +
#       # geom_sf_label(data = ltimNoNorth, mapping = aes(label = ValleyName)) +
#       coord_sf() +
#       # Closest to the tmap
#       scale_fill_stepsn(colors = temppal,
#                         breaks = tempbreaks[2:length(tempbreaks)],
#                         limits = c(min(tempbreaks), max(tempbreaks)),
#                         labels = templabels,
#                         guide = 'legend',
#                         name = legendlabel)
#     # some other scale options
#     # scale_fill_stepsn(colors = temppal, breaks = tempbreaks,
#     #                   limits = c(min(tempbreaks), max(tempbreaks)),
#     #                   guide = 'legend')
#     # scale_fill_binned_divergingx(palette = 'Spectral',rev = TRUE,
#     #                              mid = median(weraiCropTemp[[1]], na.rm = TRUE),
#     #                              breaks = tempbreaks,
#     #                              limits = c(min(tempbreaks), max(tempbreaks)))
#     if (titled) {
#       temp_gg <- temp_gg +
#         ggtitle(paste0('Two months preceding ', datewanted))
#     }
#     return(temp_gg)
#   }
#
# }
#
#
# # tempfun(datewanted)
#
# # Inundation
# inunfun <- function(datewanted, titled = TRUE, plotPkg = 'tmap') {
#   inun_sf <- weraiCropInun %>%
#     st_as_sf() %>%
#     select(all_of(datewanted)) %>%
#     rename(InundationVolume = 1) %>%
#     mutate(logVolume = log10(1+InundationVolume)) # don't actually log, that happens in the plot
#
#   legendlabel <- 'ML Inundation\nat max extent'
#
#   if (plotPkg == 'tmap') {
#   inun_tm <- inun_sf %>%
#     tm_shape() +
#     tm_fill(col = 'logVolume', palette = inunpal_log,
#             midpoint = midinun_log,
#             breaks = inunbreaks_log,
#             labels = inunlabels_log,
#             title = legendlabel)
#   if (titled) {
#     inun_tm <- inun_tm +
#       tm_layout(title = paste0('Two months preceding ', datewanted))
#   }
#   return(inun_tm)
#   }
#
#   if (plotPkg == 'ggplot') {
#     inun_gg <- ggplot() +
#       geom_sf(data = inun_sf, mapping = aes(fill = logVolume), color = NA) +
#       # geom_sf_label(data = ltimNoNorth, mapping = aes(label = ValleyName)) +
#       coord_sf() +
#       # Closest to the tmap
#       scale_fill_stepsn(colors = inunpal_log,
#                         breaks = inunbreaks_log[2:length(inunbreaks_log)],
#                         limits = c(min(inunbreaks_log), max(inunbreaks_log)),
#                         labels = inunlabels_log,
#                         guide = 'legend',
#                         name = legendlabel)
#     # inun_gg
#     if (titled) {
#       inun_gg <- inun_gg +
#         ggtitle(paste0('Two months preceding ', datewanted))
#     }
#     return(inun_gg)
#   }
# }
# # inunfun(datewanted)
#
#
# # GPP
# # I guess for the moment without uncertainty
# gppfun <- function(datewanted, titled = TRUE, plotPkg = 'tmap') {
#   gpp_sf <- weraiCropPredGPP[1,,] %>%
#     st_as_sf() %>%
#     select(all_of(datewanted)) %>%
#     rename(GPP = 1) %>%
#     mutate(logGPP = log10(1+GPP))
#
#   # getting the 02 subscripted and linebreaks to work in both tmap and ggplot is
#   # a huge pain, so I'm giving up and just doing something that kind of works
#   legendlabel <- 'GPP (kg 02/day)\nat max extent'
#
#   if (plotPkg == 'tmap') {
#   gpp_tm <- gpp_sf %>%
#     tm_shape() +
#     tm_fill(col = 'logGPP', palette = gpppal_log,
#             breaks = gppbreaks_log,
#             labels = gpplabels_log,
#             title = legendlabel)
#   if (titled) {
#     gpp_tm <- gpp_tm +
#       tm_layout(title = paste0('Two months preceding ', datewanted))
#   }
#   return(gpp_tm)
#   }
#
#
#   if (plotPkg == 'ggplot') {
#     gpp_gg <- ggplot() +
#       geom_sf(data = gpp_sf, mapping = aes(fill = logGPP), color = NA) +
#       # geom_sf_label(data = ltimNoNorth, mapping = aes(label = ValleyName)) +
#       coord_sf() +
#       # Closest to the tmap
#       scale_fill_stepsn(colors = gpppal_log,
#                         breaks = gppbreaks_log[2:length(gppbreaks_log)],
#                         limits = c(min(gppbreaks_log), max(gppbreaks_log)),
#                         labels = gpplabels_log,
#                         guide = 'legend',
#                         name = legendlabel)
#     # gpp_gg
#
#     if (titled) {
#       gpp_gg <- gpp_gg +
#         ggtitle(paste0('Two months preceding ', datewanted))
#     }
#     return(gpp_gg)
#   }
#
# }
# # gppfun(datewanted)
#
#
# # ER plot
# erfun <- function(datewanted, titled = TRUE, plotPkg = 'tmap') {
#
#   er_sf <- weraiCropPredER[1,,] %>%
#     st_as_sf() %>%
#     select(all_of(datewanted)) %>%
#     rename(ER = 1) %>%
#     mutate(logER = log10(1+ER))
#
#   legendlabel <- 'ER (kg 02/day)\nat max extent'
#
#   if (plotPkg == 'tmap') {
#   er_tm <- er_sf %>%
#     tm_shape() +
#     tm_fill(col = 'logER', palette = erpal_log,
#             breaks = erbreaks_log,
#             labels = erlabels_log,
#             title = legendlabel)
#   if (titled) {
#     er_tm <- er_tm +
#       tm_layout(title = paste0('Two months preceding ', datewanted))
#   }
#
#   return(er_tm)
#   }
#
#
#   if (plotPkg == 'ggplot') {
#     er_gg <- ggplot() +
#       geom_sf(data = er_sf, mapping = aes(fill = logER), color = NA) +
#       # geom_sf_label(data = ltimNoNorth, mapping = aes(label = ValleyName)) +
#       coord_sf() +
#       # Closest to the tmap
#       scale_fill_stepsn(colors = erpal_log,
#                         breaks = erbreaks_log[2:length(erbreaks_log)],
#                         limits = c(min(erbreaks_log), max(erbreaks_log)),
#                         labels = erlabels_log,
#                         guide = 'legend',
#                         name = legendlabel)
#     # er_gg
#     if (titled) {
#       er_gg <- er_gg +
#         ggtitle(paste0('Two months preceding ', datewanted))
#     }
#     return(er_gg)
#   }
# }
# # erfun(datewanted)
#
#
# # Can I make a big facet thing
#
# # skip for the ggplot stuff
# inputsfun <- function(datewanted) {
#   tmap_arrange(tempfun(datewanted), inunfun(datewanted))
# }
# # inputsfun(datewanted)
#
# predictfun <- function(datewanted) {
#   tmap_arrange(gppfun(datewanted), erfun(datewanted))
# }
#
# allfun <- function(datewanted) {
#   tmap_arrange(tempfun(datewanted), inunfun(datewanted),
#                gppfun(datewanted), erfun(datewanted))
# }
#
#
# # find a good day.
# # 18 is OK, there';s some variation.
# # 16 has good variation in inundation and outcomes, but temp is all cold. 17 is a bit better
# availDays <- st_get_dimension_values(weraiCropTemp, which = 'time')
# datewanted <- as.character(availDays[17]) # just pick something for now
#
# allfun(datewanted)
# # Yep. Though two might make more sense- one for inputs, one for predictions
#
#
# # Test whether there's a faster way to serve the data
#   # This doesn't work in shiny, not sure why
# # fastserveInun <- foreach(i = 1:5) %do% {
# #   inunfun(datewanted = as.character(availDays[i]))
# # }
# #
# # # Getting argument length zero again
# # fastgrab <- function(datewanted) {
# #   fastserveInun[which(as.character(availDays) == datewanted)]
# # }

# # Static plots for paper --------------------------------------------------
#
# # not sure if I should just change the tmap_mode or to use ggplot and geom_stars
#
# # I guess look at using tmap, that'd be easier than re-writing the above (maybe?)
# tmap_mode('plot')
# allfun(datewanted)
# # That actually looks OK. Would be nice to have a bit of context though
# # I tried adding werai outline, but the wetlands overhang it so it's just super confusing and ugly
# # tmap_arrange(tm_shape(ramsarW1) + tm_polygons(col = NA, border.col = 'black') +
# #                tempfun(datewanted),
# #              tm_shape(ramsarW1) + tm_polygons(col = NA, border.col = 'black') +
# #                inunfun(datewanted, titled = FALSE) ,
# #              tm_shape(ramsarW1) + tm_polygons(col = NA, border.col = 'black') +
# #                gppfun(datewanted, titled = FALSE),
# #              tm_shape(ramsarW1) + tm_polygons(col = NA, border.col = 'black') +
# #                erfun(datewanted, titled = FALSE))
#
# # So, I think just put a grey background on it.
# # probably pull the title off but it's nice to have as reference while i'm making these
#
# tm_grid_static <- tmap_arrange(tempfun(datewanted) + tm_layout(bg.color = "grey85"),
#              inunfun(datewanted, titled = FALSE) + tm_layout(bg.color = "grey85"),
#              gppfun(datewanted, titled = FALSE) + tm_layout(bg.color = "grey85"),
#              erfun(datewanted, titled = FALSE) + tm_layout(bg.color = "grey85"))
# tm_grid_static
#
# # I think for completeness let's make a ggplot version too and see which is better.
# # takes more work, but have more control (probably just because I speak ggplot)
# gg_grid_static <- ggpubr::ggarrange(
#   tempfun(datewanted, plotPkg = 'ggplot') +
#     guides(fill = guide_legend(title.position = 'top')) +
#     theme_grey(base_size = 8) +
#     theme(legend.position = 'bottom',
#           legend.background = element_blank(),
#           legend.key.size = unit(0.3, 'cm'), ),
#   inunfun(datewanted, titled = FALSE, plotPkg = 'ggplot') +
#     guides(fill = guide_legend(title.position = 'top')) +
#     theme_grey(base_size = 8) +
#     theme(legend.position = 'bottom',
#           legend.background = element_blank(),
#           legend.key.size = unit(0.3, 'cm'), ),
#   gppfun(datewanted, titled = FALSE, plotPkg = 'ggplot') +
#     guides(fill = guide_legend(title.position = 'top')) +
#     theme_grey(base_size = 8) +
#     theme(legend.position = 'bottom',
#           legend.background = element_blank(),
#           legend.key.size = unit(0.3, 'cm'), ),
#   erfun(datewanted, titled = FALSE, plotPkg = 'ggplot') +
#     guides(fill = guide_legend(title.position = 'top')) +
#     theme_grey(base_size = 8) +
#     theme(legend.position = 'bottom',
#           legend.background = element_blank(),
#           legend.key.size = unit(0.3, 'cm')),
#   ncol = 2, nrow = 2)
# gg_grid_static
#
#
# # Can I print those?
# pdf(file.path(scriptOut, 'Werai_tm.pdf'),
#     onefile = FALSE, height = 12/2.54, width = 16/2.54, useDingbats = FALSE)
# print(tm_grid_static)
# dev.off()
#
# png(file.path(scriptOut, 'Werai_tm.png'),
#     height = 12/2.54, width = 16/2.54, units = 'in', res = 300)
# print(tm_grid_static)
# dev.off()
#
# # Can I print those?
# pdf(file.path(scriptOut, 'Werai_gg.pdf'),
#     onefile = FALSE, height = 12/2.54, width = 16/2.54, useDingbats = FALSE)
# print(gg_grid_static)
# dev.off()
#
# png(file.path(scriptOut, 'Werai_gg.png'),
#     height = 12/2.54, width = 16/2.54, units = 'in', res = 300)
# print(gg_grid_static)
# dev.off()
#
#
# # Uncertainty ------------------------------------------------------------- I
# # could make this an argument to the functions above, but let's just make it
# # with ggplot for now
#
# # I'm sure there's a slick way to select all three attributes and do this in one
# # go, but I just need to get this done
#
# # ER
# er_sf <- weraiCropPredER[1,,] %>%
#   st_as_sf() %>%
#   select(all_of(datewanted)) %>%
#   rename(ER = 1) %>%
#   mutate(logER = log10(1+ER), type = 'estimate')
# er_sfPU <- weraiCropPredER[2,,] %>%
#   st_as_sf() %>%
#   select(all_of(datewanted)) %>%
#   rename(ER = 1) %>%
#   mutate(logER = log10(1+ER), type = 'upper')
# er_sfPL <- weraiCropPredER[3,,] %>%
#   st_as_sf() %>%
#   select(all_of(datewanted)) %>%
#   rename(ER = 1) %>%
#   mutate(logER = log10(1+ER), type = 'lower')
#
# er_sf_LMU <- bind_rows(er_sf, er_sfPU, er_sfPL) %>%
#   mutate(namedPI = ifelse(type == 'lower', "Lower 95% PI",
#                           ifelse(type == 'estimate', "Estimate",
#                                  ifelse(type == 'upper', "Upper 95% PI",
#                                         'SCREWUP'))))
#
# erlabel <- 'ER (kg 02/day)\nat max extent'
#
#   er_gg_uncertain <- ggplot() +
#     geom_sf(data = er_sf_LMU, mapping = aes(fill = logER), color = NA) +
#     # geom_sf_label(data = ltimNoNorth, mapping = aes(label = ValleyName)) +
#     coord_sf() +
#     # Closest to the tmap
#     scale_fill_stepsn(colors = erpal_log,
#                       breaks = erbreaks_log[2:length(erbreaks_log)],
#                       limits = c(min(erbreaks_log), max(erbreaks_log)),
#                       labels = erlabels_log,
#                       guide = 'legend',
#                       name = legendlabel) +
#     facet_grid(fct_reorder(namedPI, .x = logER, .fun = max)~.) # lower, estimat and upper should always fall in that order for their maxes
#
# er_gg_uncertain
#
# # GPP
# # I'm sure thgppe's a slick way to select all three attributes and do this in one
# # go, but I just need to get this done
# gpp_sf <- weraiCropPredGPP[1,,] %>%
#   st_as_sf() %>%
#   select(all_of(datewanted)) %>%
#   rename(GPP = 1) %>%
#   mutate(logGPP = log10(1+GPP), type = 'estimate')
# gpp_sfPU <- weraiCropPredGPP[2,,] %>%
#   st_as_sf() %>%
#   select(all_of(datewanted)) %>%
#   rename(GPP = 1) %>%
#   mutate(logGPP = log10(1+GPP), type = 'upper')
# gpp_sfPL <- weraiCropPredGPP[3,,] %>%
#   st_as_sf() %>%
#   select(all_of(datewanted)) %>%
#   rename(GPP = 1) %>%
#   mutate(logGPP = log10(1+GPP), type = 'lower')
#
# gpp_sf_LMU <- bind_rows(gpp_sf, gpp_sfPU, gpp_sfPL) %>%
#   mutate(namedPI = ifelse(type == 'lower', "Lower 95% PI",
#                           ifelse(type == 'estimate', "Estimate",
#                                  ifelse(type == 'upper', "Upper 95% PI",
#                                         'SCREWUP'))))
#
# gpplabel <- 'GPP (kg 02/day)\nat max extent'
#
#
# gpp_gg_uncertain <- ggplot() +
#   geom_sf(data = gpp_sf_LMU, mapping = aes(fill = logGPP), color = NA) +
#   # geom_sf_label(data = ltimNoNorth, mapping = aes(label = ValleyName)) +
#   coord_sf() +
#   # Closest to the tmap
#   scale_fill_stepsn(colors = gpppal_log,
#                     breaks = gppbreaks_log[2:length(gppbreaks_log)],
#                     limits = c(min(gppbreaks_log), max(gppbreaks_log)),
#                     labels = gpplabels_log,
#                     guide = 'legend',
#                     name = legendlabel) +
#   facet_grid(fct_reorder(namedPI, .x = logGPP, .fun = max)~.) # lower, estimat and uppgpp should always fall in that ordgpp for their maxes
#
# gpp_gg_uncertain
#
# # IN THE PROCESS OF MAKING OBHECTS ARE WRONG
# both_uncertain <- ggpubr::ggarrange(gpp_gg_uncertain +
#                                    guides(fill = guide_legend(title.position = 'top')) +
#                                    theme_grey(base_size = 8) +
#                                    theme(legend.position = 'bottom',
#                                          legend.background = element_blank(),
#                                          legend.key.size = unit(0.3, 'cm')),
#                                  er_gg_uncertain +
#                                    guides(fill = guide_legend(title.position = 'top')) +
#                                    theme_grey(base_size = 8) +
#                                    theme(legend.position = 'bottom',
#                                          legend.background = element_blank(),
#                                          legend.key.size = unit(0.3, 'cm')),
#                                  ncol = 2, nrow = 1)
# both_uncertain
#
# # Just print the ggplots
# # Can I print those?
# pdf(file.path(scriptOut, 'Werai_uncertainty.pdf'),
#     onefile = FALSE, height = 12/2.54, width = 16/2.54, useDingbats = FALSE)
# print(both_uncertain)
# dev.off()
#
# png(file.path(scriptOut, 'Werai_uncertainty.png'),
#     height = 12/2.54, width = 16/2.54, units = 'in', res = 300)
# print(both_uncertain)
# dev.off()
#
#
# # Bar chart for some subset -----------------------------------------------
# # make a wide version of the data so I can have mins and maxes
# # OR, should I do all of them and fct_order them?
#
# ## Joining seems safe, but geographic joins are a mess. It doesn't work at all.
# ## binding cols is actually safer and works
#
# # gpp_sf_LMU_wide <- st_join(gpp_sf[,-4],
# #                            rename(gpp_sfPU[,-4],
# #                                   UGPP = GPP,
# #                                   upper_logGPP = logGPP)) %>%
# #   st_join(rename(gpp_sfPL[,-4],
# #                  LGPP = GPP,
# #                  lower_logGPP = logGPP)) %>%
# #   mutate(wetlandID = row_number())
#
# gpp_sf_LMU_wide <- bind_cols(gpp_sf[,-4],
#                            st_drop_geometry(rename(gpp_sfPU[,-c(4)],
#                                   UGPP = GPP,
#                                   upper_logGPP = logGPP)),
#                            st_drop_geometry(rename(gpp_sfPL[,-4],
#                                   LGPP = GPP,
#                                   lower_logGPP = logGPP))) %>%
#   mutate(wetlandID = row_number())
#
# # Grab some set of wetlands
# which(gpp_sf$geometry == gpp_sf_LMU$geometry[3])
# exampleWets <- c(1:100)
#
# # Plot Is there any reason to choose a particular set? No idea. The idea is
# # really just that we could targt a small number, but not sure WHICH small
# # number
# bargpp <- ggplot(gpp_sf_LMU_wide[exampleWets, ],
#                  aes(x = wetlandID, y = logGPP, fill = logGPP)) +
#   geom_col() + # the geom_bar equivalent when it's not counting cases.
#   geom_errorbar(mapping = aes(ymin = lower_logGPP, ymax = upper_logGPP)) +
#   scale_fill_stepsn(colors = gpppal_log,
#                     breaks = gppbreaks_log[2:length(gppbreaks_log)],
#                     limits = c(min(gppbreaks_log), max(gppbreaks_log)),
#                     labels = gpplabels_log,
#                     guide = 'legend',
#                     name = gpplabel) +
#   scale_y_continuous(breaks = gppbreaks_log[2:length(gppbreaks_log)],
#                      labels = round(10^(gppbreaks_log[2:length(gppbreaks_log)]))) +
#   labs(x = 'Wetland', y = gpplabel)
# bargpp
#
# # I assume it is a mes to just throw them all on
# # actually, with a fair amount of monkeying with the look, that's OK
# bargppall <- ggplot(gpp_sf_LMU_wide,
#                  aes(x = fct_reorder(as.factor(wetlandID), logGPP, .desc = TRUE), y = logGPP, fill = logGPP)) +
#   geom_col() + # the geom_bar equivalent when it's not counting cases.
#   geom_linerange(mapping = aes(ymin = lower_logGPP, ymax = upper_logGPP),
#                  color = 'grey50', alpha = 0.2) +
#   scale_fill_stepsn(colors = gpppal_log,
#                     breaks = gppbreaks_log[2:length(gppbreaks_log)],
#                     limits = c(min(gppbreaks_log), max(gppbreaks_log)),
#                     labels = gpplabels_log,
#                     guide = 'legend',
#                     name = gpplabel) +
#   scale_y_continuous(breaks = gppbreaks_log[2:length(gppbreaks_log)],
#                      labels = round(10^(gppbreaks_log[2:length(gppbreaks_log)]))) +
#   labs(x = 'Wetland', y = gpplabel)
# bargppall
#
# # I actually like that that encompasses an area that integrates to total
# # production across the Werai
#
# # I can do points, but the integration implied above is nice
# bargppallpoint <- ggplot(gpp_sf_LMU_wide,
#                          aes(x = fct_reorder(as.factor(wetlandID), logGPP),
#                              y = logGPP, color = logGPP)) +
#   geom_linerange(mapping = aes(ymin = lower_logGPP, ymax = upper_logGPP), color = 'grey50') +
#   geom_point() + # the geom_bar equivalent when it's not counting cases.
#   scale_color_stepsn(colors = gpppal_log,
#                      breaks = gppbreaks_log[2:length(gppbreaks_log)],
#                      limits = c(min(gppbreaks_log), max(gppbreaks_log)),
#                      labels = gpplabels_log,
#                      guide = 'legend',
#                      name = gpplabel) +
#   scale_y_continuous(breaks = gppbreaks_log[2:length(gppbreaks_log)],
#                      labels = round(10^(gppbreaks_log[2:length(gppbreaks_log)]))) +
#   labs(x = 'Wetland', y = gpplabel)
# bargppallpoint
#
# # TODO::
# # Can I make the same with ER? It might look really cool to have GPP going up
# # and ER going down on the same plot
#
# # same, ER
# erlabel <- 'GPP (kg 02/day)\nat max extent'
#
# er_sf_LMU_wide <- bind_cols(er_sf[,-4],
#                              st_drop_geometry(rename(er_sfPU[,-c(4)],
#                                                      UER = ER,
#                                                      upper_logER = logER)),
#                              st_drop_geometry(rename(er_sfPL[,-4],
#                                                      LER = ER,
#                                                      lower_logER = logER))) %>%
#   mutate(wetlandID = row_number())
#
# # barplot
# barerall <- ggplot(er_sf_LMU_wide,
#                     aes(x = fct_reorder(as.factor(wetlandID), logER, .desc = TRUE), y = logER, fill = logER)) +
#   geom_col() + # the geom_bar equivalent when it's not counting cases.
#   geom_linerange(mapping = aes(ymin = lower_logER, ymax = upper_logER),
#                  color = 'grey50', alpha = 0.2) +
#   scale_fill_stepsn(colors = erpal_log,
#                     breaks = erbreaks_log[2:length(erbreaks_log)],
#                     limits = c(min(erbreaks_log), max(erbreaks_log)),
#                     labels = erlabels_log,
#                     guide = 'legend',
#                     name = erlabel) +
#   scale_y_continuous(breaks = erbreaks_log[2:length(erbreaks_log)],
#                      labels = round(10^(erbreaks_log[2:length(erbreaks_log)]))) +
#   labs(x = 'Wetland', y = erlabel)
# barerall
#
# # Can I make one with them both there going opposite direction?
# both_sf_LMU_wide <- bind_cols(mutate(gpp_sf_LMU_wide),
#                               mutate(st_drop_geometry(er_sf_LMU_wide[,-c(8)])))
#
# # Need new labels and breaks. Could cobble, but getting confusing and misaligned
# # bothbreaks_log <- c(-rev(erbreaks_log[2:length(erbreaks_log)]), gppbreaks_log)
#
# # # and those breaks might not quite yield 10, so maximise the palette differences
# # erpal_log <- sequential_hcl(length(erbreaks_log)-1, palette = 'Purples', rev = TRUE)
# # I think here I want continuous colors. but maybe still a broken up legend?
#
# # have to do the negatives before delogging
# # bothlabels_log <- c(-10^rev(erbreaks_log), 10^gppbreaks_log)
# #
# # bothlabels_log <- format(bothlabels_log, big.mark=",",
#                        # scientific=FALSE, trim = TRUE, digits = 0)
#
# # This is really getting complex. Why don't I just use 0,1,10,100,1000,10000?
# # They've both been shifted by 1 to log, so
# # This is just used to get the labels, the actual log-scale is plotted and so the zeros are done correctly
# bothbreaks_log <- c(-10^(4:0), 10^(1:4))
# bothbreaks_log[5] <- 0 # Because both were shifted so 1 = 0
# bothlabels_log <- as.character(bothbreaks_log)
#
#
# # erstart <- erlabels_log[1:(length(erlabels_log)-1)]
# # erstart[1] <- "0" # instead of 1
# # bothlabels_log <- paste0(erstart, ' to ', erlabels_log[2:length(erlabels_log)])
# # erlabels_log
#
# # AAAA try to get some lables for the x. I JUST WANT IT TO HAVE THE NUMBER BUT IT WON"T DO IT
# reordx <- fct_reorder(as.factor(both_sf_LMU_wide$wetlandID),
#                       both_sf_LMU_wide$logGPP, .desc = TRUE)
# # I'm confused. that didn't work. this is anoyuting
#
#
# # can I just do it as two calls? or do I need to stack and do it the tidy way?
# barboth <- ggplot(both_sf_LMU_wide) +
#   geom_col(mapping = aes(x = fct_reorder(as.factor(wetlandID),
#                                          logER, .desc = TRUE),
#                          y = -logER, fill = -logER)) + # the geom_bar equivalent when it's not counting cases.
#   geom_linerange(mapping = aes(x = fct_reorder(as.factor(wetlandID),
#                                                logER, .desc = TRUE),
#                                ymin = -lower_logER, ymax = -upper_logER),
#                  color = 'grey50', alpha = 0.2) +
#   # scale_fill_stepsn(colors = erpal_log,
#   #                   breaks = erbreaks_log[2:length(erbreaks_log)],
#   #                   limits = c(min(erbreaks_log), max(erbreaks_log)),
#   #                   labels = erlabels_log,
#   #                   guide = 'legend',
#   #                   name = erlabel) +
#
#   # scale_fill_stepsn(colors = c(rev(erpal_log), gpppal_log),
#   #            breaks = c(-rev(erbreaks_log[2:length(erbreaks_log)]), 0,
#   #                       gppbreaks_log[2:length(gppbreaks_log)]),
#   #            limits = c(min(-erbreaks_log), max(gppbreaks_log)),
#   #            # labels = c(erlabels_log, gpplabels_log),
#   #            guide = 'legend',
#   #            name = 'kg O2\nat max extent') +
#   geom_col(mapping = aes(x = fct_reorder(as.factor(wetlandID),
#                                          logGPP, .desc = TRUE),
#                          y = logGPP, fill = logGPP)) +
#   geom_linerange(mapping = aes(x = fct_reorder(as.factor(wetlandID),
#                                                logGPP, .desc = TRUE),
#                                ymin = lower_logGPP, ymax = upper_logGPP),
#                  color = 'grey50', alpha = 0.2) +
#   geom_line(mapping = aes(x = fct_reorder(as.factor(wetlandID),
#                                            logGPP, .desc = TRUE),
#                            y = logGPP-logER, group = NA)) +
#
#   # This palette is very slightly different because it's not binned and GPP above uses Emerald instead of green, but it's sure close and way easier
#   scale_fill_continuous_diverging(palette = "Purple-Green",
#                                   limits = c(-4, 4),
#                                   breaks = -4:4,
#                                   labels = bothlabels_log) +
#   scale_y_continuous(limits = c(-4, 4),
#                      breaks = -4:4,
#                      labels = bothlabels_log) +
#   # scale_y_continuous(breaks = c(-erbreaks_log[2:length(erbreaks_log)],
#   #                               gppbreaks_log[2:length(gppbreaks_log)]),
#   #                    labels = c(-round(10^(erbreaks_log[2:length(erbreaks_log)])),
#   #                               round(10^(gppbreaks_log[2:length(gppbreaks_log)])))) +
#   labs(x = 'Wetland (ordered by GPP)', y = 'kg O2', fill = 'kg O2') +
#   # argh doesn't work because has been reordered
#   # scale_x_discrete(breaks = as.character(c(1, 250, 500, 750, 1000))) +
#   theme(legend.position = c(0.75, 0.54)) +
#   theme(axis.text.x = element_blank(),
#         axis.ticks.x = element_blank(),
#         legend.position = c(0.75, 0.55))
# barboth
#
#
# # The inability to label x in a reasonable way is infuriating. Can I do it differently?
# both_sf_LMU_wide2 <- both_sf_LMU_wide %>%
#   arrange(desc(logGPP)) %>%
#   mutate(GPPrank = row_number())
#
# # can I just do it as two calls? or do I need to stack and do it the tidy way?
# barboth <- ggplot(both_sf_LMU_wide2) +
#   geom_col(mapping = aes(x = GPPrank,
#                          y = -logER, fill = -logER)) + # the geom_bar equivalent when it's not counting cases.
#   geom_linerange(mapping = aes(x = GPPrank,
#                                ymin = -lower_logER, ymax = -upper_logER),
#                  color = 'grey50', alpha = 0.2) +
#
# geom_col(mapping = aes(x = GPPrank,
#                        y = logGPP, fill = logGPP)) +
#   geom_linerange(mapping = aes(x = GPPrank,
#                                ymin = lower_logGPP, ymax = upper_logGPP),
#                  color = 'grey50', alpha = 0.2) +
#   geom_line(mapping = aes(x = GPPrank,
#                           y = logGPP-logER, group = NA)) +
#
#   # This palette is very slightly different because it's not binned and GPP above uses Emerald instead of green, but it's sure close and way easier
#   scale_fill_continuous_diverging(palette = "Purple-Green",
#                                   limits = c(-4, 4),
#                                   breaks = -4:4,
#                                   labels = bothlabels_log) +
#   scale_y_continuous(limits = c(-4, 4),
#                      breaks = -4:4,
#                      labels = bothlabels_log) +
#   labs(x = 'Wetland (ordered by GPP)', y = 'kg O2', fill = 'kg O2') +
#   # scale_x_discrete(breaks = c(1, 250, 500, 750, 1000)) +
#   theme_bw(base_size = 8) + theme(legend.position = c(0.75, 0.53),
#                                   panel.grid.major = element_blank(),
#                                   panel.grid.minor = element_blank())
# barboth
#
# # print
# pdf(file.path(scriptOut, 'Werai_mirrorgram.pdf'),
#     onefile = FALSE, height = 8/2.54, width = 12/2.54, useDingbats = FALSE)
# print(barboth)
# dev.off()
#
# png(file.path(scriptOut, 'Werai_mirrorgram.png'),
#     height = 8/2.54, width = 12/2.54, units = 'in', res = 300)
# print(barboth)
# dev.off()
#
# # What about Darren's fingerprints? They would maybe take the GPP and ER and
# # make a density with them? OUt of what? The wetlands? Could work. Would need to
# # sort out the weighting by volume. it's already in there, but that means it
# # will give each location the same weight. I think a better thing for the
# # fingerprints would be to remove it, and then give each wetland its predicted
# # per liter rate and weight by volume IE use the straight predictions and then
# # weight by inundaton Should be straightforward, but likely won't be
#
# ## Would be a really cool way to look at scenarios. especially if I can do it
# # for whole catchments
#
# ## BUT, because GPP and ER are both linear fits of temp, they will exactly
# ## predict each other according to a linear relationship, and so there won't be
# ## any 2-d variation and all the points will fall on a line. IE for a given GPP
# ## there will only be one ER, and so the fingerprint will be a line. Cool idea,
# ## but we'd need more info about their variance relative to each other
#
# # -------------------------------------------------------------------------
#
#
# # How about annual reporting? ---------------------------------------------
# tmap_mode('view')
# # Can I make simple annual reporting?
# interDates <- as.POSIXct(c("2014-06-30", "2015-06-30", "2016-06-30", "2017-06-30", "2018-06-30", "2019-06-30"))
#
# tempannual <- tempaggregate(weraiCropTemp, by_t = as.Date(interDates), FUN = mean, na.rm = TRUE)
# inunannual <- tempaggregate(weraiCropInun, by_t = as.Date(interDates), FUN = sum, na.rm = TRUE)
# gppannual <- tempaggregate(weraiCropPredGPP[1,,], by_t = as.Date(interDates), FUN = sum, na.rm = TRUE)
# erannual <- tempaggregate(weraiCropPredER[1,,], by_t = as.Date(interDates), FUN = sum, na.rm = TRUE)
#
# # aggregate to werai
# # temp area-weighted
# areas <- tempannual %>%
#   st_geometry() %>%
#   st_area() %>%
#   as.numeric()
# tempW <- catchAgg <- catchAggW(strict = tempannual, strictWeights = areas,
#                                FUN = mean, summaryPoly = ramsarW1)
#
# inunW <- aggregate(inunannual,
#                       by = ramsarW1,
#                       FUN = sum, na.rm = TRUE)
#
# gppW <- aggregate(gppannual,
#                    by = ramsarW1,
#                    FUN = sum, na.rm = TRUE)
# erW <- aggregate(erannual,
#                    by = ramsarW1,
#                    FUN = sum, na.rm = TRUE)
#
# # Try a facetted tmap
#
# # Make plots --------------------------------------------------------------
#
# # Data organisation
# # AHHH the flopped dims
# if (attributes(st_dimensions(gppannual))$name[1] != 'geometry') {
#   gppannual <- aperm(gppannual, c(2,1))
# }
# if (attributes(st_dimensions(erannual))$name[1] != 'geometry') {
#   erannual <- aperm(erannual, c(2,1))
# }
#
# # GPP
# gppYear_sf <- gppannual %>%
#   st_as_sf() %>%
#   pivot_longer(cols = -geometry, names_to = 'WaterYear', values_to = 'GPP') %>%
#   mutate(logGPP =  log10(1+GPP)) %>%
#   st_as_sf() # REALLY???
# gppYear_sf
#
# max(gppYear_sf$logGPP) # is within
# gppbreaks_log
#
# # ER
# erYear_sf <- erannual %>%
#   st_as_sf() %>%
#   pivot_longer(cols = -geometry, names_to = 'WaterYear', values_to = 'ER') %>%
#   mutate(logER =  log10(1+ER)) %>%
#   st_as_sf() # REALLY???
# erYear_sf
#
# # the range should still work for tthe colors
# max(erYear_sf$logER) # is within
# erbreaks_log
#
#
# ## FACETTED TMAP
# # Either view or plot should work?
# # Would be nice if I could only have the legend on one of them...
# tmap_mode('view')
# tmap_mode('plot')
# # GPP The units are weird, because this is added up across bimonths. It's
# # neither the total or the average.Not really sure what to call it
# gppyrlabel <- 'Total Yearly GPP (kg 02)\nat max bimonth extents'
#
#   gppAnnual_tm <- gppYear_sf %>%
#     filter(WaterYear != '2014-06-29') %>% # because a 5-panel is ugly
#     tm_shape() +
#     tm_fill(col = 'logGPP',
#             palette = gpppal_log,
#             breaks = gppbreaks_log,
#             labels = gpplabels_log,
#             title = gppyrlabel) +
#     tm_facets(by = 'WaterYear')
#   gppAnnual_tm
#
#   # ER
#   eryrlabel <- 'Total Yearly ER (kg 02)\nat max bimonth extents'
#
#   erAnnual_tm <- erYear_sf %>%
#     filter(WaterYear != '2014-06-29') %>% # because a 5-panel is ugly
#     tm_shape() +
#     tm_fill(col = 'logER',
#             palette = erpal_log,
#             breaks = erbreaks_log,
#             labels = erlabels_log,
#             title = eryrlabel) +
#     tm_facets(by = 'WaterYear')
#   erAnnual_tm
#
#   ### GGPLOT
#   gppAnnual_gg <- gppYear_sf %>%
#     filter(WaterYear != '2014-06-29') %>% # because a 5-panel is ugly
#     ggplot() +
#     geom_sf(mapping = aes(fill = logGPP), color = NA) +
#     coord_sf() +
#     # Closest to the tmap
#     scale_fill_stepsn(colors = gpppal_log,
#                       breaks = gppbreaks_log[2:length(gppbreaks_log)],
#                       limits = c(min(gppbreaks_log), max(gppbreaks_log)),
#                       labels = gpplabels_log,
#                       guide = 'legend',
#                       name = gppyrlabel) +
#     facet_wrap(vars(WaterYear))
#   gppAnnual_gg
#
#   erAnnual_gg <- erYear_sf %>%
#     filter(WaterYear != '2014-06-29') %>% # because a 5-panel is ugly
#     ggplot() +
#     geom_sf(mapping = aes(fill = logER), color = NA) +
#     coord_sf() +
#     # Closest to the tmap
#     scale_fill_stepsn(colors = erpal_log,
#                       breaks = erbreaks_log[2:length(erbreaks_log)],
#                       limits = c(min(erbreaks_log), max(erbreaks_log)),
#                       labels = erlabels_log,
#                       guide = 'legend',
#                       name = eryrlabel) +
#     facet_wrap(vars(WaterYear))
#   erAnnual_gg
#
# ### Tempted to stack GPP and ER for the doc
#   gppAnnual_gg3 <- gppYear_sf %>%
#     filter(WaterYear != '2014-06-29' & WaterYear != '2018-06-29') %>% # because a 5-panel is ugly
#     ggplot() +
#     geom_sf(mapping = aes(fill = logGPP), color = NA) +
#     coord_sf() +
#     # Closest to the tmap
#     scale_fill_stepsn(colors = gpppal_log,
#                       breaks = gppbreaks_log[2:length(gppbreaks_log)],
#                       limits = c(min(gppbreaks_log), max(gppbreaks_log)),
#                       labels = gpplabels_log,
#                       guide = 'legend',
#                       name = gppyrlabel) +
#     facet_grid(WaterYear~.) +
#     theme(legend.position = 'bottom')
#   gppAnnual_gg3
#
#   erAnnual_gg3 <- erYear_sf %>%
#     filter(WaterYear != '2014-06-29' & WaterYear != '2018-06-29') %>% # because a 5-panel is ugly
#     ggplot() +
#     geom_sf(mapping = aes(fill = logER), color = NA) +
#     coord_sf() +
#     # Closest to the tmap
#     scale_fill_stepsn(colors = erpal_log,
#                       breaks = erbreaks_log[2:length(erbreaks_log)],
#                       limits = c(min(erbreaks_log), max(erbreaks_log)),
#                       labels = erlabels_log,
#                       guide = 'legend',
#                       name = eryrlabel) +
#     facet_grid(WaterYear~.) +
#     theme(legend.position = 'bottom')
#   erAnnual_gg3
#
#   annualGPPER <- ggpubr::ggarrange(gppAnnual_gg3 +
#                                      guides(fill = guide_legend(title.position = 'top')) +
#                                      theme_grey(base_size = 8) +
#                                      theme(legend.position = 'bottom',
#                                            legend.background = element_blank(),
#                                            legend.key.size = unit(0.3, 'cm')),
#                     erAnnual_gg3 +
#                       guides(fill = guide_legend(title.position = 'top')) +
#                       theme_grey(base_size = 8) +
#                       theme(legend.position = 'bottom',
#                             legend.background = element_blank(),
#                             legend.key.size = unit(0.3, 'cm')),
#                     ncol = 2, nrow = 1)
#   annualGPPER
#
#   # Just print the ggplots
#   # Can I print those?
#   pdf(file.path(scriptOut, 'Werai_gg_Annual.pdf'),
#       onefile = FALSE, height = 12/2.54, width = 16/2.54, useDingbats = FALSE)
#   print(annualGPPER)
#   dev.off()
#
#   png(file.path(scriptOut, 'Werai_gg_Annual.png'),
#       height = 12/2.54, width = 16/2.54, units = 'in', res = 300)
#   print(annualGPPER)
#   dev.off()
#