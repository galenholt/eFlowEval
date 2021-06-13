# some plots for the scenario comparison

# This assumes for the moment that scenarioDemoSMtopTemp has been run, and the
# yearly objects are in memory

# LTIM areas, useful for plotting
ltimCut <- LTIM_Valleys %>%
  select(ValleyName) %>% # Three different ways to reference, basically
  filter(ValleyName != 'Northern Unregulated') # Deal with the northern unregulated issue

ltimCut <- st_transform(ltimCut, st_crs(lachAll))


# Illustrate local stuff in cumbung again
whichcrs <- st_crs(lachAll)
cumbung <- st_bbox(c(xmin = 143.85, ymin = -34.35, xmax = 144.4, ymax = -34.05), crs = whichcrs)

# The component parts (baseline, climate alone) local stuff got moved to
# scratch, not sure how useful it really is, except for illustration, for which I include one here for the change

# Can I just do a comparison easily?
# Find the difference (should be proportion, or somethign better thought out)
deltaltc <- lippia_topclimYr$fullCycle_Lippia_yr - lippia_baseYr$fullCycle_Lippia_yr

lippiabtcCrop <- deltaltc[,4:5,] # Get time slices first or it's absurd
lippiabtcCrop <- st_as_sf(lippiabtcCrop, long = TRUE) # SUPER annoying to have to do this NOW, but it requires a square grid for stars
lippiabtcCrop <- st_crop(lippiabtcCrop, st_as_sfc(cumbung)) # Crop

# Argh. scale_id
ggplot() + 
  geom_sf(data = lippiabtcCrop, aes(fill = propDaysPassed), color = NA) + 
  facet_wrap(vars(as.character(time))) +
  scale_fill_viridis(option = 'plasma', name = 'Change in prop days passed') +
  theme_bw() + ggtitle('Lippia survival')


# Let's build this up: baseline, change with climate, change with  --------

# Start with each alone, then the changes

# First, aggregate to the catchment
fullCatch_Centipeda_base <- catchAggW(strict = centipeda_baseYr$fullCycleLippia_Centipeda_yr, 
                                      strictWeights = lachArea, 
                                      FUN = sum, summaryPoly = ltimCut)
names(fullCatch_Centipeda_base) <- 'areaDaysPassed'
fullCatch_Centipeda_base[[1]][notlach, ] <- NA


fullCatch_Centipeda_clim <- catchAggW(strict = centipeda_climYr$fullCycleLippia_Centipeda_yr, 
                                      strictWeights = lachArea, 
                                      FUN = sum, summaryPoly = ltimCut)
names(fullCatch_Centipeda_clim) <- 'areaDaysPassed'
fullCatch_Centipeda_clim[[1]][notlach, ] <- NA

fullCatch_Centipeda_top <- catchAggW(strict = centipeda_topYr$fullCycleLippia_Centipeda_yr, 
                                      strictWeights = lachArea, 
                                      FUN = sum, summaryPoly = ltimCut)
names(fullCatch_Centipeda_top) <- 'areaDaysPassed'
fullCatch_Centipeda_top[[1]][notlach, ] <- NA


fullCatch_Centipeda_topclim <- catchAggW(strict = centipeda_topclimYr$fullCycleLippia_Centipeda_yr, 
                                      strictWeights = lachArea, 
                                      FUN = sum, summaryPoly = ltimCut)
names(fullCatch_Centipeda_topclim) <- 'areaDaysPassed'
fullCatch_Centipeda_topclim[[1]][notlach, ] <- NA


# baseline
centbasePlot <- ggplot() +
  geom_stars(data = fullCatch_Centipeda_base) +
  coord_sf() +
  facet_wrap(~as.character(time)) +
  theme_void()  +
  scale_fill_viridis(option = 'viridis') +
  ggtitle('Baseline centipeda success')
centbasePlot

# climate
centclimPlot <- ggplot() +
  geom_stars(data = fullCatch_Centipeda_clim) +
  coord_sf() +
  facet_wrap(~as.character(time)) +
  theme_void()  +
  scale_fill_viridis(option = 'viridis') +
  ggtitle('Centipeda success with 2C warming')
centclimPlot

# Intervention
centtopPlot <- ggplot() +
  geom_stars(data = fullCatch_Centipeda_top) +
  coord_sf() +
  facet_wrap(~as.character(time)) +
  theme_void()  +
  scale_fill_viridis(option = 'viridis') +
  ggtitle('Centipeda success with topup moisture')
centtopPlot

# And climate + topup
centtopclimPlot <- ggplot() +
  geom_stars(data = fullCatch_Centipeda_topclim) +
  coord_sf() +
  facet_wrap(~as.character(time)) +
  theme_void()  +
  scale_fill_viridis(option = 'viridis') +
  # scale_color_manual(values = 'white') +
  ggtitle('Centipeda success with 2C + management')
centtopclimPlot

# Do I want to do this for lippia too? not sure

# Now, the changes

# Change in centipeda success with climate change
centchangeClimvBase <- fullCatch_Centipeda_clim - 
  fullCatch_Centipeda_base

# Change in centipeda success with management
centchangeTopvBase <- fullCatch_Centipeda_top - 
  fullCatch_Centipeda_base

# Change in centipeda success with management as a climate remediation
centchangeTopClimvClim <- fullCatch_Centipeda_topclim - 
  fullCatch_Centipeda_clim

# Change due to climate
centClimvBasePlot <- ggplot() +
  geom_stars(data = centchangeClimvBase) +
  coord_sf() +
  facet_wrap(~as.character(time)) +
  theme_void()  +
  scale_fill_viridis(option = 'viridis') +
  ggtitle('Change in centipeda success with 2C warming')
centClimvBasePlot

# Change due to management
centTopvBasePlot <- ggplot() +
  geom_stars(data = centchangeTopvBase) +
  coord_sf() +
  facet_wrap(~as.character(time)) +
  theme_void()  +
  scale_fill_viridis(option = 'viridis') +
  ggtitle('Change in centipeda success with moisture topup')
centTopvBasePlot

# How is this 0 again???
# What if I do the subtraction before the aggregatioN?
centchangeTB <- centipeda_topclimYr$fullCycleLippia_Centipeda_yr - 
  centipeda_baseYr$fullCycleLippia_Centipeda_yr
fullCatch_Centipeda_topclim_2 <- catchAggW(strict = centchangeTB, 
                                         strictWeights = lachArea, 
                                         FUN = sum, summaryPoly = ltimCut)
names(fullCatch_Centipeda_topclim_2) <- 'areaDaysPassed'

# climate + management
centTopClimvClimPlot <- ggplot() +
  geom_stars(data = centchangeTopClimvClim) +
  coord_sf() +
  facet_wrap(~as.character(time)) +
  theme_void()  +
  scale_fill_viridis(option = 'viridis') +
  ggtitle('Change in centipeda success with 2C warming + management')
centTopClimvClimPlot

# 0? Again?? No way the management doesn't have an effect with


# For CEWO presentation ---------------------------------------------------

fullCatch_Lippia_base <- catchAggW(strict = lippia_baseYr$fullCycle_Lippia_yr, 
                                      strictWeights = lachArea, 
                                      FUN = sum, summaryPoly = ltimCut)
names(fullCatch_Lippia_base) <- 'areaDaysPassed'

# I don't actually want it to spill into other basins, since this is only anae
# units that overlap, NOT real estimates
fullCatch_Lippia_base 

which(ltimCut$ValleyName == 'Lachlan')

str(fullCatch_Lippia_base[[1]])
str(fullCatch_Lippia_base[1,12,])

# For now, easiest to just use the [[1]] method, even though it's crude
notlach <- which(ltimCut$ValleyName != 'Lachlan')
fullCatch_Lippia_base[[1]][notlach, ] <- NA

plot3L <- ggplot() +
  geom_stars(data = fullCatch_Lippia_base[,,4:6]) +
  coord_sf() +
  facet_wrap(~as.character(as.Date(time))) +
  theme_void() +
  scale_fill_viridis(option = 'viridis')
plot3L

# AHHH. What is the low? Why is this not capturing the range??
plot(fcb3)
# It's spilling into the northern basin. I thought I fixed that?
ltimCut

# Make matching for the anaes themselves. This is just lachlan, so no need to de-northern it
# I actually want these to be DAYS, not years

lip3days <- filter(lippia_base$fullCycle_Lippia, time > lubridate::ymd('20140816'), time < lubridate::ymd('20140819'))

plot3detail <- ggplot() +
  geom_stars(data = lip3days) + # why on earth are the dimensions flipped?
  coord_sf() +
  facet_wrap(~as.character(as.Date(time))) +
  theme_void() +
  scale_fill_viridis(option = 'viridis')
plot3detail

# ggplot really not liking that
plot(lip3days)

# Why do I need to zoom in here? just do the catchagg
lip10days <- filter(lippia_base$fullCycle_Lippia, time > lubridate::ymd('20140816'), time < lubridate::ymd('20140827'))

fullCatch_Lippia_10 <- catchAggW(strict = lip10days, 
                                   strictWeights = lachArea, 
                                   FUN = sum, summaryPoly = ltimCut)
names(fullCatch_Lippia_10) <- 'sumprop'
# notlach <- which(ltimCut$ValleyName != 'Lachlan')
fullCatch_Lippia_10[[1]][notlach, ] <- NA

plot10 <- ggplot() +
  geom_stars(data = fullCatch_Lippia_10[,,7:9]) +
  coord_sf() +
  facet_wrap(~as.character(as.Date(time))) +
  theme_void() +
  scale_fill_viridis(option = 'viridis', begin = 0.25, end = 0.75)
plot10

# Need to mock somethign up for multiple taxa. Maybe try to actually use centip
# and lippia, but just illustrate somehow if not able

fullCatch_Centipeda_base[[1]][notlach, ] <- NA

fullCatch_Centipeda_nolip <- catchAggW(strict = centipeda_baseYr$seedGermFruit_Centipeda_yr, 
                                           strictWeights = lachArea, 
                                           FUN = sum, summaryPoly = ltimCut)
names(fullCatch_Centipeda_nolip) <- 'areaDaysPassed'

fullCatch_Centipeda_nolip[[1]][notlach, ] <- NA


plot3C <- ggplot() +
  geom_stars(data = fullCatch_Centipeda_nolip[,,4:6]) +
  coord_sf() +
  facet_wrap(~as.character(as.Date(time))) +
  theme_void() +
  scale_fill_viridis(option = 'viridis')
plot3C

plot3L

plot3CL <- ggplot() +
  geom_stars(data = fullCatch_Centipeda_base[,,4:6]) +
  coord_sf() +
  facet_wrap(~as.character(as.Date(time))) +
  theme_void() +
  scale_fill_viridis(option = 'viridis', begin = 0.25)
plot3CL


## Scenarios but only 3
# Change due to climate
centClimvBasePlot3 <- ggplot() +
  geom_stars(data = centchangeClimvBase[,,4:5]) +
  coord_sf() +
  facet_wrap(~as.character(time)) +
  theme_void()  +
  scale_fill_viridis(option = 'viridis')
centClimvBasePlot3

# Change due to management
centTopvBasePlot3 <- ggplot() +
  geom_stars(data = centchangeTopvBase[,,4:5]) +
  coord_sf() +
  facet_wrap(~as.character(time)) +
  theme_void()  +
  scale_fill_viridis(option = 'viridis', end = 0.5) 
centTopvBasePlot3

# climate + management
centTopClimvClimPlot3 <- ggplot() +
  geom_stars(data = centchangeTopClimvClim[,,4:5]) +
  coord_sf() +
  facet_wrap(~as.character(time)) +
  theme_void()  +
  scale_fill_viridis(option = 'viridis', begin = 0.25, end = 0.75)
centTopClimvClimPlot3
