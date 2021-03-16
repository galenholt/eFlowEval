# some plots for the scenario comparison

# This assumes for the moment that scenarioDemoSMtopTemp has been run, and the
# yearly objects are in memory

# Illustrate local stuff in cumbung again
whichcrs <- st_crs(lachAll)
cumbung <- st_bbox(c(xmin = 143.85, ymin = -34.35, xmax = 144.4, ymax = -34.05), crs = whichcrs)


# Let's just plot a few things, before getting fancy
  # What do I expect to change? maybe lippia

# Lippia all the way through
lippiaBaseCrop <- lippia_baseYr$fullCycle_Lippia_yr[,4:5,] # Get time slices first or it's absurd
lippiaBaseCrop <- st_as_sf(lippiaBaseCrop, long = TRUE) # SUPER annoying to have to do this NOW, but it requires a square grid for stars
lippiaBaseCrop <- st_crop(lippiaBaseCrop, st_as_sfc(cumbung)) # Crop

# Argh. scale_id
ggplot() + 
  geom_sf(data = lippiaBaseCrop, aes(fill = propDaysPassed), color = NA) + 
  facet_wrap(vars(as.character(time))) +
  scale_fill_viridis(option = 'plasma', name = 'Proportion\ndays\npassed') +
  theme_bw() + ggtitle('Seed survival')

# Same, for climate change
# Lippia all the way through
lippiatopclimCrop <- lippia_topclimYr$fullCycle_Lippia_yr[,4:5,] # Get time slices first or it's absurd
lippiatopclimCrop <- st_as_sf(lippiatopclimCrop, long = TRUE) # SUPER annoying to have to do this NOW, but it requires a square grid for stars
lippiatopclimCrop <- st_crop(lippiatopclimCrop, st_as_sfc(cumbung)) # Crop

# Argh. scale_id
ggplot() + 
  geom_sf(data = lippiatopclimCrop, aes(fill = propDaysPassed), color = NA) + 
  facet_wrap(vars(as.character(time))) +
  scale_fill_viridis(option = 'plasma', name = 'Proportion\ndays\npassed') +
  theme_bw() + ggtitle('Seed survival')

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
  theme_bw() + ggtitle('Seed survival')

# That looks like it'll work, now need to get more sophisticated.

# Look at catchment scale -------------------------------------------------


# Test with centipeda (let's leave lippia out for now)
fullCatch_Centipeda_base <- catchAggW(strict = centipeda_baseYr$seedGermFruit_Centipeda_yr, 
                                      strictWeights = lachArea, 
                                 FUN = sum, summaryPoly = lachOnly)
names(fullCatch_Centipeda_base) <- 'areaDaysPassed'

catchPlot_c_b <- ggplot() +
  geom_stars(data = fullCatch_Centipeda_base) +
  coord_sf() +
  facet_wrap(~as.character(time)) +
  theme_void()  +
  scale_fill_viridis(option = 'viridis') +
  ggtitle('Full life cycle success')
catchPlot_c_b # + theme(legend.title = element_text('Area of success'))

fullCatch_Centipeda_topclim <- catchAggW(strict = centipeda_topclimYr$seedGermFruit_Centipeda_yr, 
                                      strictWeights = lachArea, 
                                      FUN = sum, summaryPoly = lachOnly)
names(fullCatch_Centipeda_topclim) <- 'areaDaysPassed'

catchPlot_c_tc <- ggplot() +
  geom_stars(data = fullCatch_Centipeda_topclim) +
  coord_sf() +
  facet_wrap(~as.character(time)) +
  theme_void()  +
  scale_fill_viridis(option = 'viridis') +
  ggtitle('Full life cycle success')
catchPlot_c_tc # + theme(legend.title = element_text('Area of success'))

# comparison
centchange <- centipeda_topclimYr$seedGermFruit_Centipeda_yr - centipeda_baseYr$seedGermFruit_Centipeda_yr

cent_compTC_B <- catchAggW(strict = centchange, 
                                         strictWeights = lachArea, 
                                         FUN = sum, summaryPoly = lachOnly)
names(cent_compTC_B) <- 'areaDaysPassed'

catchPlot_c_delta <- ggplot() +
  geom_stars(data = cent_compTC_B) +
  coord_sf() +
  facet_wrap(~as.character(time)) +
  theme_void()  +
  scale_fill_viridis(option = 'viridis') +
  ggtitle('Change in Full life cycle success')
catchPlot_c_delta # + theme(legend.title = element_text('Area of success'))

# Well, the structure works, the results aren't good. still, that'll do as a start, I think

