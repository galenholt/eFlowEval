# Scenario plotting scratch

# This assumes for the moment that scenarioDemoSMtopTemp has been run, and the
# yearly objects are in memory

# Illustrate local stuff in cumbung again
whichcrs <- st_crs(LachlanANAE)
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
  theme_bw() + ggtitle('Lippia survival')

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
  theme_bw() + ggtitle('Lippia survival')

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
# centchange <- centipeda_topclimYr$seedGermFruit_Centipeda_yr - centipeda_baseYr$seedGermFruit_Centipeda_yr
# 
# cent_compTC_B <- catchAggW(strict = centchange, 
#                                          strictWeights = lachArea, 
#                                          FUN = sum, summaryPoly = lachOnly)
# names(cent_compTC_B) <- 'areaDaysPassed'
# 
# catchPlot_c_delta <- ggplot() +
#   geom_stars(data = cent_compTC_B) +
#   coord_sf() +
#   facet_wrap(~as.character(time)) +
#   theme_void()  +
#   scale_fill_viridis(option = 'viridis') +
#   ggtitle('Change in Full life cycle success')
# catchPlot_c_delta # + theme(legend.title = element_text('Area of success'))

# Well, the structure works, the results aren't good. still, that'll do as a start, I think



# Testing comparisons at the catchment --------------------

# Change in lippia success at the catchment scale
# comparison
lippiachange <- lippia_topclimYr$fullCycle_Lippia_yr - lippia_baseYr$fullCycle_Lippia_yr

lippia_compTC_B <- catchAggW(strict = lippiachange, 
                             strictWeights = lachArea, 
                             FUN = sum, summaryPoly = lachOnly)
names(lippia_compTC_B) <- 'changeInAreaDays'

catchPlot_l_delta <- ggplot() +
  geom_stars(data = lippia_compTC_B) +
  coord_sf() +
  facet_wrap(~as.character(time)) +
  theme_void()  +
  scale_fill_viridis(option = 'viridis') +
  ggtitle('Change in Lippia success')
catchPlot_l_delta # + theme(legend.title = element_text('Area of success'))

# Change in centipeda success
centchange <- centipeda_topclimYr$fullCycleLippia_Centipeda_yr - 
  centipeda_baseYr$fullCycleLippia_Centipeda_yr

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

# So yeah, on the whole, most years don't change at all, but 3,4, and 5 do (though 5 not much)
slice(cent_compTC_B, time, 1)
slice(cent_compTC_B, time, 2)
slice(cent_compTC_B, time, 3)
slice(cent_compTC_B, time, 4)
slice(cent_compTC_B, time, 5)
slice(cent_compTC_B, time, 6)
slice(cent_compTC_B, time, 7)
# And those correspond to the years with the biggest changes in lippia, as expected



# Scratch to figure out lippia dependence that makes sense ----------------

# The condition here is seed survival going down as more days over 60, but there just aren't that many
centchangeS <- centipeda_topclimYr$seed60_Centipeda_yr - 
  centipeda_baseYr$seed60_Centipeda_yr
# Not much different,  but different

# These are identical because the thresholds were 0.2 and 0.1, and so don't
# respond to changing 0.5 to 0.9
centchangeG <- centipeda_topclimYr$germ_Centipeda_yr - 
  centipeda_baseYr$germ_Centipeda_yr
# identical


centchangeF <- centipeda_topclimYr$fruit10_Centipeda_yr - 
  centipeda_baseYr$fruit10_Centipeda_yr
# identical

centchangeSG <- centipeda_topclimYr$seedGerm_Centipeda_yr - 
  centipeda_baseYr$seedGerm_Centipeda_yr
# very close to identical

centchangeSGF <- centipeda_topclimYr$seedGermFruit_Centipeda_yr - 
  centipeda_baseYr$seedGermFruit_Centipeda_yr
# VERY close to identical

# So there are good reasons the centipeda wasn't affected, but if lippia was, still should get a change. Unless lippia doesn't actually matter
# How to check? are there no overlaps in occurrence??
centipeda_topclimYr$seedGermFruit_Centipeda_yr # where cent is
lippia_topclimYr$fullCycle_Lippia_yr # where lippia is
# with a lippia threshold of 1
# the overlap is where centipeda would be, but lippia is successful all year
centipeda_topclimYr$seedGermFruit_Centipeda_yr * (lippia_topclimYr$fullCycle_Lippia_yr == 1)
centipeda_baseYr$seedGermFruit_Centipeda_yr * (lippia_baseYr$fullCycle_Lippia_yr == 1)
# Effectively nowhere has overlap
# what if I change the lippia requirement?
centipeda_baseYr$seedGermFruit_Centipeda_yr * !(lippia_baseYr$fullCycle_Lippia_yr > 0.9)
centipeda_topclimYr$seedGermFruit_Centipeda_yr * !(lippia_topclimYr$fullCycle_Lippia_yr > 0.9)
centipeda_topYr$seedGermFruit_Centipeda_yr * !(lippia_topYr$fullCycle_Lippia_yr > 0.9)
# Those are different (though not much). why are these the same???
centipeda_topYr$fullCycleLippia_Centipeda_yr
centipeda_baseYr$fullCycleLippia_Centipeda_yr
# Clearly this seems wrong?
centipeda_topYr$fullCycleLippia_Centipeda_yr == centipeda_baseYr$fullCycleLippia_Centipeda_yr


# Is it that the differences occur only in the wrong ANAE zones? Yep. At least
# now I know WHAT's happening. To fix it though...
  # 'Fix' is a funny word, I guess. Make it so there's a visible change
isANAE <- LachlanANAE$ANAE_CODE %in% c("Pt1.2.1","Pt1.8.1")
centipeda_baseYr$seedGermFruit_Centipeda_yr * isANAE *
  !(lippia_baseYr$fullCycle_Lippia_yr > 0.9)
centipeda_topYr$seedGermFruit_Centipeda_yr * isANAE *
  !(lippia_topYr$fullCycle_Lippia_yr > 0.9) 

# Let's see if I can get a visible change without too much thrashing about
# what do we want? Also remember that there's a time lag, but hopefully if we get this to differ, it'll differ in other years too
# cut centipeda to the anae (lippia fullcycle already is) for testing values
cta <- centipeda_topYr$seedGermFruit_Centipeda_yr * isANAE
cba <- centipeda_baseYr$seedGermFruit_Centipeda_yr * isANAE

# try different lippia thresholds, otherwise will have to actually go re-jig the
# way the strictures work (either in terms of lippia success or centipeda
# response)
  # sum gives the total propdayspassed- NOT A LOGICAL OF PASS/FAIL
  # as the prop goes down, I think it just converges on places where there's not overlap between the species
sum((cba * 
  !(lippia_baseYr$fullCycle_Lippia_yr > 0.66))[[1]]) 
sum((cta * 
  !(lippia_topYr$fullCycle_Lippia_yr > 0.66))[[1]])

# Let's plot it
propdiff <- vector(mode = 'numeric', length = 100)
reldiff <- propdiff
for (i in 1:100) {
  propdiff[i] <- sum((cta * 
                        !(lippia_topYr$fullCycle_Lippia_yr > (i/100)))[[1]]) - 
    sum((cba * 
         !(lippia_baseYr$fullCycle_Lippia_yr > (i/100)))[[1]]) 
  reldiff[i] <- propdiff[i]/sum((cba * 
                                   !(lippia_baseYr$fullCycle_Lippia_yr > (i/100)))[[1]])
  
}

plot(propdiff)
plot(reldiff) 
# So the max reldiff is ~5%
which(reldiff == max(reldiff))
which(propdiff == max(propdiff))

# Let's try that?
# 75 still SHOULD have been noticeable. But 90 not.


# Are the constituent parts identical
lippia_topYr$fullCycle_Lippia_yr == lippia_baseYr$fullCycle_Lippia_yr
# no
centipeda_topYr$seedGermFruit_Centipeda_yr == centipeda_baseYr$seedGermFruit_Centipeda_yr
# yes
clt <- centipeda_topYr$seedGermFruit_Centipeda_yr * !(lippia_topYr$fullCycle_Lippia_yr > 0.9)
clb <- centipeda_baseYr$seedGermFruit_Centipeda_yr * !(lippia_baseYr$fullCycle_Lippia_yr > 0.9)
clt == clb

centipeda_baseYr$seedGermFruit_Centipeda_yr * (lippia_baseYr$fullCycle_Lippia_yr > 0.75)
centipeda_topclimYr$seedGermFruit_Centipeda_yr * (lippia_topclimYr$fullCycle_Lippia_yr > 0.75)
