# demo plots for CEWO 1/12/20

  # Lachlan with a few demo strictures for centipeda and lippia based on soil moisture and temp

  # Plots here were all just copy-pasted into ppt, so little effort went into saving them

  # Most of the catchment plots are done very similarly to the plotting
  # functions in helpers, but I pulled them out here to do things like take
  # logs, etc.(ie get more control)
  
library(here)
library(calecopal)
library(lubridate)

# Get the bb stuff and example plots from those and scratch and addSoilMoistLachDemo.R




# Make some examples in the bbox ------------------------------------------
whichcrs <- st_crs(lachAll)

# it's a little annoying that st_crop is a hard crop, but the [] cropping lets
# the polys hang over, and so we get different extents. So doing the polygon
# crops a bit different than above, so we DO get the same extents

# Let's set up a bbox for subsampled plotting without taking 8 million years
bb = st_bbox(c(xmin = 144.4, ymin = -33.8, xmax = 144.6, ymax = -33.6), crs = whichcrs)
plot(bb)
plot(st_as_sfc(bb))
plot(lachOnly)

# Where is the box?
ggplot() + 
  geom_sf(data = ltimCut, aes(fill = ValleyName)) + 
  geom_sf(data = st_as_sfc(bb)) +
  # # geom_sf(data = roads) +
  # coord_sf(xlim = c(144, 145),
  #          ylim = c(-35.5, -36)) +
  # scale_fill_brewer(palette = 'SeltimCutt1') +
  theme_bw() + theme(legend.position = 'top')


ggplot() + 
  geom_sf(data = filter(ltimCut, ValleyName == 'Lachlan'), aes(fill = ValleyName)) + 
  geom_sf(data = st_as_sfc(bb)) +
  # # geom_sf(data = roads) +
  # coord_sf(xlim = c(144, 145),
  #          ylim = c(-35.5, -36)) +
  # scale_fill_brewer(palette = 'SeltimCutt1') +
  theme_bw()  + theme(legend.position = 'none')

# Let's try to find a couple that are more relevant
cumbung <- st_bbox(c(xmin = 143.85, ymin = -34.35, xmax = 144.4, ymax = -34.05), crs = whichcrs)

ggplot() + 
  geom_sf(data = filter(ltimCut, ValleyName == 'Lachlan'), aes(fill = ValleyName)) + 
  geom_sf(data = st_as_sfc(cumbung)) +
  theme_bw()  + theme(legend.position = 'none')

# ANAE
anaeSubC <- st_crop(lachAll, st_as_sfc(cumbung))
# plot(anaeSub[,'ANAE_DESC'])

# Are there ANY of the relevant ANAE codes
# c("Pt1.2.1","Pt1.8.1")
# Yeah, so let's stay with this BB I guess
ggplot() + 
  geom_sf(data = anaeSubC, aes(fill = ANAE_CODE %in% c("Pt1.2.1","Pt1.8.1"))) + 
  theme_bw()

# That'll work

# Try for the booligal area in center
booligal <- st_bbox(c(xmin = 144.5, ymin = -33.9, xmax = 145.3, ymax = -33.4), crs = whichcrs)

ggplot() + 
  geom_sf(data = filter(ltimCut, ValleyName == 'Lachlan'), aes(fill = ValleyName)) + 
  geom_sf(data = st_as_sfc(booligal)) +
  theme_bw()  + theme(legend.position = 'none')

# ANAE
anaeSubB <- st_crop(lachAll, st_as_sfc(booligal))
# plot(anaeSub[,'ANAE_DESC'])

# Are there ANY of the relevant ANAE codes
# c("Pt1.2.1","Pt1.8.1")
ggplot() + 
  geom_sf(data = anaeSubB, aes(fill = ANAE_CODE %in% c("Pt1.2.1","Pt1.8.1"))) + 
  theme_bw()

# Town of Booligal is centre bottom (-33.9, 144.9)


# Argh. Find a bigger color scale
# library(calecopal)
kelppal <- cal_palette(name = "kelp1", n = 21, type = "continuous")
# That'll do, I guess
ggplot() + 
  geom_sf(data = anaeSubC, aes(fill = ANAE_DESC)) + 
  # geom_sf(data = st_as_sfc(bb)) +
  # # geom_sf(data = roads) +
  # coord_sf(xlim = c(144, 145),
  #          ylim = c(-35.5, -36)) +
  scale_fill_manual(values = kelppal) +
  # scale_fill_brewer(palette = 'Set2') +
  theme_bw() + theme(legend.position = 'none') + ggtitle('ANAE Classifications')

ggplot() + 
  geom_sf(data = anaeSubB, aes(fill = ANAE_DESC)) + 
  # geom_sf(data = st_as_sfc(bb)) +
  # # geom_sf(data = roads) +
  # coord_sf(xlim = c(144, 145),
  #          ylim = c(-35.5, -36)) +
  scale_fill_manual(values = kelppal) +
  # scale_fill_brewer(palette = 'Set2') +
  theme_bw()  #+ theme(legend.position = 'none')

# So, Cumbung is prettier (more zones), Booligal more likely to have hits on the strictures. I guess just go from here and see what they look like?


# Get some base variables -------------------------------------------------
# Read the files in
load(file.path(datOut, 'lachTempMatched.rdata'))
load(file.path(datOut, 'lachSMMatched.rdata'))

# The relevant stars are made and discarded in the stricture functions, so make
# them again here for illustration. I don't really want to return them there, I
# don't think. Though I suppose could make it a switch

soilMoist_Min5 <- dailyPolySMavg # initialize

system.time(soilMoist_Min5[[1]] <- timeRoll(soilMoist_Min5[[1]], 
                                            FUN = RcppRoll::roll_min, 
                                            rolln = 5, 
                                            align = 'right',
                                            na.rm = TRUE))

soilTemp <- dailyPolyTempavg - 273
rm(dailyPolyTempavg)
# Booligal is huge and takes forever.

# Soil moisture example (rolling min 5-day), on a couple days
SM4Crop <- soilMoist_Min5[,,202] # Get time slices first or it's absurd 200:201 isn't bad, but SUPER low
SM4Crop <- st_as_sf(SM4Crop, long = TRUE) # SUPER annoying to have to do this NOW, but it requires a square grid for stars
SM4Crop <- st_crop(SM4Crop, st_as_sfc(cumbung)) # Crop
# SM4Crop

# plot(SM4Crop)

# plot(soilSub[,,,10:13])
  # Argh. scale_id
ggplot() + 
  geom_sf(data = SM4Crop, aes(fill = sm_pct), color = NA) + 
  facet_wrap(vars(as.character(time))) +
  scale_fill_viridis(option = 'cividis', direction = -1) +
  theme_bw() + ggtitle('5-day min soil moisture')


# Soil temp
ST4Crop <- soilTemp[,,400] # Get time slices first or it's absurd
ST4Crop <- st_as_sf(ST4Crop, long = TRUE) # SUPER annoying to have to do this NOW, but it requires a square grid for stars
ST4Crop <- st_crop(ST4Crop, st_as_sfc(cumbung)) # Crop
# ST4Crop

# plot(ST4Crop)

# plot(soilSub[,,,10:13])
# Argh. scale_id
ggplot() + 
  geom_sf(data = ST4Crop, aes(fill = LST_Day_1km), color = NA) + 
  facet_wrap(vars(as.character(time))) +
  scale_fill_viridis(option = 'magma', name = 'Temp') +
  theme_bw() + ggtitle('Soil Temp (Daily)')


# Life stages pass/fail ---------------------------------------------------

# Seed survival
  # Maybe I should aggregate to year?
  # seedYr is proportion of days stricture met
seedCrop <- centipeda_baseYr$seed60_Centipeda_yr[,4:5,] # Get time slices first or it's absurd
seedCrop <- st_as_sf(seedCrop, long = TRUE) # SUPER annoying to have to do this NOW, but it requires a square grid for stars
seedCrop <- st_crop(seedCrop, st_as_sfc(cumbung)) # Crop
# ST4Crop

# plot(ST4Crop)

# plot(soilSub[,,,10:13])
# Argh. scale_id
ggplot() + 
  geom_sf(data = seedCrop, aes(fill = propDaysPassed), color = NA) + 
  facet_wrap(vars(as.character(time))) +
  scale_fill_viridis(option = 'plasma', name = 'Proportion\ndays\npassed') +
  theme_bw() + ggtitle('Seed survival')

# Seed survival and germ
# Nothing. what does germ look like
# Maybe I should aggregate to year?
# seedYr is proportion of days stricture met
sgaCrop <- centipeda_baseYr$seedGerm_Centipeda_yr[,4:5,] # Get time slices first or it's absurd
sgaCrop <- st_as_sf(sgaCrop, long = TRUE) # SUPER annoying to have to do this NOW, but it requires a square grid for stars
sgaCrop <- st_crop(sgaCrop, st_as_sfc(cumbung)) # Crop
# ST4Crop

# plot(ST4Crop)

# plot(soilSub[,,,10:13])
# Argh. scale_id
ggplot() + 
  geom_sf(data = sgaCrop, aes(fill = propDaysPassed), color = NA) + 
  facet_wrap(vars(as.character(time))) +
  scale_fill_viridis(option = 'plasma', name = 'Proportion\ndays\npassed') +
  theme_bw() + ggtitle('Survival and germination')

# Nothing. what does germ look like

# Germ
germCrop <- centipeda_baseYr$germ_Centipeda_yr[,,] # Get time slices first or it's absurd
germCrop <- st_as_sf(germCrop, long = TRUE) # SUPER annoying to have to do this NOW, but it requires a square grid for stars
germCrop <- st_crop(germCrop, st_as_sfc(cumbung)) # Crop
# ST4Crop

# plot(ST4Crop)

# plot(soilSub[,,,10:13])
# Argh. scale_id
ggplot() + 
  geom_sf(data = germCrop, aes(fill = propDaysPassed), color = NA) + 
  facet_wrap(vars(as.character(time))) +
  scale_fill_viridis(option = 'plasma', name = 'Proportion\ndays\npassed') +
  theme_bw() + ggtitle('germ')
# well, no germ is pretty lame. I think need to loosen that strict up a bit



# Spatial agg -------------------------------------------------------------
# fullCatch_Centipeda <- catchAggW(strict = fullYr_Centipeda, strictWeights = lachArea, 
#                                  FUN = sum, summaryPoly = lachOnly)
# 
# fullPlot_Centipeda <- catchAggPlot(fullCatch_Centipeda, title = 'Full Life Cycle Success')
# fullPlot_Centipeda

# Color and legend
fullPlot_Centipeda + scale_fill_viridis(option = 'plasma') + theme(legend.title = element_text('Area of Success'))


# Why doesn't the function allow me to change the legend? strip out the call and sort it out
  # Can do it withoin the scale_fill part
catchPlot <- ggplot() +
  geom_stars(data = fullCatch_Centipeda) +
  coord_sf() +
  facet_wrap(~as.character(time)) +
  theme_void()  +
  scale_fill_viridis(option = 'viridis') +
  ggtitle('Full life cycle success')
catchPlot # + theme(legend.title = element_text('Area of success'))

# Geom_stars is doing somethign weird under the hood with titles
  # and it's hard to get at the data

fullCatch_Centipeda
# Let's sf-it. that will be easier, I think

CatchCent_sf <- st_as_sf(fullCatch_Centipeda, long = TRUE)

ggplot() + 
  geom_sf(data = CatchCent_sf, aes(fill = log(areaDaysPassed)), color = NA) + 
  facet_wrap(vars(as.character(as.Date(time))), ncol = 2) +
  scale_fill_viridis(option = 'plasma', name = 'Area of\nsuccess\n(log)') +
  theme_bw() + ggtitle('Yearly Life Cycle Success') +
  theme(legend.position = c(0.75, 0.1), legend.direction = 'horizontal')



# Make a basin plot for full cycle wo centipeda but cutting to ANAE -------------------------------------------------------
fullBasin_Centipeda <- catchAggW(strict = centipeda_baseYr$seedGermFruit_Centipeda_yr*centipeda_base$isANAE_Centipeda, 
                                 strictWeights = lachArea, 
                                 FUN = sum, summaryPoly = ltimCut)
names(fullBasin_Centipeda) <- 'areaDaysPassed'
mdbCent_sf <- st_as_sf(fullBasin_Centipeda, long = TRUE)

ggplot() + 
  geom_sf(data = mdbCent_sf, aes(fill = log(areaDaysPassed))) + 
  facet_wrap(vars(as.character(as.Date(time))), ncol = 2) +
  scale_fill_viridis(option = 'plasma', name = 'Area of\nsuccess\n(log)') +
  theme_bw() + ggtitle('Yearly Life Cycle Success') +
  theme(legend.position = c(0.75, 0.1), legend.direction = 'horizontal')


ggplot() + 
  geom_sf(data = filter(mdbCent_sf, time > ymd('20170501') & time < ymd('20190501')), 
          aes(fill = log(areaDaysPassed))) + 
  facet_wrap(vars(as.character(as.Date(time))), ncol = 2) +
  scale_fill_viridis(option = 'plasma', name = 'Area of\nsuccess\n(log)') +
  theme_bw() + ggtitle('Yearly Life Cycle Success') +
  theme(legend.position = 'bottom')



fullBasin_Lippia <- catchAggW(strict = lippia_baseYr$fullCycle_Lippia_yr,
                              strictWeights = lachArea, 
                                 FUN = sum, summaryPoly = ltimCut)
names(fullBasin_Lippia) <- 'areaDaysPassed'
mdbLip_sf <- st_as_sf(fullBasin_Lippia, long = TRUE)

ggplot() + 
  geom_sf(data = mdbLip_sf, aes(fill = log(areaDaysPassed))) + 
  facet_wrap(vars(as.character(as.Date(time))), ncol = 2) +
  scale_fill_viridis(option = 'plasma', name = 'Area of\nsuccess\n(log)') +
  theme_bw() + ggtitle('Yearly Life Cycle Success') +
  theme(legend.position = c(0.75, 0.1), legend.direction = 'horizontal')

# Just make a plot of the basin -------------------------------------------

# with the kelp colors
kelp2 <- cal_palette(name = "kelp1", n = 28, type = "continuous")

ggplot() + 
  geom_sf(data = ltimCut, aes(fill = ValleyName)) + 
  # geom_sf(data = st_as_sfc(bb)) +
  # # geom_sf(data = roads) +
  # coord_sf(xlim = c(144, 145),
  #          ylim = c(-35.5, -36)) +
  scale_fill_manual(values = kelp2) +
  theme_bw() + theme(legend.position = 'none')


# Can I plot the whole of the lachlan for just a couple timesteps> --------

# Just use something at random
seedGerm_Centipeda_Sum42 <- centipeda_base$seedGerm_Centipeda # initialize

system.time(seedGerm_Centipeda_Sum42[[1]] <- timeRoll(centipeda_base$seedGerm_Centipeda[[1]],
                                                      FUN = RcppRoll::roll_sum,
                                                      rolln = 42,
                                                      align = 'right',
                                                      na.rm = TRUE))

lach4d <- st_as_sf(seedGerm_Centipeda_Sum42[,,228:231], long = TRUE)

ggplot() + 
  geom_sf(data = lachOnly, fill = 'antiquewhite') +
  geom_sf(data = lach4d, aes(fill = passedStricts), color = NA) + 
  
  facet_wrap(vars(as.character(as.Date(time))), ncol = 2) +
  scale_fill_viridis(option = 'plasma', name = 'DaysPassing') +
  theme_bw() + ggtitle('Yearly Life Cycle Success') +
  theme(legend.position = c(0.75, 0.1), legend.direction = 'horizontal') 

