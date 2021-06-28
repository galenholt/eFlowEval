# raster scratch

# Let's get libraries here, then sort out git then sort out making this a library so we don't have to deal with all the library crap
# library(sp)
# library(rgeos)
# library(here)
library(tidyverse)
# library(sf)
# library(RNetCDF) # Probably not, raster can handle geographic netCDF
library(raster) # maybe?
library(stars)
# library(ggplot2)
# library(stringr)

myhome <- str_remove(path.expand("~"), "/Documents")
datDir <- file.path(myhome, "Deakin University/QAEL - MER/Model/dataBase") # "C:/Users/Galen/Deakin University/QAEL - MER/Model/dataBase"

datOut <- "datOut"

# polygon data -------------------------------------------------------------

# And get the LTIM_Valleys to use to subset for toy models at scale, but not enormous scale
LTIM_Valleys <- read_sf(dsn = file.path(datDir, 'ANAE/MDB_ANAE_Aug2017/MDB_ANAE.gdb'), layer = 'LTIM_Valleys') %>%
  st_cast("MULTIPOLYGON") # cleans up an issue with multisurfaces

# and the basin boundary, might be useful, especially for clipping rasters
basin <- read_sf(dsn = file.path(datDir, 'ANAE/MDB_ANAE_Aug2017/MDB_ANAE.gdb'), layer = 'MDB_Boundary') %>%
  st_cast("MULTIPOLYGON")  %>% # cleans up an issue with multisurfaces
  dplyr::select(LEVEL2NAME) # no need for other info

# LTIM areas
ltimNoNorth <- LTIM_Valleys %>%
  dplyr::select(ValleyName, ValleyID, ValleyCode) # Three different ways to reference, basically


# Read in soil moisture data ----------------------------------------------
# Top bit is stolen from EPA, but I think use raster
# # Open a coneection to the netcdf
# soil.nc <- open.nc(file.path(datDir, 'soilmoisture/sm_pct_2020_Actual_day.nc'))
# 
# # Below tells you what's in the nc
# file.inq.nc(soil.nc) # what's it like?
# 
# print.nc(soil.nc) # get some more info about the structure and its variables
# 
# # # The above is metadata, the below gets the data
# soilDat <- read.nc(soil.nc)
# str(soilDat) # Look at it again

# soilMoist = stack(file.path(datDir, 'soilmoisture/sm_pct_2020_Actual_day.nc'))
# dim(soilMoist)
# plot(soilMoist[[10]])
# plot(soilMoist[[200]])


# Or, perhaps `stars` package, which is made to interface with sf
# it uses RNetCDF to get netCDF, but then does some post-processing etc to make compatible
# or possibly through GDAL? test and check

# uses RNetCDF, faster
soilMstars <- read_ncdf(file.path(datDir, 'soilmoisture/sm_pct_2020_Actual_day.nc'))

# uses GDAL, able to save on disk and only access as needed. So slower because
# more overhead, but able to work when memory-limited
# soilMstarsG <- read_stars(file.path(datDir, 'soilmoisture/sm_pct_2020_Actual_day.nc'))

# No idea why crs not coming in for the gdal
# soilcrs <- st_crs(soilMstars)
# soilMstarsG <- st_set_crs(soilMstarsG, soilcrs)

soilMstarsSub <- soilMstars %>%
  dplyr::slice(time, c(1, 50, 100, 200)) # slice is like filter, but using indices instad of names/boolean

soilMstarsSub

# Look at those
plot(soilMstarsSub)

# ggplot probably works, but is SUPER slow. I think plot() must auto-downscale,
# and ggplot plots every single tile
# Seems to still be slow for the GDAL version, even though that version downscales plot()
# soilMstarsSubG <- soilMstarsG %>%
#   dplyr::slice(time, c(1, 50, 100, 200))
# That's not acutally slicing. Looks like it's trying to be lazy, but the slicing ends up not happning
# 
# # WHY IS THIS ERRORING???
# ggplot() +
#   geom_stars(data = soilMstars, downsample = c(10, 10, 10)) +
#   coord_equal() +
#   facet_wrap(~time) +
#   theme_void() +
#   scale_fill_viridis() +
#   scale_x_discrete(expand = c(0, 0)) +
#   scale_y_discrete(expand = c(0, 0))



# A couple tests with the raster cube and clipping et
# Overlay doesn't work for multiple slices
supersub <- soilMstars %>% dplyr::slice(time, 1)

plot(supersub, reset = FALSE)
plot(basin, add = TRUE, reset = FALSE, col = NA, border = 'red')

# still trying to follow vignette https://r-spatial.github.io/stars/articles/stars1.html
basinMatch <- st_transform(basin, st_crs(supersub)) # datum transformation
plot(supersub[basinMatch], reset = FALSE)
plot(basinMatch, add = TRUE, reset = FALSE, col = NA, border = 'red')

# Can I clip the whole stars obj?
basinMatchA <- st_transform(basin, st_crs(soilMstars)) # datum transformation. same as above, but in case that gets commented
clipSoil <- soilMstars[basinMatchA]

clipslice <- clipSoil %>%
  dplyr::slice(time, c(1, 50, 100, 200))

plot(clipslice)

# Cool. So, that works to extract the intersections over all sheets

# Now, can I sort out how to find means over a polygon? And what does that return?

# try with the catchments first
valleyMatch <- st_transform(ltimNoNorth, st_crs(soilMstars))
moistVal <- aggregate(soilMstarsSub, by = valleyMatch, FUN = mean)

# Things seem to happen differently with the gdal, so try that too
# valleyMatchG <- st_transform(ltimNoNorth, st_crs(soilMstarsG))
# moistValG <- aggregate(soilMstarsSubG, by = valleyMatch, FUN = mean, na.rm = TRUE)

# Valleymatch looks as expected, right?
ggplot(valleyMatch) + geom_sf(aes(fill = ValleyName))

# This worked, and now it doesn't. WTH?
  # Fails without a max.plot argument. Which is annoying, and I think indicates
  # an error anyway. I think the same one that's killing everything, probably
    # AND, if there are only 2, why are there 2 and not one?
  # This SEEMS to be the same issue with uneven slicing yielding c(date, date, etc) rather than what we expect
  # but does it only show up with aggregate()?
plot(moistVal, max.plot = 2) 
plot(moistValG, max.plot = 2)

# There are 4 dates here: so is the aggregate screwing things up somehow?
plot(soilMstarsSub)

# Does it work if the slices are consecutive?
soilMstarsSubc <- soilMstars %>%
  dplyr::slice(time, 1:4)
plot(soilMstarsSubc)
moistValc <- aggregate(soilMstarsSubc, by = valleyMatch, FUN = mean)
plot(moistValc)

# What if slices are consecutive but don't start at 1?

soilMstarsSubc2 <- soilMstars %>%
  dplyr::slice(time, 202:208)
plot(soilMstarsSubc2)
moistValc2 <- aggregate(soilMstarsSubc2, by = valleyMatch, FUN = mean)
plot(moistValc2)
# Works

# What if the slicing is done with []
# whichslices <- c(1,5,10) # Doing it outside like this doesn't work either.
soilMstarsSubb <- soilMstars[,,,c(1,5,10)]
plot(soilMstarsSubb)    # as before, this still works
moistValb <- aggregate(soilMstarsSubb, by = valleyMatch, FUN = mean)
plot(moistValb) # BUt this doesn't

# How about filtering?
soilFiltc <- soilMstars %>%
  dplyr::filter(time < lubridate::ymd(20200104))
plot(soilFiltc)
agFiltc <- aggregate(soilFiltc, by = valleyMatch, FUN = mean)
plot(agFiltc)
# Works

# two-condition filtering, but still continuous
  # WOrks. but uses the weird , instead of &
soilFilt2 <- soilMstars %>%
  dplyr::filter(time > lubridate::ymd(20200104), time < lubridate::ymd(20200108))
plot(soilFilt2)
agFilt2 <- aggregate(soilFilt2, by = valleyMatch, FUN = mean)
plot(agFilt2)

# discontinuous filtering
soilFiltu <- soilMstars %>%
  dplyr::filter(time < lubridate::ymd(20200104) | time > lubridate::ymd(20200722))
plot(soilFiltu)
agFiltu <- aggregate(soilFiltu, by = valleyMatch, FUN = mean)
plot(agFiltu)



# do I fundamentally not understand the dimensions
# See http://132.72.155.230:3838/r/raster-algebra.html
st_dimensions(soilMstars)
str(st_dimensions(soilMstars))
# Look at time, two different ways
str(st_dimensions(soilMstars)[[3]])
str(st_dimensions(soilMstars)$time)

# Look at the values
head(st_get_dimension_values(soilMstars, "time"))

# That would let me write conditionals I want to get indices, if I could then slice() or [] on them
# e.g.
timevals <- st_get_dimension_values(soilMstars, "time")
timeends <- which(timevals < lubridate::ymd(20200104) | timevals > lubridate::ymd(20200722))
plot(soilMstars[,,,timeends])

#So, that works fine. The issue is the aggregating. That's good, I think
  # I wonder if aggregate() works if instead of slcigin discontinous, I have a
  # weekly average or something? In reality, it'd be running, probably, but may
  # not, if trying to match other datasources

# Going to need to sort that out anyway. So i suppose try that now, and then if it fails too, elevate to an issue on git


# Time-grouped means etc ------------------------------------------------------


# Can I use st_apply to get the time mean? As a start over evertything?
# God, apply syntax confuses me. MARGIN = 3 returns JUST that margin, so averages over space
  # Pixel means need margin = 1:2
  # So, one solution is just to do this in a loop over chunks
timemean <- st_apply(X = soilMstars, MARGIN = 1:2, FUN = mean, na.rm = TRUE)
timemean
plot(timemean)
# Can I aggregate that?
agApp <- aggregate(timemean, by = valleyMatch, FUN = mean)
plot(agApp)
# Cool

# Fine. but, can I roll? Or weekly, or something?
# Some of the stuff on focal filters might be good. They're spatial, not temporal, but should be the same idea. Basically suggest have to roll your own and for-loop them
# http://132.72.155.230:3838/r/geometric-operations-with-rasters.html#focal-filters

# # So, one option is to chunk into weeks (leave the date handling for later, just 7-day units for now), then for-loop over them?
# timevals <- st_get_dimension_values(soilMstars, "time")
# lubridate::week(timevals)
# # well, that's funny for the first one, but whatever
#   # Would sure be nice to be able to do a group_by and summarize() here...
# # suppose I could try...
# soilMstarsG <- soilMstars %>% group_by(lubridate::week(time)) %>% summarize(meanprecip = mean(sm_pct))
# # nope
# # but the first vignette implies it MIGHT work with a tbl_cube
# # https://r-spatial.github.io/stars/articles/stars1.html
# # Yeesh. that's gonna be a pain
# # soilMstarsG <- soilMstars %>% 
# #   as.tbl_cube.stars() %>%
# #   group_by(lubridate::week(time)) %>%
# #   summarize(meanprecip = mean(sm_pct))
# 
# # cut to make the indexing not stupid
# soilMstarsCut <- soilMstars[,,,2:208]
# timevals <- st_get_dimension_values(soilMstarsCut, "time")
# soilweeks <- lubridate::week(timevals)
# 
# # AAAA so hacky
# weekind <- which(soilweeks == 1)
# weekstart <- timevals[weekind[1]]
# weekend <- timevals[weekind[length(weekind)]]
# weekavg <- st_apply(X = soilMstars[,,,weekind], MARGIN = 1:2, FUN = mean, na.rm = TRUE)
# 
# weekavg <- list()
# for (i in 1:3) { # max(soilweeks)
#   weekind <- which(soilweeks == i)
#   weekstart <- timevals[weekind[1]]
#   weekend <- timevals[weekind[length(weekind)]]
#   weekavg[[i]] <- st_apply(X = soilMstars[,,,weekind], MARGIN = 1:2, FUN = mean, na.rm = TRUE)
#   # weekavg <- c(weekavg, weekavgsingle, along = 3)
# }


# DOES aggregate() work??
weekmeans <- aggregate(soilMstarsCut, by = 'weeks', FUN = mean, na.rm = TRUE)
# Yep. well, that was a wasted morning... Oh well
# Note also that we can pass a vector of times if we want something else: 
  # for temporal aggregation a vector with time values (Date, POSIXct, or PCICt)
  # that is interpreted as a sequence of left-closed, right-open time intervals
  # or a string like "months", "5 days" or the like (see cut.POSIXt)
plot(weekmeans)


# Rolling means, maxs etc -------------------------------------------------
# Ugh. might have to either roll my own or do some fancy dimension swap back and forth or something.
testdim <- st_dim_to_attr(weekmeans, which = 1) # Time ended up in dim 1 after the aggregate
# That loses the attribute. So, bad idea
# I think come back to this if we need it. I think we can get around it for the moment? Though really, a lot of the max temp or days since business is going to need it.
  # can I do that on the polygons? Thinking specifically of getting this into a dataframe format, where it'd be easy. BUT, not sure that's true, because theres a massive advantage of staying in a cube
library(RcppRoll)
roll_max(weekmeans, n = 4, align = 'center')


# Need a smaller bit to test
soilShort <- soilMstars %>%
  dplyr::slice(time, 1:50) %>%
  dplyr::slice(longitude, 500:800) %>%
  dplyr::slice(latitude, 200:500)

plot(soilShort[,,,1:4]) 

# Creates a new dimension, not time
timemeanRoll <- st_apply(X = soilShort, MARGIN = 1:2, FUN = roll_mean, n = 5, na.rm = TRUE)
plot(timemeanRoll[,,,10:20])

# Nope
# tryagain <- aggregate(soilShort, by = 'days', FUN = roll_mean, n = 5, na.rm = TRUE)

st_raster_type(soilShort)

argh <- pull(soilShort)

# try something
testrast <- as(soilShort, "Raster")
plot(testrast)
# Why does that lose time, and just end up with "layer"?

# Suppose I can probably read it in with Raster:: something, but then what do I lose? I think the ability to do the aggregation by polygons?
# The following doesn't work, but testrast does. looks like there are methods for raster objects, so just need to try them
# test <- as.data.frame.table(apply(soilShort, c(2,3), rollmean, 2))
# plot(test)



# try using movingFun inside calc. Not entirely sure why inside calc, btut whatever
  # And unclear why it knows to go on dim 3? Besides that that is just the
  # expected behavior (from ?calc:  RasterBrick, fun should operate on a vector
  # of values (one vector for each cell))
# https://gis.stackexchange.com/questions/139253/how-to-compute-climatology-of-3-days-moving-averages-for-rasters-pixel-by-pixel
testroll <- calc(testrast, function(x) movingFun(x, 3, mean))
# Does it work for the stars? Nope
# testroll <- calc(soilShort, function(x) movingFun(x, 3, mean))
# That does appear to have done something, layer 4 and 5 show smoothing between them
plot(testroll)

# Try a longer range and min over previous 10 days, roughly matching the sort of thing we'd want to do for say "needs 10 days of temps above x"
testroll <- calc(testrast, function(x) movingFun(x, 10, min, type = 'to'))
# That does appear to have done something, layer 4 and 5 show smoothing between them
plot(testroll)

# Cool, so that works. And we can likely crop the raster to the basin extent or something to make it a bit faster


# Still to do -------------------------------------------------------------

# Get that raster back to stars so we can average over the polygons (or just go
# straight from raster to polygon, possibly if that's a thing)
  # Can I just aggregate()?
# Well, doesn't throw error, but doesn't work either
agRoll <- aggregate(testroll, by = valleyMatch, FUN = mean)
plot(agRoll)

rollstars <- st_as_stars(testroll)
plot(rollstars)
# That seems to be working, but it's lost the time, and the attributes have the wrong name
  # basically, it has screwed up the naming and scaling. Time isn't even there
  # in testrast. So some of this happened going from stars to raster

agRoll <- aggregate(rollstars[,,,10:15], by = valleyMatch, FUN = mean)
plot(agRoll)

# Cool. But where did time go?

# Do something similar after cutting to polygon, so we have the option of going
# either way: ie moving average or chunk average the raster, then spatial mean
# (or max or whatever), or spatial mean and then time average
  # Something like a rolling on 
moistValc
# Which is not a raster, and so probably can't use movingFun on it






















# Other stuff -------------------------------------------------------------


# How about the aggregations that happened consecutively, but then make discontinuous?
st_dimensions(moistValc)
# One less dimension because now it's a polygon instead of x-y
str(st_dimensions(moistValc)[[2]])
plot(moistValc[,,1:3])
plot(moistValc[,,c(1,3)]) # Plots, but wrong again.

# What does this do? suggested here: http://132.72.155.230:3838/r/combining-rasters-and-vector-layers.html
moistSF <- st_as_sf(moistVal)
# Yeesh. what has happened? Shouldn't have lost attributes OR ended up with such weird names
  # Hacky to try to plot, will need to sort out the WHY though
names(moistSF)[1:4] <- c('first', 'second', 'third', 'fourth')

# GODG{D:KSD:GLkj}
moistSFp <- tidyr::pivot_longer(moistSF, cols = -Shape)

# Jesus. what the hell is screwed up here?? I know there's data in those NA regions
ggplot(moistSF) + geom_sf(aes(color = first, fill = first)) 


# 
# moistPlot <- ggplot() + 
#   # geom_stars(data = valleyMatch, 
#   #         aes(color = ValleyName), alpha = 0.5) +
#   geom_stars(data = moistVal, aes(fill = sm_pct)) +
#   facet_grid(~time) +
#   # Fill doesn't work without closed shape, as happens with the coord_sf call below
#   # coord_sf(xlim = c(145.65, 145.71),
#   #          ylim = c(-35.94, -35.89)) +
#   theme_bw() + theme(legend.position = 'none')
# 
# moistPlot

# Try tutorial to see if something has changed ----------------------------
# https://geobgu.xyz/r/combining-rasters-and-vector-layers.html

# ndvi
r = read_stars(here::here('Testing', "MOD13A3_2000_2019.tif"))
names(r) = "NDVI"
r = st_warp(r, crs = 32636)
dates = read.csv(here::here('Testing', "MOD13A3_2000_2019_dates2.csv"), stringsAsFactors = FALSE)
dates$date = as.Date(dates$date)
r = st_set_dimensions(r, 3, values = dates$date, names = "time")

# Some averages 
r_avg = st_apply(r, 1:2, mean, na.rm = TRUE)
names(r_avg) = "NDVI"


# Borders
borders = st_read(here::here('Testing', "israel_borders.shp"), stringsAsFactors = FALSE)


# nafot mask
nafot = st_read(here::here('Testing', "nafot.shp"), stringsAsFactors = FALSE)
nafot = st_transform(nafot, st_crs(r))
pol = nafot[nafot$name_eng == "Be'er Sheva", ]

# single-dimension
r_avg = r_avg[borders]
ndvi_nafot = aggregate(x = r_avg, by = nafot, FUN = mean, na.rm = TRUE)
ndvi_nafot_sf <- st_as_sf(ndvi_nafot)
plot(ndvi_nafot)
plot(ndvi_nafot_sf)

# multi-dim
ndvi_nafot_multi = aggregate(r[,,,1:5], nafot, FUN = mean, na.rm = TRUE)
ndvi_nafot_mSF <- st_as_sf(ndvi_nafot_multi)
plot(ndvi_nafot_multi)
plot(ndvi_nafot_mSF)


# So, that DOES seem to work
# So, how does r differ from soilMstarsSub
  # and I htink the indexing of r[,,,1:5] and dplyr::slice(soilmoisture)?

# I made 
# soilMstarsSub <- soilMstars %>%
#   dplyr::slice(time, c(1, 50, 100, 200)) ,
# While r was subset with [] indexing.

# how do the inputs compare
r
soilMstars

# basically the same, but 
# r time has NA offset and delta, Date is the refsys, and has values
# soilMstars has offset and delta values, refsys is posixct, and values is NULL

# What is 
r[,,,1:5]
soilMstars[,,,1:5]
# sure look similar
# what about
dplyr::slice(r, time, 1:5)
dplyr::slice(soilMstars, time, 1:5)
# those look the same
r[,,,1:5] == dplyr::slice(r, time, 1:5) # looks like everything evaluates to either true or na

# try to do the soil aggregate again with the []
moistValbrack <- aggregate(soilMstars[,,,1:5], by = valleyMatch, FUN = mean, na.rm = TRUE)
# wtf. 
moistValbrack
names(moistValbrack)
plot(moistValbrack)
moistB <- st_as_sf(moistValbrack)
moistB
plot(moistB)

# so, why doesn't slice work?

# DOES slice work if it's colon?
moistValslice <- aggregate(dplyr::slice(soilMstars, time, 1:5), by = valleyMatch, FUN = mean, na.rm = TRUE)
moistValslice
names(moistValslice)
plot(moistValslice)

moistS <- st_as_sf(moistValslice)
moistS

# HUH. Why doesn't uneven indexing work?
# as a test, this fails:
moistValbrackuneven <- aggregate(soilMstars[,,,c(1,10,20)], by = valleyMatch, FUN = mean, na.rm = TRUE)

# and what happened to the feature info (e.g. valleyname?) Suppose I could add
# in later. Might be better anyway, rather than duplicating it t times
# AND WHY ARE SOME NA? they can't possibly actually be NA for every cell in the condamine, for ex. 
    # AND WHY DO THEY PLOT() as values, but ggplot() as NA?

ggplot() +
  geom_stars(data = moistValbrack) # +
  # facet_wrap(~time) +
  # theme_void() +
  # # scale_fill_viridis() +
  # scale_x_discrete(expand = c(0, 0)) +
  # scale_y_discrete(expand = c(0, 0))
ggplot() + geom_sf(data = moistS, aes(fill = `2019-12-31 12:00:00`))


# Does r work with uneven? NO. So it's something I'm really not understanding with the indexing
ndvi_nafot_multiU = aggregate(r[,,,c(1,3,6)], nafot, FUN = mean, na.rm = TRUE)
ndvi_nafot_mSFU <- st_as_sf(ndvi_nafot_multiU)
plot(ndvi_nafot_multiU)
plot(ndvi_nafot_mSFU)

# what happens if I SKIP the indexing? I REALLY don't understand why the slicing isn't working, and will need to figure this out if we want to do ANYTHING other than daily
moistValsSum <- aggregate(soilMstars, by = valleyMatch, FUN = mean, na.rm = TRUE)
moistValsSum
names(moistValsSum)
plot(moistValsSum, max.plot = 15)

moistSs <- st_as_sf(moistValsSum)
moistSs
# SO, that all seems to work, and happen pretty quick. Maybe we start there? Still unclear why we ALWAYS have NA in the northern basin
ggplot() + geom_sf(data = moistSs, aes(fill = `2019-12-31 12:00:00`))
ggplot() + geom_sf(data = moistSs, aes(fill = `2020-06-30 12:00:00`))


# Why are some NA? --------------------------------------------------------

# From above:
# DOES slice work if it's colon?
moistValslice <- aggregate(dplyr::slice(soilMstars, time, 1:5), by = valleyMatch, FUN = mean, na.rm = TRUE)
moistValslice
names(moistValslice)
plot(moistValslice)

moistS <- st_as_sf(moistValslice)
moistS
valleyMatch

basinPlot <- ggplot() + geom_sf(data = moistS, aes(fill = `2020-01-01 12:00:00`)) +
  geom_sf(data = valleyMatch, aes(color = ValleyName), fill = NA) +
  scale_fill_gradient(low = 'tan', high = 'forestgreen') +
  geom_sf_label(data = valleyMatch, aes(label = ValleyName))
basinPlot

# Cut to two catchments and 1 time
valCut <- dplyr::filter(valleyMatch, ValleyName %in% 
                   c('Paroo', 'Warrego', 'Barwon Darling', 
                     'Condamine Balonne', 'Northern Unregulated'))
# Including 'Northern Unregulated' breaks it, AND breaks the warrgeo, condamine, and B-D
  # Norther unreg. INCLUDES the others. That can't happen.
  # SO, will likely always need a check to ensure there are no overlapping shapes...
valCut <- dplyr::filter(valleyMatch, ValleyName !=  'Northern Unregulated')

mval <- aggregate(dplyr::slice(soilMstars, time, 1:5), by = valCut, FUN = mean, na.rm = TRUE)
cutsf <- st_as_sf(mval)
mval
plot(mval)
cutsf
mplot <- ggplot() + 
  geom_sf(data = cutsf, aes(fill = `2019-12-31 12:00:00`)) + # First timestep if multiple
  # geom_sf(data = cutsf, aes(fill = sm_pct)) + # If only one timestep
  geom_sf(data = valCut, aes(color = ValleyName), fill = NA) +
  scale_fill_gradient(low = 'tan', high = 'forestgreen')+
  geom_sf_label(data = valCut, aes(label = ValleyName))
mplot

# huh. that works. even wtih 1:5. SO... what? huh?
# On the ones that DO work, does geom_stars work?
mplotstars <- ggplot() + 
  geom_stars(data = mval, aes(fill = sm_pct)) + # First timestep if multiple
  coord_equal() +
  facet_wrap(~time)
mplotstars

# Doesn't seem to. Can I do a stack n plot?
cutstack <- tidyr::pivot_longer(cutsf, cols = -Shape, names_to = 'Date', values_to = 'sm_pct')

mplotT <- ggplot() + 
  geom_sf(data = cutstack, aes(fill = sm_pct, geometry = Shape)) +  # Annoying how this keeps moving away from stars object. Wish that could just be handled directly
  facet_wrap(~Date) +
  geom_sf(data = valCut, aes(color = ValleyName), fill = NA) +
  scale_fill_gradient(low = 'tan', high = 'forestgreen') # +
  # geom_sf_label(data = valCut, aes(label = ValleyName)) # Too cluttered if facetted
mplotT


# NOW... can I do the aggregate on slices that aren't sequential? Or is that a separate issue?
mval2 <- aggregate(dplyr::slice(soilMstars, time, c(1,100,200)), by = valCut, FUN = mean, na.rm = TRUE)
# Some other options for the selecting
# Doesn't actually filter
  # aggregate(filter(soilMstars, time < as.POSIXct("2020-10-06", tz = 'GMT')), by = valCut, FUN = mean, na.rm = TRUE)
cutsf2 <- st_as_sf(mval2)
mval2
plot(mval2)
cutsf2

# Should I just go for the whole thing?
mvalAll <- aggregate(soilMstars, by = valCut, FUN = mean, na.rm = TRUE)
# Some other options for the selecting
# Doesn't actually filter
# aggregate(filter(soilMstars, time < as.POSIXct("2020-10-06", tz = 'GMT')), by = valCut, FUN = mean, na.rm = TRUE)
cutsfAll <- st_as_sf(mvalAll)
mvalAll
plot(mvalAll)
cutsfAll

# Doesn't seem to. Can I do a stack n plot?
cutstackA <- tidyr::pivot_longer(cutsfAll, cols = -Shape, names_to = 'Date', values_to = 'sm_pct') %>%
  dplyr::mutate(Date = ymd_hms(Date))


mplotA <- ggplot() + 
  geom_sf(data = dplyr::slice(cutstackA, 1:5),
          aes(fill = sm_pct, geometry = Shape)) +  # Annoying how this keeps moving away from stars object. Wish that could just be handled directly
  facet_wrap(~Date) +
  geom_sf(data = valCut, aes(color = ValleyName), fill = NA) +
  scale_fill_gradient(low = 'tan', high = 'forestgreen') # +
# geom_sf_label(data = valCut, aes(label = ValleyName)) # Too cluttered if facetted
mplotA


# From above, this works with the set of 1:5. can I modify to work in THIS case?
cutstack <- tidyr::pivot_longer(cutsf, cols = -Shape, names_to = 'Date', values_to = 'sm_pct')
# Works:
csW <- cutstack %>% dplyr::filter(Date %in% c("2019-12-31 12:00:00", "2020-01-04 12:00:00"))

# Does NOT work
# cs <- cutstack %>% 
#   dplyr::mutate(Date = ymd_hms(Date)) %>%
#   dplyr::filter(Date > ymd('20200101'))

# ALSO does not work
cs <- cutstack %>% 
  dplyr::mutate(Date = ymd_hms(Date)) %>%
  dplyr::filter(Date %in% c(ymd_hms("2019-12-31 12:00:00"), ymd_hms("2020-01-04 12:00:00"))) %>%
  dplyr::mutate(Date2 = as.character(Date))

# WORKS with Date2, NOT with Date. so somehow this is a ggplot not wanting to facet on columns of posixct
mplotT <- ggplot() + 
  geom_sf(data = cs, aes(fill = sm_pct, geometry = Shape)) +  # Annoying how this keeps moving away from stars object. Wish that could just be handled directly
  facet_wrap(~Date2) +
  geom_sf(data = valCut, aes(color = ValleyName), fill = NA) +
  scale_fill_gradient(low = 'tan', high = 'forestgreen') # +
# geom_sf_label(data = valCut, aes(label = ValleyName)) # Too cluttered if facetted
mplotT
