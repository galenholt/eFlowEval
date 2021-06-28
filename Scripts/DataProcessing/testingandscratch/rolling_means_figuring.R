# Rolling means over time for both raster and polygons
  # Using raster and using stars?

# Modified from the data raster and vector time series example at
# https://r-spatial.github.io/stars/ to illustrate

library(stars)
library(tidyverse)

# Get the rasters: these look similar to what my data has
prec_file = system.file("nc/test_stageiv_xyt.nc", package = "stars")
(prec = read_ncdf(prec_file, curvilinear = c("lon", "lat"), ignore_bounds = TRUE))

# Get north carolina
sf::read_sf(system.file("gpkg/nc.gpkg", package = "sf"), "nc.gpkg") %>%
  st_transform(st_crs(prec)) -> nc # transform from NAD27 to WGS84
nc_outline = st_union(st_geometry(nc))

plot(prec[,,,1])
plot(nc_outline, add = TRUE)
# Do the aggregate to make polygons with temporal layers
a = aggregate(prec, by = nc, FUN = max)

# Rolling means over time 
# These don't work because the functions are from `raster`, but this is the sort
# of thing I want to do.
# rastRoll <- calc(prec, function(x) movingFun(x, 3, mean))
# vectRoll <- calc(a, function(x) movingFun(x, 3, mean))

# The wiki at stars suggests st_apply is a replacement for calc
# rr <- st_apply(prec, MARGIN = 3, FUN = zoo::rollmax, k = 3)
  # Replaces the dimension, not the attributes
rr <- st_apply(prec, MARGIN = 3, FUN = raster::movingFun, n = 3)
rr

rra <- st_apply(a, MARGIN = 2, FUN = zoo::rollmean, k = 3)
rra

# That just doesn't seem to work. 

# PLots
plot_hook = function() plot(nc_outline, border = 'red', add = TRUE)
prec %>%
  slice(index = 1:12, along = "time") %>%
  plot(downsample = c(5, 5, 1), hook = plot_hook)
# 
plot(a, max.plot = 23, border = 'grey', lwd = .5)

# Why doesn't this work?
plot(prec[,,,1:2], hook = plot_hook)
plot(nc_outline, add = TRUE) # This is gobbledygook, because plot() is stupid

# Rolling on polygons -----------------------------------------------------


# maybe there's a way to use purrr::map? If I understood it?

plot(a[,,1:4])
a[,,1]
View(a[,,1])
plot(a[,80,1])

sfa <- st_as_sf(a)
sfa
sfa1 <- st_as_sf(a[,,1])
sfa1


# see if I can do basic math
a2 <- a
a2[,,2] <- a[,,1]+a[,,2] # Nope
?alist
pull(a[,,1])
purrr::map(a, max, na.rm = TRUE)
purrr::map(a, zoo::rollmax, k = 3)

# What about a[[1]]? That gives a geom x time matrix?
str(a[[1]])

# hmmm. That's tempting...

# Does it work? And what dimensions?
# so, cols are timesteps, but i think the rolling functions want to roll down columns. Let's see
b <- a[[1]]
str(b)
rcppB <- RcppRoll::roll_max(b, n = 3)
# rcppB
str(rcppB)

# zoo directly
zooB <- zoo::rollmax(b, k = 3)
str(zooB)

# Another zoo, more flexible but likely slower, according to help
zooR <- zoo::rollapply(b, width = 3, FUN = max)
str(zooR)

# how about tossing in a transpose? Dunno how costly that is, but likely less so than some sort of purrr thing
system.time(rcppC <- t(RcppRoll::roll_max(t(b), n = 3, align = 'right', fill = c(NA, NA, NA), na.rm = FALSE)))
str(rcppC)
rcppC

rcppC[1:10, 1:10]
b[1:10, 1:10]

system.time(zooC <- t(zoo::rollmax(t(b), k = 3, fill = c(NA, NA, NA), align = 'right'))) # fill = c('extend', 'extend', 'extend')
str(zooC)

b[1:10, 1:10]
rcppC[1:10, 1:10]
zooC[1:10, 1:10]          

# NOW, the question is whether I can put it back together
a
atest <- a
atest[[1]][1:10, 1:10]
# put in new data
atest[[1]] <- rcppC
atest[[1]][1:10, 1:10]
atest

plot(a)
plot(atest)

# YEAH!!

# so, now I need to
  # speed test zoo vs rcpproll (possibly vs others)
    # but will need a bigger dataset to pick up real issues
  # Sort out the RASTER running means, because those are on pixels
    # use `raster`, in which case I'll need to sort out where the dates went
    # use `stars` and indexing, in which case I'll need to sort out the extra dimension
  # Port the above over to the basin


# Rolling on Rasters ------------------------------------------------------


str(prec[[1]])
str(t(prec[[1]]))

# does rcpp or zoo allow rolling on arrays? If so, might be able to permute it back and forth?
  # or just use raster, rather than bother with it
pTest <- RcppRoll::roll_max(prec[[1]], n = 3, align = 'right', fill = c(NA, NA, NA), na.rm = FALSE)
str(pTest)
pTest <- zoo::rollmax(prec[[1]], k = 3, align = 'right', fill = c(NA, NA, NA), na.rm = FALSE)

# Nope
# But
str(aperm(prec[[1]]), perm = c(3,2,1))
pTest <- RcppRoll::roll_max(aperm(prec[[1]], perm = c(3,2,1)), n = 3, align = 'right', fill = c(NA, NA, NA), na.rm = FALSE)
str(pTest)


# Still not working, would need to go sheet-wise. which is possible and not too difficult, but Let's
# just see if I can get `raster` to work and then swap to `stars`


# I can get calc to work on a raster object
# https://gis.stackexchange.com/questions/139253/how-to-compute-climatology-of-3-days-moving-averages-for-rasters-pixel-by-pixel
# testrast <- as(prec, "Raster") # Hmmm, this worked for me earlier. damn
library(raster)
testrast <- raster::brick(prec_file, crs = "+proj=longlat +datum=WGS84") # weird errors. and no idea why I need to put in the default crs
testroll <- calc(testrast, function(x) movingFun(x, 3, mean))
# Seems to work, but no time data, even reading in as a raster. Damn
# Suppose I could then move it to a stars, and add time. But what a pain, and would require reading in twice.

# What if i make the raster from stars?
testrastS <- as(prec, "Raster")
prec
# fails because prec is curvilinear

# get rid of the lat-long business
prec_file = system.file("nc/test_stageiv_xyt.nc", package = "stars")
(precF = read_ncdf(prec_file))
  # STILL curvilinear; it picked it up by default
testrastS <- as(precF, "Raster")
# So, looks like might be able to do an st_warp, but let's focus on just reading rasters in as rasters, even though they're annoying


# does it work for the soil nc?
soilMR <- brick(file.path(datDir, 'soilmoisture/sm_pct_2020_Actual_day.nc'))
# Nope
soilMR
# This is also going to be a pain because I won't be able to crop in sf syntax
plot(soilMR)

# What happens if I push that to stars
soilS <- st_as_stars(soilMR) # getting GDAL errors around UTF-8, but ignore for now
soilS
plot(soilS[,,,1:6])
# hmmm. dates are back. So can I just ignore?


# Check the st_as_stars method on the testing data
ncS <- st_as_stars(testrast, crs = st_crs(prec))
ncS
plot(ncS[,,,1:4])
# Cool. Now, two questions:

# Can I do the crop on the raster?
# What about getting it in and cropping with stars, then moving to raster and back?
  # Then I run into the curvilinear issue
  # Which is a real issue. ie, the below is very different from the above. So if
  # I DO have to start with raster, I'll have to do some warping anyway?
plot(prec[,,,1:4])

ncst <- st_transform(ncS, st_crs(prec))

# i have no idea why the NC data isn't allowing a CRS, but the soil moisture
# does. So, maybe I can use that? But this is super annoying, because it will make testing very difficult
soilMR

# Well, I can force it here. Not sure why I can't in the brick call
crs(testrast) <- crs(soilMR)

# Or, starting over, basically
testrast <- raster::brick(prec_file, crs = "+proj=longlat +datum=WGS84") # weird errors. and no idea why I need to put in the default crs
testrast
plot(testrast$X146396)
plot(nc_outline, add = TRUE)
# uhhh, mixing packages seems to not work so hot.

# Let's try to just go for it. but not holding my breath
ncCrop <- crop(testrast, nc_outline)

# Akjasd;kj
ncoutc <- st_transform(nc_outline, crs(testrast))
ncSP <- as_Spatial(ncoutc)
ncSP
# Plotting is a pain in the ass
plot(testrast$X146396)
plot(ncoutc, add = TRUE)
# Clearly something is wrong.and I want to crop to the SHAPE, not the whole extent rectangle. 
  # raster is an unclear pain in the ass.
ncCrop <- crop(testrast, ncSP)
extent(testrast)
extent(ncSP)

ncMask <- mask(testrast, ncSP)
plot(ncMask)
# Something's stuffed up with the locations. This is driving me bonkers. Raster is a pain, and would STILL enforce a rectangle, I think?
ncMask2 <- mask(nc_outline, ncSP)
# Fails

# Two last things before trying to homebrew:
  # stars-raster-stars with the soil data
  # read the shapefile in as sp instead of sf. At that point I've totally changed approaches. 


# stars-raster-stars for the basin ------------------------------------------------------

# and the basin boundary, might be useful, especially for clipping rasters
basin <- read_sf(dsn = file.path(datDir, 'ANAE/MDB_ANAE_Aug2017/MDB_ANAE.gdb'), layer = 'MDB_Boundary') %>%
  st_cast("MULTIPOLYGON")  %>% # cleans up an issue with multisurfaces
  dplyr::select(LEVEL2NAME) # no need for other info
soilMstars <- read_ncdf(file.path(datDir, 'soilmoisture/sm_pct_2020_Actual_day.nc'))
# Can I clip the whole stars obj?
basinMatchA <- st_transform(basin, st_crs(soilMstars)) # datum transformation. same as above, but in case that gets commented
clipSoil <- soilMstars[basinMatchA]

# now, on to the rollings
clipSoil
# Turn it into a raster
soilR <- as(clipSoil, "Raster")
soilR
# This is where the roll would happen
# Back to stars
soilR_S <- st_as_stars(soilR)
soilR_S
# Dims are f'ed
# But can I just
st_dimensions(clipSoil)
st_dimensions(soilR_S)
soilRSD <- soilR_S
st_dimensions(soilRSD) <- st_dimensions(clipSoil)
soilRSD
# apparently so. 
plot(clipSoil[,,,1:4])
plot(soilR_S[,,,1:4])
plot(soilRSD[,,,1:4])

# So, let's just do that until we run into the curvilinear grid issue, and then sort
# that out later with warping (st_warp) or homebrew, because raster isn't user-friendly

# So, I want to go back up to soilR, and roll it
  # pretty quick, really
soilRroll <- calc(soilR, function(x) movingFun(x, 3, max, type = 'to'))
soilroll <- st_as_stars(soilRroll)
soilroll
# Fix dims
soR <- soilroll # temp to make sure this works
st_dimensions(soR) <- st_dimensions(clipSoil)
soR
plot(soR[,,,1:4])


# Can I homebrew the rolling? Using raster is getting absurd --------------

# From above
str(aperm(prec[[1]]), perm = c(3,2,1))

permPrec <- aperm(prec[[1]], perm = c(3,2,1))

length(permPrec)
dim(permPrec)

# # Returns a giant vector
# testS <- sapply(permPrec, FUN = RcppRoll::roll_max, n = 3, align = 'right', fill = c(NA, NA, NA), na.rm = FALSE, simplify = 'array')
# 
# pTest <- RcppRoll::roll_max(aperm(prec[[1]], perm = c(3,2,1)), n = 3, align = 'right', fill = c(NA, NA, NA), na.rm = FALSE)
# str(pTest)


# write as a loop; should be trivial to parallelize
# initialize a new matrix

pp <- permPrec
for (i in 1:dim(permPrec)[3]) {
  pp[,,i] <- RcppRoll::roll_max(permPrec[,,i], n = 3, align = 'right', fill = c(NA, NA, NA), na.rm = FALSE)
}
# huh. that's super fast. 
precMax <- prec
precMax[[1]] <- aperm(pp, perm = c(3,2,1))
plot(prec[,,,1:4])
plot(precMax[,,,1:4])

# Can I just do that on clipsoil and call it a day?
clipSoil
# way bigger than prec, but let's time it and see

# The permutation order here of 2 and 1 isn't super important, the key is that
# rows are timesteps, so the function rolls the rows within columns and sheets,
# and then puts things back together. 3,2,1 is just easy to remember and swap
soilPerm <- aperm(clipSoil[[1]], perm = c(3,2,1))

# make a new array (don't do this in reality, it'll eat memory)
spt <- soilPerm
# this loops over sheets, not time (time is rows). Sheets is unimportant, the
# rolling functions are only vectorized in 2-d so need to feed them 2-d data
for (i in 1:dim(soilPerm)[3]) {
  spt[,,i] <- RcppRoll::roll_max(soilPerm[,,i], n = 3, align = 'right', fill = c(NA, NA, NA), na.rm = FALSE)
}
# That took basically no time.
# don't screw up clipoil
clipP <- clipSoil
clipP[[1]] <- aperm(spt, perm = c(3,2,1))
plot(clipP[,,,1:6])
plot(clipSoil[,,,1:6])


# convert to raster vs homebrew loop --------------------------------------

# Might come down to memory, but let's compare speed of the whole thing

rastver <- function(starsobj) {
  soilR <- as(starsobj, "Raster")
  
  # So, I want to go back up to soilR, and roll it
  # pretty quick, really
  soilRroll <- calc(soilR, function(x) movingFun(x, 3, max, type = 'to'))
  soilroll <- st_as_stars(soilRroll)
  # Fix dims
  # soR <- soilroll # temp to make sure this works
  st_dimensions(soilroll) <- st_dimensions(starsobj)
  return(soilroll)
}

homever <- function(starsobj) {
  soilPerm <- aperm(starsobj[[1]], perm = c(3,2,1))
  # this loops over sheets, not time (time is rows). Sheets is unimportant, the
  # rolling functions are only vectorized in 2-d so need to feed them 2-d data
  for (i in 1:dim(soilPerm)[3]) {
    soilPerm[,,i] <- RcppRoll::roll_max(soilPerm[,,i], n = 3, align = 'right', fill = c(NA, NA, NA), na.rm = FALSE)
  }
  # # That took basically no time.
  # # don't screw up 
  # clipP <- starsobj
  starsobj[[1]] <- aperm(soilPerm, perm = c(3,2,1))
  return(starsobj)
}

system.time(rastSoil <- rastver(clipSoil))
system.time(homeSoil <- homever(clipSoil))
# huh. the homebrew is 20x faster. Dunno if that'll last if this gets truly
# huge, but sure would be nice to be able to stay in one ecosystem and use the
# same method for rasters and polygons
plot(rastSoil[,,,1:6])
plot(homeSoil[,,,1:6])
# AND, I can write a handy function that does both, with a ccheck to see if it should aperm or t()
# AND, that will work with curvilinear grids
# So, that's good. but super annoying. Back to the main thing

# OK, just wrote timeRoll. need to check it

# Don't screw up clipsoil
funSoil <- clipSoil
funSoil[[1]] <- timeRoll(funSoil[[1]], FUN = RcppRoll::roll_max, rolln = 3, align = 'right')
plot(funSoil[,,,1:6])
# Wow. that works?!?!?!

a
funPoly <- a
funPoly[[1]] <- timeRoll(funPoly[[1]], FUN = RcppRoll::roll_max, rolln = 3, align = 'right')
plot(funPoly[,,1:6])
# Holy cow! It Works!