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


# I can get calc to work on a raster object
# https://gis.stackexchange.com/questions/139253/how-to-compute-climatology-of-3-days-moving-averages-for-rasters-pixel-by-pixel
# testrast <- as(prec, "Raster") # Hmmm, this worked for me earlier. damn
library(raster)
testrast <- raster::brick(prec_file)
testroll <- calc(testrast, function(x) movingFun(x, 3, mean))
# Seems to work, but no time data
  # Suppose I could then move it to a stars, and add time. But what a pain

# maybe there's a way to use purrr::map?

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
