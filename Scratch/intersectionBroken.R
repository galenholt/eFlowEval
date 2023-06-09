# Does downgrading to 0.9-6 work?

library(sf)
library(stars)
library(dplyr)
# From the stars example https://r-spatial.github.io/stars/articles/stars1.html
prec_file = system.file("nc/test_stageiv_xyt.nc", package = "stars")
prec = read_ncdf(prec_file)
nc = sf::read_sf(system.file("gpkg/nc.gpkg", package = "sf"), "nc.gpkg")
prec_slice = slice(prec, index = 17, along = "time")

# datum transformation and crop
nc = st_transform(nc, st_crs(prec_slice)) 
prec_nc <- prec_slice[nc]

# Make nc have fewer cols for easier examination
nc <- nc[c('NAME', 'BIR74')]

# make prec an sf for intersecting
precSF <- st_as_sf(prec_nc, as_points = FALSE, merge = FALSE, na.rm = FALSE)

# Zoom in closely so we can see the resulting polygons
bb <- st_bbox(c(xmin = -78.2, ymin = 35.75, xmax = -78.3, ymax = 35.85), crs = st_crs(nc))

# Where is that bbox?
plot(nc['NAME'], reset = FALSE)
plot(st_as_sfc(bb), add = TRUE)

# Crop to the bbox
cutnc <- st_crop(nc, st_as_sfc(bb))
cutprec <- st_crop(precSF, st_as_sfc(bb))

# Plot the resulting cropped data
plot(cutprec, reset = FALSE)
plot(cutnc['NAME'], reset = FALSE, add = TRUE, col = NA, border = 'red')

# Intersect those polygons
ncinter <- st_intersection(cutnc, cutprec)

# The results are shuffled
plot(ncinter)
# focusing on examples from both input datasets
plot(ncinter['NAME']) 
plot(ncinter[3])

# This has split the polygons as expected, but the attributes within each are
# not what I expected.
# I would have expected the attributes associated with each set of original
# polygons to stay constant across the same area in the output polygons.
# Instead, they have been shuffled.
# Using county name as an example:
plot(cutnc['NAME'])
plot(ncinter['NAME'])

# Using a projected coordinate system works!
cutncT <- st_transform(cutnc, crs = 32119)
cutprecT <- st_transform(cutprec, crs = 32119)
ncinterT <- st_intersection(cutncT, cutprecT)
plot(cutncT['NAME'])
plot(ncinterT['NAME'])
plot(ncinterT)

# AS DOES ROLLING BACK TO 
# renv::install('sf@0.9-5') # For testing
# renv::install('stars@0.4-3')

# And, checking (though horribly skewed) for the Australian Albers EPSG:3577
cutncT <- st_transform(cutnc, crs = 3577)
cutprecT <- st_transform(cutprec, crs = 3577)
ncinterT <- st_intersection(cutncT, cutprecT)
plot(cutncT['NAME'])
plot(ncinterT['NAME'])
plot(ncinterT)