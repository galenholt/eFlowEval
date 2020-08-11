# Trying to sort out the other problems I'm having in a reproducible way
  # Slicing (and filtering)
  # Vector facetting

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


# PLots
plot_hook = function() plot(nc_outline, border = 'red', add = TRUE)
prec %>%
  slice(index = 1:12, along = "time") %>%
  plot(downsample = c(5, 5, 1), hook = plot_hook)
# 
plot(a, max.plot = 23, border = 'grey', lwd = .5)

# # so far so good (and have changed nothing)

# Now, some ggplot facetting errors -------------------------------------------------

# Plot the above with geom_stars in ggplot
# Gives Error: `scale_id` must not be `NA`
ggplot() + 
  geom_stars(data = a,
             aes(fill = Total_precipitation_surface_1_Hour_Accumulation)) +
  facet_wrap(~time)

# But making time a character works
ggplot() + 
  geom_stars(data = a,
             aes(fill = Total_precipitation_surface_1_Hour_Accumulation)) +
  facet_wrap(~as.character(time))

# Make an sf, and plot that in ggplot
asf <- st_as_sf(a)

# Works with base plot()
plot(asf)

# To facet with ggplot, need to stack all those columns
stackA <- tidyr::pivot_longer(asf, 
                              cols = -geom, names_to = 'Time', values_to = 'precip') %>%
  dplyr::mutate(Time2 = lubridate::ymd_hms(Time))
# Gives a warning about number of items not multiple of replacement, which seems odd.

# This works: Time is a character
ggplot() + 
  geom_sf(data = stackA,
             aes(fill = precip, geometry = geom)) +
  facet_wrap(~Time)

# This fails, Time2 is POSIXct
ggplot() + 
  geom_sf(data = stackA,
          aes(fill = precip, geometry = geom)) +
  facet_wrap(~Time2)

# Quick look at posix -----------------------------------------------------

# Can I facet that without geom_sf? 
ggplot(stackA) + 
  geom_histogram(aes(x = precip)) + facet_wrap(~Time)

suppressWarnings(library(tidyverse))
ggplot(stackA) + 
  geom_histogram(aes(x = precip)) + facet_wrap(~Time2)

# Super simple answer to Edzer's question
iris %>% mutate(date = rep_along(Petal.Length, c('2020-09-08', '2020-08-09')), 
                date2 = as.POSIXct(date, origin = '1970-01-01')) %>%
  ggplot(aes(x = Petal.Length, y = Sepal.Length)) + geom_point() + facet_wrap(~date2)
