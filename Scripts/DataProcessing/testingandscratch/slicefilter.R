# Trying to understand how slice and filtering works
# slice and filter --------------------------------------------------------

library(stars)
library(dplyr)


# Using vignette 3 --------------------------------------------------------

system.file("tif/L7_ETMs.tif", package = "stars") %>%
  read_stars -> x
x
plot(x)

# Slice off a single band
x %>% slice(band, 6) -> x6
x6
# check
plot(x6)

# Slice the first three bands
x %>% slice(band, 1:3) -> x3
x3
plot(x3)

# Slice an non-continuous selection
x %>% slice(band, c(1,3,5)) -> x5
x5
plot(x5)
# interesting. That DIDN't work when I tried time slices. Come back to that. Is it possible it's the same issue as the ggplot issue?

# How about filter
x %>% filter(band > 3) -> f3
f3
plot(f3)

# Discontinuous
x %>% filter(band %in% c(2,4,6)) -> f2
f2
plot(f2)

# Again, that didn't work for me when operating on a time dimension

# Trying to slice and filter on a time dimension -----------------------------------------------
# 
# library(stars)
# library(dplyr)


# Using vignette 2 --------------------------------------------------------

filenames = c("avhrr-only-v2.19810901.nc",
      "avhrr-only-v2.19810902.nc",
      "avhrr-only-v2.19810903.nc",
      "avhrr-only-v2.19810904.nc",
      "avhrr-only-v2.19810905.nc",
      "avhrr-only-v2.19810906.nc",
      "avhrr-only-v2.19810907.nc",
      "avhrr-only-v2.19810908.nc",
      "avhrr-only-v2.19810909.nc")
file_list = system.file(paste0("netcdf/", filenames), package = "starsdata")
y = read_stars(file_list, quiet = TRUE, proxy = TRUE)

# Turn into stars, not proxied to avoid any issues with lazy eval
xt <- st_as_stars(adrop(y["sst"]))
xt
plot(xt)


# Slice off a single time
xt %>% slice(time, 6) -> t6
t6
# check
plot(t6)

# Slice the first three times
xt %>% slice(time, 1:3) -> t3
t3
plot(t3)

# Non-continuous
xt %>% slice(time, c(1,3,6)) -> t6
t6
plot(t6)

# huh. that works. so, where was the problem I was running into?

# How about filter
xt %>% filter(time > lubridate::ymd(19810904)) -> ft3
ft3
plot(ft3)

# # Discontinuous: this is failing, apparently because can't use & or |? just single conditions with commas?
# xt %>% filter(time > lubridate::ymd(19810908) | time < lubridate::ymd(19810906)) -> ft2
# ft2
# plot(ft2)

# So, why does THAT work, but when I try it with my data, it doesn't? 
# What's the difference?
  # The aggregate(). The slicing anyway. Hven't tried the filtering yet...

