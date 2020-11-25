# aggregation scratch

# a few notes from sorting out the area-weighting to get aggregate to work over the catchment
fullSum <- aggregate(fullCycleANAE, by = "years", FUN = sum, na.rm = TRUE)

# Can I just do a spatial aggregate? That's almost too easy
# Weighting might be tricky?
fullSpace <- aggregate(fullSum, by = ltimCut, FUN = sum, na.rm = TRUE)
fullSpace
plot(fullSpace)

# hmm. shouldn't really go into other shapes. Maybe
fullSpace <- aggregate(fullSum, by = filter(ltimCut, ValleyName == "Lachlan"), FUN = sum, na.rm = TRUE)
fullSpace
plot(fullSpace)
# Why is the color ramp goofy?
# Look at it
st_as_sf(fullSpace)
# Well, OK, 2016 has a huge sum, and the others are 0. Which makes it hard to say much. Maybe the numbers are coming from an anae shape that extends outside the lachlan?
# Still, the color ramp SHOULD be there.

# Let's sort this out on something with data
# seedyr is seed60 aggregated by year. Lt's put it in the lachlan
testSpace <- aggregate(seedYr, by = filter(ltimCut, ValleyName == "Lachlan"), FUN = sum, na.rm = TRUE)
testSpace
plot(testSpace)
st_as_sf(testSpace)

ggplot() +
  geom_stars(data = testSpace) +
  coord_sf() +
  facet_wrap(~time) +
  theme_void()  +
  scale_fill_gradient(low = 'firebrick', high = 'forestgreen' )
# scale_fill_viridis() # +
#   scale_x_discrete(expand = c(0, 0)) +
#   scale_y_discrete(expand = c(0, 0))


# OK, that's working, ish. Not very interesting data, but working

# Now, can we weight? Ie the area that had seed survival?

# Area is 
lachArea <- st_area(lachAll)

# Hacky

test <- seedYr[[1]]
test <- test[1:6, 1:10]
artest <- lachArea[1:10]
# artest <- artest*0+10
t(t(test)*artest)

seedArea <- seedYr
seedArea[[1]] <- t(t(seedYr[[1]])*lachArea)
seedArea

# Now aggregate over space
testSpace <- aggregate(seedArea, by = filter(ltimCut, ValleyName == "Lachlan"), FUN = sum, na.rm = TRUE)
testSpace

# Plot
ggplot() +
  geom_stars(data = testSpace) +
  coord_sf() +
  facet_wrap(~time) +
  theme_void()  +
  scale_fill_gradient(low = 'firebrick', high = 'forestgreen' )
