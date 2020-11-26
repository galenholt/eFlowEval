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



# TEMP AGGREGATION SORTING OUT --------------------------------------------



# TESTING -----------------------------------------------------------------

# but why is this producing 8 aggregations instead of 7? ie 8 dates should be 7 intervals aggregated over, right?
# try a test with 'years'. this should be 2014,15,16,17,18,19, so 6 plots
fullYr_LippiaY <- aggregate(fullCycleANAE_Lippia, by = 'years', FUN = propor, na.rm = TRUE)

fullCatch_LippiaY <- catchAggW(strict = fullYr_LippiaY, strictWeights = lachArea, FUN = sum, summaryPoly = lachOnly)

fullPlot_LippiaY <- catchAggPlot(fullCatch_LippiaY, title = 'Full Cycle Lippia')
fullPlot_LippiaY

fullCatch_LippiaY
yeartimes <- st_get_dimension_values(fullCatch_LippiaY, 'time')
yeartimes
# so, that looks like the start

# What if I feed those into the aggregate? Do I get the same thing?
fullYr_LippiaY2 <- aggregate(fullCycleANAE_Lippia, by = yeartimes, FUN = propor, na.rm = TRUE)

fullCatch_LippiaY2 <- catchAggW(strict = fullYr_LippiaY2, strictWeights = lachArea, FUN = sum, summaryPoly = lachOnly)

fullPlot_LippiaY2 <- catchAggPlot(fullCatch_LippiaY2, title = 'Full Cycle Lippia')
fullPlot_LippiaY2

fullCatch_LippiaY2
st_get_dimension_values(fullCatch_LippiaY, 'time')

all(fullCatch_LippiaY[[1]] == fullCatch_LippiaY[[1]])
# Ok, so that works. What does it mean? It means it aggregates from the last date given to the end.

# Can I double-check that that works by using a different summary function?
# yeartimes Does NOT work; it tosses the final year
fullYr_LippiaY3 <- aggregate(fullCycleANAE_Lippia, by = yeartimes, FUN = length)

fullCatch_LippiaY3 <- catchAggW(strict = fullYr_LippiaY3, strictWeights = 1, FUN = mean, summaryPoly = lachOnly)

fullPlot_LippiaY3 <- catchAggPlot(fullCatch_LippiaY3, title = 'Full Cycle Lippia')
fullPlot_LippiaY3

# 'years' DOES work; it retains the info in the final year.
fullYr_LippiaY3 <- aggregate(fullCycleANAE_Lippia, by = 'years', FUN = length)

fullCatch_LippiaY3 <- catchAggW(strict = fullYr_LippiaY3, strictWeights = 1, FUN = mean, summaryPoly = lachOnly)

fullPlot_LippiaY3 <- catchAggPlot(fullCatch_LippiaY3, title = 'Full Cycle Lippia')
fullPlot_LippiaY3

# What if we include the final date to make the interval work?
ytend <- c(yeartimes, enddate)

fullYr_LippiaY3 <- aggregate(fullCycleANAE_Lippia, by = ytend, FUN = length)

fullCatch_LippiaY3 <- catchAggW(strict = fullYr_LippiaY3, strictWeights = 1, FUN = mean, summaryPoly = lachOnly)

fullPlot_LippiaY3 <- catchAggPlot(fullCatch_LippiaY3, title = 'Full Cycle Lippia')
fullPlot_LippiaY3
# So, that's right, except it has a 'bonus' empty sheet
fullYr_LippiaY3
# Could throw it away, but???

# Is rightmost.closed or left.open the answer?
# No combination of them changes anything, using either ytend OR yeartimes

fullYr_LippiaY4 <- aggregate(fullCycleANAE_Lippia, by = ytend, FUN = length, rightmost.closed = TRUE, left.open = TRUE)

fullCatch_LippiaY4 <- catchAggW(strict = fullYr_LippiaY4, strictWeights = 1, FUN = mean, summaryPoly = lachOnly)

fullPlot_LippiaY4 <- catchAggPlot(fullCatch_LippiaY4, title = 'Full Cycle Lippia')
fullPlot_LippiaY4

fullYr_LippiaY4
# Could throw it away, but???

# I guess work through the examples
tif = system.file("tif/L7_ETMs.tif", package = "stars")
t1 = as.Date("2018-07-31")
x = read_stars(c(tif, tif, tif, tif), along = list(time = c(t1, t1+1, t1+2, t1+3)))[,1:30,1:30]
st_get_dimension_values(x, "time")
x_agg_time = aggregate(x, by = t1 + c(0, 2, 4), FUN = max) 
plot(x_agg_time)
# ??? that seems to leave the weird end

# aggregate time dimension in format Date - interval
by_t = "2 days"
x_agg_time2 = aggregate(x, by = by_t, FUN = max) 
st_get_dimension_values(x_agg_time2, "time")
x_agg_time - x_agg_time2
# So, that's not even working in the demo

# aggregate time dimension in format POSIXct
x = st_set_dimensions(x, 4, values = as.POSIXct(c("2018-07-31", 
                                                  "2018-08-01", 
                                                  "2018-08-02", 
                                                  "2018-08-03")), 
                      names = "time")
by_t = as.POSIXct(c("2018-07-31", "2018-08-02"))
x_agg_posix = aggregate(x, by = by_t, FUN = max)
st_get_dimension_values(x_agg_posix, "time")
x_agg_time - x_agg_posix
x_agg_time
x_agg_posix
x_agg_time2

plot(x_agg_posix) # No data in second
# but, how many are in those?
x_agg_posixS = aggregate(x, by = by_t, FUN = length)
plot(x_agg_posixS)
x_agg_posixS
x_agg_posixS[,1,,]
x_agg_posixS[,2,,]
# So, yeah, 2 is just NA.

# Try rightmost.closed etc
by_t = as.POSIXct(c("2018-07-31", "2018-08-02"))
x_agg_posixS = aggregate(x, by = by_t, FUN = length, rightmost.closed = TRUE)
plot(x_agg_posixS)
x_agg_posixS[,1,,]
x_agg_posixS[,2,,]
# rightmost.closed puts the final date in the preceding group, but still saves a NA group

by_t = as.POSIXct(c("2018-07-31", "2018-08-02"))
x_agg_posixS = aggregate(x, by = by_t, FUN = length, left.open = TRUE)
plot(x_agg_posixS)
x_agg_posixS[,1,,]
x_agg_posixS[,2,,]
# Unclear.

# Try including the final time 
by_t = as.POSIXct(c("2018-07-31", "2018-08-02", "2018-08-03"))
x_agg_posixS = aggregate(x, by = by_t, FUN = length)
plot(x_agg_posixS)
x_agg_posixS[,1,,]
x_agg_posixS[,2,,]
# only one point because right-open

# Try including the final time AND rightmost.closed
by_t = as.POSIXct(c("2018-07-31", "2018-08-02", "2018-08-03"))
x_agg_posixS = aggregate(x, by = by_t, FUN = length, rightmost.closed = TRUE)
plot(x_agg_posixS)
x_agg_posixS[,1,,]
x_agg_posixS[,2,,]
# Ok, so that has the right number in each sheet, but has a 'bonus' sheet, just like before
x_agg_posixS

# is the easiest thing to do to throw away that sheet? probably, but that feels stupid

# Nothing useful in the git issues, though there is an old solution that's even hackier; I think maybe before aggregate worked at all for time?

# before I go down that road, are intervals the answer?
# I think intervals are what the characters feed it

x_agg_time2 = aggregate(x, by = "2 days", FUN = length) 
st_get_dimension_values(x_agg_time2, "time")
x_agg_time2[,1,,]
x_agg_time2[,2,,]
# OK, so that DOES seem to be chunking correctly
library(lubridate)
?interval
# feeding it interal(by_t) fails
testint1 <- interval(by_t[1], by_t[2])
testint2 <- interval(by_t[2], by_t[3])
testint <- c(testint1, testint2)
testint
str(testint)

x_agg_timeI = aggregate(x, by = testint, FUN = length) 
st_get_dimension_values(x_agg_time2, "time")
x_agg_time2[,1,,]
x_agg_time2[,2,,]
# Fails, can't use lubridate intervals
as.POSIXct(testint)


# Maybe I can sort out what cut.Posix is doing
alldates <- as.POSIXct(c("2018-07-31", 
                         "2018-08-01", 
                         "2018-08-02", 
                         "2018-08-03"))
# what we WANT
cut(alldates, breaks = '2 days')                       

# Categorizes the last day into an NA
cut(alldates, breaks = by_t)

# Puts the FIRST day as an NA
cut(alldates, breaks = by_t, right = TRUE)

# Add to the last day, so it goes a bit further
by_tplus <- by_t
by_tplus[3] <- by_tplus[3] + 200000 # just add a bunch of seconds

cut(alldates, breaks = by_t)
by_t

# include.lowest?
# From help: Note that the default for right differs from the default method.
# Using include.lowest = TRUE will include both ends of the range of dates.
cut(alldates, breaks = by_t, include.lowest = TRUE)

cutC <- cut(alldates, breaks = '2 days')
cutIL <- cut(alldates, breaks = by_t, include.lowest = TRUE)
cutC == cutIL

# Now, can I feed that to aggregate???
# No, there's no way to pass arguments to cut
by_t = as.POSIXct(c("2018-07-31", "2018-08-02", "2018-08-03"))
x_agg_posixS = aggregate(x, by = by_t, FUN = length, include.lowest = TRUE)
plot(x_agg_posixS)
x_agg_posixS[,1,,]
x_agg_posixS[,2,,]

# Can we get find.interval to work?
# What is cut doing to get it to work?
findInterval(alldates, vec = by_t, rightmost.closed = TRUE)
# That sure implies that it SHOULD work; it's not adding any categories off the end
# I think I'll do rightmost closed and then throw out the last NA category, I
# guess. Feels dumb. But I dont have time to dig into the aggregate.stars
# function to figure out why it won't return the right thing

# IE
by_t = as.POSIXct(c("2018-07-31", "2018-08-02", "2018-08-03"))
x_agg_posixS = aggregate(x, by = by_t, FUN = length, rightmost.closed = TRUE)
plot(x_agg_posixS)
x_agg_posixS[,1,,]
x_agg_posixS[,2,,]
# Ok, so that has the right number in each sheet, but has a 'bonus' sheet, just like before
x_agg_posixS

# Hacky, but going with it for now.
# Will need to fix ALL the time aggregators. RRGH
# AND, make it fix ONLY when using a user-supplied by_t
# suppose a just wrape aggregate in a thing that takes its same arguments, runs aggregate, then if (is.character(by_t)) return, if is.posix, toss and then return
tfix <- slice(x_agg_posixS, time, -length(by_t))
plot(tfix)
tfix[,1,,]
tfix[,2,,]

# Check to write the function. What if those are DATES, not posix?
# need to set the data to have dates too
x2 = st_set_dimensions(x, 4, values = as.Date(c("2018-07-31", 
                                                "2018-08-01", 
                                                "2018-08-02", 
                                                "2018-08-03")), 
                       names = "time")


by_t = as.Date(c("2018-07-31", "2018-08-02", "2018-08-03"))
x_agg_posixS2 = aggregate(x2, by = by_t, FUN = length, rightmost.closed = TRUE)
plot(x_agg_posixS2)
x_agg_posixS2[,1,,]
x_agg_posixS2[,2,,]
x_agg_posixS2
# Still the bonus


# Worth a check
slice(x_agg_posixS, time, length(by_t))[[1]]
x_agg_posixS[,3,,][[1]] # I *think* this works, but less clear 
all(is.na(slice(x_agg_posixS, time, length(by_t))[[1]]))


