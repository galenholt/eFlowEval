# Script to build on the ANAE without having to reprocess it
library(here)
library(tidyverse)
library(sf)
library(stars)

# Argh. sort all this directory crap out later
# Trying to at least separate scripts and functions, looking towards library
source(here('Functions', 'rastPolyJoin.R'))
source(here('Functions', 'timeRoll.R'))


myhome <- str_remove(path.expand("~"), "/Documents")
datDir <- file.path(myhome, "Deakin University/QAEL - MER/Model/dataBase") # "C:/Users/Galen/Deakin University/QAEL - MER/Model/dataBase"

datOut <- "datOut"

# load the processed anae files, cut to lachlan
load(file.path(datOut, 'lachAll.rdata'))
# load(file.path(datOut, 'bothANAE.rdata'))

# To allow plotting the ltim zones (otherwise their polygons get lost)
# And get the LTIM_Valleys to use to subset for toy models at scale, but not enormous scale
LTIM_Valleys <- read_sf(dsn = file.path(datDir, 'ANAE/MDB_ANAE.gdb'), layer = 'LTIM_Valleys') %>%
  st_cast("MULTIPOLYGON") # cleans up an issue with multisurfaces

# LTIM areas, useful for plotting
ltimCut <- LTIM_Valleys %>%
  select(ValleyName, ValleyID, ValleyCode) # Three different ways to reference, basically


# Read in the soil data
# soilMstarsOrig <- read_ncdf(file.path(datDir, 'soilmoisture/sm_pct_2010_Actual_day.nc'))

# Arh. the temp doesn't read in the same way as the soil moist. back to the drawing board
# Try for all
tempfile <- list.files(file.path(datDir, 'testTemp'), pattern = '.nc')
temppath <- file.path(datDir, 'testTemp', tempfile)

# # Read_stars brings it in as a stars_proxy
# soilTstarsNC <-  read_ncdf(temppath)
# soilTstars <-  read_stars(temppath)
# # read_stars is failing, unclear why. somethign about $ on atomic vectors. But not getting that on the test file?
#   # Why am I only getting Day_View_Time attribute? Why are there 93 days when I only have one month? WHAT IS THIS DATASET???
# # read_ncdf is failing because it needs to allocate a 95gb vector. So... Going to need to do some exploring, I guess
#   # is there a way to bring in only a subset so we can see what the hell is in it?
# soilTstarsNC
# soilTstars
# 
# soilNCtest <- read_ncdf(temppath, var = names(soilTstarsNC)[2])
# soilNCtest
# # The projection is weird. Going to re-send request with WGS84 (ie 4326) specified. I
# # think that's what it was in, but "native projection" is lame and
# # uninformative. I just assumed it would TELL me what that was
# 
# # So, the var = bit worked for the ncdf. Does it work for read_stars?
#   # If not, I'll just loop earlier I guess. Esp. if I can sub the big ncdf on
#   # import somehow. Don't REALLY want to submit separate requests just to get
#   # separate files, but I suppose I could
# soilStest <- read_stars(temppath, var = names(soilTstarsNC)[2])
#   # Ah. the time dimension is 3x too long. So it's shoving the times along with
#   # the second and third attributes into it. Which also stuffs up the datatype
# # Changes nothing
# soilStest <- read_stars(temppath, options = names(soilTstarsNC)[2])
# names(soilStest)
# 
# # Can I separate somehow?
# st_dimensions(soilStest)$time$values
# # Yikes. That's nonsense
# 
# 
# st_as_stars(soilStest) # Still 93 time. HOW DO I TELL IT WHAT THE ATTRIBUTES ARE????
# 
# # Really looking like maybe the only way to do this is to chunk up the requests
# # and read_ncdf. Would be REALLY nice if I could send a subset request with the
# # read_ncdf though
# 
# # A few more tries, just cycling through the poorly documented arguments...
# soilStest <- read_stars(temppath, sub = names(soilTstarsNC)[2])
# soilStest
# 
# # HA! That might have done it. Looks like sub *might* be able to subset along dimensions as well. 
# 
# # Has it brought over the crs?
# st_crs(soilStest)
# st_crs(soilNCtest)
# # nope, got lost. BUT, we TOLD it to use 4326 this time, so
# 
# # Let's see how much of this works
# st_crs(soilStest) <- 4326
# st_crs(soilNCtest) <- 4326
# 
# plot(soilStest[,,,1:4])
# plot(soilNCtest[,,,1:4])
# 
# # LOTS of NAs. gonna have to keep an eye on that with the big dataset


# Read in data (fixed, I think, moving on) -------------------------------------------
soilTstars <-  read_stars(temppath, sub = "LST_Day_1km", proxy = TRUE) # Force a proxy for testing the subsetting
st_crs(soilTstars) <- 4326

# Getting negative vals after cropping. Do i if I leave it as ncdf?
soilTnc <- read_ncdf(temppath, var = "LST_Day_1km")
st_crs(soilTnc) <- 4326

# Previous work -----------------------------------------------------------

# and the basin boundary, for clipping rasters; though likely will start with lachlan
basin <- read_sf(dsn = file.path(datDir, 'ANAE/MDB_ANAE.gdb'), layer = 'MDB_Boundary') %>%
  st_cast("MULTIPOLYGON")  %>% # cleans up an issue with multisurfaces
  dplyr::select(LEVEL2NAME) # no need for other info

# Set the crs.
# Doing this here because it's not yet clear which is best, so we want
# everything in before we choose one to get the donor CRS from
# using a regular (not curvilinear) grid is necessary to avoid retaining the
# full extent and size of the raster
# soilMstars comes in as regular, so use that. Though st_warp() likely would
# do it on a different projection (see help and vignettes)
whichcrs <- st_crs(soilTstars)
# TODO:: Check this is right on PC, I didn't need to set the CRS there for some reason
st_crs(lachAll) <- 4283 # This is what's there when I look at st_crs, but it's not registering for some reason. 
lachAll <- st_transform(lachAll, whichcrs)
ltimCut <- st_transform(ltimCut, whichcrs)
basin <- st_transform(basin, whichcrs)

# Crop. Have to use as_points = FALSE or it crops to the raster centers, and misses stuff around the edges.
  # I *think*, but am not sure, that proxies might have to cut on a bounding
  # box, and this is barfing for some reason. possibly because the raster is
  # smaller than the box? That would seem dumb, but let's check
bb <- st_bbox(c(xmin = 144.4, ymin = -33.8, xmax = 144.6, ymax = -33.6), crs = whichcrs)
bbs <- st_as_sfc(bb)

lachcutter <- filter(ltimCut, ValleyName == 'Lachlan')
lachTemp <- st_crop(soilTstars, filter(ltimCut, ValleyName == 'Lachlan'), as_points = FALSE)
lachTemp2 <- st_crop(soilTstars, lachcutter, as_points = FALSE)
lachTemp3 <- st_crop(soilTstars, bbs, as_points = FALSE)
# So, that worked, but we had negatives when we used the full data. so unclear what's actually going on. Thinking I might have to do a brute force memory thrash
# OR, can i read_stars it to read a closer subset, and then st_as_stars()? and then do the crop? ie some sort of double-crop (incl. time?)
  # Though at that point, I THINK I can use the ncsub argument to read_ncdf:
# Example
f <- system.file("nc/reduced.nc", package = "stars")
read_ncdf(f)
read_ncdf(f, var = c("anom"))
read_ncdf(f, var = "anom", ncsub = cbind(start = c(1, 1, 1, 1), count = c(10, 12, 1, 1)))

# How would I set that up?
soilTnc
# I'm not sure HOW to find the from/to I want for lat/long? Might have to hunt?
# They start at 1 for the raster, which means the values I want will be
# determined by the specific raster I'm reading in. 
# BUT, maybe I can hunt by reading in one timeslice, cropping, and then putting
# a bit of a buffer on it
  # Then there would have to be a big loopy function. blehc

# Start working through how that would work
#  If the count value is NA then all steps are included. EXCEPt THATS NOT TRUE. IT THROWS AN ERROR
# So, how to get the WHOLE THING?
  # The proxy will give it, and doesn't require any RAM. basically, use it as a dimension query
soilTstars
# Grab a single day (I'm avoiding 1 just in case there's anything funny)

# I should be able to use st_dimensions to get the numbers programatically
dimtestS <- st_dimensions(soilTstars)

allspace1time <- cbind(start = c(1,1,10), count = c(dimtestS$x$to, dimtestS$y$to, 1))
soilTnc1 <- read_ncdf(temppath, var = "LST_Day_1km", ncsub = allspace1time)
st_crs(soilTnc1) <- 4326
soilTnc1
plot(soilTnc1)

# Now, crop that single slice to the lachlan
lTnc <- st_crop(soilTnc1, filter(ltimCut, ValleyName == 'Lachlan'), as_points = FALSE)
plot(lTnc)

# And get its fromto
lTnc
soilTnc1

# this is a bit funny because this test doesn't fully enclose the lach, but whatever.

# the limits on the lTnc now give the numbers, but will have to back out start and count
lTnc
# Get ALL the times this time
dimtest <- st_dimensions(lTnc)

# lat and long are the clipped
loncount <- dimtest$lon$to - dimtest$lon$from
latcount <- dimtest$lat$to - dimtest$lat$from

# Time is all times
  # Though really, we'll likely be looping over this...
tcount <- dimtestS$time$to

lachsub <- cbind(start = c(dimtest$lon$from, dimtest$lat$from, 1), count = c(loncount, latcount, tcount))

lachBox <- read_ncdf(temppath, var = "LST_Day_1km", ncsub = lachsub)
st_crs(lachBox) <- 4326

# Something gets stuffed with the offset (see below). try to fix
#   # This makes it even worse. 
# st_dimensions(lachBox)$lon$offset <- dimtest$lon$offset
# st_dimensions(lachBox)$lat$offset <- dimtest$lat$offset
lachBox
plot(lachBox[,,,1:6])

# Compare with the uncropped
plot(soilTnc1)
plot(lachBox[,,,10])

lachCrop <- st_crop(lachBox, filter(ltimCut, ValleyName == 'Lachlan'), as_points = FALSE)

# No data?? Yeah, it's there. Dunno what all the NA are griping about
plot(lachCrop[,,,1:6])
# plot(lTnc[,,,1:6])

# # check it worked
# The direct crop
lachCropFull <- st_crop(soilTnc, filter(ltimCut, ValleyName == 'Lachlan'), as_points = FALSE)
plot(lachCropFull[,,,1:6])

# Well, that's bad. It seems to have shifted the raster, rather than cropped it or something
# Something's off wiht the offset.
st_dimensions(lachBox)
dimtest
# Fine to shift the start to 1, but 223*0.00833 != 145.33-141.833

# CAN I shift the lachBox to match what it came from?
lb2 <- lachBox
# st_dimensions(lb2)[1:2] <- dimtest[1:2, ]
lbt <- st_set_dimensions(lb2, "lon", value = dimtest["lon"])
lbt
lTnc
st_dimensions(lb2)$lon$offset <- dimtest$lon$offset
lb2

# Ok, pull that up above and check
  # Fails. Jesus. 

# How bad is this to just bring the sheets in a few at a time, crop and process?
# (And if we're processing to ANAE and discarding, does cropping help?)


# TRY WHOLE BASIN? --------------------------------------------------------

tempfile <- list.files(file.path(datDir, 'testTempBasin'), pattern = '.nc')
temppath <- file.path(datDir, 'testTempBasin', tempfile)

soilTstars <-  read_stars(temppath, sub = "LST_Day_1km", proxy = TRUE) # Force a proxy for testing the subsetting
st_crs(soilTstars) <- 4326

dimtestS <- st_dimensions(soilTstars)

# Try just bringing 1 time in for now to see how huge
croptime <- cbind(start = c(1,1,1), count = c(dimtestS$x$to, dimtestS$y$to, 10))
soilTnc <- read_ncdf(temppath, var = "LST_Day_1km", ncsub = croptime)
st_crs(soilTnc) <- 4326
soilTnc
format(object.size(soilTnc), units = "auto") # Check size
# Cycled through a bunch, 8 is the best as a test (though 5,6, and 9 are good too)
plot(soilTnc[,,,8], reset = FALSE)
plot(st_geometry(lachcutter), border = 'red', add = TRUE)
# well, specifying the datum at least lets me read it in...
  # Those are pretty crap extents though. Might grab a different set for testing so I can see what I'm doing

# Now, crop that to the lachlan
system.time(lachTnc <- st_crop(soilTnc, filter(ltimCut, ValleyName == 'Lachlan'), as_points = FALSE))

plot(lachTnc[,,,5:8])
# OK, and that lines up as it does with the outlines above.
# So, that'll work, I think.

# Quickly, how long does it take to do the anae smash on that?

# Why did this work for 10, but not for 2?
system.time(chunk1 <- rastPolyJoin(polysf = lachAll, 
                                   rastst = lachTnc[,,,1:10], 
                                   grouper = 'SYS2', 
                                   maintainPolys = TRUE))
# 2 sheets

# 10 sheets
# user  system elapsed 
# 1159.69   48.48 1220.19 
# Yikes

# is there any reason to do the st_crop to the lachlan?
  # IE if what we want is the ANAE averaging, do we need to do the crop?
  # although, does the crop take much time?
    # 5 seconds. So, in the margins compared to the rastPolyJoin
# I suppose I could check whether it's necessary, but that would take 20 mins....at least. 


####################################
# NOTE
# Basically, we're going to have to read in slices and process them. I was
# trying to only read in a subset around the lachlan, but that was shifting
# things around in a weird way. Not sure how muhc it would save anyway.
  # Also, it's possible it WON'T shift things around if the whole lachlan is THERE in the raster
# I now have it working to read the whole basin in and crop to the lachlan, so
# that's good. I suppose the/a question is whether we should (or can) read
# everything in and crop in a loop, and then use THAT to do the anae averaging,
# or just do the anae averaging as we go? I think probably the latter, since I
# doubt we'll be able to hld the whole lachlan for 5 years.

# do we want to (and can we) write the lachlan out as an nc (or a bunch of ncs?)
# for later use? WOuld be nice to not have to do all the processing every time






# Cut out lots of plot tests here, because they dont' work without reading in the proxy. And we don't want to do that.

# 1. Spatial average of raster into the ANAE polys at each timestep ---------------------------

# Test the new function
#   # Doesn't work wiht the stars_proxy lachSoil
# system.time(dailySMpolyavg <- rastPolyJoin(polysf = lachAll, rastst = lachSoil, grouper = 'SYS2', maintainPolys = TRUE))

# Runs with st_as_stars(stars_proxy)
# Honestly kind of doubt this will fit in memory for the soil temp. BUt hard to check without the full data. Guess it's time to do that.
# yaaaa! 610gb. Going to need to sort SOMETHING out here.
# AND, lachTemp now has negative 'to' values for x and y. that was happening with the bb prevously. Ugh. 
# So, if I am going to have to subset the proxy in chunks to st_as_stars,
# should i just read it in as read_ncdf in a loop? It'd be a lot more memory
# thrashing; ie doing the lachlan cut as a proxy I think probably helps, if
# possible (ie no negative vals)
# but, how to test? the full data is SO big... and the test data is small, and so doesn't proxy. Though I think I can force it?

# STILL getting negatives on the to vals.... what's going on? It works with
# read_ncdf. and the ONLY thing that's happening is the st_crop. Which worked
# for soil moisture as proxy, and is working for temp as ncdf. So I have no idea


deProxySoil <- st_as_stars(lachTemp) # Not sure we want to do this, rather than put it in the function call so we don't keep it in memory? Will see, I suppose
names(deProxySoil) <- 'sm_pct' # Attributes got named 'attr'. Change it to what it is with read_ncdf for consistency
format(object.size(deProxySoil), units = "auto") # Check size
# plot check
plot(deProxySoil[,,,1:6])

## TESTING
# # So, this runs out of RAM, and I'm unsure why. It should be <3Gb. It makes one
# # copyf the raster, but that still shouldn't come anywhere near filling up ram.
# # Also, SUPER slow running for even 5 years. (WAY more than 5x one year). So probably loop anyway.
# 
# # Two options: loop over YEARS, or sheets 
#   # Or, maybe othertime units, I think stars might be able to do that?
# # Original
# system.time(dailySMpolyavg <- rastPolyJoin(polysf = lachAll, rastst = deProxySoil, grouper = 'SYS2', maintainPolys = TRUE))
# 
# # Loopy
# 
# # Testing first: If we break up a big stars, how do we put it back together?
# d1 <- rastPolyJoin(polysf = lachAll, rastst = deProxySoil[,,,1:10], grouper = 'SYS2', maintainPolys = TRUE)
# d2 <- rastPolyJoin(polysf = lachAll, rastst = deProxySoil[,,,11:20], grouper = 'SYS2', maintainPolys = TRUE)
# d3 <- rastPolyJoin(polysf = lachAll, rastst = deProxySoil[,,,21:30], grouper = 'SYS2', maintainPolys = TRUE)
# 
# # The output of raspolyjoin is a list with the stars and the index
# d1[[1]]
# d1[[2]]
# d2[[2]] == d1[[2]]
# 
# teststarc <- c(d1[[1]], d2[[1]])
# teststarc
# 
# # Can I c over a list? doesn't really look like it
# starlist <- list(d1[[1]], d2[[1]], d3[[1]])
# teststarc <- c(starlist)
# teststarc
# 
# # How bout some timings. Is there an optimal number of days? Or, more coarsely, how does this cale?
# system.time(d10 <- rastPolyJoin(polysf = lachAll, rastst = deProxySoil[,,,1:10], grouper = 'SYS2', maintainPolys = TRUE))
# # user  system elapsed 
# # 137.32    0.47  146.47 
# 146/10
# system.time(d100 <- rastPolyJoin(polysf = lachAll, rastst = deProxySoil[,,,1:100], grouper = 'SYS2', maintainPolys = TRUE))
# # user  system elapsed 
# # 190.22    0.35  191.45
# 191/100
# # Anothe rround    
# # user  system elapsed 
# # 228.98    0.44  233.91 
# 233/100
# system.time(d500 <- rastPolyJoin(polysf = lachAll, rastst = deProxySoil[,,,1:500], grouper = 'SYS2', maintainPolys = TRUE))
# # user  system elapsed 
# # 575.69    3.09  616.82 
# 616/500
# system.time(d200 <- rastPolyJoin(polysf = lachAll, rastst = deProxySoil[,,,1:200], grouper = 'SYS2', maintainPolys = TRUE))
# # user  system elapsed 
# # 321.61    0.65  342.98
# 342/200
# system.time(d150 <- rastPolyJoin(polysf = lachAll, rastst = deProxySoil[,,,1:150], grouper = 'SYS2', maintainPolys = TRUE))
# # user  system elapsed 
# # 278.66    0.59  288.09 
# 288/150
# 
# # So, the 500 FELT long, but was actually way faster on a seconds/layer measure. Let's keep ratcheting up
# system.time(d750 <- rastPolyJoin(polysf = lachAll, rastst = deProxySoil[,,,1:750], grouper = 'SYS2', maintainPolys = TRUE))
# # user  system elapsed 
# # 1010.96    5.86 1070.57 
# 750/1070
# 
# # Tried 1000, and barfed,
# # system.time(d1000 <- rastPolyJoin(polysf = lachAll, rastst = deProxySoil[,,,1:1000], grouper = 'SYS2', maintainPolys = TRUE))

# TODO: CHUNK IT UP INTO 750-day chunks, run, and put back together
# I could do a fancy optimization thing here to get the larges possible number
# evenly split or something, (ie 750 was just chosen by hand, and often leaves a
# short tail). But not worth it now. And how much of this is even necessary on
# the HPC? Probably getting to be time to start just writing HPC code

# get days
chunksize <- 750
days <- dim(deProxySoil[[1]])[3]
days/chunksize
# Round up number of chunks
nchunks <- ceiling(days/chunksize)

# do the first chunk outside, so there's something to c() to
from <- 1
# protect short ends
firstend <- ifelse(chunksize < days, chunksize, days)
# Get the first chunk
system.time(chunk1 <- rastPolyJoin(polysf = lachAll, 
                                   rastst = deProxySoil[,,,1:chunksize], 
                                   grouper = 'SYS2', 
                                   maintainPolys = TRUE))

# Then start at 2
# is this worth parallelizing? The issue is memory, not processing (I think).
# But maybe for HPC? Though then would we just not do this at all?

# Wrapping in a function to be able to time gets pretty close to parallelizing though.
timechunker <- function(chunk1) {
  for (t in 2:nchunks) {
    from <- (t-1)*(750) + 1
    to <- ifelse(t * 750 < days, t * 750, days)
    dtemp <- rastPolyJoin(polysf = lachAll, 
                          rastst = deProxySoil[,,,from:to], 
                          grouper = 'SYS2', 
                          maintainPolys = TRUE)
    
    # Do I want to error check? It takes a really long time
    # (I think. Will check)
    # With checking the loop takes
    # user  system elapsed 
    # 2032.86  150.85 2265.33 
    # Without
    # user  system elapsed 
    # 2467.63   51.00 2730.60 
    # Might as well leave it in.
    if (!all(chunk1[[2]] == dtemp[[2]])) {
      stop('indexing is off between timechunks')
    }
    
    # c them together.
    chunk1[[1]] <- c(chunk1[[1]], dtemp[[1]])
  }
  return(chunk1)
}

# Run it
system.time(dailySMpolyavg <- timechunker(chunk1 = chunk1))
# 2300 seconds for 2014-19 in Lachlan

# 290 seconds with 280 timesteps in Lachlan
# There has GOT to be a way to speed this up...
# It's the grouped summarize that kills it, NOT the intersection. i think it
# might be the work sf is doing to put the polygons back together?
# TODO: see if it's faster to st_drop_geometry, do the summarize, and then join back to lachAll with SYS2?

# unpack the list
dailyPolyindex <- dailySMpolyavg[[2]]
# at least for testing, don't overwrite, or have to run the whole thing again
dailyPolySMavg <- dailySMpolyavg[[1]]

# Get rid of the list to save space
rm(dailySMpolyavg)
# Why not keep the list and not do the above? I dunno. There was a reason.

# Check ordering
all(dailyPolyindex$SYS2 == lachAll$SYS2)

# Test plot
# Let's set up a bbox for subsampled plotting without taking 8 million years
bb <- st_bbox(c(xmin = 144.4, ymin = -33.8, xmax = 144.6, ymax = -33.6), crs = whichcrs)
dailyPolySub <- dailyPolySMavg[st_as_sfc(bb)]
plot(dailyPolySub[,,10:13])

# ggplot is a bigger pain, skip for now, we just want to look at things
# testDaily <- ggplot() + 
#   geom_stars(data = dailyPolySub[,,10:13], aes(fill = sm_pct)) +
#   facet_grid(cols = as.character)
# testDaily

# Rolling through time over the spatial polygons ---------------------

# From Cherie via Ash:
# Soil moisture (at a minimum % content??) – moist, >10%, – needs to be
# maintained for a certain period of time (??) – for 6 to 8 weeks – to enable
# plants to produce fruits and set-seed
# So, let's get the 42-day min

# And inundation is important for germ
# sadly the number 1 stricture is seed germination during inundation.  not sure
# where it is worth trying a soil moisture analogue at this point.

# data note
# The Actual soil moisture grids estimate the actual percentage of available water 
# content for each time-step (rather than actual total soil water volume or depth)
# But is it IN percent? Or 0-1?
# probably should have rm(deProcySoil) by now, but glad I didn't
range(deProxySoil[[1]], na.rm = T)
hist(deProxySoil[[1]])

# Can we pseudo-inundate? Just making up 80% here as a high threshold, since the max is 0.92
# There are ppl doing some fancy modelling to predict inundation from soil
# moist, but they have complex eq'ns, and so better to just wait for hydrology
# Don't know how long needs to stay inundated for germination.
# Let's say 5 days? 
sum(dailyPolySMavg[[1]] > 0.8)

# Set up new stars objects to put the results in
# These will actually both be rolling mins, just over different timespans.
# To meet the "dead if not moist > 10% for 6 weeks" condition, we need to check if min(last 6 weeks) is below 10
# To meet the "needs inundation (>80%) for 5 days" condition, we need to check if min(last 5 days) is below 80
soilMoistMin42 <- dailyPolySMavg 
soilMoistMin5 <- dailyPolySMavg

# Get a 42-day rolling min for each polygon
system.time(soilMoistMin42[[1]] <- timeRoll(soilMoistMin42[[1]], 
                                            FUN = RcppRoll::roll_min, 
                                            rolln = 42, 
                                            align = 'right',
                                            na.rm = TRUE))

# 10 seconds

# Get a 5-day rolling min for each polygon
system.time(soilMoistMin5[[1]] <- timeRoll(soilMoistMin5[[1]], 
                                           FUN = RcppRoll::roll_min, 
                                           rolln = 5, 
                                           align = 'right',
                                           na.rm = TRUE))

# 5 seconds

# Test plot
min5polysub <- soilMoistMin5[st_as_sfc(bb)]
plot(min5polysub[,,10:13])

# Test plot
min42polysub <- soilMoistMin42[st_as_sfc(bb)]
plot(min42polysub[,,100:103])

## COMMENT OUT ALL THE OTHER WAYS WE DEVELOPED IN THE TEST SCRIPT
# # Rolling through time on the raster, then putting into polygons --------
# 
# # Could probably streamline with the dailies in the first section, but for now
# rastTimeSmean_7 <- lachSoil
# system.time(rastTimeSmean_7[[1]] <- timeRoll(rastTimeSmean_7[[1]],
#                                              FUN = RcppRoll::roll_mean, 
#                                              rolln = 7, 
#                                              align = 'right',
#                                              na.rm = TRUE))
# # 0.7 seconds
# # Test plot the raster itself
# rastweekSub <- rastTimeSmean_7[st_as_sfc(bb)]
# plot(rastweekSub[,,,10:13])
# 
# 
# # Now, put those time-means into polygons
# system.time(weeklySMpoly <- rastPolyJoin(polysf = lachAll, rastst = rastTimeSmean_7, grouper = 'SYS2', maintainPolys = TRUE))
# 
# 
# # unpack the list
# weeklyPolyindex <- weeklySMpoly[[2]]
# # at least for testing, don't overwrite, or have to run the whole thing again
# weeklyRastavgSMpoly <- weeklySMpoly[[1]]
# 
# # Check ordering
# all(weeklyPolyindex$SYS2 == lachAll$SYS2)
# 
# # Test plot
# weekmeanSub <- weeklyRastavgSMpoly[st_as_sfc(bb)]
# plot(weekmeanSub[,,10:13])
# 
# 
# 
# # Split the polygons by the rasters to get smaller scale ------------------
# 
# # Let's do this with the dailies, because then it would be easy to, for example, get a rolling min or something.
# 
# # Test the new function
# system.time(dailySMpolysplit <- rastPolyJoin(polysf = lachAll, 
#                                              rastst = lachSoil, 
#                                              grouper = 'SYS2', 
#                                              maintainPolys = FALSE))
# # 290 seconds with 280 timesteps in Lachlan
# # There has GOT to be a way to speed this up...
# # It's the grouped summarize that kills it, NOT the intersection. i think it
# # might be the work sf is doing to put the polygons back together?
# # TODO: see if it's faster to st_drop_geometry, do the summarize, and then join back to lachAll with SYS2?
# 
# # unpack the list
# dailyPolySplitindex <- dailySMpolysplit[[2]]
# # at least for testing, don't overwrite, or have to run the whole thing again
# dailyPolySMsplit <- dailySMpolysplit[[1]]
# 
# # Check ordering
#   # This SHOULD be false
# all(dailyPolySplitindex$SYS2 == lachAll$SYS2)
# 
# # Test plot
# dailyPolySplitSub <- dailyPolySMsplit[st_as_sfc(bb)]
# plot(dailyPolySplitSub[,,10:13])
# 
# 
# # all the test plots in one place -----------------------------------------
#   #including making the subsets and bbox, so we can use something different here, or comment out above
# # load(file.path(datOut, 'kanDemo.rdata'))
# 
# # it's a little annoying that st_crop is a hard crop, but the [] cropping lets
# # the polys hang over, and so we get different extents. So doing the polygon
# # crops a bit different than above, so we DO get the same extents
# 
# # Let's set up a bbox for subsampled plotting without taking 8 million years
# bb = st_bbox(c(xmin = 144.4, ymin = -33.8, xmax = 144.6, ymax = -33.6), crs = whichcrs)
# 
# # ANAE
# anaeSub <- st_crop(lachAll, st_as_sfc(bb))
# plot(anaeSub[,'ANAE_DESC'])
# 
# # Soil moisture raster
# soilSub <- lachSoil[st_as_sfc(bb)]
# plot(soilSub[,,,10:13])
# 
# # Daily sm in the polygons
# dailyPolySub <- st_crop(st_as_sf(dailyPolySMavg[,,10:13]), st_as_sfc(bb))
# plot(dailyPolySub)
# 
# # 10-day max in the polygons directly
# max10PolySub <- st_crop(st_as_sf(polyTimeSmax_10[,,10:13]), st_as_sfc(bb))
# plot(max10PolySub)
# 
# # Weekly mean in the raster (as a raster still)
# rastweekSub <- rastTimeSmean_7[st_as_sfc(bb)]
# plot(rastweekSub[,,,10:13])
# 
# # Weekly mean put into polygons
# weekmeanSub <- st_crop(st_as_sf(weeklyRastavgSMpoly[,,10:13]), st_as_sfc(bb))
# plot(weekmeanSub)
# 
# # and the split by raster units
# dailyPolySplitSub <- st_crop(st_as_sf(dailyPolySMsplit[,,10:13]), st_as_sfc(bb))
# plot(dailyPolySplitSub)

# Save a lot of stuff
save(lachAll, 
     deProxySoil, 
     dailyPolySMavg, 
     soilMoistMin42, 
     soilMoistMin5, 
     file = file.path(datOut, 'lachSoilprocessedAllOut.rdata'))

# Save just the outputs, separately (for memory management on the strictures side)

save(lachAll, 
     soilMoistMin42,
     file = file.path(datOut, 'lachSoilmin42.rdata'))

save(lachAll, 
     soilMoistMin5,
     file = file.path(datOut, 'lachSoilmin5.rdata'))

