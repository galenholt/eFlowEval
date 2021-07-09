# scratch work to update rastPolyJoin to accept a FUN argument.



# Some copy-paste to get to a single short rastPolyJoin run ---------------


bb = st_bbox(c(xmin = 147.4, ymin = -33.7, xmax = 147.6, ymax = -33.6), 
             crs = st_crs(tifTimes))
# plot(st_as_sfc(bb), col = NA, border = 'red', add = TRUE)

# All I need is the crs from boxInun, so don't worry about the fact that it's
# made elsewhere for now
# why is this taking so long?
boxwet <- LachlanANAE %>%
  st_transform(st_crs(boxInun)) %>%
  st_crop(st_as_sfc(bb))

# INUNTIFS HAS BEEN CUT TO AVOID CORRUPTED FILE AT THiS POITN

# I'm doing this in the middle of inundationScratch, so likely won't work without running that partway down
tifdates <- inunTifs %>% # Set of filenames
  str_extract_all("[1-2][0-9][0-9][0-9]_[0-9][0-9]_WaterDepth.tif") %>%
  # now delete the safety
  str_remove("_WaterDepth.tif") %>%
  # add the first of the month on there
  str_c('_01') %>%
  # turn into dates
  lubridate::ymd() %>%
  as.POSIXct() # soilMstars uses posix instead of the Date class that lubridate returns, so be consistent


# Read in a few stars proxies
# Is there any harm here in reading them all in as PROXIES?
tifTimes <- inunTifs[1:ntimes] %>% # filenames
  read_stars() %>% # read in
  merge() %>% # make a brick 
  setNames('depth') %>% # name the attribute
  # Make dates
  # soilMstars uses an offset and delta, while this has 'values'.
  # I seem to remember values might cause an issue somewhere down the track, so
  # might need to revisit
  st_set_dimensions(3, values = tifdates[1:ntimes]) %>% 
  st_set_dimensions(names = c("x", "y", "time"))

# For looping at first, use a subset. Come back to test time looping

# a subset for testing
cutwet <- boxwet[20, ]

thiscrop <- st_crop(tifTimes, cutwet[1,], as_points = FALSE)

thisdepth <- rastPolyJoin(polysf = cutwet[1,], rastst = thiscrop, 
                          grouper = 'UID', maintainPolys = TRUE,
                          na.replace = 0, whichcrs = 3577)

depthAns <- thisdepth[[1]]
depthIndex <- thisdepth[[2]]
depthAns
depthIndex


# Testing -----------------------------------------------------------------

# Not happy with this, but it does work if we insist the second argument to FUN
# is area
# BUT, this will give us the mean depth of inundation across the polygon
thisdepthWM <- rastPolyJoin(polysf = cutwet[1,], rastst = thiscrop, 
                          grouper = 'UID', FUN = weighted.mean,
                          maintainPolys = TRUE,
                          na.replace = 0, whichcrs = 3577)
thisdepthWM[[1]]

# Confirm it works with a default FUN
thisdepthD <- rastPolyJoin(polysf = cutwet[1,], rastst = thiscrop, 
                            grouper = 'UID',
                            maintainPolys = TRUE,
                            na.replace = 0, whichcrs = 3577)
thisdepthD[[1]]
# So, the above is the area-weighted mean depth

# What else do we want to make?
  # Remember that the 'area' variable I make is BEFORE the summarise, so it is the area of each raster cell

# volume
  # area*depth for each raster, so sum(depth, area)
volSummary <- function(z, area) {
  sum(z*area)
}


# Volume of water <= 10cm (or arbitrary photic limit)
  # depth up to 10cm * area, then summed
  # So, get the minimum of depth or photic limit, multiply by area, and
  # summarise with sum to get all depths
  # Have to sort of write a min() out of ifelse or it does min() over the column, not the rows
volLimit <- function(x, area, limit) {
  sum(ifelse(x > limit, limit*area, x * area))
}


# Area of inundation
areaInun <- function(x, area) {
  sum(ifelse(x > 0, area, 0))
}

# mean depth of the inundated area- this potentially much different than mean
# depth across the polygon
  # give 0 weights to those that aren't inundated
avgInunDepth <- function(x, area) {
  areaifinun <- ifelse(x > 0, area, 0)
  weighted.mean(x, areaifinun)
}

# Fraction inundated
fracInun <- function(x, area) {
  areaInun(x, area) / sum(area)
}

# Test functions on something easier to deal with  ------------------------
# Should be able to test everything with just this
simpledf <- tibble(depth = c(0.1, 0.2, 3, 1, 0, 4, 0.5, 0, 0, 1),
                   area = c(5, 5, 5, 2, 5, 5, 1, 5, 4, 5))
simpledf

# the volume
simpledf %>% summarise(vs = volSummary(depth, area))
# Expected
sum(simpledf$depth*simpledf$area) # check

# volume up to limit
simpledf %>% summarise(vs = volLimit(depth, area, 1))
# what do I expect?
limdepth <- simpledf$depth
limdepth[simpledf$depth > 1] <- 1
sum(limdepth*simpledf$area)

# Area inundated
simpledf %>% summarise(a = areaInun(depth, area))
# what do I expect?
sum(simpledf$area[simpledf$depth > 0])

# average inun depth
simpledf %>% summarise(a = avgInunDepth(depth, area))
# what do I expect?
weighted.mean(simpledf$depth[simpledf$depth > 0], simpledf$area[simpledf$depth > 0])

# Fraction inundated
# average inun depth
simpledf %>% summarise(fi = fracInun(depth, area))
# what do I expect?
# area inundated/total area
sum(simpledf$area[simpledf$depth > 0]) / sum(simpledf$area)


# Test with rastPolyJoin --------------------------------------------------

# Total Volume
thisdepthVol <- rastPolyJoin(polysf = cutwet[1,], rastst = thiscrop, 
                             grouper = 'UID', FUN = volSummary,
                             maintainPolys = TRUE,
                             na.replace = 0, whichcrs = 3577)
thisdepthVol[[1]]

# Volume in photic
thisdepthVol10 <- rastPolyJoin(polysf = cutwet[1,], rastst = thiscrop, 
                               grouper = 'UID', FUN = volLimit, limit = 0.1,
                               maintainPolys = TRUE,
                               na.replace = 0, whichcrs = 3577)
thisdepthVol10[[1]]

# area inundated
thisdepthAreaInun <- rastPolyJoin(polysf = cutwet[1,], rastst = thiscrop, 
                             grouper = 'UID', FUN = areaInun,
                             maintainPolys = TRUE,
                             na.replace = 0, whichcrs = 3577)
thisdepthAreaInun[[1]]

# Average inundation depth
thisdepthavgInunDepth <- rastPolyJoin(polysf = cutwet[1,], rastst = thiscrop, 
                             grouper = 'UID', FUN = avgInunDepth,
                             maintainPolys = TRUE,
                             na.replace = 0, whichcrs = 3577)
thisdepthavgInunDepth[[1]]

# Fraction inundate
thisdepthFracInun <- rastPolyJoin(polysf = cutwet[1,], rastst = thiscrop, 
                                  grouper = 'UID', FUN = fracInun,
                                  maintainPolys = TRUE,
                                  na.replace = 0, whichcrs = 3577)
thisdepthFracInun[[1]]

# OK, it is now time to choose which of those to do.
  # I think 
# areaInun, volSummary, then either avgInunDepth or volLimit

# And then incorporate in the testing script for the HPC