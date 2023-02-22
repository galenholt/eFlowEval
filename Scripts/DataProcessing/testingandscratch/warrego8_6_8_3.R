# Testing warrego error anae

# THIS IS JUST PROCESSINUNDATIONSUBCHUNKS with the right set of args[] and some
# debugging/testing down by the foreach loops

# Very similar to inundationCleanSpeed, but focused just on speed testing the plans, not on also checking assorted things work
# The hpc_wrapper tends to source this too, but if I do it here I don't have to
# wrap the script to run locally. Sort out a cleaner way to do this
source('directorySet.R')

# Let's get libraries here, then sort out git then sort out making this a library so we don't have to deal with all the library crap
# library(sp)
# library(rgeos)
library(here)
library(tidyverse)
library(sf)
# library(RNetCDF) # Probably not, raster can handle geographic netCDF
# library(raster) # maybe?
library(stars)
library(foreach)
library(doFuture)



# Set up parallel backend
registerDoFuture()
# plan(multicore) # multicore on HPC

# # For local testing
plan(multisession)
summaryFun <- 'areaInun'
# GET 10 around it
args <- c('blah', 'b', 'c', 'g', '3', 't', 'a', '8', 'Warrego', '8', '6')
# Get the offending anae itself
# args <- c('blah', 'b', 'c', 'g', '3', 't', 'a', '3', 'Warrego', '8', '6', '8')
# The 910 around it
# args <- c('blah', 'b', 'c', 'g', '3', 't', 'a', '8', 'Warrego')
# args <- c('blah', 'b', 'c', 'g', '5', 't', 'a', '9', 'Warrego', '8', '6', '10')
# ## The outerchunks need to start outer and go in, ie '8', '6' is the 6th subchunk of the 8th main chunk
# Need to handle the edge case wehre there aren't enough polys to do the array we're asking for

# Make a sub-directory for the subchunk
scriptOut <- paste0(datOut, '/Inundationprocessed/', summaryFun, '/chunked/', args[9], '/sub', 
                    str_flatten(args[10:length(args)], collapse = '/sub_'))



# Choose a size for the chunks. This is likely better elsewhere, but
nchunks <- 10
arraynum <- as.numeric(args[8])
chunkName <- args[8]

outerchunks <- as.integer(args[10:length(args)])
# chunksize <- 6000

# Which function ----------------------------------------------------------

if (summaryFun == 'areaInun') {
  # Area of inundation
  chosenSummary <- function(x, area) {
    sum(ifelse(x > 0, area, 0))
  }
} else if (summaryFun == 'volInun') {
  # area*depth for each raster, so sum(depth, area)
  chosenSummary <- function(z, area) {
    sum(z*area)
  }
} else if (summaryFun == 'volLimit') {
  # Volume of water <= 10cm (or arbitrary photic limit)
  # depth up to 10cm * area, then summed
  # So, get the minimum of depth or photic limit, multiply by area, and
  # summarise with sum to get all depths
  # Have to sort of write a min() out of ifelse or it does min() over the column, not the rows
  chosenSummary <- function(x, area, limit = 0.1) {
    sum(ifelse(x > limit, limit*area, x * area))
  }
} else if (summaryFun == 'avgInunDepth') {
  # mean depth of the inundated area- this potentially much different than mean
  # depth across the polygon
  # give 0 weights to those that aren't inundated
  chosenSummary <- function(x, area) {
    areaifinun <- ifelse(x > 0, area, 0)
    weighted.mean(x, areaifinun)
  }
} else {
  stop('need to choose a summary function')
}

# Read in inundation data --------------------------------------------------

## Inundation
# data location
inunDir <- file.path(datDir, 'Inundation', 'WaterDepth_TwoMonthly', 'geotiff')

# Get the file names
alltifs <- list.files(inunDir, pattern = '.tif$')
inunTifs <- file.path(inunDir, alltifs)

# get the crs from the first one (will read them all in later, but first need
# to deal with the corrupt file)
# As long as they're proxies, we can't change their crs, so have to shift
# everything else to them
starCRS <- st_crs(read_stars(inunTifs[1]))

# Read in all ANAE ----------------------------------
# The whole-basin version is ANAEbasinclim.rdata
# the particular file we want here is passed as args[9]
thisAName <- paste0(args[9], 'ANAE') 
allANAES <- list.files(file.path(datOut, 'ANAEprocessed'), pattern = paste0('^', thisAName))
## Read in all ANAE polygons
load(file.path(datOut, 'ANAEprocessed', allANAES))
# datOut is location-aware, based on directorySet.R, so this should work
# locally or on HPC

# change its name to something generic so can be looped over
# This is annoying, but I guess not too bad
anaePolys <- get(thisAName)
rm(list = thisAName)

# Somehow some are invalid, despite the st_make_valid in processANAE
anaePolys <- st_make_valid(anaePolys)

# change its name to something generic so can be looped over
# This is annoying, but I guess not too bad

# FIRST, get the correct outer chunk that we want to drill into
# I *think* this wouldn't be TOO terrible to make recursive if needed, by
# looping over args[10]: length(args) and grabbing the desired bit each time
# as long as we weren't also chaning nchunks. Although that wouldn't be too
# bad either, really, just would need more args
# Get the row indices from the array number 

# For loop lets us drill down by feeding additional arguments to the shell script
for (chun in 1:length(outerchunks)) {
  
  # Handle the case where we've drilled down to where there are fewer rows than chunks
  # If nchunks > nrow(), it will break it up fine. Catch the case where that's not true
  if (nchunks >= nrow(anaePolys)) {
    # If we're trying to grab a chunk that is in the available rows, just pass out the row
    # Otherwise, we don't want to pass out anything.
    if (outerchunks[chun] <= nrow(anaePolys)) {
      anaePolys <- anaePolys[outerchunks[chun], ]
    } else {
      stop('indexing beyond end of anaePolys')
    }
  } else { 
    # The usual case, define the edges of the chunk of anaePolys
    outersize <- ceiling(nrow(anaePolys)/nchunks)
    # arraynum <- 3
    prevoutertop <- outersize*(outerchunks[chun]-1)
    outerbottom <- prevoutertop+1
    
    if (outerchunks[chun] == nchunks) {
      outertop <- nrow(anaePolys) # make sure we finish
    } else {
      outertop <- prevoutertop + outersize
    }
    
    # cut to this chunk of polygons
    anaePolys <- anaePolys[outerbottom:outertop, ]
  }
  
  
}


# THEN, break up into the sub-chunk-
# This is exactly the same, but uses arraynum to define the chunk instead of a
# predefined argument. Could easily put in the loop but not going to for clarity

# As above, pass a single anaePoly if we're indexing in too far, skip entirely
# if we're past the end, otherwise define the chunk
if (nchunks >= nrow(anaePolys)) {
  # If we're trying to grab a chunk that is in the available rows, just pass out the row
  # Otherwise, we don't want to pass out anything.
  if (arraynum <= nrow(anaePolys)) {
    anaePolys <- anaePolys[arraynum, ]
  } else {
    stop('indexing beyond end of anaePolys')
  }
} else {
  chunksize <- ceiling(nrow(anaePolys)/nchunks)
  # arraynum <- 3
  prevtop <- chunksize*(arraynum-1)
  bottom <- prevtop+1
  
  if (arraynum == nchunks) {
    top <- nrow(anaePolys) # make sure we finish
  } else {
    top <- prevtop + chunksize
  }
  
  # cut to this chunk of polygons
  anaePolys <- anaePolys[bottom:top, ]
}



print(paste0('number of polygons processing is ', nrow(anaePolys)))

# Transform to stars crs
anaePolys <- st_transform(anaePolys, starCRS)

# handle corrupt tif ------------------------------------------------------
# Not needed (was a corrupt download)
# Keeping but commented out for a bit longer in case I'm wrong
# Need to do this after the ANAE because I have to read it in
# There's a corrupt tif, so cut it out
# Have to try to read in, but can't read in the whole thing. So do a little
# crop and call it good
# passer <- vector(mode = 'logical', length(inunTifsALL))
# for (tif in 1:length(inunTifsALL)) {
#   checkTif <- read_stars(inunTifsALL[tif])
#   cropTif <- st_crop(checkTif, anaePolys[1,], as_points = FALSE)
#   testsf <- NA
#   try(testsf <- st_as_sf(cropTif, as_points = FALSE, merge = FALSE, na.rm = FALSE),
#       silent = TRUE)
#   if (class(testsf) == 'logical') {
#     passer[tif] <- FALSE
#   } else if ('sf' %in% class(testsf)) {
#     passer[tif] <- TRUE
#   }
#   rm(testsf)
# }

# # and now the list of functional tifs is
# inunTifs <- inunTifsALL[passer]



# Get the stars in as a proxy ---------------------------------------------


# Get the dates from the tif names so we can set dimensions
tifdates <- inunTifs %>% # Set of filenames
  str_extract_all("[1-2][0-9][0-9][0-9]_[0-9][0-9]_WaterDepth.tif") %>%
  # now delete the safety
  str_remove("_WaterDepth.tif") %>%
  # add the first of the month on there
  str_c('_01') %>%
  # turn into dates
  lubridate::ymd() %>%
  as.POSIXct() # soilMstars uses posix instead of the Date class that lubridate returns, so be consistent


# Make the proxy
tifTimes <- inunTifs %>% # filenames
  read_stars() %>% # read in
  merge() %>% # make a brick 
  setNames('depth') %>% # name the attribute
  # Make dates
  # soilMstars uses an offset and delta, while this has 'values'.
  # I seem to remember values might cause an issue somewhere down the track, so
  # might need to revisit
  st_set_dimensions(3, values = tifdates) %>% 
  st_set_dimensions(names = c("x", "y", "time"))


# Since we want to combine the two list bits differently, just return the list
# and let foreach make a list of lists for now



# TESTING -----------------------------------------------------------------


plot(anaePolys["UID"])
anaePolys %>% mutate(area = as.numeric(st_area(.))) %>% st_drop_geometry() %>% select(UID, area)

# bb area
box3 <- st_bbox(anaePolys[3, ])
# For comparison
box5 <- st_bbox(anaePolys[5, ])

st_area(st_as_sfc(box3))/st_area(st_as_sfc(box5))

# So, I'm not entirely sure WHY it's returning null. The BB is HUGE, but that should be SLOW, not NULL-
  # unless it's blowing up RAM, but I don't think that's it?

# How big are we talking?
ltimNoNorth <- read_sf(dsn = file.path(datDir, 'ANAE/MDB_ANAE_Aug2017/MDB_ANAE.gdb'), layer = 'LTIM_Valleys') %>%
  st_cast("MULTIPOLYGON") %>% # cleans up an issue with multisurfaces
  st_make_valid() %>%
  select(ValleyName, ValleyID, ValleyCode) %>%
  filter(ValleyName != 'Northern Unregulated')

plot(ltimNoNorth["ValleyName"], reset = FALSE)
plot(anaePolys["UID"], add = TRUE)
# Oh, maybe that IS too big

# let's try sorting bboxes
bbareafn <- function(x) {
  bbarea <- as.numeric(st_area(st_as_sfc(st_bbox(x))))
  return(bbarea)
}
# I think I'd need map(), but that's not implemented for sf
anaePolys %>% select(geometry) %>% bbareafn() # %>% st_drop_geometry() %>% select(UID, bbarea)
# SO
anaePolys$bbarea <- NA
for(r in 1:nrow(anaePolys)) {
  bbarea <- bbareafn(anaePolys[r, ])
  anaePolys$bbarea[r] <- bbarea
}
anaePolys %>% st_drop_geometry() %>%
  select(UID, bbarea)

# For 910
bigpolys <- anaePolys %>% st_drop_geometry() %>%
  select(UID, bbarea) %>%
  arrange(desc(bbarea))

bigpolys

toppolys <- which(anaePolys$UID %in% bigpolys$UID[1:3])

plot(filter(ltimNoNorth[, "ValleyName"], ValleyName == 'Warrego'), reset = FALSE)
plot(anaePolys[toppolys, ], add = TRUE)



# sorting out splitting the polygon (ie chunking within a polygon) --------
plot(anaePolys[3, "UID"])

# HOW TO SPLIT?
grid <- st_make_grid(anaePolys[3, ], n = 100)
plot(anaePolys[3, "UID"], reset = FALSE)
plot(grid, add = TRUE)

# Get the one that's an issue
badanae <- anaePolys[3, ]
badint <- st_intersection(badanae, grid)
badint

# That seems promising. Let's debug into rastPolyJoin
thiscrop <- st_crop(tifTimes, badanae, as_points = FALSE) # This is a bit funny, because we're going to crop this again, later
thisdepth <- rastPolyJoin(polysf = badanae, rastst = thiscrop, FUN = chosenSummary,
                          grouper = 'UID', maintainPolys = TRUE,
                          na.replace = 0, whichcrs = 3577)

# Assume in rastPolyjoin, we'll have an if to set up the grid,

grid <- st_make_grid(polysf, n = 100)
gridsf <- st_intersection(polysf, grid)

# Loop over each of the bits to bring in the raster
for (r in 1:nrow(gridsf)) {
  thissmall <- gridsf[r, ]
  smallcrop <- st_crop(thiscrop, thissmall, as_points = FALSE)
  smallRSF <- st_as_sf(smallcrop, as_points = FALSE, merge = FALSE, na.rm = FALSE)
  
  # This stuff all verbatim from the main function, just with names changed
  # replace NA?
  if (!is.na(na.replace)) {
    smallRSF[is.na(smallRSF)] <- na.replace
  }
  
  # Transform to correct crs.
  # Doing this here, because if rastst is a proxy, we won't be able to transform until it's read in as st_as_sf
  if (st_crs(smallRSF)$epsg != whichcrs) {
    smallRSF <- st_transform(smallRSF, whichcrs) %>%
      st_make_valid()
  }
  # ensure the polygons match
  if (st_crs(thissmall)$epsg != whichcrs) {
    thissmall <- st_transform(thissmall, whichcrs) %>%
      st_make_valid()
  }
  
  # Have to intersect with the polygons to get average.
  # Less fiddly (because it's one-to-one), and it ensures the averages are area
  # weighted (they're not with aggregate; see timePolyRastScratch for testing)
  intPRsmall <- st_intersection(thissmall, smallRSF)
  
}

# For testing glueing together
r <- 1
intPR1 <- intPRsmall
r <- 2
intPR2 <- intPRsmall
plot(intPR2["UID"], reset = FALSE)
plot(intPR1["UID"], add = TRUE)

# I've turned merge = FALSE on to make sure i'm doing this right, but I think merge = TRUE would probably be 'cheaper'
  # Though it seems to merge across time too? That's not good. Let's always leave it off for safety
glueInt <- bind_rows(intPR1, intPR2)
# seems pretty straightforward. a foreach would be super clean, since I can just .combine.
# It'll still be massive, but not NEARLY as massive.


# Try the foreach

grid <- st_make_grid(polysf, n = 100)
gridsf <- st_intersection(polysf, grid)

# Loop over each of the bits to bring in the raster
intPR <- foreach(r = 1:3, # testing
# intPR <- foreach(r = 1:nrow(gridsf),
                 .combine=bind_rows,
                 .multicombine=TRUE) %dopar% {
  thissmall <- gridsf[r, ]
  smallcrop <- st_crop(thiscrop, thissmall, as_points = FALSE)
  smallRSF <- st_as_sf(smallcrop, as_points = FALSE, merge = FALSE, na.rm = FALSE)
  
  # This stuff all verbatim from the main function, just with names changed
  # replace NA?
  if (!is.na(na.replace)) {
    smallRSF[is.na(smallRSF)] <- na.replace
  }
  
  # Transform to correct crs.
  # Doing this here, because if rastst is a proxy, we won't be able to transform until it's read in as st_as_sf
  if (st_crs(smallRSF)$epsg != whichcrs) {
    smallRSF <- st_transform(smallRSF, whichcrs) %>%
      st_make_valid()
  }
  # ensure the polygons match
  if (st_crs(thissmall)$epsg != whichcrs) {
    thissmall <- st_transform(thissmall, whichcrs) %>%
      st_make_valid()
  }
  
  # Have to intersect with the polygons to get average.
  # Less fiddly (because it's one-to-one), and it ensures the averages are area
  # weighted (they're not with aggregate; see timePolyRastScratch for testing)
  intPRsmall <- st_intersection(thissmall, smallRSF)
  intPRsmall
}
  
plot(intPR["UID"])


# OK, push that over to rastPolyJoin (and sort out the bbox size trigger), do a quick test, and get it runnning on the HPC

# sorting out the bbox size trigger
  # Should be based on how many raster cells I'm trying to read in. BUt I can't figure out how to get THEIR dimensions

# can I extract a single cell, do the transform, then get its size? This is stupid
  # and takes FOREVER. Doesn't seem to actually work to only pull in one.
# singlerast <- thiscrop %>% 
#   slice(x, 1) %>% 
#   slice(y, 1) %>% 
#   slice(time, 1) %>% 
#   st_as_sf(as_points = FALSE, merge = FALSE, na.rm = FALSE)

# Maybe just do it with only reading a subchunk way earlier
whichcrs = 3577
rasterio = list(nXOff = 1, nYOff = 1, nXSize = 10, nYSize = 10)
tif1 <- inunTifs[1] %>% # filenames
  read_stars(RasterIO = rasterio) %>% # cut
  st_as_sf(as_points = FALSE, merge = FALSE, na.rm = FALSE) %>% # Read in as sf polygons
  # Get area before and after transform to see if necessary
  mutate(areapre = as.numeric(st_area(.))) %>%
  st_transform(whichcrs) %>% # transform? Prob not REALLY necessary- worth checking the area of each- they're the same
  st_make_valid() %>%
  mutate(areapost = as.numeric(st_area(.)))
tif1  

# What units are those areas? M^2
st_area(tif1)

# OK, so that's a dumb, roundabout way to get the area. And we'll have to get the pixel size early in the process, but it will work
# Then, get the bbarea (which I already have on badanae)
# Can do this all with the above call, just separating for testing
pixarea <- tif1 %>% st_drop_geometry() %>% summarize(area = mean(areapre, na.rm = TRUE)) %>% pull()
badanae$bbarea/pixarea

# Let's see what the range we're looking at it: what works and what doesn't?
anaePolys %>% mutate(neededRastPix = bbarea/pixarea) %>% select(UID, bbarea, neededRastPix) %>% st_drop_geometry()

# 5 took 15 seconds, so that's fine. Does it roughly scale? No. 7 took 14
# seconds and 10 took 13, despite being roughly half of each other

# I wonder where the bottleneck starts. I'm sure I could roll up to 10,000 no problem. 100,000? 1 million?
# The offending anae has 21 million that it's trying to read. 

  # just looking at bbarea, the offending one has 17350630611, and the 10th biggest is 66037977
  # but this is just chunk 8 of the warrego. thos others were all done pretty quickly, I think.
  # one was 1500 seconds, and one was 6500. Otherwise, fine
  # the third biggest is 377792953, second biggest was 3284784590
17350630611/pixarea
3284784590/pixarea
377792953/pixarea
# So, relatively fast until somewhere between 460,000 and 4,000,000
# I guess we should have the cutoff as an argument to rastPolyJoin

# If I want to cut the polygon down to 500k, how many grid cells do I need?
maxPixels <- 500000
17350630611/pixarea/maxPixels # Oh, that's not actually bad at all. Probably actually better to go smaller to match the contours better
3284784590/pixarea/maxPixels # That's kind of silly to bother with
maxPixels <- 100000
17350630611/pixarea/maxPixels 
3284784590/pixarea/maxPixels

# None of these are getting down to the 10,000 I tried yesterday.
grid <- st_make_grid(anaePolys[3, ], n = ceiling(sqrt(17350630611/pixarea/maxPixels)))
plot(anaePolys[3, "UID"], reset = FALSE)
plot(grid, add = TRUE)

# does 'cellsize' let us avoid all the calcs?
  # It's not clear what cellsize is doing, but it's NOT based on area. Seems to be relative to width?
#   # DOn't use this
# gridcell <- st_make_grid(anaePolys[3, ], cellsize = c(0.5, 0.5))
# plot(anaePolys[3, "UID"], reset = FALSE)
# plot(gridcell, add = TRUE)


# So, I should be able to get that to work: pass rastPolyJoin the raster pixel area, and the maxPixels to read
# then do the foreach loop above
  # since gridsf has already thrown out all the grids with no anae in them, it'll only need to read in a subset

# merge() %>% # make a brick 
  # setNames('depth') %>% # name the attribute
  # # Make dates
  # # soilMstars uses an offset and delta, while this has 'values'.
  # # I seem to remember values might cause an issue somewhere down the track, so
  # # might need to revisit
  # st_set_dimensions(3, values = tifdates) %>% 
  # st_set_dimensions(names = c("x", "y", "time"))

rasterio = list(nXOff = 6, nYOff = 6, nXSize = 100, nYSize = 100, bands = c(1, 3, 4))
(x = read_stars(tif, RasterIO = rasterio))


# How much overhead are we using to just push everything through the grid-> foreach process when it's not needed
  # Let's benchmark for anaePolys[2, ]
# In both cases, we'll need the check
rasterio = list(nXOff = 1, nYOff = 1, nXSize = 10, nYSize = 10)
pixarea <- inunTifs[1] %>% # filenames
  read_stars(RasterIO = rasterio) %>% # cut
  st_as_sf(as_points = FALSE, merge = FALSE, na.rm = FALSE) %>% # Read in as sf polygons
  # transform not necessary - tested in warrego8_6_3.R
  mutate(area = as.numeric(st_area(.))) %>%
  st_drop_geometry() %>% 
  summarize(area = mean(area, na.rm = TRUE)) %>% 
  pull()
pixelsize <- pixarea

# apoly <- anaePolys[2, ]

maxPix <- 100000
na.replace <- 0

noeach <- function(apoly) {
  bbarea <- as.numeric(st_area(st_as_sfc(st_bbox(apoly))))
  pixneeded <- bbarea/pixelsize
  
  thissmall <- apoly
  # crop the raster to JUST this grid-cut polygon
  smallcrop <- st_crop(tifTimes, thissmall, as_points = FALSE)
  # merge = TRUE is very slightly slower, but it will preserve memory better
  smallRSF <- st_as_sf(smallcrop,
                       as_points = FALSE,
                       merge = TRUE,
                       na.rm = FALSE)
  
  # This stuff all verbatim from the unchunked function, just with names changed
  # replace NA?
  if (!is.na(na.replace)) {
    smallRSF[is.na(smallRSF)] <- na.replace
  }
  
  # Transform to correct crs.
  # Doing this here, because if rastst is a proxy, we won't be able to transform until it's read in as st_as_sf
  if (st_crs(smallRSF)$epsg != whichcrs) {
    smallRSF <- st_transform(smallRSF, whichcrs) %>%
      st_make_valid()
  }
  # ensure the polygons match
  if (st_crs(thissmall)$epsg != whichcrs) {
    thissmall <- st_transform(thissmall, whichcrs) %>%
      st_make_valid()
  }
  
  # Now intersect with the chunk of the polysf
  # Less fiddly (because it's one-to-one), and it ensures the averages are area
  # weighted (they're not with aggregate; see timePolyRastScratch for testing)
  intPRsmall <- st_intersection(thissmall, smallRSF)
  # And return to the foreach, these get bind_row'ed together
  intPRsmall
}


pareach <- function(apoly) {
  
  bbarea <- as.numeric(st_area(st_as_sfc(st_bbox(apoly))))
  pixneeded <- bbarea/pixelsize
  
  grid <- st_make_grid(apoly, n = ceiling(sqrt(pixneeded/maxPixels)))
  # Now, the intersection gets the polysf split by grids (and drops grids with no polysf in them)
  gridsf <- st_intersection(apoly, grid) 
  # rastSF <- tryCatch()
  
  # Loop over each of the bits to bring in the raster
  # NOT sure this is worth the foreach overhead if there aren't any to loop
  # over? I'd like to not have to write a parallel and non-parallel version
  # though with a conditional to choose
  # testing
  # intPR <- foreach(r = 1:3,
  intPR <- foreach(r = 1:nrow(gridsf),
                   .combine = bind_rows,
                   .multicombine = TRUE) %dopar% {
                     # Get the grid-cut polygon r
                     thissmall <- gridsf[r,]
                     # crop the raster to JUST this grid-cut polygon
                     smallcrop <- st_crop(rastst, thissmall, as_points = FALSE)
                     # merge = TRUE is very slightly slower, but it will preserve memory better
                     smallRSF <- st_as_sf(smallcrop,
                                          as_points = FALSE,
                                          merge = TRUE,
                                          na.rm = FALSE)
                     
                     # This stuff all verbatim from the unchunked function, just with names changed
                     # replace NA?
                     if (!is.na(na.replace)) {
                       smallRSF[is.na(smallRSF)] <- na.replace
                     }
                     
                     # Transform to correct crs.
                     # Doing this here, because if rastst is a proxy, we won't be able to transform until it's read in as st_as_sf
                     if (st_crs(smallRSF)$epsg != whichcrs) {
                       smallRSF <- st_transform(smallRSF, whichcrs) %>%
                         st_make_valid()
                     }
                     # ensure the polygons match
                     if (st_crs(thissmall)$epsg != whichcrs) {
                       thissmall <- st_transform(thissmall, whichcrs) %>%
                         st_make_valid()
                     }
                     
                     # Now intersect with the chunk of the polysf
                     # Less fiddly (because it's one-to-one), and it ensures the averages are area
                     # weighted (they're not with aggregate; see timePolyRastScratch for testing)
                     intPRsmall <- st_intersection(thissmall, smallRSF)
                     # And return to the foreach, these get bind_row'ed together
                     intPRsmall
                   }
}

# Do the benchmarking
# This actually isn't really fair, this would happen outside
apoly <- anaePolys[4, ]
rastst <- st_crop(tifTimes, apoly, as_points = FALSE)
paroverhead <- microbenchmark::microbenchmark("noeach" = { b <- noeach(apoly)},
                                           "pareach" = { b <- pareach(apoly) },
                                           times = 5)
paroverhead
# adds 0.15 seconds for 1 (rastpix 59)
# adds 0.08 seconds for 4 (149 pix)
# If there are 200,000, adding 0.15 seconds for each one adds a total of 
200000*0.15/60/60 # 8.33 hours. Yikes when looked at that way.

# Guess I'll write the internal stuff as a function, and then a conditional to run it once or to foreach it.

# Prototype the function here- THIS IS THE CORE FUNCTION THAT does a single
# polygon x raster intersect (or a whole sf dataframe) and ensures transforms,
# etc
rpintersect <- function(singlesf, singleraster, 
                        na.replace = NA, whichcrs = 3577) {
  
  # read in the stars as sf polys
  rastSF <- st_as_sf(singleraster, as_points = FALSE, merge = TRUE, na.rm = FALSE)
  
  # replace NA?
  if (!is.na(na.replace)) {
    rastSF[is.na(rastSF)] <- na.replace
  }
  
  # Transform to correct crs.
  # Doing this here, because if rastst is a proxy, we won't be able to transform until it's read in as st_as_sf
  if (st_crs(rastSF)$epsg != whichcrs) {
    rastSF <- st_transform(rastSF, whichcrs) %>%
      st_make_valid()
  }
  # ensure the polygons match
  if (st_crs(singlesf)$epsg != whichcrs) {
    singlesf <- st_transform(singlesf, whichcrs) %>%
      st_make_valid()
  }
  
  # Have to intersect with the polygons to get average.
  # Less fiddly (because it's one-to-one), and it ensures the averages are area
  # weighted (they're not with aggregate; see timePolyRastScratch for testing)
  intersectedPR <- st_intersection(singlesf, rastSF)
} 






# OK, run the top part to get things loaded in, then test rastPoly --------
  # let's cut to a couple that aren't huge, but where we can make maxPix low enough to do something to test both loops

anaeTest <- anaePolys[c(1, 4, 6), ]

rasterio = list(nXOff = 1, nYOff = 1, nXSize = 10, nYSize = 10)
pixarea <- inunTifs[1] %>% # filenames
  read_stars(RasterIO = rasterio) %>% # cut
  st_as_sf(as_points = FALSE, merge = FALSE, na.rm = FALSE) %>% # Read in as sf polygons
  # transform not necessary - tested in warrego8_6_3.R
  mutate(area = as.numeric(st_area(.))) %>%
  st_drop_geometry() %>% 
  summarize(area = mean(area, na.rm = TRUE)) %>% 
  pull()


dpListAll <- foreach(s = 1:nrow(anaeTest)) %dopar% {
  thiscrop <- st_crop(tifTimes, anaeTest[s,], as_points = FALSE)
  thisdepth <- rastPolyJoin(polysf = anaeTest[s,], rastst = thiscrop, FUN = chosenSummary,
                            grouper = 'UID', maintainPolys = TRUE,
                            na.replace = 0, whichcrs = 3577,
                            maxPixels = 100000,
                            pixelsize = pixarea)
}

dpListPar <- foreach(s = 1:nrow(anaeTest)) %dopar% {
  thiscrop <- st_crop(tifTimes, anaeTest[s,], as_points = FALSE)
  thisdepth <- rastPolyJoin(polysf = anaeTest[s,], rastst = thiscrop, FUN = chosenSummary,
                            grouper = 'UID', maintainPolys = TRUE,
                            na.replace = 0, whichcrs = 3577,
                            maxPixels = 50, # Stupidly small, but lets us test
                            pixelsize = pixarea)
}


chunkcost <- microbenchmark::microbenchmark("all" = { dpListAll <- foreach(s = 1:nrow(anaeTest)) %dopar% {
                                thiscrop <- st_crop(tifTimes, anaeTest[s,], as_points = FALSE)
                                thisdepth <- rastPolyJoin(polysf = anaeTest[s,], rastst = thiscrop, FUN = chosenSummary,
                                                          grouper = 'UID', maintainPolys = TRUE,
                                                          na.replace = 0, whichcrs = 3577,
                                                          maxPixels = 100000,
                                                          pixelsize = pixarea)
} },
                               "par" = { dpListPar <- foreach(s = 1:nrow(anaeTest)) %dopar% {
                                 thiscrop <- st_crop(tifTimes, anaeTest[s,], as_points = FALSE)
                                 thisdepth <- rastPolyJoin(polysf = anaeTest[s,], rastst = thiscrop, FUN = chosenSummary,
                                                           grouper = 'UID', maintainPolys = TRUE,
                                                           na.replace = 0, whichcrs = 3577,
                                                           maxPixels = 50, # Stupidly small, but lets us test
                                                           pixelsize = pixarea)
                               } },
                               times = 2)

chunkcost # oof. I guess the question is, does it let things run that WOULDN'T otherwise? I dunno. Guess we'll find out...
  # THe payoff for ignoring grid cells will certainly get bigger as we scale up to reasonable maxPixels
# The foreach loops -------------------------------------------------------


# dpList <- foreach(s = 8:12) %dopar% {
dpList <- foreach(s = 1:nrow(anaePolys)) %dopar% {
  thiscrop <- st_crop(tifTimes, anaePolys[s,], as_points = FALSE)
  thisdepth <- rastPolyJoin(polysf = anaePolys[s,], rastst = thiscrop, FUN = chosenSummary,
                            grouper = 'UID', maintainPolys = TRUE,
                            na.replace = 0, whichcrs = 3577)
} # end foreach

# Then, unpack the lists also using foreach
depthAns <- foreach(l = 1:length(dpList),
                    .combine=function(...) c(..., along = 1), # Pass dimension argument to c.stars
                    .multicombine=TRUE) %dopar% {
                      dpList[[l]][[1]]
                    }

depthIndex <- foreach(l = 1:length(dpList),
                      .combine=bind_rows,
                      .multicombine=TRUE) %dopar% {
                        dpList[[l]][[2]]
                      }

