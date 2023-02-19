# Script to process inundation for birds
# Script for chunk-processing the catchments, following inundation sub-chunks
# The hpc_wrapper tends to source this too, but if I do it here I don't have to
# wrap the script to run locally. Sort out a cleaner way to do this
# source('directorySet.R')

# Let's get libraries here, then sort out git then sort out making this a library so we don't have to deal with all the library crap
# library(sp)
# library(rgeos)
# library(here)
# library(tidyverse)
# library(sf)
# # library(RNetCDF) # Probably not, raster can handle geographic netCDF
# # library(raster) # maybe?
# library(stars)
# library(foreach)
# library(doFuture)

# source('Functions/rastPolyJoin.R') # Shouldn't be needed anymore

# Set up parallel backend
# registerDoFuture()
# plan(multicore) # multicore on HPC

# # For local testing
# plan(multisession)
# summaryFun <- 'volInun'

# args order: 
# SLURM_ARRAY_TASK_ID (ie the chunk) is 8
# Catchment name is 9
# summaryFun is 7
# the script to run (this one) is 6
# Subchunks are 10+

# args <- c('blah', 'b', 'c', 'g', '3', 't', 'a', '3', 'Warrego', '8', '6', '8')
# args <- c('blah', 'b', 'c', 'g', '5', 't', 'a', '9', 'Warrego', '8', '6', '10')
# Does it break with one level of chunking?
# args <- c('blah', 'b', 'c', 'g', '5', 't', 'a', '10', 'Avoca')

# args <- c('blah', 'b', 'c', 'g', '5', 't', 'a', '96', 'Broken')

# ## The outerchunks need to start outer and go in, ie '8', '6' is the 6th subchunk of the 8th main chunk
# Need to handle the edge case wehre there aren't enough polys to do the array we're asking for

processInundationGeneral <- function(datOut, args) {
  # Make a sub-directory for the subchunk
  # scriptOut <- paste0(datOut, '/Tempprocessed/', summaryFun, '/chunked/', 
  #                     str_flatten(args[9:length(args)], collapse = '/sub_'))
  
  summaryFun <- args[7]
  
  scriptOut <- paste0(datOut, '/Inundationprocessed/', summaryFun, '/chunked/', 
                      str_flatten(args[9:length(args)], collapse = '/sub_'))
  
  # Set the projected CRS we want to use (australian Albers, typically)
  commonCRS <- 3577
  
  # Choose a size for the chunks. This is likely better elsewhere, but
  nchunks <- 100
  arraynum <- as.numeric(args[8])
  chunkName <- args[8]
  
  # this may be NA if there are no outer chunks
  if (length(args) > 9) {
    outerchunks <- as.integer(args[10:length(args)])
    onelayer <- FALSE
  } else {
    onelayer <- TRUE
  }
  
  
  # and the maximum-ish (there's a ceiling() involved, so this is approximate)
  # number of pixels to read in for any one ANAE
  maxPix <- 100000 # Seems likely to be a good balance based on local testing, but we'll see
  
  
  # INUNDATION UNITS ARE METERS ---------------------------------------------
  
  # NO INUNDATION IS NA *and* 0, as far as I can tell------------------------
  
  
  
  # Set up the function -----------------------------------------------------
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
  } else if (summaryFun == 'weightedMean') {
    # Weighted mean, ignoring NA by default
    chosenSummary <- function(x, area) {
      weighted.mean(x, area, na.rm = TRUE)
    }
  } else if (summaryFun == 'areaSpoonbillForage') {
    # area of water <= 40cm and > 0
    # Have to sort of write a min() out of ifelse or it does min() over the column, not the rows
    chosenSummary <- function(x, area, limitShallow = 0, limitDeep = 0.4) {
      sum(ifelse(x > limitShallow & x <= limitDeep, area, 0))
    }
  } else if (summaryFun == 'areaSpoonbillBreed') {
    # area of water <= 150cm and >= 50cm
    # Have to sort of write a min() out of ifelse or it does min() over the column, not the rows
    chosenSummary <- function(x, area, limitShallow = 0.5, limitDeep = 1.5) {
      sum(ifelse(x >= limitShallow & x <= limitDeep, area, 0))
    }
  } else if (summaryFun == 'maxInunDepth') {
    # get the maximum depth. so far just used for testing
    chosenSummary <- function(x, area) {
      max(x, na.rm = TRUE)
    }
  } else if (summaryFun == 'lippiaAdultSurvive') {
    # area of water <= 30cm
    chosenSummary <- function(x, area, limitShallow = 0, limitDeep = 0.3) {
      sum(ifelse((x <= limitDeep)|(is.na(x)), area, 0))
    }
  } else {
    stop('need to choose a summary function')
  }
  
  # Read in all the data ----------------------------------------------------
  
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
  
  # Weird geometry types were causing problems for the grid, but don't seem to
  # be an issue here. If there are potential issues, throw a warning.
  if (any(!st_is(anaePolys, c('POLYGON', 'MULTIPOLYGON')))) {
    warning("anaePolys have geometries other than polygons, which tends to cause 
            problems with crops. Do somethign like 
            `st_collection_extract(anaePolys, 'POLYGON')` to get rid of the 
            non-areal geometries. 
            See the grid loop in rastPolyJoin for an example")
  }
  
  # change its name to something generic so can be looped over
  # This is annoying, but I guess not too bad
  
  
  # Sort out the chunks -----------------------------------------------------
  
  # FIRST, get the correct outer chunk that we want to drill into
  # I *think* this wouldn't be TOO terrible to make recursive if needed, by
  # looping over args[10]: length(args) and grabbing the desired bit each time
  # as long as we weren't also chaning nchunks. Although that wouldn't be too
  # bad either, really, just would need more args
  # Get the row indices from the array number 
  
  # For loop lets us drill down by feeding additional arguments to the shell script
  if (!onelayer) { # handle the case where we're not subchunking
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
          # Can be too high even for chunks before the last one in weird edge
          # cases, so handle that
          if (outertop > nrow(anaePolys)) {
            outertop <- nrow(anaePolys) 
          }
        }
        
        # cut to this chunk of polygons
        anaePolys <- anaePolys[outerbottom:outertop, ]
      }
    }
  }
  
  
  # THEN, break up into the main chunks that need to be run here
  # This is exactly the same, but uses arraynum to define the chunk instead of a
  # predefined argument. Could easily put in the loop but not going to for
  # clarity, and because the ordering is a bit backwards
  
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
      # Can be too high even for chunks before the last one in weird edge cases,
      # so handle that
      if (top > nrow(anaePolys)) {
        top <- nrow(anaePolys) 
      }
    }
    
    # When running many chunks, the ceiling() call can make the chunks large
    # enough to finish in chunks before the end, and then grab NAs and the
    # endpoint. Need to bypass that, but I still want the script that checks what
    # has finished to work, as well as the script to concatenate, so need to
    # output a dummy- check those scripts,so get the file type and naming
    # convention right- ie if I export a text does it get picked up error checking
    # but not concat? Or can I spit out an sf with no rows?
    # They both expect a .rdata. So, can I return an empty one? the catch is
    # they expect an index version too, and will try to concat. So I actually
    # need to build a dummy dpList
    # Well, I could also put a flag in their name to keep them out of
    # catchfiles in concat. Not sure what is safest
    # Probably the dpList with no-row entries, but let's see what that does
    # Does not work to just cut to 0 row dataframe- rastpolyjoin fails
    if (bottom > nrow(anaePolys)) {
      bottom <- 0
      top <- 0
    }
    
    # cut to this chunk of polygons
    anaePolys <- anaePolys[bottom:top, ]
  }
  
  
  
  print(paste0('number of polygons processing is ', nrow(anaePolys)))
  
  # Transform to stars crs
  anaePolys <- st_transform(anaePolys, starCRS)
  
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
  
  # Get pixel area (for reference, but also to pass to rastPolyJoin as a check if the read-in is too large)
  # Read in 100 pixels of the first sheet
  rasterio = list(nXOff = 1, nYOff = 1, nXSize = 10, nYSize = 10)
  pixarea <- inunTifs[1] %>% # filenames
    read_stars(RasterIO = rasterio) %>% # cut
    st_as_sf(as_points = FALSE, merge = FALSE, na.rm = FALSE) %>% # Read in as sf polygons
    # transform not necessary - tested in warrego8_6_3.R
    mutate(area = as.numeric(st_area(.))) %>%
    st_drop_geometry() %>% 
    summarize(area = mean(area, na.rm = TRUE)) %>% 
    pull()
  
  # test- what are the units?
  areaunits <- inunTifs[1] %>% # filenames
    read_stars(RasterIO = rasterio) %>% # cut
    st_as_sf(as_points = FALSE, merge = FALSE, na.rm = FALSE) %>% # Read in as sf polygons
    # transform not necessary - tested in warrego8_6_3.R
    mutate(area = st_area(.))
  
  # Do the aggregation ------------------------------------------------------
  
  # the below gets weird if I run with nrow(anaePolys) == 0. tried to fix in
  # rastPolyJoin, and did, but the list unpacking is still a pain. so, use a
  # workaround 
  
  # Need to change the way we do the grid-cropped situation. It should be out
  # here so we don't have to potentially crop twice.
  # dpList <- foreach(s = 8:12) %dopar% {
  dpList <- foreach(s = 1:nrow(anaePolys)) %dopar% {

    thiscrop <- st_crop(tifTimes, anaePolys[s,], as_points = FALSE)
    thistemp <- rastPolyJoin(polysf = anaePolys[s,], rastst = thiscrop, FUN = chosenSummary,
                             grouper = 'UID', maintainPolys = TRUE,
                             na.replace = 0, whichcrs = commonCRS, 
                             maxPixels = maxPix,
                             pixelsize = pixarea,
                             uncropraster = tifTimes)
  } # end foreach
  
  # the below gets weird if I run with nrow(anaePolys) == 0. tried to fix in
  # rastPolyJoin, and did, but the list unpacking is still annoying (the combine
  # functions turn NULLs into things). so, use a workaround
  
  if (nrow(anaePolys) == 0) {
    tempAns <- NULL
    tempIndex <- NULL
  } else {
    # Then, unpack the lists also using foreach
    tempAns <- foreach(l = 1:length(dpList),
                       .combine=function(...) c(..., along = 1), # Pass dimension argument to c.stars
                       .multicombine=TRUE) %dopar% {
                         dpList[[l]][[1]]
                       }
    
    tempIndex <- foreach(l = 1:length(dpList),
                         .combine=bind_rows,
                         .multicombine=TRUE) %dopar% {
                           dpList[[l]][[2]]
                         }
  }
  
  
  # If we did this over all of ANAEbasinclim it'd be cleaner, but all the catchments would be mixed up
  # I could do something like matching UIDS to ValleyName, and then splitting the output up into separate folders
  # For now, I'm actually going to change some things to just run this within-catchment though
  # ultimately, this should use a future.batchapply to spawn chunks within catchments
  
  # Make the out directory, in case it doesn't exist
  if (!dir.exists(scriptOut)) {dir.create(scriptOut, recursive = TRUE)}
  
  thisInunName <- str_remove_all(thisAName, 'ANAE')
  if (!onelayer) {
    thistemp <- paste0(thisInunName, '_', summaryFun, '_', 
                       str_flatten(args[10:length(args)], collapse = '_'),
                       '_', chunkName)
    thisIndex <- paste0(thisInunName, '_', summaryFun, '_index', '_',
                        str_flatten(args[10:length(args)], collapse = '_'),
                        '_', chunkName)
  } else {
    thistemp <- paste0(thisInunName, '_', summaryFun, '_', chunkName)
    thisIndex <- paste0(thisInunName, '_', summaryFun, '_index', '_', chunkName)
  }
  
  assign(thistemp, tempAns)
  assign(thisIndex, tempIndex)
  
  # Could just use thisInunName for the rdata, since there's a folder structure,
  # but this is more explicit
  # This does not actually save a list; to save with character vectors need to
  # use the list of characters
  save(list = c(thistemp, thisIndex), file = file.path(scriptOut, paste0(thistemp, '.rdata')))
}



