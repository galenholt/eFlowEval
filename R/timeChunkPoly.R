# Functions to do the averages into rasters in time chunks

# Write a function to grab a chunk from t to t+chunksize
chunkprocess <- function(rastpath, rastvar, totaltime, starttime, chunksize, catchCrop, polyAvg, 
                         saverast = FALSE, firstdim = NULL) {
  
  # Deal with fewer days than the size of the chunk
  thischunk <- ifelse(starttime + chunksize < totaltime, chunksize, days - starttime)
  
  # Unlike with soil moist, now I have to do the read-in and cut to lachlan too
  croptime <- cbind(start = c(1,1,starttime), count = c(soilDims$x$to, soilDims$y$to, thischunk))
  
  rastNC <- read_ncdf(rastpath, var = rastvar, ncsub = croptime)
  st_crs(rastNC) <- 4326
  
  # if not starting at time 1, have to brute force fix the dimensions
  if (starttime != 1) {
    if (is.null(firstdim)) {
      rastNC1 <- read_ncdf(rastpath, var = rastvar, ncsub = cbind(start = c(1,1,1), count = c(soilDims$x$to, soilDims$y$to, 2)))
      st_crs(rastNC1) <- 4326
      
      rastNC <- dimfixer(rastNC, rightdim = st_dimensions(rastNC1), from = starttime, to = (starttime + thischunk - 1))
    } else {
      rastNC <- dimfixer(rastNC, rightdim = firstdim, from = starttime, to = (starttime + thischunk - 1))
    }
  }
  
  
  # # Brief looks, cut later
  # rastNC
  # format(object.size(rastNC), units = "auto") # Check size
  # # Cycled through a bunch, 8 is the best as a test (though 5,6, and 9 are good too)
  # plot(rastNC[,,,8], reset = FALSE)
  # plot(st_geometry(lachcutter), border = 'red', add = TRUE)
  # # well, specifying the datum at least lets me read it in...
  # # Those are pretty crap extents though. Might grab a different set for testing so I can see what I'm doing
  
  # Now, crop that to the catchment
  # system.time(lachTnc <- st_crop(rastNC, filter(ltimNoNorth, ValleyName == 'Lachlan'), as_points = FALSE))
  cropRast <- st_crop(rastNC, catchCrop, as_points = FALSE)
  
  # system.time(chunk1 <- rastPolyJoin(polysf = polyAvg, 
  #                                    rastst = rastNC, # Doesn't need to be indexed by time now, because the time-indexing is done on read-in and object creation
  #                                    grouper = 'SYS2', 
  #                                    maintainPolys = TRUE))
  chunk <- rastPolyJoin(polysf = polyAvg, 
                        rastst = cropRast, # Doesn't need to be indexed by time now, because the time-indexing is done on read-in and object creation
                        grouper = 'SYS2', 
                        maintainPolys = TRUE)
  
  # Let's try writing the lachlan as a stars on each loop, see how inefficient that will be
  # st_drivers(what = "raster")
  
  # trying to save as netcdf is barfing. Tif seems to work, though time gets
  # turned into band. I think this will get memory-intensive fast though
  if(saverast) {
    write_stars(obj = cropRast, dsn = file.path(datOut, paste0(deparse(substitute(catchCrop)),"_", rastvar, "_", as.character(starttime), ".tif")),
                layer = rastvar)
  }
  
  return(chunk)
}

# And a wrapper to loop over time
chunklooper <- function(nchunks, rastpath, rastvar, 
                        totaltime, starttime, chunksize, 
                        catchCrop, polyAvg, saverast = FALSE, 
                        savepoints = 0, savepath = NULL) {
  
  # Get the first chunk so we can append to it
  chunk1 <- chunkprocess(rastpath = rastpath, rastvar = rastvar, 
                         totaltime = totaltime, starttime = 1, chunksize = chunksize, 
                         catchCrop = catchCrop, polyAvg = polyAvg, saverast = saverast)
  
  # WHY is this huge?
    # "Large dimensions(191.4Mb), whcih is ALSO the size of chunk1"
    # BECUASE I"M NOT USING THE FIRST PART
    # NO, (well, not only). I think it's because even the first dim is the OUTPUT. BUT we need the RASTER dims, not the input dims.
      # So, could replace the firstdim below with NULL to get it every time, but that takes time. INstead, let's do it once here and not loop over it
  # dim1 <- st_dimensions(chunk1[[1]])
  rastNC1 <- read_ncdf(rastpath, var = rastvar, 
                       ncsub = cbind(start = c(1,1,1), 
                                     count = c(soilDims$x$to, soilDims$y$to, 2)))
  st_crs(rastNC1) <- 4326
  # TODO: make this not dependent on getting soilDims implicitly
  
  dim1 <- st_dimensions(st_dimensions(rastNC1))
  
  # Loop and append
  for (t in 2:nchunks) {
    from <- (t-1)*(chunksize) + 1
    dtemp <- chunkprocess(rastpath = rastpath, rastvar = rastvar, 
                          totaltime = totaltime, starttime = from, chunksize = chunksize, 
                          catchCrop = catchCrop, polyAvg = polyAvg, saverast = saverast, firstdim = dim1) # The chunkprocess function handles a chunk that wants to go past the end.
    
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
    
    if (t %in% savepoints) {
      save(chunk1, file = paste0(savepath, "_", as.character(t), '.rdata'))
    }
  }
  return(chunk1)
  

}

# Function to fix the dimensions if doesn't start at 0
dimfixer <- function(rastnc, rightdim, from, to) {
  # Get the dims from the first set
  # replace the from-to
  # Glue back on
  thesedims <- st_dimensions(rastnc)
  thesedims$time <- rightdim$time
  thesedims$time$from <- from
  thesedims$time$to <- to
  
  st_dimensions(rastnc) <- thesedims
  return(rastnc)
  
}







