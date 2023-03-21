

processData <- function(dataname,
                        data_dir,
                        poly_path = file.path(out_dir, 'ANAEprocessed'),
                        summaryFun, 
                        out_dir, 
                        catchment,
                        thischunk, # character. formerly `arrayNum` (numeric) and `chunkName` (the character version)
                        subchunkArgs = NULL,
                        nchunks = 100,
                        whichcrs = 3577,
                        maxPix = 100000,
                        rastRollArgs = NULL,
                        saveout = TRUE) {
  start_time <- Sys.time()
  # Make a sub-directory for the subchunk
  if (length(subchunkArgs) == 0 | is.null(subchunkArgs)) {chunkpath <- catchment} 
  if (length(subchunkArgs) > 0) {
    chunkpath <- stringr::str_flatten(c(catchment, subchunkArgs),
                                         collapse = '/sub_')
    }
  scriptOut <- file.path(out_dir, paste0(dataname, 'processed'), summaryFun, 'chunked',
                         chunkpath)
  
  # Get the function indicated by summaryFun. Why don't we just use the
  # character name and have the functions defined? I don't know. That'd be
  # better
  chosenSummary <- get_data_proc_function(summaryFun)
  
  # Get the needed chunk of anaes
  anaePolys <- get_anae_chunk(anae_path = poly_path,
                              catchment = catchment, 
                              thischunk = thischunk, 
                              subchunkArgs = subchunkArgs,
                              nchunks = nchunks)
  
  npolys <- nrow(anaePolys)
  print(paste0('number of polygons processing is ', npolys))
  
  # Get the stars proxy
  if (grepl(dataname,'inundation', ignore.case=TRUE)) {
    # For now hardcode inun_dir
    inunDir <- file.path(data_dir, 'Inundation', 'WaterDepth_TwoMonthly', 'geotiff')
    proxylist <- read_inundation(inun_dir = inunDir)
    # inundation has NA as zeros, so set na.replace
    rlang::inform("In inundation dataset NAs mean 0. Setting to 0")
    na.replace <- 0
  }
  
  if (grepl(dataname, 'soiltemp', ignore.case=TRUE) | 
      grepl(dataname, 'soil_temp', ignore.case=TRUE) |
      grepl(dataname, 'soil temp', ignore.case=TRUE)) {
    tempDir <- file.path(data_dir, 'soilTemp14_20')
    
    # The `sub = 'LST_Day_1km'` argument happens inside here
    proxylist <- read_soil_temp(tempDir)
    na.replace <- NA
    
  }
  
  if (grepl(dataname, 'moist', ignore.case=TRUE) |
      grepl('moist', dataname, ignore.case= TRUE)) {
    moistDir <- file.path(data_dir, 'soilmoisture')
    
    # units are soil moisture percent 'sm_pct' as 0-1, not 0-100
    proxylist <- read_soil_moisture(moistDir)
    na.replace <- NA
    
  }
  
  
  # Transform ANAE to stars_proxy crs because we can't transform a proxy
  anaePolys <- st_transform(anaePolys, st_crs(proxylist$proxy_data))
  
  # Do the aggregation ------------------------------------------------------
  # If we did this over all of ANAEbasinclim it'd be cleaner, but all the
  # catchments would be mixed up I could do something like matching UIDS to
  # ValleyName, and then splitting the output up into separate folders For now,
  # I'm actually going to change some things to just run this within-catchment
  # though ultimately, this should use a future.batchapply to spawn chunks
  # within catchments (and skip the whole outer SLURM/foreach entirely)
  
  # parallel loop over the anae polygons
  dpList <- foreach(s = 1:nrow(anaePolys)) %dopar% {
    # moved the cropping all the way in to rpintersect
    thistemp <- rastPolyJoin(polysf = anaePolys[s,], 
                             rastst = proxylist$proxy_data, 
                             FUN = chosenSummary,
                             grouper = 'UID', maintainPolys = TRUE,
                             na.replace = na.replace,
                             whichcrs = whichcrs, 
                             maxPixels = maxPix,
                             pixelsize = as.numeric(proxylist$pixarea),
                             rastRollArgs = rastRollArgs)
  } # end foreach
  
  
  
  # the below gets weird if I run with nrow(anaePolys) == 0. tried to fix in
  # rastPolyJoin, and did, but the list unpacking is still annoying (the combine
  # functions turn NULLs into things). so, use a workaround
  # There's GOT to be a cleaner way to do this though. 
  if (nrow(anaePolys) == 0) {
    tempAns <- NULL
    tempIndex <- NULL
  } else {
    # Then, unpack the lists also using foreach 
      # does making these %do% actually
      # speed things up overall by giving more resources to the dopars?
      # Does using dopar potentially shuffle these relative to each other?
    tempAns <- foreach(l = 1:length(dpList),
                       .combine=function(...) c(..., along = 1), # Pass dimension argument to c.stars
                       .multicombine=TRUE) %do% {
                         dpList[[l]][[1]]
                       }
    
    tempIndex <- foreach(l = 1:length(dpList),
                         .combine=bind_rows,
                         .multicombine=TRUE) %do% {
                           dpList[[l]][[2]]
                         }
  }
  
  # Make the out directory, in case it doesn't exist
  if (!dir.exists(scriptOut)) {dir.create(scriptOut, recursive = TRUE)}
  
  # These subchunk indices feel backwards (the outer chunk is on the right,
  # instead of building L -> R). But other things depend on that so don't fix
  # unless planning a big restructure 
  # str_replace is because when there's no subchunk we get two _ next to each
  # other.
  thistemp <- paste0(catchment, '_', summaryFun, '_', 
                     str_flatten(subchunkArgs, collapse = '_'),
                     '_', thischunk) |>
    stringr::str_replace_all('__', '_')
  
  thisIndex <- paste0(catchment, '_', summaryFun, '_index', '_',
                      str_flatten(subchunkArgs, collapse = '_'),
                      '_', thischunk) |>
    stringr::str_replace_all('__', '_')
  
  # Got to be a cleaner way to do this.
  assign(thistemp, tempAns)
  assign(thisIndex, tempIndex)
  
  # probably a cleaner way to do this, likely with saveRDS to allow cleaner
  # read-in
  
  # Could just use catchment for the rdata, since there's a folder structure,
  # but this is more explicit
  # This does not actually save a list; to save with character vectors need to
  # use the list of characters
  # saveout is a way to bypass saving while testing
  if (saveout) {
    save(list = c(thistemp, thisIndex), file = file.path(scriptOut, paste0(thistemp, '.rdata')))
  }
  
  end_time <- Sys.time()
  elapsed <- end_time-start_time
  
  sumtab <- tibble::tibble(catchment, chunknumber = as.numeric(thischunk), summaryFun, npolys, pixarea = proxylist$pixarea, elapsed)
  
  return(sumtab)
}