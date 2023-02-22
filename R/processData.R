# Things we'll now need to unpack outside (esp on HPC)

# Set up parallel backend
# registerDoFuture()
# plan(multicore) # multicore on HPC

# # For local testing
# plan(multisession)
# args order: 
# SLURM_ARRAY_TASK_ID (ie the chunk) is 8
# Catchment name is 9
# summaryFun is 7
# the script to run (this one) is 6
# Subchunks are 10+

# summaryFun <- args[7]

# Note stash TO get pixarea and areaunits for soil moisture, need to deal with
# time differently than in inundation because they are yearly NCs. Something like
# read_stars(allpaths[1], RasterIO = rasterio, proxy = FALSE)[,,,1] %>% 
# st_set_crs(4326) %>% 
#   adrop() %>% 
#   st_as_sf(as_points = FALSE, merge = FALSE, na.rm = FALSE) %>% 
#   mutate(area = st_area(.))

# Make the pixarea and units its own function. It will be useful for any future
# setup of new datasets too to see how big their pixels are. Unclear why I did
# it twice- the units is exactly the same thing, just not averaged? ie all we
# need is %>% dplyr::summarize(mean_area = mean(area)) on the end

# For testing
catchment <- 'Avoca'
thischunk <- '9'
subchunkArgs <- c('8', '6', '10')
subchunkArgs <- NULL
dataname <- 'Inundation'
data_dir <- datDir
summaryFun <- 'weightedMean'

processData <- function(dataname,
                        data_dir,
                        poly_path = file.path(datOut, 'ANAEprocessed'),
                        summaryFun, 
                        datOut, 
                        catchment,
                        thischunk, # character. formerly `arrayNum` (numeric) and `chunkName` (the character version)
                        subchunkArgs = NULL,
                        nchunks = 100,
                        whichcrs = 3577,
                        maxPix = 100000) {
  # Make a sub-directory for the subchunk
  if (length(subchunkArgs) == 0 | is.null(subchunkArgs)) {chunkpath <- catchment} 
  if (length(subchunkArgs) > 0) {
    chunkpath <- stringr::str_flatten(c(catchment, subchunkArgs),
                                         collapse = '/sub_')
    }
  scriptOut <- file.path(datOut, paste0(dataname, 'processed'), summaryFun, 'chunked',
                         chunkpath)
  
  # Get the function indicated by summaryFun. Why don't we just use the
  # character name and have the functions defined? I don't know. That'd be
  # better
  chosenSummary <- get_data_proc_function(summaryFun)
  
  # Get the needed chunk of anaes
  anaePolys <- get_anae_chunk(anae_path = poly_path,
                              catchment = catchment, 
                              thischunk = thischunk, subchunkArgs)
  
  print(paste0('number of polygons processing is ', nrow(anaePolys)))
  
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
    tempDir <- file.path(datDir, 'soilTemp14_20')
    
    proxylist <- read_soil_temp(tempDir)
    
  }
  
  if (grepl(dataname, 'moist', ignore.case=TRUE)) {
    moistDir <- file.path(datDir, 'soilmoisture')
    
    # units are soil moisture percent 'sm_pct' as 0-1, not 0-100
    proxylist <- read_soil_moisture(tempDir)
    
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
                             pixelsize = as.numeric(proxylist$pixarea))
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
  save(list = c(thistemp, thisIndex), file = file.path(scriptOut, paste0(thistemp, '.rdata')))

  
}