

#' General function to process rasters into polygons
#'
#' @param dataname name of the data to process (and name of output directory for the data type). Current options 'inundation', 'soil_temp', 'moisture'
#' @param data_dir directory with raw data
#' @param poly_path path to the polygons to process into
#' @param summaryFun character name of the summary function
#' @param out_dir output directory
#' @param catchment catchment to chunk into
#' @param thischunk helps with chunking for parallel
#' @param subchunkArgs more chunking help, default NULL
#' @param nchunks default 100, how many chunks to use. If 1, the chunked system cancels out and the data is just saved to out_dir/dataname/summaryFun/Catchment_summaryFun.rds.
#' @param whichcrs desired crs
#' @param maxPix maximum number of pixels before individual polygons are broken up in [rastPolyJoin()]
#' @param rastRollArgs arguments to [rastPolyJoin()] to roll the raster before aggregating
#' @param saveout default TRUE, save data out.
#'
#' @return a tibble of the time taken, main output is the saved data
#' @export
#'
#' @examples
process_data <- function(dataname,
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

  ## NAMING AND DIR MGMT

  # Make a sub-directory for the subchunk
  if (length(subchunkArgs) == 0 | is.null(subchunkArgs)) {chunkpath <- catchment}
  if (length(subchunkArgs) > 0) {
    chunkpath <- stringr::str_flatten(c(catchment, subchunkArgs),
                                         collapse = '/sub_')
    }

  # If not chunking, don't need the inner chunked dir.
  if (nchunks == 1) {
    scriptOut <- file.path(out_dir, paste0(dataname), summaryFun)
  } else if (nchunks > 1) {
    scriptOut <- file.path(out_dir, paste0(dataname), summaryFun, 'chunked',
                           chunkpath)
  }

  # Make the out directory, in case it doesn't exist
  if (!dir.exists(scriptOut)) {dir.create(scriptOut, recursive = TRUE)}

  # These subchunk indices feel backwards (the outer chunk is on the right,
  # instead of building L -> R). But other things depend on that so don't fix
  # unless planning a big restructure
  # str_replace is because when there's no subchunk we get two _ next to each
  # other.
  unique_chunkname <- paste0(catchment, '_', summaryFun, '_',
                             stringr::str_flatten(subchunkArgs, collapse = '_'),
                             '_', thischunk) |>
    stringr::str_replace_all('__', '_')

  unique_indexname <- paste0(catchment, '_', summaryFun, '_index', '_',
                             stringr::str_flatten(subchunkArgs, collapse = '_'),
                             '_', thischunk) |>
    stringr::str_replace_all('__', '_')

  # Got to be a cleaner way to do this.
  # assign(unique_chunkname, tempAns)
  # assign(unique_indexname, tempIndex)

  # If 1, we don't append the chunkname and it goes in a different directory.
  if (nchunks == 1) {
    unique_chunkname <- stringr::str_remove(unique_chunkname, '_1$')
    unique_indexname <- stringr::str_remove(unique_indexname, '_1$')
  }


  # Get the needed chunk of anaes
  anaePolys <- get_anae_chunk(anae_path = poly_path,
                              catchment = catchment,
                              thischunk = thischunk,
                              subchunkArgs = subchunkArgs,
                              nchunks = nchunks)

  npolys <- nrow(anaePolys)
  # This gets sunk to stderr when non-interactive, which is probably good.
  rlang::inform(glue::glue('In {catchment} chunk {thischunk}, number of polygons processing is {npolys}'))

  # Nothing to do if there are no polys in this chunk, so make the outlist return.
  if (nrow(anaePolys) == 0) {
    outlist <- list(tempAns = NULL,
                    tempIndex <- NULL)
    sumtab <- tibble::tibble(catchment,
                             chunknumber = as.numeric(thischunk),
                             summaryFun, npolys,
                             pixarea = NA,
                             elapsed = Sys.time() - start_time)
    # still save the list, since later functions count expected files. I think.
    if (saveout) {
      saveRDS(outlist, file = file.path(scriptOut, paste0(unique_chunkname, '.rds')))
      # save(list = c(unique_chunkname, unique_indexname), file = file.path(scriptOut, paste0(unique_chunkname, '.rdata')))
    }
    return(sumtab)
  }


  # Get the stars proxy
  if (grepl(dataname,'inundation', ignore.case=TRUE)) {
    # For now hardcode inun_dir
    # THe old one
    # inunDir <- file.path(data_dir, 'Inundation', 'WaterDepth_TwoMonthly', 'geotiff')
    # The new data
    inunDir <- data_dir # file.path(data_dir, 'Inundation_new', 'data', 'MDB_Water_Depth_v3.6_4326')
    proxylist <- read_inundation(inun_dir = inunDir)
    # inundation has NA as zeros, so set na.replace
    # rlang::inform("In inundation dataset NAs mean 0. Setting to 0")
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
  anaePolys <- sf::st_transform(anaePolys, sf::st_crs(proxylist$proxy_data))

  # Do the aggregation ------------------------------------------------------
  # If we did this over all of ANAEbasinclim it'd be cleaner, but all the
  # catchments would be mixed up I could do something like matching UIDS to
  # ValleyName, and then splitting the output up into separate folders For now,
  # I'm actually going to change some things to just run this within-catchment
  # though ultimately, this should use a future.batchapply to spawn chunks
  # within catchments (and skip the whole outer SLURM/foreach entirely)

  # parallel loop over the anae polygons
  dpList <- foreach::foreach(s = 1:nrow(anaePolys),
                    .options.future = list(seed = TRUE,
                                           globals = structure(TRUE, add = summaryFun))) %dofuture% {
    # moved the cropping all the way in to rpintersect
    oneanae <- rastPolyJoin(polysf = anaePolys[s,],
                             rastst = proxylist$proxy_data,
                             FUN = summaryFun,
                             grouper = 'UID', maintainPolys = TRUE,
                             na.replace = na.replace,
                             whichcrs = whichcrs,
                             maxPixels = maxPix,
                             pixelsize = as.numeric(proxylist$pixarea),
                             rastRollArgs = rastRollArgs)
  } # end foreach



    # Then, unpack the lists also using foreach
      # does making these %do% actually
      # speed things up overall by giving more resources to the dopars?
      # Does using dopar potentially shuffle these relative to each other?
    outlist <- concat_star_index(dpList, dimension = 1, starname = unique_chunkname,
                                 indexname = unique_indexname)

    # undo any shuffling that's happened
    outlist <- matchStarsIndex(index1 = anaePolys, stars1 = NULL,
                               index2 = outlist[[unique_indexname]],
                               stars2 = outlist[[unique_chunkname]],
                               indexcol = c(1, 1), testfinal = FALSE)

  # name the list appropriately (again, matchstars changes the names)
  names(outlist) <- c(unique_chunkname, unique_indexname)


  # probably a cleaner way to do this, likely with saveRDS to allow cleaner
  # read-in.
  # Could just use catchment for the rdata, since there's a folder structure,
  # but this is more explicit
  # saveout is a way to bypass saving while testing
  if (saveout) {
    saveRDS(outlist, file = file.path(scriptOut, paste0(unique_chunkname, '.rds')))
    # save(list = c(unique_chunkname, unique_indexname), file = file.path(scriptOut, paste0(unique_chunkname, '.rdata')))
  }

  end_time <- Sys.time()
  elapsed <- end_time-start_time

  sumtab <- tibble::tibble(catchment, chunknumber = as.numeric(thischunk), summaryFun, npolys, pixarea = proxylist$pixarea, elapsed)

  return(sumtab)
}
