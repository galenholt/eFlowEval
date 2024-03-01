get_anae_chunk <- function(anae_path, catchment, thischunk, subchunkArgs, nchunks) {

  # Why don't I just use `cut` and be done with it? Maybe iteratively for subchunks?

  # need numeric for calcs.
  arraynum <- as.integer(thischunk)

  # Read in all ANAE ----------------------------------
  # The whole-basin version is ANAEbasinclim.rdata
  # the particular file we want here is passed as catchment
  thisAName <- paste0(catchment, 'ANAE')
  allANAES <- list.files(anae_path, pattern = paste0('^', catchment))
  ## Read in all ANAE polygons

  # Old way with save - load
  if (grepl('.rdata', allANAES)) {
    load(file.path(anae_path, allANAES))
    # datOut is location-aware, based on directorySet.R, so this should work
    # locally or on HPC

    #change its name to something generic so can be looped over This is annoying,
    #but I guess not too bad
    #TODO:: if we re-structure the project, should saveRDS() to create the
    #anaepolys so we can set the name on read-in
    anaePolys <- get(thisAName)
    rm(list = thisAName)
  } else if (grepl('.rds', allANAES)) {
    anaePolys <- readRDS(file.path(anae_path, allANAES))
  }


  # Somehow some are invalid, despite the st_make_valid in processANAE
  anaePolys <- sf::st_make_valid(anaePolys)

  # Weird geometry types were causing problems for the grid, but don't seem to
  # be an issue here. If there are potential issues, throw a warning.
  if (any(!sf::st_is(anaePolys, c('POLYGON', 'MULTIPOLYGON')))) {
    origrows <- nrow(anaePolys)
    anaePolys <- anaePolys %>%
      dplyr::filter(!st_is_empty(.)) %>%
      sf::st_collection_extract('POLYGON')

    droppedrows <- origrows - nrow(anaePolys)
    rlang::inform(glue::glue("anaePolys have empty geometries or geometries other than polygons, which tends to cause
            problems with crops. These have been dropped, yielding {droppedrows} fewer rows"))
  }

  # Sort out the chunks -----------------------------------------------------

  # turn thischunk (the array on HPC) into a numeric
  arrayNum <- as.numeric(thischunk)

  # FIRST, get the correct outer chunk that we want to drill into
  # I *think* this wouldn't be TOO terrible to make recursive if needed, by
  # looping over args[10]: length(args) and grabbing the desired bit each time
  # as long as we weren't also chaning nchunks. Although that wouldn't be too
  # bad either, really, just would need more args
  # Get the row indices from the array number

  if (length(subchunkArgs) == 0 | is.null(subchunkArgs)) {onelayer <- TRUE}
  if (length(subchunkArgs) > 0) {
    outerchunks <- as.integer(subchunkArgs)
    onelayer <- FALSE
  }

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

  return(anaePolys)
}
