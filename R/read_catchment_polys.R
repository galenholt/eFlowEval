#' Helper to read in groups of polygons (typically ANAEs broken into catchments)
#'
#' @inheritParams process_data
#'
#' @return valid sf of the polys for the requested group `catchment`
#' @export
#'
read_catchment_polys <- function(poly_path, catchment) {

  allANAES <- list.files(poly_path, pattern = paste0('^', catchment))
  ## Read in all ANAE polygons

  # informative error if we try to grab something that doesn't exist.
  if (length(allANAES) == 0) {
    rlang::abort(c("No ANAE file for",
                   glue::glue("Catchment {catchment} in {poly_path}")))
  }

  # Old way with save - load
  if (grepl('.rdata', allANAES)) {
    load(file.path(poly_path, allANAES))
    # the particular file we want here is passed as catchment
    thisAName <- paste0(catchment, 'ANAE')
    #change its name to something generic so can be looped over This is annoying,
    #but I guess not too bad
    #TODO:: if we re-structure the project, should saveRDS() to create the
    #anaepolys so we can set the name on read-in
    anaePolys <- get(thisAName)
    rm(list = thisAName)
  } else if (grepl('.rds', allANAES)) {
    anaePolys <- readRDS(file.path(poly_path, allANAES))
  }

  return(anaePolys)
}
