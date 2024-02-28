#' Process the ANAE files including intersecting
#'
#' @param datIn path to directory with the ANAE
#' @param outDir path to output. Typically 'datOut/ANAEprocessed' or similar
#' @param outname name of this particular output, e.g. 'ANAEbasinclim'
#' @param intersections character vector or list of characters and sf objects to intersect with the anaes
#' @param commonCRS default 3577
#' @param chunksave If want to save chunks of the ANAEs, this is the column name to chunk into, e.g. 'ValleyName'. If NULL, only the full ANAE set is saved.
#'
#' @return
#' @export
#'
#' @examples
process_anae <- function(datIn, outDir,
                        outname = 'ANAE',
                        intersections = c('climate', 'valleys'),
                        commonCRS = 3577,
                        chunksave = 'ValleyName') {

  # The sf::st_cast and sf::st_make_valid clean things up so the intersects work.
  # https://www.r-spatial.org/r/2017/03/19/invalid.html says we should make valid,
  # and then cast, but that didn't work for me
  # Takes a while. Didn't time, but > 10 mins
  wetlands <- read_sf(dsn = file.path(datIn,
                                      'ANAE/ANAE_Wetlands_v3_24mar2021/Wetlands_ANAE_v3_24mar2021/Wetlands_ANAE_v3_24mar2021.shp')) %>%
    sf::st_cast("MULTIPOLYGON") %>% # cleans up an issue with multisurfaces
    sf::st_transform(anaePolys, commonCRS) %>%
    sf::st_make_valid()

  # Suppose I'll keep SYSID for now, but really we want to shift over to the
  # geohash UID (see Shane's discussion in the metadata)
  # 'WaterRegim' seems to be a typo- was water regime last time
  # There are others here we might want to bring along, but for now, not
  wetlands <- wetlands |>
    dplyr::select(UID, SYSID, ANAE_DESC, ANAE_CODE, WaterType, WaterRegim, SystemType, Confidence)

  # This is a bit fragile, but lets us pass sfs or characters. Some judicious use of `get` would be better.
  if (is.list(intersections)) {
    char_intersections <- intersections[purrr::map_lgl(intersections, \(x) is.character(x))]
    sf_intersections <- intersections[purrr::map_lgl(intersections, \(x) inherits(x, 'sf'))]
  } else {
    char_intersections <- intersections
  }

  # TODO: make `intersections` able to take sf objects instead of just names.
  # I think it might just work if it comes in as a list. ie a character vector or a list with the characters in the slots, provided each has their own list item
  if ('climate' %in% char_intersections) {

    # Get the climate layer
    # The rest of this still needs to come from the V2 for now, since V3 is just the shapefiles
    # Get koppen climate region as a test of the joining of data
    kopSub <- read_sf(dsn = file.path(datIn, 'ANAE/MDB_ANAE_Aug2017/MDB_ANAE.gdb'), layer = 'BoM_Koppen_subregions') %>%
      sf::st_cast("MULTIPOLYGON") %>% # cleans up an issue with multisurfaces
      sf::st_transform(commonCRS) %>%
      sf::st_make_valid() |>
      dplyr::select(ANAEField, CODE, Zone)

    # intersect
    wetlands <- wetlands |>
      sf::st_intersection(kopSub)
  }

  if ('valleys' %in% char_intersections | 'ltimNoNorth' %in% char_intersections) {
    # no need to do anything here anymore, they're already prepared as a data object `ltimNoNorth`
    wetlands <- wetlands |>
      sf::st_intersection(ltimNoNorth)
  }

  # Do all the sfs in one go.
  for (i in sf_intersections) {
    wetlands <- wetlands |>
      sf::st_intersection(i)
  }

  # add a unique identifier
  wetlands <- wetlands |>
    dplyr::mutate(UID = lwgeom::sf::st_geohash(geometry, precision = 11)) # Re-set the UID. This is the way to keep every polygon unique, and still retains the original SYSIDs from before.
  # 11 prevents all but one duplicate. that one is basically a line, and precision can go out to hundreds and not separate it
  # There is one remaining duplicate that never goes away, no matter the
  # resolution of the geohash. Brute-foce fix
  while (any(duplicated(bothANAE$SYS2))) {
    bothANAE$SYS2[which(duplicated(bothANAE$SYS2))] <- paste0(bothANAE$SYS2[which(duplicated(bothANAE$SYS2))], '_DUP')
  }

  # Save the full data
  # Make the out directory, in case it doesn't exist
  if (!dir.exists(outDir)) {dir.create(outDir, recursive = TRUE)}

  saveRDS(wetlands, file = file.path(outDir, paste0(outname, '.rds')))

  # save the data in chunks
  if (!is.null(chunksave)) {
    uniquechunks <- unique(sf::st_drop_geometry(wetlands)[,chunksave])
    for (b in 1:length(uniquechunks)) {
      thischunk <- dplyr::filter(wetlands, .data[[chunksave]] == b)

      thisname <- stringr::str_remove_all(uniquechunks[b], ' ')
      thisname <- paste0(thisname, '_ANAE')

      saveRDS(thischunk, file = filepath(outDir, paste0(thisname, '.rds')))
    }

  }
}
