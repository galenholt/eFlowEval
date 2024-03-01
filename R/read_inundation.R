read_inundation <- function(inun_dir) {
  # Get the file names
  alltifs <- list.files(inun_dir, pattern = '.tif$')
  inunTifs <- file.path(inun_dir, alltifs)

  # get the crs from the first one
  # As long as they're proxies, we can't change their crs, so have to shift
  # everything else to them
  starCRS <- sf::st_crs(stars::read_stars(inunTifs[1]))

  # Get the dates from the tif names so we can set dimensions
  # filenames differ between versions
  if (grepl('v3', inunTifs[1])) {
    tifdates <- inunTifs |>
      stringr::str_extract_all("[1-2][0-9][0-9][0-9]_[0-9][0-9]")
  } else if (grepl('WOFS', inunTifs[1])) {
    # Old version
    tifdates <- inunTifs |> # Set of filenames
      stringr::str_extract_all("[1-2][0-9][0-9][0-9]_[0-9][0-9]_WaterDepth.tif") |>
      # now delete the safety
      stringr::str_remove("_WaterDepth.tif")
  }


  # clean up the dates
  tifdates <- tifdates |>
    # add the first of the month on there
    stringr::str_c('_01') |>
    # turn into dates
    lubridate::ymd() |>
    as.POSIXct() # soilMstars uses posix instead of the Date class that lubridate returns, so be consistent

  # Make the proxy
  tifTimes <- inunTifs |> # filenames
    stars::read_stars() |> # read in
    merge() |> # make a brick
    setNames('depth') |> # name the attribute
    # Make dates
    # soilMstars uses an offset and delta, while this has 'values'.
    # I seem to remember values might cause an issue somewhere down the track, so
    # might need to revisit
    stars::st_set_dimensions(3, values = tifdates) |>
    stars::st_set_dimensions(names = c("x", "y", "time"))

  # Units can't actually be set on a proxy, so we just have to be careful I'm
  # going to use m consistently, since the volume is easier to calculate that
  # way. There's potentially some memory savings from using int mm, but the data
  # reads in as double anyway.
  if (grepl('v3', inunTifs[1])) {
    # New version in mm
    # tifTimes[[1]] <- units::set_units(tifTimes[[1]], "mm")
    tifTimes <- tifTimes / 1000
  } else if (grepl('WOFS', inunTifs[1])) {
    # Old version in m
    # tifTimes[[1]] <- units::set_units(tifTimes[[1]], "m")
  }

  pixarea <- get_pixarea(inunTifs[1], sf::st_crs(tifTimes))

  return(list(proxy_data = tifTimes, pixarea = pixarea))
}
