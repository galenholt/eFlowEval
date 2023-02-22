read_inundation <- function(inun_dir) {
  # Get the file names
  alltifs <- list.files(inun_dir, pattern = '.tif$')
  inunTifs <- file.path(inun_dir, alltifs)
  
  # get the crs from the first one (will read them all in later, but first need
  # to deal with the corrupt file)
  # As long as they're proxies, we can't change their crs, so have to shift
  # everything else to them
  starCRS <- st_crs(read_stars(inunTifs[1]))
  
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
  
  pixarea <- get_pixarea(inunTifs[1], st_crs(tifTimes))
  
  return(list(proxy_data = tifTimes, pixarea = pixarea))
}