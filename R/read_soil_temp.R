read_soil_temp <- function(soil_temp_dir) {
  ## Temp data
  # data location
  # Set up where the soil temp data is
  tempfile <- list.files(soil_temp_dir, pattern = '.nc')
  temppath <- file.path(soil_temp_dir, tempfile)

  # First, use a stars object to get the overall dimensions without reading data
  # use sub because the other variable is just QC values
  soilTstars <-  stars::read_stars(temppath, sub = "LST_Day_1km", proxy = TRUE)

  # As long as they're proxies, we can't change their crs, so have to shift
  # everything else to them.
  # this *usually* reads in as 4326, but sometimes doesn't. This is
  # dangerous, but I am telling NASA to give me this CRS, so make it so but warn
  sf::st_crs(soilTstars) <- 4326


  pixarea <- get_pixarea(temppath, sf::st_crs(soilTstars))

  return(list(proxy_data = soilTstars, pixarea = pixarea))
}
