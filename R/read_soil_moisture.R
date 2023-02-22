read_soil_moisture <- function(soil_moisture_dir) {
  
  # Paths to files
  allmoistyears <- list.files(soil_moisture_dir, pattern = 'day.nc')
  moistpaths <- file.path(soil_moisture_dir, allmoistyears)
  
  # Read_stars brings it in as a stars_proxy
  soilMstars <-  read_stars(moistpaths, proxy = TRUE)
  
  # That comes in with no CRS, but the metadata from BOM says it's "GDA94
  # (equivalent to WGS84 for all practical purposes)"
  
  # I'm going to set the crs to wgs 84 instead of GDA94 because when I set it to
  # GDA 94 it is interpreting the units as meters and not degrees. So pixels
  # that should be 0.05 degrees on a side (approx 5km x 5km) come out as 0.05m x
  # 0.05m. So that crs is clearly not working right, and won't put it in the
  # right location- we'll end up with a miniaturized area
  st_crs(soilMstars) <- 4326
  
  # fix the name to be sm_pct. THESE ARE 0-1, NOT 0-100.
  names(soilMstars) <- 'sm_pct'
  
  # the units always throw a warning because {units} doesn't recognize 'fraction
  # of fullness'. Don't think we can easily avoid that- it's an internal stars
  # check that happens when it reads to disk
  
  pixarea <- get_pixarea(moistpaths[1], st_crs(soilMstars))
  
  return(list(proxy_data = soilMstars, pixarea = pixarea))
  
}