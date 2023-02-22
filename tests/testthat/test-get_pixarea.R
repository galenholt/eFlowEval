test_that("pixarea works for inundation", {
  source('directorySet.R')
  inunDir <- file.path(datDir, 'Inundation', 'WaterDepth_TwoMonthly', 'geotiff')
  # Get the file names
  alltifs <- list.files(inunDir, pattern = '.tif$')
  inunTifs <- file.path(inunDir, alltifs)
  starCRS <- sf::st_crs(stars::read_stars(inunTifs[1]))
  
  inunpix <- get_pixarea(inunTifs[1], crs = starCRS)
  testthat::expect_equal(round(inunpix, 4), units::set_units(818.5237, m^2))
})

test_that("pixarea works for soil temps", {
  source('directorySet.R')
  soil_temp_dir <- file.path(datDir, 'soilTemp14_20')
  # Get the file names
  tempfile <- list.files(soil_temp_dir, pattern = '.nc')
  temppath <- file.path(datDir, 'soilTemp14_20', tempfile)
  starCRS <- 4326
  
  temppix <- get_pixarea(temppath, crs = starCRS, sub = 1)
  testthat::expect_equal(round(temppix, 1), units::set_units(787718.7, m^2))
})

test_that("pixarea works for soil moisture", {
  source('directorySet.R')
  soil_moisture_dir <- file.path(datDir, 'soilmoisture')
  # Paths to files
  allmoistyears <- list.files(soil_moisture_dir, pattern = 'day.nc')
  moistpaths <- file.path(soil_moisture_dir, allmoistyears)
  starCRS <- 4326
  
  moistpix <- get_pixarea(moistpaths[1], crs = starCRS)
  testthat::expect_equal(moistpix, units::set_units(30419863, m^2))
})