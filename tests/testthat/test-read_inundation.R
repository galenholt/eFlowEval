test_that("read_inundation returns what it should", {
  data_list <- read_inundation(file.path(data_dir, 'Inundation', 'WaterDepth_TwoMonthly', 'geotiff'))
  data_dims <- st_dimensions(data_list$proxy_data)
  
  # Not sure why the expect_s3_class failed
  testthat::expect_true(inherits(data_list$proxy_data, 'stars_proxy'))
  testthat::expect_equal(st_crs(data_list$proxy_data)$epsg, 4326)
  testthat::expect_equal(names(data_list$proxy_data), 'depth')
  
  testthat::expect_equal(data_dims$x$from, 1)
  testthat::expect_equal(data_dims$x$to, 51668)
  
  testthat::expect_equal(data_dims$y$from, 1)
  testthat::expect_equal(data_dims$y$to, 48592)
  
  testthat::expect_equal(data_dims$time$from, 1)
  testthat::expect_equal(data_dims$time$to, 197)

  # I *should* be using expect_snapshot_value(data_dims), but that only works when I can automate testing in a package
  
})
