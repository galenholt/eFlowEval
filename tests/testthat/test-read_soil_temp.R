test_that("read_soil_temp returns what it should", {
  data_list <- read_soil_temp(file.path(datDir, 'soilTemp14_20'))
  data_dims <- st_dimensions(data_list$proxy_data)
  
  # Not sure why the expect_s3_class failed
  testthat::expect_true(inherits(data_list$proxy_data, 'stars_proxy'))
  testthat::expect_equal(st_crs(data_list$proxy_data)$epsg, 4326)
  testthat::expect_equal(names(data_list$proxy_data), 'LST_Day_1km')
  
  testthat::expect_equal(data_dims$x$from, 1)
  testthat::expect_equal(data_dims$x$to, 1933)
  
  testthat::expect_equal(data_dims$y$from, 1)
  testthat::expect_equal(data_dims$y$to, 1865)
  
  testthat::expect_equal(data_dims$time$from, 1)
  testthat::expect_equal(data_dims$time$to, 2557)
  
  # I *should* be using expect_snapshot_value(data_dims), but that only works when I can automate testing in a package
  
})
