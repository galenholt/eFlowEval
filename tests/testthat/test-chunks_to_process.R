test_that("output expected", {
  # Need a better test directory
  allchunks <- chunks_to_process(out_dir = file.path('..', 'SRA', 'datOut'),
                                 dataname = 'inundation',
                                 summaryFun = 'areaInun05cm',
                                 nchunks = 100,
                                 catchment = 'all',
                                 returnForR = TRUE,
                                 produce_sh = TRUE,
                                 filetype = '.rds')

  expect_true(inherits(allchunks, 'tbl_df'))
})
