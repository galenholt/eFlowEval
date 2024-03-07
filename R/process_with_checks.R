process_with_checks <- function(dataname,
                                data_dir,
                                poly_path = file.path(out_dir, 'ANAEprocessed'),
                                summaryFun,
                                out_dir,
                                catchment = 'all',
                                nchunks = 100,
                                whichcrs = 3577,
                                maxPix = 100000,
                                rastRollArgs = NULL) {


  # Get the runs
  runlist <- chunks_to_process(out_dir = out_dir,
                               dataname = dataname,
                               summaryFun = summaryFun,
                               nchunks = nchunks,
                               catchment = catchment,
                               returnForR = TRUE,
                               filetype = '.rds')

  # Let's leave some checkpoints around
  rlang::inform(glue::glue("Processing {dataname} with {summaryFun} into {poly_path} starting at {Sys.time()}"),
                .file = stdout())

  # do the runs
  alltimes <- parallel_data(runframe = runlist,
                            dataname = dataname,
                            data_dir = data_dir,
                            poly_path = poly_path,
                            summaryFun = summaryFun,
                            out_dir = out_dir,
                            nchunks = nchunks,
                            whichcrs = whichcrs,
                            maxPix = maxPix,
                            rastRollArgs = rastRollArgs,
                            saveout = TRUE)
  #
  rlang::inform(glue::glue("Processing {dataname} with {summaryFun} into {poly_path} finished at {Sys.time()}"),
                .file = stdout())

  runlist_end <- chunks_to_process(out_dir = out_dir,
                                   dataname = dataname,
                                   summaryFun = summaryFun,
                                   catchment = catchment,
                                   nchunks = nchunks,
                                   returnForR = TRUE,
                                   filetype = '.rds')

  if (nrow(runlist_end) == 0) {
    rlang::inform(glue::glue("All runs completed, concatenating"),
                  .file = stdout())

    cattime <- concat_chunks(out_dir = out_dir,
                                         dataname = dataname,
                                         summaryFun = summaryFun,
                                         catchment = catchment)
  } else {
    rlang::abort(c("Not all runs completed, did not concatenate",
                   "Failures can be found with `chunks_to_process`."))
  }

  rlang::inform(glue::glue("Concatenation completed, error checking"),
                .file = stdout())

  check_aggs <- test_anae_agg(out_dir = out_dir,
                              dataname = dataname,
                              summaryFun = summaryFun,
                              catchment = catchment,
                              poly_path = poly_path,
                              filetype = '.rds')

  findfails <- check_aggs |>
    dplyr::filter(passfail != 'pass' &
                    !grepl('Assigning based on position', passfail))

  if (nrow(findfails) == 0) {
    rlang::inform(glue::glue("Polygon structure tests passed."),
                  .file = stdout())
    return(invisible())
  } else {
    rlang::warn(glue::glue("Polygon structure tests failed, returning failing results"),
                  .file = stdout())

    return(findfails)
  }



}
