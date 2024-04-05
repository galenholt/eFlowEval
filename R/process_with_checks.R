#' identify needed analyes, run, concatenate, and check
#'
#' wrapper over [chunks_to_process()], [parallel_data()], [concat_chunks()], and
#' [test_anae_agg()], since they are often run together and take the same
#' arguments.
#'
#' @inheritParams parallel_data
#'
#' @return if failures, a tibble, else invisible
#' @export
#'
process_with_checks <- function(dataname,
                                data_dir,
                                poly_path = file.path(out_dir, 'ANAEprocessed'),
                                summaryFun,
                                out_dir,
                                catchment = 'all',
                                nchunks = 100,
                                whichcrs = 3577,
                                maxPix = 100000,
                                rastRollArgs = NULL,
                                extraname = NULL) {


  # Get the runs
  runlist <- chunks_to_process(out_dir = out_dir,
                               dataname = dataname,
                               summaryFun = summaryFun,
                               nchunks = nchunks,
                               catchment = catchment,
                               extraname = extraname,
                               returnForR = TRUE,
                               filetype = '.rds')

  # Let's leave some checkpoints around
  rlang::inform(c("i" = glue::glue("\n\nProcessing {dataname} with {summaryFun} into {poly_path} starting at {format(Sys.time(), '%a %b %d %X %Y')}\n\n")),
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
                            extraname = extraname,
                            saveout = TRUE)
  #
  rlang::inform(c("i" = glue::glue("\n\nProcessing {dataname} with {summaryFun} into {poly_path} finished at {format(Sys.time(), '%a %b %d %X %Y')}\n\n")),
                .file = stdout())

  runlist_end <- chunks_to_process(out_dir = out_dir,
                                   dataname = dataname,
                                   summaryFun = summaryFun,
                                   catchment = catchment,
                                   extraname = extraname,
                                   nchunks = nchunks,
                                   returnForR = TRUE,
                                   filetype = '.rds')

  if (nrow(runlist_end) == 0) {
    rlang::inform(c("i" = glue::glue("\n\nAll runs completed, concatenating\n\n")),
                  .file = stdout())

    cattime <- concat_chunks(out_dir = out_dir,
                                         dataname = dataname,
                                         summaryFun = summaryFun,
                                         catchment = catchment,
                             poly_path = poly_path,
                             extraname = extraname)
  } else {
    rlang::abort(c("\n\nNot all runs completed, did not concatenate",
                   "Failures can be found with `chunks_to_process`.\n\n"))
  }

  rlang::inform(c("i" = glue::glue("\n\nConcatenation completed, error checking\n\n")),
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
    rlang::inform(c("i" = glue::glue("\n\nPolygon structure tests passed.\n\n")),
                  .file = stdout())
  } else {
    rlang::warn(glue::glue("\n\nPolygon structure tests failed, returning failing results\n\n"),
                  .file = stdout())
  }

  return(list(timings = alltimes, failures = findfails))

}
