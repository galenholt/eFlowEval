#' Identify needed analyes, run, concatenate, and check
#'
#' wrapper over [chunks_to_process()], [parallel_data()], [concat_chunks()], and
#' [test_anae_agg()], since they are often run together and take the same
#' arguments.
#'
#' @inheritParams parallel_data
#' @inheritParams chunks_to_process
#'
#' @param dataname as in [parallel_data()], but if `''` or missing, assumes strictures, since they have no dataname argument.
#' @param process_function the name of the function used to process, same as `summaryFun` for data and `strict_fun` for strictures
#'
#' @return if failures, a tibble, else invisible
#' @export
#'
process_with_checks <- function(dataname,
                                data_dir,
                                poly_path = file.path(out_dir, 'ANAEprocessed'),
                                process_function,
                                out_dir,
                                catchment = 'all',
                                nchunks = NULL,
                                poly_per_chunk = NULL,
                                whichcrs = 3577,
                                maxPix = 100000,
                                rastRollArgs = NULL,
                                extraname = NULL) {


  if (missing(dataname)) {
    rlang::inform('`dataname` is missing, assuming strictures')
    dataname <- ''
  }

  if (rlang::is_function(process_function)) {
    funname <- deparse(substitute(process_function))
  } else {
    funname <- process_function
  }

  # Get the runs
  runlist <- chunks_to_process(out_dir = out_dir,
                               dataname = dataname,
                               summaryFun = funname,
                               nchunks = nchunks,
                               poly_per_chunk = poly_per_chunk,
                               catchment = catchment,
                               extraname = extraname,
                               returnForR = TRUE,
                               filetype = '.rds')

  # Process data if there is a meaningful dataname argument
  if (dataname != '') {
    # Let's leave some checkpoints around
    rlang::inform(c("i" = glue::glue("\n\nProcessing {dataname} with {funname} into {poly_path} starting at {format(Sys.time(), '%a %b %d %X %Y')}\n\n")),
                  .file = stdout())

    # do the runs
    alltimes <- parallel_data(runframe = runlist,
                              dataname = dataname,
                              data_dir = data_dir,
                              poly_path = poly_path,
                              summaryFun = process_function,
                              out_dir = out_dir,
                              whichcrs = whichcrs,
                              maxPix = maxPix,
                              rastRollArgs = rastRollArgs,
                              extraname = extraname,
                              saveout = TRUE)
    #
    rlang::inform(c("i" = glue::glue("\n\nProcessing {dataname} with {funname} into {poly_path} finished at {format(Sys.time(), '%a %b %d %X %Y')}\n\n")),
                  .file = stdout())
  }

  # Process strictures if there's not a meaningful dataname argument
  if (dataname == '') {
    # Let's leave some checkpoints around
    rlang::inform(c("i" = glue::glue("\n\nProcessing {funname} starting at {format(Sys.time(), '%a %b %d %X %Y')}\n\n")),
                  .file = stdout())

    # do the runs
    alltimes <- parallel_strictures(runframe = runlist,
                              out_dir = out_dir,
                              strict_fun = process_function,
                              whichcrs = whichcrs,
                              extraname = extraname,
                              saveout = TRUE)
    #
    rlang::inform(c("i" = glue::glue("\n\nProcessing {funname} finished at {format(Sys.time(), '%a %b %d %X %Y')}\n\n")),
                  .file = stdout())
  }




  runlist_end <- chunks_to_process(out_dir = out_dir,
                                   dataname = dataname,
                                   summaryFun = funname,
                                   nchunks = nchunks,
                                   poly_per_chunk = poly_per_chunk,
                                   catchment = catchment,
                                   extraname = extraname,
                                   returnForR = TRUE,
                                   filetype = '.rds')

  # Do the check, but not on strictures (no dataname), since their format is variable.
  if (nrow(runlist_end) > 0) {
    rlang::abort(c("\n\nNot all runs completed, did not concatenate",
                   "Failures can be found with `chunks_to_process`.\n\n"))
  }

  if (nrow(runlist_end) == 0 & dataname != '') {
    rlang::inform(c("i" = glue::glue("\n\nAll runs completed, concatenating\n\n")),
                  .file = stdout())

    cattime <- concat_chunks(out_dir = out_dir,
                                         dataname = dataname,
                                         summaryFun = process_function,
                                         catchment = catchment,
                             poly_path = poly_path,
                             extraname = extraname)
    rlang::inform(c("i" = glue::glue("\n\nConcatenation completed, error checking\n\n")),
                  .file = stdout())

    check_aggs <- test_anae_agg(out_dir = out_dir,
                                dataname = dataname,
                                summaryFun = process_function,
                                catchment = catchment,
                                poly_path = poly_path,
                                extraname = extraname,
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

  } else {
    # Don't check strictures, just return null
    findfails <- NULL
  }



  return(list(timings = alltimes, failures = findfails))

}
