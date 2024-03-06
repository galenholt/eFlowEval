#' Light parallel wrapper over [process_data()]
#'
#' @inheritParams process_data
#'
#' @param runframe a tibble or dataframe with columns 'catchment' and 'chunk'. if nchunks == 1, the 'chunk' column still needs to be there, but there should only be one row per catchment and chunk should always be 1
#'
#' @return a tibble of timings. The main output is saving the data to out_dir.
#' @export
#'
parallel_data <- function(runframe,
                          dataname,
                          data_dir,
                          poly_path = file.path(out_dir, "ANAEprocessed"),
                          summaryFun,
                          out_dir,
                          nchunks = 100,
                          whichcrs = 3577,
                          maxPix = 100000,
                          rastRollArgs = NULL,
                          saveout = TRUE) {


  # set up progress bar
  p <- progressor(steps = nrow(runframe))


  alltimes <- foreach(
    nl = 1:nrow(runframe),
    .combine = rbind,
    .errorhandling = 'remove', # ignores failed loops, which can be found post-hoc with chunks_to_process
    .options.future = list(
      seed = TRUE,
      globals = structure(TRUE, add = summaryFun)
    )
  ) %dofuture% {

    # get the looplevels
    w <- runframe$catchment[nl]
    i <- runframe$chunk[nl]

    inuntab <- process_data(
      dataname = dataname,
      data_dir = data_dir,
      poly_path = poly_path,
      summaryFun = summaryFun,
      out_dir = out_dir,
      catchment = w,
      thischunk = i,
      nchunks = nchunks,
      whichcrs = whichcrs,
      maxPix = maxPix,
      rastRollArgs = rastRollArgs,
    )

    p(glue::glue("catchment {w} chunk {i} finished"))

    inuntab # This just makes sure inuntab gets into the alltimes list
  }

  return(alltimes)
}
