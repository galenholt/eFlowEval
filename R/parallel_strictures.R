#' Light parallel wrapper over [process_data()]
#'
#' @inheritParams process_strictures
#'
#' @param runframe a tibble or dataframe with columns 'catchment' and 'chunk'. if nchunks == 1, the 'chunk' column still needs to be there, but there should only be one row per catchment and chunk should always be 1
#'
#' @return a tibble of timings. The main output is saving the data to out_dir.
#' @export
#'
parallel_strictures <- function(runframe,
                                out_dir,
                                strict_fun,
                                nchunks,
                                extraname = NULL,
                                whichcrs = 3577,
                                saveout = TRUE,
                                returnForR = FALSE) {


  # Nothing to aggregate, return a dummy.
  if (nrow(runframe) == 0) {
    return(tibble::tibble(catchment = NA, chunknumber = NA, strict_fun = NA, npolys = 0, elapsed = NA))
  }

  if (rlang::is_function(strict_fun)) {
    funname <- deparse(substitute(strict_fun))
  } else {
    funname <- strict_fun
  }

  # set up progress bar
  p <- progressor(steps = nrow(runframe))



  alltimes <- foreach(
    nl = 1:nrow(runframe),
    .combine = rbind,
    .errorhandling = 'remove', # ignores failed loops, which can be found post-hoc with chunks_to_process
    .options.future = list(
      seed = TRUE,
      globals = structure(TRUE, add = funname)
    )
  ) %dofuture% {

    # get the looplevels
    w <- runframe$catchment[nl]
    i <- runframe$chunk[nl]

    stricttab <- process_strictures(
      out_dir = out_dir,
      strict_fun = strict_fun,
      catchment = w,
      thischunk = i, # nearly always just 1
      nchunks = nchunks,
      whichcrs = whichcrs,
      saveout = saveout,
      returnForR = returnForR
    )

    p(glue::glue("catchment {w} chunk {i} finished"))

    stricttab # This just makes sure inuntab gets into the alltimes list
  }

  return(alltimes)
}
