#' Process a stricture function. Similar to process_data
#'
#' @inheritParams process_data
#'
#' @param out_dir outer output directory
#' @param strict_fun the function that calculates the strictures. Can be character or bare name, though character works best to avoid losing the name in the stack. Must have arguments `out_dir`, `catchment`, `thischunk`, `nchunks`, `whichcrs` and return a list. Often thischunk and nchunks aren't used, but should be included for generality. *Note* this uses `rlang::exec`, and so if we need more flexibility we could take an arglist here in future.
#' @param returnForR should the output be returned to the active session? default FALSE returns the timings, as in process_data
#'
#' @return
#' @export
#'
process_strictures <- function(out_dir,
                               strict_fun,
                               catchment,
                               thischunk,
                               nchunks,
                               extraname = NULL,
                               whichcrs = 3577,
                               saveout = TRUE,
                               returnForR = FALSE) {

  start_time <- Sys.time()

  ## NAMING AND DIR MGMT

  dirinfo <- make_chunk_dirs(out_dir = out_dir,
                             catchment = catchment,
                             process_function = strict_fun,
                             dataname = '',
                             nchunks = nchunks,
                             extraname = extraname)

  scriptOut <- dirinfo$scriptOut
  unique_chunkname <- dirinfo$unique_chunkname
  unique_indexname <- dirinfo$unique_indexname

  # This means we do have some more flexibility in the arguments- we can pass in a list. For now, keep standard.
  arglist <- list(out_dir = out_dir,
                  catchment = catchment,
                  thischunk = thischunk,
                  nchunks = nchunks,
                  whichcrs = whichcrs)

  strict_responses <- rlang::exec(strict_fun, !!!arglist)

  if (saveout) {
    saveRDS(strict_responses, file = file.path(scriptOut, paste0(unique_chunkname, '.rds')))

    # save each list-item separately. Just a different way to skin the cat, not sure which I'll prefer.
    for (i in names(strict_responses)) {
      if (!dir.exists(file.path(scriptOut, i))) {dir.create(file.path(scriptOut, i), recursive = TRUE)}
      saveRDS(strict_responses[[i]], file = file.path(scriptOut, i, paste0(catchment, '_', i, '.rds')))
    }
  }


  # Either return the list or a tibble of timings
  if (returnForR) {
    return(strict_responses)
  } else {
    end_time <- Sys.time()
    elapsed <- end_time-start_time

    sumtab <- tibble::tibble(catchment,
                             elapsed)
    return(sumtab)
  }

}
