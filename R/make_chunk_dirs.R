#' Set up directories for chunked (or not) data and stricture processing
#'
#' @param catchment catchment/larger group name
#' @param process_function the name of the function used to process, same as `summaryFun` for data or `strict_fun` for strictures. Works best if character so name does not get lost in the call stack
#' @param dataname input data name. Should be `''` if strictures
#' @param nchunks nchunks
#' @param subchunkArgs usually ignored, not maintained much
#' @param extraname as in other functions
#'
#' @return a list with scriptOut, unique_chunkname and unique_indexname
#' @export
#'
make_chunk_dirs <- function(out_dir,
                            catchment,
                            process_function,
                            dataname,
                            nchunks,
                            subchunkArgs = NULL,
                            extraname = NULL) {

  if (rlang::is_function(process_function)) {
    funname <- deparse(substitute(process_function))
  } else {
    funname <- process_function
  }

  if (nchunks == 1) {
    scriptOut <- file.path(out_dir, funname)
    # chunk names, don't support subchunks
    unique_chunkname <- paste0(catchment, '_', funname)
    unique_indexname <- paste0(catchment, '_', funname, '_index')
  } else if (nchunks > 1) {

    # Make a sub-directory for the subchunk
    if (length(subchunkArgs) == 0 | is.null(subchunkArgs)) {chunkpath <- catchment}
    if (length(subchunkArgs) > 0) {
      chunkpath <- stringr::str_flatten(c(catchment, subchunkArgs),
                                        collapse = '/sub_')
    }

    if (!is.null(extraname)) {
      sumextra <- paste0(funname, '/', extraname)
    } else {
      sumextra <- funname
    }
    # If not chunking, don't need the inner chunked dir.
    if (nchunks == 1) {
      scriptOut <- file.path(out_dir, paste0(dataname), sumextra)
    } else if (nchunks > 1) {
      scriptOut <- file.path(out_dir, paste0(dataname), sumextra, 'chunked',
                             chunkpath)
    }

    # These subchunk indices feel backwards (the outer chunk is on the right,
    # instead of building L -> R). But other things depend on that so don't fix
    # unless planning a big restructure
    # str_replace is because when there's no subchunk we get two _ next to each
    # other.
    unique_chunkname <- paste0(catchment, '_', funname, '_',
                               stringr::str_flatten(subchunkArgs, collapse = '_'),
                               '_', thischunk) |>
      stringr::str_replace_all('__', '_')

    unique_indexname <- paste0(catchment, '_', funname, '_index', '_',
                               stringr::str_flatten(subchunkArgs, collapse = '_'),
                               '_', thischunk) |>
      stringr::str_replace_all('__', '_')

    # Got to be a cleaner way to do this.
    # assign(unique_chunkname, tempAns)
    # assign(unique_indexname, tempIndex)

    # If 1, we don't append the chunkname and it goes in a different directory.
    if (nchunks == 1) {
      unique_chunkname <- stringr::str_remove(unique_chunkname, '_1$')
      unique_indexname <- stringr::str_remove(unique_indexname, '_1$')
    }
  }

  # Make the out directory, in case it doesn't exist
  if (!dir.exists(scriptOut)) {dir.create(scriptOut, recursive = TRUE)}

  outlist <- tibble::lst(scriptOut, unique_chunkname, unique_indexname)

  return(outlist)
}
