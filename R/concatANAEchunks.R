#' Function to concatenate chunked ANAE outputs
#'
#' @inheritParams process_data
#'
#' @param rebuild logical default FALSE does not re-concatenate if a concatenated file exists. If TRUE, it does.
#' @param filetype '.rds' (default), or '.rdata' for backwards compatibility
#'
#' @return invisible, main thing is the saving of concatenated data.
#' @export
#'
concat_chunks <- function(out_dir,
                          dataname,
                          summaryFun,
                          catchment = 'all',
                          rebuild = FALSE,
                          filetype = '.rds') {
  # loop over summary functions- deprecated, this was never used in practice, and could be done outside the code
  # for (su in 1:length(summaryFuns)) {
  #   summaryFun <- summaryFuns[su]

    # There are some that were NOT chunked- leave them alone, and just look in the chunked folder
    fundir <- file.path(out_dir, dataname, summaryFun)
    chunkdir <- file.path(fundir, 'chunked')

    if ('all' %in% catchment) {
      # get the names of the catchments
      catchment <- c("Avoca", "BarwonDarling", "BorderRivers", "Broken", "Campaspe",
                     "Castlereagh", "CentralMurray", "CondamineBalonne",
                     "EdwardWakool", "Goulburn", "Gwydir", "Kiewa", "Lachlan",
                     "Loddon", "LowerDarling", "LowerMurray", "Macquarie", "MittaMitta",
                     "Murrumbidgee", "Namoi", "Ovens", "Paroo", "UpperMurray", "Warrego", "Wimmera")
    }

    # Usually only rebuild new data
    # This looks in the concatenated folder and finds the catchments already there and removes them from the list to do.
    if (!rebuild) {
      filespresent <- list.files(fundir)
      catchespresent <- stringr::str_remove(filespresent, '_[A-z].*')
      catchment <- catchment[!(catchment %in% catchespresent)]
    }

    if (length(catchment) == 0) {
      rlang::inform("No catchments to combine, likely because `rebuild = FALSE` and they're all done")
      return()
    }

    # Loop over catchments that were chunked
      allcatch <- foreach(cn = 1:length(catchment)) %do% {
        # Get a single catchment
        thiscatch <- catchment[cn]

        # what files are there?
        catchfiles <- list.files(file.path(chunkdir, thiscatch),
                               pattern = filetype, recursive = TRUE)

      # I need to know the names of the files to get() them
      partnames <- str_remove(catchfiles, pattern = filetype) |>
        str_remove(pattern = paste0('^.*(?=(', thiscatch, '))')) # remove all the nested directories too

      if (filetype == '.rdata') {
        rlang::abort(".rdata no longer supported. While there may still be some out there, it should all be concatenated by now. Moving forward, use .rds and save the lists")
      }

      # read in all the lists and concatenate
      catch_cat <- foreach::foreach(s = 1:length(catchfiles),
                                      .combine = \(...) concat_star_index(list(...),
                                                                          dimension = 'geometry',
                                                                          starname = paste0(thiscatch, '_', summaryFun),
                                                                          indexname = paste0(thiscatch, '_', summaryFun, '_index'))) %do% {
        cc <- readRDS(file.path(chunkdir, thiscatch, catchfiles[s]))
      }

      # Seems to work to just do it directly in the foreach
      # catchchunks <- concat_star_index(catchchunks, dimension = 1,
      #                                  starname = paste0(thiscatch, '_', summaryFun),
      #                                  indexname = paste0(thiscatch, '_', summaryFun, '_index'))

      # Could just use thisInunName for the rdata, since there's a folder structure, but this is more explicit
      saveRDS(catch_cat, file = file.path(fundir, paste0(thiscatch, '_', summaryFun, '.rds')))

      return(invisible())
    }
  # }

}


