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
                          poly_path = file.path(out_dir, 'ANAEprocessed'),
                          extraname = NULL,
                          rebuild = FALSE,
                          filetype = '.rds') {
  # loop over summary functions- deprecated, this was never used in practice, and could be done outside the code
  # for (su in 1:length(summaryFuns)) {
  #   summaryFun <- summaryFuns[su]

    # There are some that were NOT chunked- leave them alone, and just look in the chunked folder
  if (!is.null(extraname)) {
    sumextra <- paste0(summaryFun, '/', extraname)
  } else {
    sumextra <- summaryFun
  }
    fundir <- file.path(out_dir, dataname, sumextra)
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

    # set up progress bar
    p <- progressor(steps = length(catchment))

    # Loop over catchments that were chunked. I do not understand why, but if
    # this is %dofuture%, concat_star_index can't use c.stars or bind_rows on an
    # sf. I've tried passing in packages in options.future and nothing is working.
      allcatch <- foreach(cn = 1:length(catchment),
                          .options.future = list(seed = TRUE)) %dofuture% {
        # Get a single catchment
        thiscatch <- catchment[cn]

        # what files are there?
        catchfiles <- list.files(file.path(chunkdir, thiscatch),
                               pattern = filetype, recursive = TRUE)

      # I need to know the names of the files to get() them
      partnames <- stringr::str_remove(catchfiles, pattern = filetype) |>
        stringr::str_remove(pattern = paste0('^.*(?=(', thiscatch, '))')) # remove all the nested directories too

      if (filetype == '.rdata') {
        rlang::abort(".rdata no longer supported. While there may still be some out there, it should all be concatenated by now. Moving forward, use .rds and save the lists")
      }

      # read in all the lists and concatenate
      catch_cat <- foreach::foreach(s = 1:length(catchfiles)) %do% {
        cc <- readRDS(file.path(chunkdir, thiscatch, catchfiles[s]))
      }

      # remove the nulls that might persist from the aggregation step if there are fewer ANAEs than chunks
      catch_cat <- catch_cat[!purrr::map_lgl(catch_cat, \(x) is.null(x[[1]]) & is.null(x[[2]]))]
      catch_cat <- concat_star_index(catch_cat,
                                          dimension = 'geometry',
                                          starname = paste0(thiscatch, '_', summaryFun),
                                          indexname = paste0(thiscatch, '_', summaryFun, '_index'))

      # Seems to work to just do it directly in the foreach with .combine, UNLESS there are NULLs.

      # undo any shuffling that's happened

      anaes <- read_catchment_polys(poly_path, thiscatch)

      catch_cat <- matchStarsIndex(index1 = anaes, stars1 = NULL,
                                 index2 = catch_cat[[paste0(thiscatch, '_', summaryFun, '_index')]],
                                 stars2 = catch_cat[[paste0(thiscatch, '_', summaryFun)]],
                                 indexcol = c(1, 1), testfinal = FALSE)

      names(catch_cat) <- c(paste0(thiscatch, '_', summaryFun), paste0(thiscatch, '_', summaryFun, '_index'))

      # Could just use thisInunName for the rdata, since there's a folder structure, but this is more explicit
      saveRDS(catch_cat, file = file.path(fundir, paste0(thiscatch, '_', summaryFun, '.rds')))

      # just kick the catchment name to the foreach output
      p(glue::glue("catchment {thiscatch} concatenated"))
      thiscatch
    }
  # }

      return(invisible())
}


