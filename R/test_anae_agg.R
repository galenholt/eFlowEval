#' Check the UIDs haven't gotten scrambled or duplicated
#'
#' @inheritParams process_data
#'
#' @param debugbreak for testing
#' @param filetype '.rds' (default), or '.rdata' for backwards compatibility
#'
#' @return tibble of pass fails
#' @export
#'
test_anae_agg <- function(catchment,
                          out_dir,
                          dataname,
                          summaryFun,
                          poly_path = file.path(out_dir, 'ANAEprocessed'),
                          extraname = NULL,
                          debugbreak = FALSE,
                          filetype = '.rds') {
  # This is a test file because I'm getting weird issues with ANAE duplication on big runs but not individual runs.
  # based on lippia.R, but moving to others too
  if ('all' %in% catchment) {
    catchment <- c("Avoca", "BarwonDarling", "BorderRivers", "Broken", "Campaspe",
                    "Castlereagh", "CentralMurray", "CondamineBalonne",
                    "EdwardWakool", "Goulburn", "Gwydir", "Kiewa", "Lachlan",
                    "Loddon", "LowerDarling", "LowerMurray", "Macquarie", "MittaMitta",
                    "Murrumbidgee", "Namoi", "Ovens", "Paroo", "UpperMurray", "Warrego", "Wimmera")
  }

  # Directory
  if (!is.null(extraname)) {
    sumextra <- paste0(summaryFun, '/', extraname)
  } else {
    sumextra <- summaryFun
  }

  datapath <- file.path(out_dir, dataname, sumextra)

  # I have no idea why this generates random numbers, but shut the warning up with doRNG
  outtib <- foreach(i = 1:length(catchment),
                    .combine = dplyr::bind_rows) %do% {
    thiscatchment <- catchment[i]

    anaes <- read_catchment_polys(poly_path, thiscatchment)
    # Somehow some are invalid, despite the st_make_valid in processANAE
    anaes <- sf::st_make_valid(anaes)


    # The data should all have the catchment names too
    in_list <- read_catchment_polys(datapath, thiscatchment) |>
      setNames(c('aggdata', 'indices')) # make names generic

    # Line everything up ------------------------------------------------------

    commonUID <- Reduce(intersect, list(anaes$UID,
                                        in_list$indices$UID))

    # typically would just logical-index rather than which, but we need to apply
    # it to the aggdata to match the indices
    # I have tested that works.
    anaedrop <- which(!(anaes$UID %in% commonUID))
    listdrop <- which(!(in_list$indices$UID %in% commonUID))

    # This all has to be conditional- a length-0 set drops everything.
    if(length(anaedrop) > 0) {anaes <- anaes[-anaedrop, ]}

    if (length(listdrop) > 0) {
      in_list$aggdata <- in_list$aggdata[,-tempdrop, ]
      in_list$indices <- in_list$indices[-tempdrop, ]
    }

    # check the indices and the anae to make sure we're not shuffled
    # This is what's been failing with weird duplicates

    # Debugbreak is needed to debug inside matchstarsindex- inside trycatch seems to fail.
    if (debugbreak) {
      list_bare <- matchStarsIndex(index1 = anaes, stars1 = NULL,
                                   index2 = in_list$indices, stars2 = in_list$aggdata,
                                   indexcol = c(1, 1), testfinal = FALSE)
    }

    # Wrap all these in tryCatches to test. I don't actually want to return the list though. Just whether it passed
    list_test <- tryCatch(if (is.list(matchStarsIndex(index1 = anaes, stars1 = NULL,
                                                      index2 = in_list$indices, stars2 = in_list$aggdata,
                                                      indexcol = c(1, 1),
                                                      testfinal = FALSE, as_test = TRUE))) {paste0('pass')},
                          error = function(c) paste0('Error: ', c$message),
                          warning = function(c) paste0('Warning: ', c$message),
                          message = function(c) paste0('Message: ', c$message)
    )

    outcome <- tibble::tibble(catchment = thiscatchment, variable = dataname,
                              agg_fun = summaryFun,
                              passfail = list_test)
  }


  return(outtib)
}
