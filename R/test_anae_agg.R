

test_anae_agg <- function(catchments, datDir, variableDir, summaryFun, debugbreak = FALSE) {
  # This is a test file because I'm getting weird issues with ANAE duplication on big runs but not individual runs.
  # based on lippia.R, but moving to others too
  # Read in soil temp in ANAEs --------------------------------------------
  
  if ('all' %in% catchments) {
    catchments <- c("Avoca", "BarwonDarling", "BorderRivers", "Broken", "Campaspe", 
                    "Castlereagh", "CentralMurray", "CondamineBalonne", 
                    "EdwardWakool", "Goulburn", "Gwydir", "Kiewa", "Lachlan", 
                    "Loddon", "LowerDarling", "LowerMurray", "Macquarie", "MittaMitta", 
                    "Murrumbidgee", "Namoi", "Ovens", "Paroo", "UpperMurray", "Warrego", "Wimmera")
  }
  
  # I have no idea why this generates random numbers, but shut the warning up with doRNG
  outtib <- foreach(i = 1:length(catchments), 
                    .combine = dplyr::bind_rows) %dorng% {
    catchment <- catchments[i]
    # we need to read them in to a standard name, rather than the pre-named
    # objects they come in as.
    outerDir <- file.path(datDir, variableDir)
    
    list_names <- c(paste0(catchment, '_', summaryFun), 
                    paste0(catchment, '_', summaryFun, '_index'))
    list_path <- file.path(outerDir, summaryFun,
                           paste0(catchment, '_', summaryFun, '.rdata'))
    
    # Brind in the index file too- it's useful to make sure everything lines up
    in_list <- load_rename(filepath = list_path, 
                           knownnames = list_names,
                           newnames = c('aggdata', 'indices'))
    
    anae_names <- paste0(catchment, 'ANAE')
    anae_path <- file.path(datOut, 'ANAEprocessed', paste0(catchment, 'ANAE.rdata'))
    anaes <- load_rename(filepath = anae_path,
                         returnOne = anae_names) |> 
      st_transform(st_crs(in_list$aggdata)) |> 
      st_make_valid()
    
    
    
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
                                                      indexcol = c(1, 1), testfinal = FALSE))) {paste0('pass')},
                          error = function(c) paste0('Error: ', c$message),
                          warning = function(c) paste0('Warning: ', c$message),
                          message = function(c) paste0('Message: ', c$message)
    )
    
    outcome <- tibble::tibble(catchment = catchment, variable = variableDir, 
                              agg_fun = summaryFun, 
                              passfail = list_test)
  }
  
  
  return(outtib)
}
