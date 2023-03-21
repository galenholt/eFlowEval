test_dup_anae <- function(catchment, outerDir, summaryFuns) {
  # This is a test file because I'm getting weird issues with ANAE duplication on big runs but not individual runs.
  # based on lippia.R, but moving to others too
  # Read in soil temp in ANAEs --------------------------------------------
  
  # we need to read them in to a standard name, rather than the pre-named
  # objects they come in as.
  
  soilTemp_names <- c(paste0(catchment, '_weightedMean'), 
                      paste0(catchment, '_weightedMean_index'))
  soilTemp_path <- file.path(datOut, 'Tempprocessed', 'weightedMean',
                             paste0(catchment, '_weightedMean.rdata'))
  
  # Brind in the index file too- it's useful to make sure everything lines up
  soilTemp <- load_rename(filepath = soilTemp_path, 
                          knownnames = soilTemp_names,
                          newnames = c('aggdata', 'indices'))
  
  # de-kelvin- strictures are easier to think about in C 
  soilTemp$aggdata <- soilTemp$aggdata - 273
  
  # if we needed the index too, we'd use this to get a list, but we don't so
  # keep it simple
  # catchlist <- load_rename(filepath = soilTemp_path, 
  #                          knownnames = soilTemp_names,
  #                          newnames = c('soilTemp', 'soilTemp_index'))
  
  
  # Read in inundation ------------------------------------------------------
  inun_names <- c(paste0(catchment, '_lippiaAdultSurvive'), 
                  paste0(catchment, '_lippiaAdultSurvive_index'))
  inun_path <- file.path(datOut, 'Inundationprocessed', 'lippiaAdultSurvive',
                         paste0(catchment, '_lippiaAdultSurvive.rdata')) 
  
  inunSurv <- load_rename(filepath = inun_path, 
                          knownnames = inun_names,
                          newnames = c('aggdata', 'indices'))
  
  # The names here aren't right- it's not depth anymore
  names(inunSurv$aggdata) <- 'area_lippia_surv'
  
  # Read in moisture ------------------------------------------------------
  moist_names <- c(paste0(catchment, '_areaCentipedaSurvive'), 
                  paste0(catchment, '_areaCentipedaSurvive_index'))
  moist_path <- file.path(datOut, 'soilmoistureprocessed', 'areaCentipedaSurvive',
                         paste0(catchment, '_areaCentipedaSurvive.rdata')) 
  
  moistSurv <- load_rename(filepath = moist_path, 
                          knownnames = moist_names,
                          newnames = c('aggdata', 'indices'))
  
  # The names here aren't right- it's not depth anymore
  names(moistSurv$aggdata) <- 'area_centipeda_surv'
  
  
  
  # Read in presence by ANAE type -------------------------------------------
  # I don't need to rename this one based on catchment
  load(file.path(datOut, 'Vegmapping', 'lippia_anae.rdata'))
  # I need some actual ANAEs too
  anae_names <- paste0(catchment, 'ANAE')
  anae_path <- file.path(datOut, 'ANAEprocessed', paste0(catchment, 'ANAE.rdata'))
  anaes <- load_rename(filepath = anae_path,
                       returnOne = anae_names) %>% 
    st_transform(st_crs(inunSurv$aggdata)) %>% 
    st_make_valid()
  
  
  
  # Line everything up ------------------------------------------------------
  
  commonUID <- Reduce(intersect, list(anaes$UID, 
                                      soilTemp$indices$UID, 
                                      inunSurv$indices$UID, 
                                      moistSurv$indices$UID))
  
  # typically would just logical-index rather than which, but we need to apply
  # it to the aggdata to match the indices
  # I have tested that works.
  anaedrop <- which(!(anaes$UID %in% commonUID))
  tempdrop <- which(!(soilTemp$indices$UID %in% commonUID))
  survdrop <- which(!(inunSurv$indices$UID %in% commonUID))
  moistdrop <- which(!(moistSurv$indices$UID %in% commonUID))
  
  # This all has to be conditional- a length-0 set drops everything.
  if(length(anaedrop) > 0) {anaes <- anaes[-anaedrop, ]}
  
  if (length(tempdrop) > 0) {
    soilTemp$aggdata <- soilTemp$aggdata[,-tempdrop, ]
    soilTemp$indices <- soilTemp$indices[-tempdrop, ]
  }
  
  if (length(survdrop) > 0) {
    inunSurv$aggdata <- inunSurv$aggdata[,-survdrop, ]
    inunSurv$indices <- inunSurv$indices[-survdrop, ]
  }
  
  if (length(moistdrop) > 0) {
    moistSurv$aggdata <- moistSurv$aggdata[,-moistdrop, ]
    moistSurv$indices <- moistSurv$indices[-moistdrop, ]
  }
  
  # check the indices and the anae to make sure we're not shuffled
  # This is what's been failing with weird duplicates
  
  # Wrap all these in tryCatches to test. I don't actually want to return the list though. Just whether it passed
  temptest <- tryCatch(if (is.list(matchStarsIndex(index1 = anaes, stars1 = NULL,
                                                   index2 = soilTemp$indices, stars2 = soilTemp$aggdata,
                                                   indexcol = c(1, 1), testfinal = FALSE))) {paste0('pass')},
                       error = function(c) paste0('Error: ', c$message),
                       warning = function(c) paste0('Warning: ', c$message),
                       message = function(c) paste0('Message: ', c$message)
    )
  
  inuntest <- tryCatch(if (is.list(matchStarsIndex(index1 = anaes, stars1 = NULL,
                                                   index2 = inunSurv$indices, stars2 = inunSurv$aggdata,
                                                   indexcol = c(1, 1), testfinal = FALSE))) {paste0('pass')},
                       error = function(c) paste0('Error: ', c$message),
                       warning = function(c) paste0('Warning: ', c$message),
                       message = function(c) paste0('Message: ', c$message)
  )
  
  moisttest <- tryCatch(if (is.list(matchStarsIndex(index1 = anaes, stars1 = NULL,
                                                    index2 = moistSurv$indices, stars2 = moistSurv$aggdata,
                                                    indexcol = c(1, 1), testfinal = FALSE))) {paste0('pass')},
                        error = function(c) paste0('Error: ', c$message),
                        warning = function(c) paste0('Warning: ', c$message),
                        message = function(c) paste0('Message: ', c$message)
  )
  
  outcome <- tibble::tibble(catchment = catchment, variable = c('temp', 'inundation', 'moisture'), passfail = c(temptest, inuntest, moisttest))
  
  return(outcome)
}
