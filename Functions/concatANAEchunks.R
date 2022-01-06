# # Function to concatenate chunked ANAE outputs

concatANAEchunks <- function(outerDir, summaryFuns, namedIndex = TRUE) {
  # loop over summary functions
  for (su in 1:length(summaryFuns)) {
    summaryFun <- summaryFuns[su]
    
    # There are some that were NOT chunked- leave them alone, and just look in the chunked folder
    scriptOut <- file.path(outerDir, summaryFun)
    
    scriptChunked <- file.path(scriptOut, 'chunked')
    
    # # Make the out directory, in case it doesn't exist- but it has to, so this is moot
    # if (!dir.exists(scriptOut)) {dir.create(scriptOut, recursive = TRUE)}
    
    # get the names of the catchments
    catchNames <- list.files(scriptChunked)
    
    
    # Loop over catchments that were chunked
    # This is actually fairly fast to step through. I think I'm just going to
    # scrap the foreach and use for
    # if we want to parallel the outer loop, will need to restructure the code a bit
    # so the inner is in the same line as outer - see
    # https://stackoverflow.com/questions/9674530/outer-loop-variable-in-nested-r-foreach-loop
    # Not actually sure I can do that, because I need the dplist. 
    for (cn in 1:length(catchNames)) {
      
      # allcatch <- foreach(cn = 1:length(catchNames[1:2])) %do% {
      thiscatch <- catchNames[cn]
      # what do these look like? quick test
      catchfiles <- list.files(file.path(scriptChunked, thiscatch), 
                               pattern = '.rdata', recursive = TRUE)
      
      # I need to know the names of the files to get() them
      partnames <- str_remove(catchfiles, pattern = '.rdata') %>%
        str_remove(pattern = paste0('^.*(?=(', thiscatch, '))')) # remove all the nested directories too
      
      # Sometimes the indices are shared across summaryFuns, and so not uniquely named
      if (namedIndex) {
        indexnames <- str_replace(partnames, pattern = summaryFun, 
                                  replacement = paste0(summaryFun, '_index'))
      } else {
        indexnames <- str_replace(partnames, pattern = summaryFun, 
                                  replacement = paste0('index'))
      }
      
      
      # Same code as for the main processing script, except I'm rebuilding the dpList
      # instead of making it initially
      # I COULD just skip to the bottom two, but then I'd need to load() twice as many times
      # in hindsight, I should have just saved the lists...
      dpList <- foreach(s = 1:length(catchfiles)) %dopar% {
        load(file.path(scriptChunked, thiscatch, catchfiles[s]))
        # outl <- list()
        starpart <- get(partnames[s])
        indexpart <- get(indexnames[s])
        outl <- list(starpart, indexpart)
      } # end foreach
      
      # Check for NULL entries- typically as the result of feeding beyond the
      # end of the anaepolys and expected. Just cut those out, but still warn.
        # Logical indexing into unnamed lists is a pain. I guess loop it
      nullindices <- foreach(l = 1:length(dpList), .combine = c) %do% {
        if (is.null(dpList[[l]][[1]]) & 
            (is.null(dpList[[l]][[2]]) || nrow(dpList[[l]][[2]]) == 0)) { # either null indices or a 0-lenght tibble
          l
        } else {
          NULL
        }
      }
      
      if (!is.null(nullindices)) {
        # For some reason this fails when vectorized (ie no loop, just dpList[[nullindices]] <- NULL. Need to go from the end to
        # the beginning or the indexed numbers shift
        for (nu in length(nullindices):1) {
          dpList[[nullindices[nu]]] <- NULL
        }
      }
      
      
      
      # Then, unpack the lists also using foreach
      depthAns <- foreach(l = 1:length(dpList),
                          .combine=function(...) c(..., along = 1), # Pass dimension argument to c.stars
                          .multicombine=TRUE,
                          .inorder = TRUE) %dopar% {
                            dpList[[l]][[1]]
                          }
      
      depthIndex <- foreach(l = 1:length(dpList),
                            .combine=bind_rows,
                            .multicombine=TRUE,
                            .inorder = TRUE) %dopar% {
                              dpList[[l]][[2]]
                            }
      
      # save the concatenated files
      thisCatchName <- thiscatch
      thisDepth <- paste0(thisCatchName, '_', summaryFun)
      thisIndex <- paste0(thisCatchName, '_', summaryFun, '_index')
      assign(thisDepth, depthAns)
      assign(thisIndex, depthIndex)
      
      # Could just use thisInunName for the rdata, since there's a folder structure, but this is more explicit
      save(list = c(thisDepth, thisIndex), file = file.path(scriptOut, paste0(thisDepth, '.rdata')))
      
      # Cleanup so we don't end up with everything read in
      rm(list = c(thisDepth, thisIndex, 'depthAns', 'depthIndex', 'dpList'))
      # Don't return anything useful- all I want is the side effect- only needed to
      # use an outer foreach
      a <- 1
    }
  }
  
}


