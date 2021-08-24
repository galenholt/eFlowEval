# Function to ID the missing files and make shell scripts to fix missings

# # For local testing- leaving this here for a bit in case this breaks with recursive sub chunking
# plan(multisession)
# summaryFun <- 'weightedMean'
# args <- c('blah', 'b', 'c', 'g', '3', 't', 'a', '3', 'Warrego', '8', '6', '8')
# args <- c('blah', 'b', 'c', 'g', '5', 't', 'a', '9', 'Warrego', '8', '6', '10')
# Does it break with one level of chunking?
# args <- c('blah', 'b', 'c', 'g', '5', 't', 'a', '9', 'Warrego')

# ## The outerchunks need to start outer and go in, ie '8', '6' is the 6th subchunk of the 8th main chunk
# Need to handle the edge case wehre there aren't enough polys to do the array we're asking for

makeSHfails <- function(outerDir, varName, summaryFuns, 
                        nchunks = 100, lengthOrChunk, runImmediate = FALSE) {
  
  # Make a sub-directory for the subchunk
  # Come back to this for iteration- we actually want one less level to check the missings
  # scriptOut <- paste0(datOut, '/Tempprocessed/', summaryFun, '/chunked/', 
  #                     str_flatten(args[9:length(args)], collapse = '/sub_'))

  # Loop over summary functions
  for (su in 1:length(summaryFuns)) {
    summaryFun <- summaryFuns[su]
    scriptOut <- file.path(outerDir, summaryFun, 'chunked')
    
    # get the names of the catchments
    # Could pull this out of the filenames, but this is easier
    catchNames <- list.files(scriptOut)
    
    # 
    # I think a list is actually better than a table here
    misslist <- foreach(cn = 1:length(catchNames), .inorder = TRUE) %do% {
      thiscatch <- catchNames[cn]
      catchfiles <- list.files(file.path(scriptOut, thiscatch), 
                               pattern = '.rdata', recursive = TRUE)
      
      # Which chunks worked?
      workedchunks <- as.numeric(str_extract(catchfiles, pattern = '[0-9]+'))
      workedchunks
      
      allchunks <- 1:nchunks
      
      missingchunks <- allchunks[!(allchunks %in% workedchunks)]
      missingchunks
    }
    names(misslist) <- catchNames
    
    # Get rid of catchments that finished (ie have empty list sections)
    misslist <- purrr::compact(misslist)
    
    finishers <- catchNames[!(catchNames %in% names(misslist))]
    cat(paste('These catchments finished:', 
              stringr::str_flatten(finishers, collapse = '\n'), sep = '\n'))
    print(" ")
    cat(paste('These catchments will run again:', 
              stringr::str_flatten(names(misslist), collapse = '\n'), sep = '\n'))
    print(" ")
    
    
    # NOW, do I just run this, or is there a way to create the shell script?
    missnames <- names(misslist)
    headline <- "#!/bin/bash"
    head2 <- "\n"
    
    # Just run longer
    if ('length' %in% lengthOrChunk) {
      # For each catchment, I need to set up the chunk in a list
      lineslist <- foreach(cn = 1:length(missnames), .inorder = TRUE, .combine = c) %do% {
        l1 <- stringr::str_c("thiscatch='", missnames[cn],"'")
        l2 <- stringr::str_c("echo 'start' $thiscatch")
        misschars <- str_flatten(misslist[[cn]], collapse = ",")
        l3 <- stringr::str_c("sbatch -J $thiscatch --array=", misschars, " all", varName, "SLURMLong.sh $thiscatch")
        l4 <- "sleep 2"
        l5 <- "\n"
        thislist <- list(l1, l2, l3, l4, l5)
      }
      
      filechars <- c(headline, head2, unlist(lineslist))
      
      # Hpc says file isn't an option
      # readr::write_lines(filechars, file = 'missingTemps.sh')
      
      writeLines(filechars, con = paste0('missing', varName, '.sh'))
      
      # Do I want to have R actually run this too?
      if (runImmediate) {
        system2(command = 'bash', args = paste0('missing', varName, '.sh'))
      }
      
    } 
    
    # Make the subchunk script
    if ('chunk' %in% lengthOrChunk) {
      # This generates an absurd number of runs. I think usually run the above first, and then only do this if necessary.
      # For each catchment, I need to set up the chunk in a list, but ALSO need to loop over the missings to sub-chunk
      lineslistSUB <- foreach(cn = 1:length(missnames)) %:%
        foreach(sc = 1:length(misslist[[cn]]), .inorder = TRUE, .combine = c) %do% {
          l1 <- stringr::str_c("thiscatch='", missnames[cn],"'")
          l1.5 <- stringr::str_c("thiscchunk='", misslist[[cn]][sc],"'")
          l2 <- stringr::str_c("echo 'start' $thiscatch $thischunk")
          
          l3 <- stringr::str_c("sbatch -J $thiscatch --array=1-100",
                               " all", varName, "TempSLURMchunk.sh $thiscatch $thischunk")
          l4 <- "sleep 2"
          l5 <- "\n"
          thislist <- list(l1, l1.5, l2, l3, l4, l5)
        }
      
      filecharsSUB <- c(headline, head2, unlist(lineslistSUB))  
      writeLines(filecharsSUB, con = paste0('missing', varName, 'Chunk.sh'))
      
      # I *should* be able to recurse this relatively easily, but hopefully won't need to.
      
      # Do I want to have R actually run this too?
      if (runImmediate) {
        system2(command = 'bash', args = paste0('missing', varName, 'Chunk.sh'))
      }
      
    }
  }
}

  
  