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

## NOTES
# The first function is designed to handle chunking. The second is for the case
# where there should only be a single file per catchment. I'm sure they could be
# done with a single function, but it's not super straightforward, so easier to
# just do separately

makeSHfails <- function(outerDir, varName, summaryFuns, 
                        nchunks = 100, lengthOrChunk, runImmediate = FALSE,
                        forceAllCatchments = FALSE, 
                        returnForR = FALSE,
                        produce_sh = TRUE) {
  # Outerdir is the outer directory, containing all summaryFun directories and chunking
  # varName is just a unique name to avoid overwriting other variables
    # it tends to be the name in the SLURM script: allvarnameSLURM.sh
  # lengthOrChunk can be one of 'short', 'long', or 'chunk'
  # runImmediate creates the file and fires off the bash call
  # forceALLCatchments just puts in a list of catchments, even if they haven't been run.
    # If TRUE, it uses all catchments in MDB. 
      # particularly useful to auto-generate the bash script for the first run.
    # If FALSE, it uses catchments that have been run previously
    # If a character vector, will force that set of catchments
    
  
  if (is.logical(forceAllCatchments)) {
    allcatch <- c("Avoca", "BarwonDarling", "BorderRivers", "Broken", "Campaspe", 
                  "Castlereagh", "CentralMurray", "CondamineBalonne", 
                  "EdwardWakool", "Goulburn", "Gwydir", "Kiewa", "Lachlan", 
                  "Loddon", "LowerDarling", "LowerMurray", "Macquarie", "MittaMitta", 
                  "Murrumbidgee", "Namoi", "Ovens", "Paroo", "UpperMurray", "Warrego", "Wimmera")
  } else {
    allcatch <- forceAllCatchments
  }

  
  # Make a sub-directory for the subchunk
  # Come back to this for iteration- we actually want one less level to check the missings
  # scriptOut <- paste0(datOut, '/Tempprocessed/', summaryFun, '/chunked/', 
  #                     str_flatten(args[9:length(args)], collapse = '/sub_'))

  # Loop over summary functions
  for (su in 1:length(summaryFuns)) {
    summaryFun <- summaryFuns[su]
    
    scriptOut <- file.path(outerDir, summaryFun, 'chunked')
    
    # If we want to ensure all catchments, create those directories
    if (forceAllCatchments) {
      scriptOut <- file.path(outerDir, summaryFun, 'chunked')
     
      for (d in 1:length(allcatch)){
        if (!dir.exists(file.path(scriptOut, allcatch[d]))) {
          dir.create(file.path(scriptOut, allcatch[d]), recursive = TRUE)
          }
      }
      
    }
    
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
    cat("\n")
    
    # If we're done, just wrap up here
    if (length(misslist) == 0) {
      print("Finished all catchments")
      if (produce_sh) {
        if ('long' %in% lengthOrChunk) {
          writeLines(c("echo 'All catchments finished, not running anything'", "\n"), con = paste0('missing', varName, 'long.sh'))
        }
        if ('short' %in% lengthOrChunk) {
          writeLines(c("echo 'All catchments finished, not running anything'", "\n"), con = paste0('missing', varName, '.sh'))
        }
        if ('chunk' %in% lengthOrChunk) {
          writeLines(c("echo 'All catchments finished, not running anything'", "\n"), con = paste0('missing', varName, 'Chunk.sh'))
        }
      }
      
      
      return()
      
    } else {
      cat(paste('These catchments will run again:', 
                stringr::str_flatten(names(misslist), collapse = '\n'), sep = '\n'))
    }
    cat("\n")
    
    
    if (produce_sh) {
      # If we're using this to support the HPC, we need to create the shell script
      missnames <- names(misslist)
      headline <- "#!/bin/bash"
      head2 <- "\n"
      
      
      # Short runs
      if ('short' %in% lengthOrChunk) {
        # For each catchment, I need to set up the chunk in a list
        lineslist <- foreach(cn = 1:length(missnames), .inorder = TRUE, .combine = c) %do% {
          l1 <- stringr::str_c("thiscatch='", missnames[cn],"'")
          l2 <- stringr::str_c("echo 'start' $thiscatch")
          misschars <- str_flatten(misslist[[cn]], collapse = ",")
          l3 <- stringr::str_c("sbatch -J $thiscatch --array=", misschars, " all", varName, "SLURM.sh $thiscatch")
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
      
      # Just run longer
      if ('long' %in% lengthOrChunk) {
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
        
        writeLines(filechars, con = paste0('missing', varName, 'long.sh'))
        
        # Do I want to have R actually run this too?
        if (runImmediate) {
          system2(command = 'bash', args = paste0('missing', varName, 'long.sh'))
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
    
  return()
}

  

# Same, but for just catchments- no chunking ------------------------------

makeSHfailsNoChunk <- function(outerDir, varName, summaryFuns,
                               lengthOrChunk, runImmediate = FALSE,
                               forceAllCatchments = TRUE) {
  # Outerdir is the outer directory, containing all summaryFun directories and chunking
  # varName is just a unique name to avoid overwriting other variables
  # lengthOrChunk can be one of 'short', 'long', or 'chunk'
  # runImmediate creates the file and fires off the bash call
  # forceALLCatchments just puts in a list of catchments, even if they haven't been run.
  # If TRUE, it uses all catchments in MDB. 
  # particularly useful to auto-generate the bash script for the first run.
  # If FALSE, it uses catchments that have been run previously
  # If a character vector, will force that set of catchments
  
  
  if (is.logical(forceAllCatchments)) {
    allcatch <- c("Avoca", "BarwonDarling", "BorderRivers", "Broken", "Campaspe", 
                  "Castlereagh", "CentralMurray", "CondamineBalonne", 
                  "EdwardWakool", "Goulburn", "Gwydir", "Kiewa", "Lachlan", 
                  "Loddon", "LowerDarling", "LowerMurray", "Macquarie", "MittaMitta", 
                  "Murrumbidgee", "Namoi", "Ovens", "Paroo", "UpperMurray", "Warrego", "Wimmera")
  } else {
    allcatch <- forceAllCatchments
  }
  
  
  # Make a sub-directory for the subchunk
  # Come back to this for iteration- we actually want one less level to check the missings
  # scriptOut <- paste0(datOut, '/Tempprocessed/', summaryFun, '/chunked/', 
  #                     str_flatten(args[9:length(args)], collapse = '/sub_'))
  
  # Loop over summary functions
  for (su in 1:length(summaryFuns)) {
    summaryFun <- summaryFuns[su]
    
    scriptOut <- file.path(outerDir, summaryFun)
    
    # If we want to ensure all catchments, create those directories
    if (forceAllCatchments) {
      scriptOut <- file.path(outerDir, summaryFun)
      
      for (d in 1:length(allcatch)){
        if (!dir.exists(file.path(scriptOut, allcatch[d]))) {
          dir.create(file.path(scriptOut, allcatch[d]), recursive = TRUE)
        }
      }
      
    }
    
    # get the names of the catchments. Here, in the unchinked, this needs to be passed in.
    catchNames <- allcatch
    
    # 
      catchfiles <- list.files(file.path(scriptOut), 
                               pattern = '.rdata', recursive = TRUE)
      
      # Which chunks worked?
      workedcatch <- str_extract(catchfiles, pattern = '[A-z]*_')
      workedcatch
      
      misslist <- catchNames[!(catchNames %in% workedcatch)]
    
      finishers <- catchNames[catchNames %in% workedcatch]
    
      cat(paste('These catchments finished:', 
              stringr::str_flatten(finishers, collapse = '\n'), sep = '\n'))
    cat("\n")
    
    # If we're done, just wrap up here
    if (length(misslist) == 0) {
      print("Finished all catchments")
      if ('length' %in% lengthOrChunk) {
        writeLines(c("echo 'All catchments finished, not running anything'", "\n"), con = paste0('missing', varName, '.sh'))
      }
      if ('chunk' %in% lengthOrChunk) {
        writeLines(c("echo 'All catchments finished, not running anything'", "\n"), con = paste0('missing', varName, 'Chunk.sh'))
      }
      
      return()
      
    } else {
      cat(paste('These catchments will run again:', 
                stringr::str_flatten(names(misslist), collapse = '\n'), sep = '\n'))
    }
    cat("\n")
    
    
    # NOW, do I just run this, or is there a way to create the shell script?
    missnames <- misslist
    headline <- "#!/bin/bash"
    head2 <- "\n"
    
    
    # Short runs
    if ('short' %in% lengthOrChunk) {
      # For each catchment, I need to set up the chunk in a list
      lineslist <- foreach(cn = 1:length(missnames), .inorder = TRUE, .combine = c) %do% {
        l1 <- stringr::str_c("thiscatch='", missnames[cn],"'")
        l2 <- stringr::str_c("echo 'start' $thiscatch")
        misschars <- str_flatten(misslist[[cn]], collapse = ",")
        l3 <- stringr::str_c("sbatch -J $thiscatch ",  "all", varName, "SLURM.sh $thiscatch")
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
    
    # Just run longer
    if ('long' %in% lengthOrChunk) {
      # For each catchment, I need to set up the chunk in a list
      lineslist <- foreach(cn = 1:length(missnames), .inorder = TRUE, .combine = c) %do% {
        l1 <- stringr::str_c("thiscatch='", missnames[cn],"'")
        l2 <- stringr::str_c("echo 'start' $thiscatch")
        misschars <- str_flatten(misslist[[cn]], collapse = ",")
        l3 <- stringr::str_c("sbatch -J $thiscatch ", "all", varName, "SLURMLong.sh $thiscatch")
        l4 <- "sleep 2"
        l5 <- "\n"
        thislist <- list(l1, l2, l3, l4, l5)
      }
      
      filechars <- c(headline, head2, unlist(lineslist))
      
      # Hpc says file isn't an option
      # readr::write_lines(filechars, file = 'missingTemps.sh')
      
      writeLines(filechars, con = paste0('missing', varName, 'long.sh'))
      
      # Do I want to have R actually run this too?
      if (runImmediate) {
        system2(command = 'bash', args = paste0('missing', varName, 'long.sh'))
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
  
  # If we are using this to support a foreach loop, we don't need to build a
  # script, we need misslist to loop over, and then we're done
  if (returnForR) {
    return(misslist)
  } else {
    return()
  }
  
}
