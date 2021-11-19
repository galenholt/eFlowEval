# function to modify anaes for scenarios, assuminmg standard directory structure

# # Function to concatenate chunked ANAE outputs

modifyANAE <- function(outerDir, summaryFuns, returnDir, FUN) {
  # loop over summary functions i guess
  for (su in 1:length(summaryFuns)) {
    summaryFun <- summaryFuns[su]
    
    
    # # Make the out directory, in case it doesn't exist
    if (!dir.exists(file.path(outerDir, returnDir))) {dir.create(file.path(outerDir, returnDir), recursive = TRUE)}
    
    # Get the names of the files to modify
    moddir <- file.path(outerDir, summaryFun)
    catchfiles <- list.files(moddir, pattern = '.rdata')
    
    # I need to know the names of the files to get() them
    partnames <- str_remove(catchfiles, pattern = '.rdata') 
    indexnames <- str_replace(partnames, pattern = summaryFun, 
                              replacement = paste0(summaryFun, '_index'))
    
    # The new names reflecting the modfication
    newnames <- str_replace(partnames, pattern = summaryFun, 
                            replacement = returnDir)
    newindex <- str_c(newnames, '_index')
    
    # Loop over catchments, perform function, and return
    
      dpList <- foreach(s = 1:length(catchfiles)) %dopar% {
        load(file.path(moddir, catchfiles[s]))
        # Standard variable names
        starpart <- get(partnames[s])
        indexpart <- get(indexnames[s])
        
        # do the modification
        starpart <- FUN(starpart)
        
        assign(newnames[s], starpart)
        assign(newindex[s], indexpart)
        
        # Save the list
        save(list = c(newnames[s], newindex[s]), 
             file = file.path(outerDir, returnDir, paste0(newnames[s], '.rdata')))
        
        # Throwaway for foreach
        toss <- 2
      } # end foreach
      
  }
}


