# Lippia output summary and figures

yearsummary <- function(strictList, whichSave = NULL, outsuffix, datebreaks) {

# Summarise over year -----------------------------------------------------
  # TODO: More flexible time summaries
  
  # Set up time summary breakpoints
  # Calendar years, months, other things set with character vectors can facet on
  # time itself in the plotting, but giving it dates can't. No idea why
  # by_t <- 'years' # calendar years
  
  # let's just pick some cut points manually to see how this works, then maybe
  # lubridate to make them programatically (e.g. every april 30 or whatever)
  
  startdate <- min(st_get_dimension_values(strictList[[1]], which = 'time'))
  enddate <-  max(st_get_dimension_values(strictList[[1]], which = 'time'))
  
  # Old, to handle differences between sm and temp
  # startdate <- min(min(st_get_dimension_values(dailyPolySMavg, which = 'time')),  
  #                  min(st_get_dimension_values(dailyPolyTempavg, which = 'time')))
  # enddate <-  max(max(st_get_dimension_values(dailyPolySMavg, which = 'time')),  
  #                 max(st_get_dimension_values(dailyPolyTempavg, which = 'time')))
  # 
  
  # intervals
  # this works, but it causes the scale_id error in ggplot (see facet_scale_id_error.R)
  # That's fixable by faceting by as.character(time). Do we want to do that? Maybe? Should be OK, I think?
  
  if (lubridate::is.POSIXt(interDates)) {
    by_t <- c(startdate, datebreaks,  enddate)
  } else {
    by_t <- interDates # If, for example, interDates is 'years'
  }
  
  # Don't average isANAE over time
  noANAE <- str_which(names(strictList), 'isANAE', negate = TRUE)
  
  yrlist <- list()
  for (y in noANAE) {
    yrlist[[y]] <- tempaggregate(strictList[[y]], by = by_t, FUN = propor, na.rm = TRUE)
    # Would be nice if temaggregate could do the rename internally dependent on FUN
    names(yrlist[[y]]) <- 'propDaysPassed'
    
  }
  
  names(yrlist) <- str_c(names(strictList)[noANAE], '_yr_')
  
  # do we save anything?
  if (!is.null(whichSave)) {
    if (is.character(whichSave)) {
      whichSave <- str_which(names(strictList), whichSave) # Get the index
    }
    
    savestar <- yrlist[[whichSave]] # why do I need to do this?
    save(savestar, 
         file = file.path('strictOut', paste0(names(yrlist)[whichSave], outsuffix, '.rdata')))
  }


  return(yrlist)
  
}



