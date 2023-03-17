# lippia strictures

# TESTING
# sure wish these were RDS files that we could name on read-in
  # Avoca is 56MB, Murrumbidgee is biggest, 771MB
# load(file.path(datOut, 'Tempprocessed', 'weightedMean', 'Murrumbidgee_weightedMean.rdata'))
# load(file.path(datOut, 'Tempprocessed', 'weightedMean', 'Avoca_weightedMean.rdata'))
# source('directorySet.R')
# catchment <- 'Avoca'

lippiastricts <- function(catchment) {
  
  #### DATA READ-IN ####
  
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

  # check the indices and the anae to make sure we're not shuffled
  # This returns a list of resorted indices and values
    soilTemp <- matchStarsIndex(index1 = anaes, stars1 = NULL,
                                      index2 = soilTemp$indices, stars2 = soilTemp$aggdata,
                                      indexcol = c(1, 1), testfinal = FALSE)
  # flops the names
  names(soilTemp) <- c('indices', 'aggdata')
  
  inunSurv <- matchStarsIndex(index1 = anaes, stars1 = NULL,
                              index2 = inunSurv$indices, stars2 = inunSurv$aggdata,
                              indexcol = c(1, 1), testfinal = FALSE)
  names(inunSurv) <- c('indices', 'aggdata')
  
  all(soilTemp$indices$UID == anaes$UID)
  all(inunSurv$indices$UID == anaes$UID)
  
    
  
  ### STRICTURE CALCS ###
  
  ## Independent ## 
  
  # Soil Temp ---------------------------------------------------------------
  
  # "Germination was highest (range 50 to 62 per cent) in temperatures ranging
  # from 25 to 35 °C by day, to 10 to 20 °C by night: a minimum day–night
  # difference of 5 °C was required to trigger germination"
  
  # We don't have night temp currently, but let's just look for day temp in a band from 25-35
  # Do we want to roll this? Maybe not? Maybe a 2-day just to smooth a bit?
  # I'm going to do not, just to demo something where we don't need to roll
  
  # So, here, we'll just ask if the daily temp is >= 25 & <= 35
  
  # Is the mean temp for the polygon in the germ band?
  system.time(germ_Lippia <- (soilTemp$aggdata >= 25) & (soilTemp$aggdata <= 35))
  
  # make it area- we can always turn it back into 0-1, but we have confirmed
  # matching ordering now, so make everything area passed
  germ_Lippia <- germ_Lippia * as.numeric(st_area(anaes))
  
  #The attribute needs a name for what it really is, not inherit from data
  names(germ_Lippia) <- 'area_lippia_germ'
  
  # These multi-checks mean it does 3 logical tests. If need be later on, could
  # speed up with Rcpp. It's available in dplyr, but doesn't seem to work on
  # soilTemp because it ses a list, not a vector Can get it to work on the [[1]]
  # array, but not sure it's worth it, at least for now (0.02 sec vs 0.09 is
  # 'big', but not really worth it- this only happens about 20 times)
  # system.time(germ_Lippia[[1]] <- between(soilTemp[[1]], 25, 35))
  
  # https://stackoverflow.com/questions/34519811/what-is-the-fastest-way-to-perform-multiple-logical-comparisons-in-r
  
  
  # Inundation -----------------------------------------------------------
  
  # Stricture 2: extended floods (>4 wks) of >30cm depth kill adult plants Our
  # inundation data is in bimonthly units (e.g. approximately the preceding 8
  # weeks). Will have to use that because it's coarser. It is *preceding* the
  # date (I figured that out somewhere- see all metabolism plots), so makes it
  # easy to use
  
  # the lippiaAdultSurvive inundation processing function returns the area less
  # than 30cm, so we can use that directly- it is the area passing in each
  # polygon
  
  # This is crude, but it's the limits of the data.
  
  
  # ANAE type ---------------------------------------------------------------
  # Not sure I want to keep more than the UID and ANAE cols, but might as well
  # This just says whether each anae is in an anae type where they have a record
  # in the ALA
  anaes <- anaes %>% 
    mutate(passed_anae = ANAE_CODE %in% unique(lippia_anae_types$ANAE_CODE))
  
  
  
  ## DEPENDENT ##
  
  # Going to build this up step-by-step
  
  # If the species  requires ANAE zones, not clear where to put: could lead with
  # it or finish. Or in the middle if they're most relevant to a certain life
  # stage
  # I think I'll end with it, because it might be interesting to see how many
  # OTHER zones look like they should work?
  
  
  # Germination not dependent on anything here ------------------------------
  
  
  # adult survival requires germination (can't survive if don't exist) 
  
  # Unlike centipeda, where we were interested in fruiting, this is just
  # survival, and so instead of needing a growing period between germ and this
  # test (i.e. option 2 in centip), here we can just ask about germination at
  # any time in the past x days (up to a generation). What should that be?
  # Lippia is a perennial, and I'm not sure life span. Maybe I'll say there
  # needs to have been successful germination in the last year?
  
  # first, ask if there was germination in the last 365 days
  germ_Lippia_prev_year <- germ_Lippia # initialize
  
  # 3-4 seconds
  # Rolling mean instead of sum because adding up areas over 365 days potentailly risks overflow, though unlikely.
  system.time(germ_Lippia_prev_year[[1]] <- timeRoll(germ_Lippia[[1]], 
                                                 FUN = RcppRoll::roll_mean, 
                                                 rolln = 365, 
                                                 align = 'right',
                                                 na.rm = TRUE))
  # Then the stricture test is whether there was germ and soil moist
    # germ_lippia_prev_year is the number of days in the previous year with germ
    # inunSurv is the area of each polygon that allows survival
  # But inunsurv is bimontly. So we need to bimonth germ_Lippia_prev_year, I guess.
  
  # Maybe I don't actually need to aggregate to bimonthly here though, just hit
  # germ_Lippia_prev_year on the days that match the bimonth records. That asks
  # whether it was germ in year previous to those days. The catch is that the dates dont' use the same format and getting them to match is going to be a pain. 

  glt <- st_get_dimension_values(germ_Lippia_prev_year, which = 'time')
  ist <- st_get_dimension_values(inunSurv$aggdata, which = 'time')
  # Checking
  # matchg <- glt[glt %in% ist]
  # matchi <- ist[ist >= min(glt)]
  # all(matchg == matchi)
  
  # Getting them to match isn't playing nice. I can (will) force it with [[1]]
  # indexing, but I don't like it. Below are checks to convince myself I'm not
  # shuffling anything. I tested using `tempaggregate` too, and it still got the
  # same error about dim 1 not matching.
  
  matchgermprev <- germ_Lippia_prev_year[,,which(glt %in% ist)]
  matchinun <- inunSurv$aggdata[,,which(ist >= min(glt))]
  
  # I really don't know why it's griping about dimension 1. Does it match?
    # geocheck <- diag(st_intersects(st_geometry(matchgermprev), 
    #                                st_geometry(matchinun), sparse = FALSE))
    # all(geocheck)
    # 
    # mgp0 <- matchgermprev > 0
    # mi0 <- matchinun > 0
    # 
    # geocheck0 <- diag(st_intersects(st_geometry(mgp0), 
    #                                st_geometry(mi0), sparse = FALSE))
    # all(geocheck0)
    
  # Those checks pass. Go ahead and force, I guess multiply by the inundation
  # survival area. The germ area is just the area of the polygon, but the
  # inundation area is meaningful because it reflects the rasters
  germSurv_Lippia <- matchinun # initialise
  names(germSurv_Lippia) <- 'area_germ_and_surv' 
  germSurv_Lippia[[1]] <- (matchgermprev > 0)[[1]] * matchinun[[1]]
  
  
  
  
  
  
  # -------------------------------------------------------------------------
  
  
  # Restrict all to ANAE for final strictures -------------------------------
  
  
  # -------------------------------------------------------------------------
  
  # we can just multiply, and we've checked the orders match on read-in.
  
  germ_anae_lippia <- germ_Lippia * anaes$passed_anae
  surv_anae_lippia <- inunSurv$aggdata * anaes$passed_anae
  fullCycle_anae_lippia <- germSurv_Lippia * anaes$passed_anae
  
  # Not renaming these, they are still whether there was germ, surv, or both. just now that depends on anae
  
  # Save those as a list, along with some of the constituents
  # this is fairly redundant. Do I actually want to do this? Or should I save
  # germ2535, surv4 and isANAE, and calculate the finals elsewhere?
  # Decide later. It's useful to have the ultimate thing as an output, I think
  # maybe the answer is to have the final output with everything, and then the
  # base, but skip stuff in the middle, because we can always re-calc that as needed
  lippiastricts <- tibble::lst(fullCycle_anae_lippia, 
                               germSurv_Lippia, 
                               germ_Lippia,
                               surv_Lippia = inunSurv$aggdata, 
                               anae_Lippia = anaes[,c('UID', 'ANAE_CODE', 'ValleyName', 'passed_anae')])
  
  return(lippiastricts)
}


