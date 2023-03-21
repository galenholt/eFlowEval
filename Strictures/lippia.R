# lippia strictures

# TESTING
# sure wish these were RDS files that we could name on read-in
# Avoca is 56MB, Murrumbidgee is biggest, 771MB
# load(file.path(datOut, 'Tempprocessed', 'weightedMean', 'Murrumbidgee_weightedMean.rdata'))
# load(file.path(datOut, 'Tempprocessed', 'weightedMean', 'Avoca_weightedMean.rdata'))
# source('directorySet.R')
# catchment <- 'Avoca'

lippiastricts <- function(catchment, savefile = TRUE, returnR = FALSE) {
  start_time <- Sys.time()
  
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
  

# # TESTING STRANGE ANAE DUPLICATION ----------------------------------------
# 
#   ## THere were a bunch of duplicates in inunSurv. Now, after re-running it,
#   ## there's not. That's worrying, but hard to debug. There are two fewer rows
#   ## because some of the ANAEs aren't polygons. Not sure why that happens for
#   ## the inunsurv but not the temps.
#   # Still, there's not a ton I can do about bad polygons. I'm going to just
#   # comment all this testing out, and see how things go, I think.
#   
#   # Something weird is happening with this, at least for BorderRivers and likely others
#     #DEBUG CODE
#   all(anaes$UID %in% soilTemp$indices$UID)
#   all(anaes$UID %in% inunSurv$indices$UID)
#   all(soilTemp$indices$UID %in% anaes$UID)
#   all(inunSurv$indices$UID %in% anaes$UID)
#   # So, they all seem to be there for soilTemp in both directions
#   # inunSurv though is missing some ANAEs, but still has the right number of rows
#   nrow(anaes)
#   nrow(inunSurv$indices)
#   
#   # soilTemp is shuffled, but that's expected, and matchStarsIndex handles it OK.
#   all(anaes$UID == soilTemp$indices$UID)
#   
#   # plots- the first set matches
#   plot(anaes[1:10, 'UID'])
#   plot(inunSurv$indices[1:10, 'UID'])
#   plot(inunSurv$aggdata[,1:10, 1])
#   
#   # The inunSurv indices and agg line up near the end, but are shifted from anaes
#   plot(anaes[15000:15010, 'UID'])
#   plot(inunSurv$aggdata[,15000:15010, 1])
#   plot(inunSurv$indices[15000:15010, 'UID'])
#   
#   # That doesn't really get to the issue though- why are some UIDs duplicated? Don't they come from the ANAE?
#   # And if they're duplicated, why are there still the same number of rows?
#   
#   # it feels like I've stuffed up the chunking somehow with an off by one, but why isn't that consistent?
#   
#   # duplication?
#   which(duplicated(anaes$UID))
#   which(duplicated(inunSurv$indices$UID))
#   
#   alldups <- which(inunSurv$indices$UID %in% inunSurv$indices$UID[which(duplicated(inunSurv$indices$UID))])
#   plot(inunSurv$indices[alldups[1:2], ]) # clearly the same
#   # These 4 are interleaved- 
#   plot(inunSurv$indices[alldups[3:4], ]) 
#   plot(inunSurv$indices[alldups[5:6], ])
#   # 3 an5 5 match
#   plot(inunSurv$indices[alldups[c(3,5)], ])
#   # as do 4 and 6
#   plot(inunSurv$indices[alldups[c(4,6)], ])
#   
#   # are those near chunk breaks?
#   chunksize = ceiling(nrow(anaes)/100)
#   
#   # not obviously. they're certainly not right on either side of a chunk break
#   seq(from = 1, to = nrow(anaes), by = chunksize)
#   alldups # for reference
#   
#   # So, it happened in what should be chunks 13 and 81
#   
#   # does it happen in other inundation outputs?- recent
#   inun_spf_names <- c(paste0(catchment, '_areaSpoonbillForage'), 
#                   paste0(catchment, '_areaSpoonbillForage_index'))
#   inun_spf_path <- file.path(datOut, 'Inundationprocessed', 'areaSpoonbillForage',
#                          paste0(catchment, '_areaSpoonbillForage.rdata'))
#   inunSPF <- load_rename(filepath = inun_spf_path, 
#                           knownnames = inun_spf_names,
#                           newnames = c('aggdata', 'indices'))
#   # Interesting
#   which(duplicated(inunSPF$indices$UID))
#   nrow(inunSPF$indices)
#   
#   # Old, also not duplicated
#   inun_ai_names <- c(paste0(catchment, '_areaInun'), 
#                       paste0(catchment, '_areaInun_index'))
#   inun_ai_path <- file.path(datOut, 'Inundationprocessed', 'areaInun',
#                              paste0(catchment, '_areaInun.rdata'))
#   inunai <- load_rename(filepath = inun_ai_path, 
#                          knownnames = inun_ai_names,
#                          newnames = c('aggdata', 'indices'))
#   # Interesting
#   which(duplicated(inunai$indices$UID))
#   nrow(inunai$indices)
#   
#   # How about the centipeda moisture data? It was the same code as this
#   inun_CS_names <- c(paste0(catchment, '_areaCentipedaSurvive'), 
#                       paste0(catchment, '_areaCentipedaSurvive_index'))
#   inun_CS_path <- file.path(datOut, 'soilmoistureprocessed', 'areaCentipedaSurvive',
#                              paste0(catchment, '_areaCentipedaSurvive.rdata'))
#   inunCS <- load_rename(filepath = inun_CS_path, 
#                          knownnames = inun_CS_names,
#                          newnames = c('aggdata', 'indices'))
#   # Interesting- these don't break either.
#   which(duplicated(inunCS$indices$UID))
#   # though 2 rows are missing. what happened there?
#   nrow(inunCS$indices)
#   
#   # are the missing ones any of those that are duplicatd aboce? or missing above?
#   anaes$UID[which(!(anaes$UID %in% inunCS$indices$UID))]
#   # not the dups
#   inunSurv$indices$UID[alldups]
#   # But they do match two of the three missings
#   anaes$UID[which(!(anaes$UID %in% inunSurv$indices$UID))]
#   
#   # SO- why are some things missing?
#   # Why are some things duplicating?
#   
#   # I think it's time to go try to replicate in lippia_inundation_data, and maybe more specifically, get_anae_chunk?
#   anae13 <- get_anae_chunk(anae_path = file.path(datOut, 'ANAEprocessed'),
#                            catchment = 'BorderRivers',
#                            thischunk = 13, subchunkArgs = NULL,
#                            nchunks = 100)
#   
#   # Well, that's why there are two fewer rows- two of them weren't polygons
#   # but no duplicates
#   which(duplicated(anae13$UID))
#   
# # END TESTING BACK TO CODE ------------------------------------------------
  
  # Check the datasets. `matchStarsIndex` *should* be sufficient, but there was
  # a change in `sf` that means some ANAEs are no longer good, but that only
  # affects more recent aggregations, which drop the bad ones. So, we need to
  # first check that the same set of polygons are in each dataset before making
  # sure they're in the right order. And we have to do it out here, because we
  # need *all* datasets to match- there's no way to, say, cut soilTemp based on
  # inunSurv in matchStarsIndex.
  
  commonUID <- Reduce(intersect, list(anaes$UID, soilTemp$indices$UID, inunSurv$indices$UID))
  
  # typically would just logical-index rather than which, but we need to apply
  # it to the aggdata to match the indices
    # I have tested that works.
  anaedrop <- which(!(anaes$UID %in% commonUID))
  tempdrop <- which(!(soilTemp$indices$UID %in% commonUID))
  survdrop <- which(!(inunSurv$indices$UID %in% commonUID))
  
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


  
  # # plot to check I get the subsetting right in the stars
  # # They failed in surv because they're really just lines. 
  # plot(soilTemp$aggdata[,tempdrop[1], 1000])
  # plot(soilTemp$aggdata[,tempdrop[2], 1000])
  # 
  # st_as_sf(soilTemp$aggdata[,tempdrop, 1000])
  # st_is_valid(st_as_sf(soilTemp$aggdata[,tempdrop, 1000]))
  # # Plot a couple nearby- I honestly can't even tell what tempdrop 2 is doin.
  # plot(soilTemp$aggdata[,((tempdrop[1]-2):(tempdrop[1])), 1000])
  # plot(soilTemp$aggdata[,((tempdrop[2]):(tempdrop[2] + 2)), 1000])
  # 
  # temptest <- soilTemp$aggdata[,-tempdrop, ]
  # 
  # # Shift the plots because we just lost rows. The first should be OK, but now the one in the tempdrop[1] spot is the old tempdrop[1] + 1
  # plot(temptest[,((tempdrop[1]-2):(tempdrop[1])), 1000])
  # # here we need to go one back because we lost tempdrop[1], and haven't iterated past tempdrop[2] yet.
  # plot(temptest[,((tempdrop[2]-1):(tempdrop[2])), 1000])
  # 
  # check the indices and the anae to make sure we're not shuffled
  # This returns a list of re-sorted indices and values
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
  
  if (savefile) {
    save(lippiastricts, file = file.path(datOut, 'Strictures', 'lippia', 
                                  paste0(catchment, '_lippia_strictures.rdata')))
  }
  
  if (returnR) {
    return(lippiastricts)
  } else {
    end_time <- Sys.time()
    elapsed <- end_time-start_time
    
    sumtab <- tibble::tibble(catchment,
                             npolys = nrow(anaes),
                             elapsed)
    
    return(sumtab)
  }
  
}


