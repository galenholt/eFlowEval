centipedastricts <- function(catchment,
                             savefile = TRUE, returnR = FALSE) {
  start_time <- Sys.time()
  # lippia threshold is the proportion of days in a year of lippia success that
  # prevents centipeda success
    # currently hardcoded as sorted in scenarioPlotScratch
  
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
  
 
  # Read in inundation ------------------------------------------------------
  # Just areaInun- was it inundated in most recent bimonth?
  inun_names <- c(paste0(catchment, '_areaInun'), 
                  paste0(catchment, '_areaInun_index'))
  inun_path <- file.path(datOut, 'Inundationprocessed', 'areaInun',
                         paste0(catchment, '_areaInun.rdata')) 
  
  inunArea <- load_rename(filepath = inun_path, 
                          knownnames = inun_names,
                          newnames = c('aggdata', 'indices'))
  
  # The names here aren't right- it's not depth anymore
  names(inunArea$aggdata) <- 'area_inundated'
  

  # Read in soil moisture ---------------------------------------------------
  moist_names <- c(paste0(catchment, '_areaCentipedaSurvive'), 
                  paste0(catchment, '_areaCentipedaSurvive_index'))
  moist_path <- file.path(datOut, 'soilmoistureprocessed', 'areaCentipedaSurvive',
                         paste0(catchment, '_areaCentipedaSurvive.rdata')) 
  
  moistSurv <- load_rename(filepath = moist_path, 
                          knownnames = moist_names,
                          newnames = c('aggdata', 'indices'))
  names(moistSurv$aggdata) <- 'area_centipeda_survival'
  
  
  # Read in presence by ANAE type -------------------------------------------
  # I don't need to rename this one based on catchment
  load(file.path(datOut, 'Vegmapping', 'centipeda_anae.rdata'))
  # I need some actual ANAEs too
  anae_names <- paste0(catchment, 'ANAE')
  anae_path <- file.path(datOut, 'ANAEprocessed', paste0(catchment, 'ANAE.rdata'))
  anaes <- load_rename(filepath = anae_path,
                       returnOne = anae_names) %>% 
    st_transform(st_crs(inunArea$aggdata)) %>% 
    st_make_valid()
  
  # Read in yearly Lippia success -------------------------------------------
  # this comes in as `lippiastricts`, a list with a few stars I could do
  # something with load_rename, but this doesn't have the central issue there of
  # having a specific name, so it's easier to just manually rename
  load(file.path(datOut, 'Strictures', 'lippia', 
                 paste0(catchment, '_lippia_strictures.rdata')))
  lippia <- list(aggdata = lippiastricts$fullCycle_anae_lippia, 
                 indices = lippiastricts$anae_Lippia)
  rm(lippiastricts)
  
  
  
  # Line everything up ------------------------------------------------------
  
  # Check the datasets. `matchStarsIndex` *should* be sufficient, but there was
  # a change in `sf` that means some ANAEs are no longer good, but that only
  # affects more recent aggregations, which drop the bad ones. So, we need to
  # first check that the same set of polygons are in each dataset before making
  # sure they're in the right order. And we have to do it out here, because we
  # need *all* datasets to match- there's no way to, say, cut soilTemp based on
  # inunSurv in matchStarsIndex.
  
  commonUID <- Reduce(intersect, list(anaes$UID, 
                                      soilTemp$indices$UID, 
                                      inunArea$indices$UID,
                                      moistSurv$indices$UID,
                                      lippia$indices$UID)) # Should already match anaes, but check
  
  # typically would just logical-index rather than which, but we need to apply
  # it to the aggdata to match the indices
  # I have tested that works.
  anaedrop <- which(!(anaes$UID %in% commonUID))
  tempdrop <- which(!(soilTemp$indices$UID %in% commonUID))
  inundrop <- which(!(inunArea$indices$UID %in% commonUID))
  moistdrop <- which(!(moistSurv$indices$UID %in% commonUID))
  lippiadrop <- which(!lippia$indices$UID %in% commonUID)
  
  # This all has to be conditional- a length-0 set drops everything.
  if(length(anaedrop) > 0) {anaes <- anaes[-anaedrop, ]}
  
  if (length(tempdrop) > 0) {
    soilTemp$aggdata <- soilTemp$aggdata[,-tempdrop, ]
    soilTemp$indices <- soilTemp$indices[-tempdrop, ]
  }
  
  if (length(inundrop) > 0) {
    inunArea$aggdata <- inunArea$aggdata[,-inundrop, ]
    inunArea$indices <- inunArea$indices[-inundrop, ]
  }
  
  if (length(moistdrop) > 0) {
    moistSurv$aggdata <- moistSurv$aggdata[,-moistdrop, ]
    moistSurv$indices <- moistSurv$indices[-moistdrop, ]
  }
  
  # For lippia, just use the fullCycle, but keep the lippia_anaes as the index too
  if (length(lippiadrop) > 0) {
    lippia$indices <- lippia$indices[-lippiadrop, ]
    lippia$aggdata <- lippia$aggdata[,-lippiadrop, ]
  }
  
  # check the indices and the anae to make sure we're not shuffled
  # This returns a list of re-sorted indices and values
  
  # Temp
  soilTemp <- matchStarsIndex(index1 = anaes, stars1 = NULL,
                              index2 = soilTemp$indices, stars2 = soilTemp$aggdata,
                              indexcol = c(1, 1), testfinal = FALSE, return1 = TRUE)
  # flops the names
  names(soilTemp) <- c('indices_anae', 'indices', 'aggdata')
  anaest <- soilTemp$indices_anae
  
  # Inundation
  inunArea <- matchStarsIndex(index1 = anaes, stars1 = NULL,
                              index2 = inunArea$indices, stars2 = inunArea$aggdata,
                              indexcol = c(1, 1), testfinal = FALSE, return1 = TRUE)
  names(inunArea) <- c('indices_anae', 'indices', 'aggdata')
  anaeia <- inunArea$indices_anae
  
  # Soil moisture
  moistSurv <- matchStarsIndex(index1 = anaes, stars1 = NULL,
                              index2 = moistSurv$indices, stars2 = moistSurv$aggdata,
                              indexcol = c(1, 1), testfinal = FALSE, return1 = TRUE)
  names(moistSurv) <- c('indices_anae', 'indices', 'aggdata')
  anaems <- moistSurv$indices_anae
  
  # Lippia
  lippia <- matchStarsIndex(index1 = anaes, stars1 = NULL,
                               index2 = lippia$indices, 
                            stars2 = lippia$aggdata,
                               indexcol = c(1, 1), testfinal = FALSE, return1 = TRUE)
  names(lippia) <- c('indices_anae', 'indices', 'aggdata')
  anael <- lippia$indices_anae
  
  
  if (!all(anaest$UID == anaeia$UID) |
      !all(anaest$UID == anaems$UID) |
      !all(anaest$UID == anael$UID)) {
    rlang::abort("something is wrong with the sorting")
  }
  # Cut the anaes off
  soilTemp <- soilTemp[-1]
  inunArea <- inunArea[-1]
  moistSurv <- moistSurv[-1]
  lippia <- lippia[-1]
  # but keep them and use the cleaned versions
  anaes <- anaest
  
  
  ### STRICTURE CALCS ###
  
  ## Independent ## 
  
  # Stricture 1: Germination requires inundation We have bimonth, so just use
  # the area of inundation to give area of germination. The best the data can do
  # is bimonthly, so if it was wet in the preceding bimonth, say germ passed.
  # Thus, this is just directly inunArea. Go ahead and name it though.
  
  germ_centipeda <- inunArea$aggdata
  names(germ_centipeda) <- 'area_centipeda_germ'
  
  
  # Stricture 2: Soil moisture needs to be maintained for a certain period of
  # time to enable fruiting and seed-set
  # As above, no numbers for the moisture level or period of time.
  # let's say they die if soil moisture is < 10% in the preceding 6-week growing
  # period
    # To demo raster-rolling we did this on the rasters, and then got the area.
    # So, the area of survival is just moistSurv
  surv_centipeda <- moistSurv$aggdata
  names(surv_centipeda) <- 'area_centipeda_survive'

  
  # Stricture 3: Loss of seed viability at temps > 60
  # Let's say the 60 degree condition can't have occurred in the last month (28
  # days)? No particular reason why I've chosen that duration.
  # To meet the "dead if temp > 60 in last month" condition, we need to check if
  # max(last 28 days) is 60
  # An alternative would be to ask whether > x days were above 60 (ie one day
  # might be fine, but a week straight might not be). This would be a two-step
  # process of checking each day > 60, then rolling over THAT for a month with a
  # sum Let's do that, actually, since it's a different way to do it.
  
  # Intialize
  seed_surv_centipeda <- soilTemp$aggdata 
  
  # Logical; ask if the day is > 60 (no rolling at this point)
  seed_surv_centipeda <- seed_surv_centipeda > 60
  
  # How many of the last 28 days are > 60 (rolling sum of the logicals)
  system.time(seed_surv_centipeda[[1]] <- timeRoll(seed_surv_centipeda[[1]], 
                                                 FUN = RcppRoll::roll_sum, 
                                                 rolln = 28, 
                                                 align = 'right',
                                                 na.rm = TRUE))
  # are there 4 or fewer days to pass?
  seed_surv_centipeda <- seed_surv_centipeda <= 4
  # the area of seed survival
  seed_surv_centipeda <- seed_surv_centipeda * as.numeric(st_area(anaes))
  # The attribute needs a name for what it really is, not inherit from data
  names(seed_surv_centipeda) <- 'area_centipeda_seed_survival'
  
  
  # ANAE type ---------------------------------------------------------------
  # Not sure I want to keep more than the UID and ANAE cols, but might as well
  # This just says whether each anae is in an anae type where they have a record
  # in the ALA
  anaes <- anaes %>% 
    mutate(passed_anae = ANAE_CODE %in% unique(centipeda_anae_types$ANAE_CODE))
  
  ## DEPENDENT ##
  
  # D1. germination requires seed survival.
  
  # Survival has daily values because it's based on soilTemp, but germ has only
  # bimonthly records. How do we want to do this? Germination on each inundation
  # day if a) all days in bimonth survived? b) any days survived, c) that
  # particular day has a passing survival? I don't think (c) makes sense- the
  # germ really is anytime in that month. I guess let's say if any day had dead
  # seeds, it fails, e.g. ALL days have to pass the survival stricture. There
  # aren't many that won't, I don't think.
  
  # How to calculate that? tempaggregate, I think.
  ist <- st_get_dimension_values(inunArea$aggdata, which = 'time')
  bimsurv <- function(x, na.rm = TRUE) {all(x > 0, na.rm = na.rm)}
  survbimonth <- tempaggregate(seed_surv_centipeda, by_t = ist, FUN = bimsurv,
                               dates_end_interval = TRUE) # because inundation is end
  # Tempaggregate flips dims
  if (attributes(st_dimensions(survbimonth))$name[1] != 'geometry') {
    survbimonth <- aperm(survbimonth, c(2,1))
  }
  
  # I want this to be the same time length as inundation, but the aggregate
  # drops a value (which is expected behavior, I just want dims to match to make
  # later arithmetic easier) So, pad with a single leading date with NA (the one
  # that got dropped)
  sbmNA <- survbimonth[,,1]*NA
  sbmNA <- st_set_dimensions(sbmNA, which = 'time', values = ist[1])
  survbimonth <- c(sbmNA, survbimonth)
  
  # check
  sbmt <- st_get_dimension_values(survbimonth, which = 'time')
  all(sbmt == ist)
  
  # Now I have whether seeds survived the preceding bimonth, and we see if they
  # did that and germinated
  # Just like lippia, this complains about dim 1, but it matches. Just force it
  seedsurv_germ_centipeda <- germ_centipeda # initialise
  # survbimoth is binary 0-1, germ_centipeda is area of germ. so multiplying gives the "and" and yields the area that both survived and germed
  seedsurv_germ_centipeda[[1]] <- survbimonth[[1]] * germ_centipeda[[1]]
  
  # The attribute needs a name for what it really is, not inherit from data
  names(seedsurv_germ_centipeda) <- 'area_surv_and_germ'
  
  
  # D2. Fruiting requires germination and subsequent soil moisture.
  # I have rolled the soil moisture for 6 weeks in the raster, which is now in `surv_centipeda`
  
  # Fruiting requires germination within the last ?? time period and sustained
  # soil moisture condition
  
  # there are a few ways to do this
  # 1. Simplest: has there been germination in the last x days and has soil
  # moisture been above some threshold (10%) for that time?
    # We have seedsurv_germ_centipeda with bimonthly germ, and surv_centipeda with
    # daily soil moisture
    # This is crude, since germ could have happened day before, for ex
  
  # 2. Growing period- germ occurred during a span x to y days ago, and moisture has existed since then
    # This makes a lot of sense, but the bimonthly intervals are an issue.
    # I suppose what I'll do is ask if germ occurred during the preceding year,
    # *except* the most recent bimonth.
    # and then whether there has been moisture since ??
      # The easiest way to do this is to ask about the same time- e.g. has there
      # been moisture for two months, and germ two months ago. If we want to do
      # something more complicated, like return the most recent germ and then ask
      # if there's been moisture since then, we'll need to do different lookbacks
      # for each polygon and day. Do we want to do that? Probably. Are we set up
      # to? Not currently.
  # So, what to do? I think germ in last year (skipping last bimonth), but soil
  # moisture in last bimonth is sufficient. It's not great, but it's what we did
  # before
  seedsurv_germ_centipedayr <- seedsurv_germ_centipeda > 0 # initialize with whether there was germ
  
  # Roll one year (6 bimonths). Use roll_sum so when we subtract off the most recent month we still know how many others passed.
  system.time(seedsurv_germ_centipedayr[[1]] <- timeRoll(seedsurv_germ_centipedayr[[1]],
                                                        FUN = RcppRoll::roll_sum,
                                                        rolln = 6,
                                                        align = 'right',
                                                        na.rm = TRUE))
  # subtract off the most recent bimonth and get whether there are still any passes
  seedsurv_germ_centipedayr <- (seedsurv_germ_centipedayr - (seedsurv_germ_centipeda > 0)) > 0
  seedsurv_germ_centipedayr[[1]] <- as.numeric(seedsurv_germ_centipedayr[[1]])
  
  # The attribute needs a name for what it really is, not inherit from data
  names(seedsurv_germ_centipedayr) <- 'bimonthperiodsum'
  
  # Now, get soil moisture over the last bimonth using tempaggregate How to
  # calculate that? tempaggregate, I think. I have `ist` (the time splits)
  # already. Instead of bimsurv, I'm going to use `min` as the function, since
  # that will give me the minimum area of sufficient moisture, and thus where
  # survival was possible the whole time
  moistbimonth <- tempaggregate(surv_centipeda, by_t = ist, FUN = min,
                               dates_end_interval = TRUE) # because inundation is end
  # Tempaggregate flips dims
  if (attributes(st_dimensions(moistbimonth))$name[1] != 'geometry') {
    moistbimonth <- aperm(moistbimonth, c(2,1))
  }
  
  # Add the NA as above (just use the same NA pad stars)
  moistbimonth <- c(sbmNA, moistbimonth)
  
  # check
  mbmt <- st_get_dimension_values(moistbimonth, which = 'time')
  all(mbmt == ist)
  
  # Now, the stricture is whether there was any germ in the last 12-2 months
  # ago, multiplied by minimum moisture area in the last two months.
  # Actually, needs to be the minimum of germ area and moisture area
  # initialise 
  seedsurv_germ_fruit_centipeda <- seedsurv_germ_centipeda
  # germ area where there was germ
  seedsurv_germ_fruit_centipeda[[1]] <- (seedsurv_germ_centipedayr[[1]] * seedsurv_germ_centipeda[[1]]) 
  seedsurv_germ_fruit_centipeda[[1]] <-  pmin(seedsurv_germ_fruit_centipeda[[1]], moistbimonth[[1]], na.rm = TRUE)
  names(seedsurv_germ_fruit_centipeda) <- 'area_centipeda_germ_fruit'
  
  
  
  
  # Restrict all to ANAE -------------------------------
  
  # we can just multiply, and we've checked the orders match on read-in.
  # Only do them stepwise, not all combos
  seed_surv_anae_centipeda <- seed_surv_centipeda * anaes$passed_anae
  germ_anae_centipeda <- seedsurv_germ_centipeda * anaes$passed_anae
  fullCycle_anae_centipeda <- seedsurv_germ_fruit_centipeda * anaes$passed_anae
  
  ### LIPPIA dependence -------------------------------------------------------
  
  # just do this for the full cycle and the anae-restricted, since we're using
  # anae-restricted lippia I had done something here with unevenTimeMult, but I
  # think I can just roll it, can't I? If lippia was successful in the last year
  # (or for lippiaThreshold of the last year (*area?)), fail.
  
  # Roll to get the max area occupied by lippia in the last year
  lippiayr <- lippia$aggdata
  lippiayr[[1]] <-  timeRoll(lippiayr[[1]],
                        FUN = RcppRoll::roll_max,
                        rolln = 6,
                        align = 'right',
                        na.rm = TRUE)
  # The na.rm turns things with all NA to -Inf
  lippiayr[[1]][is.infinite(lippiayr[[1]])] <-  NA
  
  # Now, let's say that's area not available for centipeda
  # But we have *way* more centipeda data than lippia, so have to cut.
  matchbims <- which(st_get_dimension_values(fullCycle_anae_centipeda, which = 'time') %in%
                       st_get_dimension_values(lippiayr, which = 'time'))
  
  # Do the calculation- subtract area
  # Have to initialise and force as usual
  fullCycle_lippiaLimit_centipeda <- fullCycle_anae_centipeda[,,matchbims]
  fullCycle_lippiaLimit_centipeda[[1]] <- fullCycle_anae_centipeda[,,matchbims][[1]] - 
    lippiayr[[1]]
  # no negative areas
  fullCycle_lippiaLimit_centipeda[[1]][fullCycle_lippiaLimit_centipeda[[1]] < 0] <- 0
  
  # # so, that doesn't change much. 146 cases in Avoca where there was > 0 centipeda and lippia.
  # sum(fullCycle_anae_centipeda[,,matchbims][[1]] > 0 & lippiayr[[1]] > 0, na.rm = TRUE)
  # # Ah, there's only 205 where centipeda is successful. SO, that's actually a
  # # *huge* hit from lippia. 
  # 
  # # A bit of testing- none of this seems obviously wrong. 
  # # there are 1862 ANAEs where both could be present in
  # # Avoca In Avoca, there's a HUGE drop from seed surv to germ (90k to 4500),
  # # then another 20x to fruiting. ANAE doesn't lose much.
  # sum(fullCycle_anae_centipeda[,,matchbims][[1]] > 0, na.rm = TRUE)
  # sum(seedsurv_germ_fruit_centipeda[,,matchbims][[1]] > 0, na.rm = TRUE)
  # sum(seedsurv_germ_centipeda[,,matchbims][[1]] > 0, na.rm = TRUE)
  # sum(seed_surv_centipeda[,,matchbims][[1]] > 0, na.rm = TRUE)
  # 
  # # Is it the dependencies? Not with germ- it is a huge hit on its own.
  # # and germ is just inundation, so that's not going to get any bigger.
  # sum(germ_centipeda[,,matchbims][[1]] > 0, na.rm = TRUE)
  # # survival is reasonably large
  # sum(surv_centipeda[,,matchbims][[1]] > 0, na.rm = TRUE)
  # # but gets cut a lot by germ (this is crude and not the same as the real calcs, but it's ballpark)
  # sum(surv_centipeda[,,matchbims][[1]] > 0 & germ_centipeda[,,matchbims][[1]] > 0, na.rm = TRUE)
  # 
  # Save those as a list, along with some of the constituents
  # this is fairly redundant. Do I actually want to do this? 
  # Decide later. It's useful to have the ultimate thing as an output, I think
  # maybe the answer is to have the final output with everything, and then the
  # base, but skip stuff in the middle, because we can always re-calc that as needed
  centipedastricts <- tibble::lst(fullCycle_lippiaLimit_centipeda, 
                               fullCycle_anae_centipeda, 
                               seed_surv_centipeda, 
                               seedsurv_germ_centipeda,
                               seedsurv_germ_fruit_centipeda, 
                               anae_centipeda = anaes[,c('UID', 'ANAE_CODE', 'ValleyName', 'passed_anae')])
  
  if (savefile) {
    save(centipedastricts, file = file.path(datOut, 'Strictures', 'centipeda', 
                                         paste0(catchment, '_centipeda_strictures.rdata')))
  }
  
  if (returnR) {
    return(centipedastricts)
  } else {
    end_time <- Sys.time()
    elapsed <- end_time-start_time
    
    sumtab <- tibble::tibble(catchment,
                             npolys = nrow(anaes),
                             elapsed)
    
    return(sumtab)
  }
  
}
 
 
