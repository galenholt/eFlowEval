# Royal Spoonbill Strictures.

spoonbillstricts <- function(catchment, savefile = TRUE, returnR = FALSE) {
  
  start_time <- Sys.time()
  
  # Read in data ------------------------------------------------------------
  
  # read in Ramsar boundaries
  load(file.path(datOut, 'WetlandBoundaries', "ramsarBoundsMDB.rdata"))
  
  # load the ltim valleys
  load(file.path(datOut, 'ANAEprocessed', 'ltimNoNorth.rdata'))
  ltimNoNorth <- ltimNoNorth %>% st_transform(st_crs(ramsarBoundsMDB))
  
  catchpoly <- ltimNoNorth |> 
    filter(stringr::str_remove_all(ValleyName, ' ') == catchment)
  
  ramsarBoundCatchment <- st_intersection(ramsarBoundsMDB, catchpoly)
  
  # ANAE 'preferences' ranked from most to least times observed
  # McGinness, Langston and Brooks (2020) VEWH Prioritisation Project: Stage 2 Final Report
  # Royal Spoonbill (Platalea regia) requirements, distribution and habitat mapping
  breedANAEcodes <- read.table(
    file.path(datDir, "dataStrict/BreedingANAERoyalSpoonBill.txt"), 
    skip = 2, sep = ":", header = TRUE)
  forageANAEcodes <- read.table(
    file.path(datDir, "dataStrict/ForagingANAERoyalSpoonBill.txt"), 
    skip = 2, sep = ":", header = TRUE)
  
  
  ## Areas of each ANAE meeting depth stricture
  
  # Breeding
  breed_names <- c(paste0(catchment, "_areaSpoonbillBreed"),
                   paste0(catchment, '_areaSpoonbillBreed_index'))
  
  breed_path <- file.path(datOut, 'Inundationprocessed', 'areaSpoonbillBreed',
                         paste0(catchment, '_areaSpoonbillBreed.rdata'))
  
  breedArea <- load_rename(filepath = breed_path, 
                          knownnames = breed_names,
                          newnames = c('aggdata', 'indices'))
  
  
  # Foraging
  forage_names <- c(paste0(catchment, "_areaSpoonbillForage"),
                   paste0(catchment, '_areaSpoonbillForage_index'))
  
  forage_path <- file.path(datOut, 'Inundationprocessed', 'areaSpoonbillForage',
                          paste0(catchment, '_areaSpoonbillForage.rdata'))
  
  forageArea <- load_rename(filepath = forage_path, 
                           knownnames = forage_names,
                           newnames = c('aggdata', 'indices'))
  
  # The ANAEs themselves
  anae_names <- paste0(catchment, 'ANAE')
  anae_path <- file.path(datOut, 'ANAEprocessed', paste0(catchment, 'ANAE.rdata'))
  anaes <- load_rename(filepath = anae_path,
                       returnOne = anae_names) %>% 
    st_transform(st_crs(breedArea$aggdata)) %>% 
    st_make_valid()
  # 
  
  # Line everything up ------------------------------------------------------
  
  # This error-checking should be offloaded to a function. `test_anae_agg` is
  # nearly it, but is designed to check everything, not to return cleaned data.
  # Easy to port over with mods though.
  
  # Check the datasets. `matchStarsIndex` *should* be sufficient, but there was
  # a change in `sf` that means some ANAEs are no longer good, but that only
  # affects more recent aggregations, which drop the bad ones. So, we need to
  # first check that the same set of polygons are in each dataset before making
  # sure they're in the right order. And we have to do it out here, because we
  # need *all* datasets to match- there's no way to, say, cut soilTemp based on
  # inunSurv in matchStarsIndex.
  
  commonUID <- Reduce(intersect, list(anaes$UID, 
                                      breedArea$indices$UID, 
                                      forageArea$indices$UID))
  
  # typically would just logical-index rather than which, but we need to apply
  # it to the aggdata to match the indices
  # I have tested that works.
  anaedrop <- which(!(anaes$UID %in% commonUID))
  breeddrop <- which(!(breedArea$indices$UID %in% commonUID))
  foragedrop <- which(!(forageArea$indices$UID %in% commonUID))
  
  # This all has to be conditional- a length-0 set drops everything.
  if(length(anaedrop) > 0) {anaes <- anaes[-anaedrop, ]}
  
  if (length(breeddrop) > 0) {
    breedArea$aggdata <- breedArea$aggdata[,-breeddrop, ]
    breedArea$indices <- breedArea$indices[-breeddrop, ]
  }
  
  if (length(foragedrop) > 0) {
    forageArea$aggdata <- forageArea$aggdata[,-foragedrop, ]
    forageArea$indices <- forageArea$indices[-foragedrop, ]
  }
  
  # Breeding
  breedArea <- matchStarsIndex(index1 = anaes, stars1 = NULL,
                              index2 = breedArea$indices, stars2 = breedArea$aggdata,
                              indexcol = c(1, 1), testfinal = FALSE, return1 = TRUE)
  names(breedArea) <- c('indices_anae', 'indices', 'aggdata')
  anaeba <- breedArea$indices_anae
  
  # forage
  forageArea <- matchStarsIndex(index1 = anaes, stars1 = NULL,
                               index2 = forageArea$indices, stars2 = forageArea$aggdata,
                               indexcol = c(1, 1), testfinal = FALSE, return1 = TRUE)
  names(forageArea) <- c('indices_anae', 'indices', 'aggdata')
  anaefa <- forageArea$indices_anae
  
  if (!all(anaeba$UID == anaefa$UID)) {
    rlang::abort("something is wrong with the sorting")
  }
  # Cut the anaes list off
  breedArea <- breedArea[-1]
  forageArea <- forageArea[-1]
  # but keep them and use the cleaned versions
  anaes <- anaeba
  
  # Clean up rounding errors with area
  breedArea$aggdata <- clean_area(breedArea$aggdata, anaes)
  forageArea$aggdata <- clean_area(forageArea$aggdata, anaes)
  
  # Clean up the units
  names(breedArea$aggdata) <- 'Area'
  names(forageArea$aggdata) <- 'Area'
  
  ### STRICTURE CALCS ###
  
  # Inundation for Breeding stricture ---------------------------------------
  
  # Areas of each ANAE meeting Breeding depth stricture. Bimonthly timestep 
  # No point having one anae meeting depth strictures is it is not big enough
  # to support a colony.
  
  
  # Identidy TRUE/FALSE which polys have breed habitat
  breedANAE <-(anaes$ANAE_CODE %in% breedANAEcodes$ANAE_CODE) #class logical i.e. no index, geometry
  
  # turn non-breeding-suitable polys to zero, other values still areas
  breedANAEstrict <- breedArea$aggdata*breedANAE
  
  
  # limit to wetland boundaries
  
  ramsarTF <- st_covered_by(((breedANAEstrict)), 
                            (st_buffer(ramsarBoundsMDB,250))) %>% lengths > 0
  
  ramsarStrict <- breedANAEstrict*ramsarTF
  
  # polygons wet for three consecutive bimonths in breeding season. 
  # Test by summing the three and checking it equals 3.
  
  timeStrict <- ramsarStrict > 0  #  areas over 0 = TRUE
  
  timeStrict[[1]] <- timeRoll(timeStrict[[1]], 
                              FUN = RcppRoll::roll_sum, 
                              rolln = 3, 
                              align = 'right',
                              na.rm = TRUE)
  
  timeStrict[[1]] <- timeStrict$Area == 3
  
  seasonStrict <- timeStrict
  
  times <- st_get_dimension_values(seasonStrict, "time")
  
  #  for southern basin there is only one 6month period so the 'time' must be march. 
  # TODO for wetlands north of macquarie marshes check dates are within breeding season. lubridate interval?
  breedTimes <- which(lubridate::month(times)== 03)
  
  seasonStrict[[1]] <- 0
  seasonStrict[[1]][,breedTimes] <- 1 
  
  seasonStrict <- seasonStrict*timeStrict #  Times and polygon combinations where 3 bimonthly and season strictures are TRUE
  
  # get max inundation for each 6 month (3 x bimonthly period)
  breedStrict <- breedArea$aggdata
  
  breedStrict[[1]] <- timeRoll(breedStrict[[1]], 
                              FUN = RcppRoll::roll_max, 
                              rolln = 3, 
                              align = 'right',
                              na.rm = TRUE)
  
  breedStrict <- breedStrict*seasonStrict #max area of inundation across 3 bimos in the breeding season 
  
  # sum area up to wetland complex scale
  breedAreaByWetland <- aggregate(breedStrict, ramsarBoundCatchment, sumna)
  
  
  # test against minimum total area inundated threshold.
  
  
  # calculate historical max area of passing breeding strictures
  
  by_t <- dim(breedAreaByWetland)["time"]*2 #number of months in dataset
  maxWetlandBreedArea <- tempaggregate(breedAreaByWetland, 
                       by_t = paste(by_t,"months"), 
                       FUN = maxna, dates_end_interval = TRUE) %>% 
    aperm(c('geometry', 'time'))
  
  areaPC <- 0.7 # proportion of max inundation as threshold for breeding
  
  breedAreaStricture <- breedAreaByWetland
  
  breedAreaStricture[[1]] <- breedAreaByWetland[[1]] > 
    areaPC*matrix(rep(maxWetlandBreedArea[[1]], 
                      dim(breedAreaByWetland)[2]), 
                  ncol = dim(breedAreaByWetland)[2])

  # --- foraging ----
  
  # Identify TRUE/FALSE which polys have breed habitat
  forageANAE <-(anaes$ANAE_CODE %in% forageANAEcodes$ANAE_CODE) #class logical i.e. no index, geometry
  
  # turn non-foraging-suitable polys to zero, other values still areas
  forageANAEstrict <- forageArea$aggdata*forageANAE
  
  # aggregate into ramsarbounds
  
  # forage area available in each wetland (ramsar) boundary over time
  forageAreaByWetland <- aggregate(forageANAEstrict, ramsarBoundCatchment, sumna)
  
  buffer_ramsar <- st_buffer(ramsarBoundCatchment, 10000)
  
  
  forageAreaBuffer <- aggregate(forageANAEstrict, buffer_ramsar, sumna)
  # 
  # put Area from buffered area into original geometry
  forageAreaByWetlandBuffer <- forageAreaByWetland
  forageAreaByWetlandBuffer[[1]] <- forageAreaBuffer[[1]]
  
  # calculate historical max area of inundation matching foraging habitat
  
  by_t <- dim(forageAreaByWetland)["time"]*2 #number of months in dataset

  maxWetlandForageArea <- tempaggregate(forageAreaByWetlandBuffer, 
                                      by_t = paste(by_t,"months"), 
                                      FUN = maxna, dates_end_interval = TRUE) %>% 
    aperm(c('geometry', 'time'))
  
  
  # proportion of max forage area (including buffer) as threshold for foraging
  
  forageAreaPC <- 0.01
  
  forageAreaStricture <- forageAreaByWetland
  forageAreaStricture[[1]] <- forageAreaByWetlandBuffer[[1]] > 
    forageAreaPC*matrix(rep(maxWetlandForageArea[[1]], 
                            dim(forageAreaByWetland)[2]), 
                        ncol = dim(forageAreaByWetland)[2])

  # sum(forageAreaStricture$Area) # 1% is 183, 2% is 121

  # combine breeding and foraging strictures
  
  forageBreedStricture <- breedAreaStricture*forageAreaStricture
  sum(forageBreedStricture$Area, na.rm = T) #
  
  # Do we really need to save all these?
  spoonbillstricts <- tibble::lst(breedStrict, 
                                  forageAreaByWetland, 
                                  forageAreaBuffer,
                                  forageAreaByWetlandBuffer,
                                  breedAreaByWetland,
                                  forageANAEstrict, 
                                  breedANAEstrict,
                                  breedAreaStricture, 
                                  forageAreaStricture, 
                                  forageBreedStricture)
  
  
  # save and return
  if (savefile) {
    if (!dir.exists(file.path(datOut, 'Strictures', 'Spoonbill'))) {
      dir.create(file.path(datOut, 'Strictures', 'Spoonbill'), recursive = TRUE)
    }
    # The objects needed for the plots. should be cleaned up into a list and saved as RDS
    save(spoonbillstricts,
         file = file.path(datOut, 'Strictures', 'Spoonbill', 
                          paste0(catchment, '_spoonbill_strictures.rdata')))
  }
  
  if (returnR) {
    return(spoonbillstricts)
  } else {
    end_time <- Sys.time()
    elapsed <- end_time-start_time
    
    sumtab <- tibble::tibble(catchment,
                             npolys = nrow(anaes),
                             elapsed)
    
    return(sumtab)
  }
  
  
}





















