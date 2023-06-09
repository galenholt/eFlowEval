

veg_yr_prep <- function(datOut, species = NULL, whichcrs = 3577) {
  # my intention was to make this take a species argument, but then I have to
  # `get` things, so I'm just leaving it as a placeholder and doing both
  # hardcoded Setup
  #
  # Directory to export TO- but we don't ever save? scriptOut <-
  # file.path('strictOut', 'vegetation', 'basin') if (!dir.exists(scriptOut))
  # {dir.create(scriptOut, recursive = TRUE)}
  
  # Catchments
  basinRef <- file.path(datOut, 'ANAEprocessed', 'ltimNoNorth.rdata')
  
  # For the metabolism, I brought in the driver data. I think I'll skip it here,
  # at least for now
  
  # directories with strictures This is different than metabolism- now these are
  # in lists of stars, and there are two species I want in together
  lippiaIn <- file.path(datOut, 'Strictures', 'lippia', 'basinConcat', 'catchmentAggregated.rdata')
  centipedaIn <- file.path(datOut, 'Strictures', 'centipeda', 'basinConcat', 'catchmentAggregated.rdata')
  
  # Read in the data from those folders 
  
  # Should make a function
  
  # The catchment boundaries
  load(basinRef)
  # Need to re-process the anaes
  ltimNoNorth <- st_transform(ltimNoNorth, 3577)
  
  # The stricture lists
  load(lippiaIn)
  load(centipedaIn)
  
  
  # Time aggregate 
  
  # The time steps and limits are all over the place, depending on what data is
  # available. We have daily for some things and bimonthly for others, but the
  # bimonthly goes back much further than the daily
  
  # I could bring everything to bimonthly with the short limits? Or I could just
  # go straight to years?
  
  # What metric should I use? As before (metabolism), the bimonth sort of enforces
  # a max, since the mean potentailly misses big events, and the sum isn't right.
  
  # I'm just going to do year, but I think doing bimonth would be pretty much the
  # same.
  
  # as in metabolism, kill the hours part of the time dim, but do it differnetly
  # here since they're in a list
  remove_hours <- function(x) {
    x <- st_set_dimensions(x, which = 'time',
                           values = as.Date(st_get_dimension_values(x, which = 'time')))
  }
  
  # only want to do this for those with dates (e.g. not the anaes).
  whichanaeL <- which(str_detect(names(lippia_stricts_catchment), '^anae') )
  whichanaeC <- which(str_detect(names(centipeda_stricts_catchment), '^anae') )
  
  lippia_stricts_catchment[-whichanaeL] <- purrr::map(lippia_stricts_catchment[-whichanaeL], remove_hours)
  centipeda_stricts_catchment[-whichanaeC] <- purrr::map(centipeda_stricts_catchment[-whichanaeC], remove_hours)
  
  ### AGG TO WATER YEAR AS IN METAB ###
  # Can I make simple annual reporting?
  interDates <- as.POSIXct(c("2014-06-30", "2015-06-30", "2016-06-30", "2017-06-30", "2018-06-30", "2019-06-30", "2020-06-30"))
  
  # when everything being summed is NA, sum(na.rm = TRUE) gives 0, but I need it
  # to be NA. So those functions (meanna, sumna, maxna) are defined in helpers.R
  
  # Use max for everything here, since the bimonthly data is defined at max extent
  
  # define fun for purring. It's just a bit too complicated to do anonymously
  aggandflip <- function(x) {
    y <- tempaggregate(x, by_t = as.Date(interDates), FUN = maxna, na.rm = TRUE) %>% 
      aperm(c('Shape', 'time'))
  }
  
  # Leave the ANAEs of the stars at this point- year-agging them doesn't make sense
  lippiayr <- lippia_stricts_catchment[-whichanaeL] %>% 
    purrr::map(aggandflip)
  
  centipedayr <- centipeda_stricts_catchment[-whichanaeC] %>% 
    purrr::map(aggandflip)
  
  ## TESTING- why is there MORE area with the germSurv than germ alone?
    # THIS SHOULD NOT BE NEGATIVE
  lippiayr$germ_Lippia[[1]] - lippiayr$germSurv_Lippia[[1]]
    # THIS SHOULD BE FALSE
  lippiayr$germSurv_Lippia[[1]] > as.numeric(st_area(st_as_sf(lippia_stricts_catchment$anae_Lippia)))
  lippiayr$germ_Lippia[[1]] > as.numeric(st_area(st_as_sf(lippia_stricts_catchment$anae_Lippia)))
  # There is *not* more area than with surv alone. 
  lippiayr$surv_Lippia[[1]] - lippiayr$germSurv_Lippia[[1]]
  # I cannot figure out how this is happening- the stricture is if germ, then
  # get surv area. So it makes no sense that it could be ADDING area. I think
  # I'll need to go back to the strictures and explicitly build bimonthly germ
  # and surv data, use that in the stricts, and return it in the list. Then
  # we'll be able to test directly. The only thing I can think is that somehow
  # that time rolling that happens stuffs thigns up. but its all limited to max,
  # and comes down to survival if germ, so there's no way to ADD area
  
  ## make dataframes for bar/line etc plots
  # can sometimes pipe in the purr, but it's fragile. Make a function
  catch_and_pos <- function(x) {
    sfandcatch(x, catches = ltimNoNorth, newname = names(x)) %>% 
      mutate(waterYear = lubridate::year(date),
             center = st_centroid(Shape),
             latpos = st_coordinates(center)[,2])
  }
  
  lippiadf <- lippiayr %>% 
    purrr::map(catch_and_pos)
  
  centipedadf <- centipedayr %>% 
    purrr::map(catch_and_pos)
  
  # NOW, but do it by left_joining them to the others In hindsight, I really
  # should have just left this an sf in vegCatchmentAggregate, and peeled it
  # back off here, since i have to peel it anyway
  lippiaanae <- lippia_stricts_catchment[[whichanaeL]] %>% 
    st_as_sf() 
  
  centipedaanae <- centipeda_stricts_catchment[[whichanaeC]] %>% 
    st_as_sf() 
  
  # ugh this is annoying that each stars (now sf) doesn't have the same name, so
  # have to do lots of acrosses
 lippiadf <- lippiadf %>% 
    purrr::map(\(x) st_join(x, lippiaanae, join = st_equals_exact, par = 1)) %>% 
   purrr::map(\(x) mutate(x,catchment_area = st_area(Shape))) %>% 
   purrr::map(\(x) mutate(x, 
                          across(starts_with('area'), \(y) as.numeric(y/catchment_area),
                                    .names = "proportion_catchment"),
                          across(starts_with('area'), \(y) as.numeric(y/wetland_area),
                                 .names = "proportion_wetlands"),
                          across(starts_with('area'), \(y) as.numeric(y/habitat_area),
                                 .names = "proportion_habitat")))
 
 centipedadf <- centipedadf %>% 
   purrr::map(\(x) st_join(x, centipedaanae, join = st_equals_exact, par = 1)) %>% 
   purrr::map(\(x) mutate(x,catchment_area = st_area(Shape))) %>% 
   purrr::map(\(x) mutate(x, 
                          across(starts_with('area'), \(y) as.numeric(y/catchment_area),
                                 .names = "proportion_catchment"),
                          across(starts_with('area'), \(y) as.numeric(y/wetland_area),
                                 .names = "proportion_wetlands"),
                          across(starts_with('area'), \(y) as.numeric(y/habitat_area),
                                 .names = "proportion_habitat")))
  
  # Can I make one big df for each? Should be able to with some cleanup. first,
  # I need to clean up names- the area and proportion cols need to be unique or
  # it tries to match on them
 lippiadfsimple <- lippiadf %>% 
   purrr::map(\(x) st_drop_geometry(x) %>% 
                select(date, ValleyName, area_passing = starts_with('area'), starts_with('proportion'))) %>% 
   bind_rows(.id = 'strict_level')
 
 # We do want some of the other info about the catchments, so add back on
 lippiadf_cat <- lippiadf$fullCycle_anae_lippia %>% 
   dplyr::select(date, ValleyName, waterYear, center, latpos, wetland_area, habitat_area, catchment_area) %>%
   left_join(lippiadfsimple)
 
 # Can I make one big df for each? Should be able to with some cleanup. first,
 # I need to clean up names- the area and proportion cols need to be unique or
 # it tries to match on them
 centipedadfsimple <- centipedadf %>% 
   purrr::map(\(x) st_drop_geometry(x) %>% 
                select(date, ValleyName, area_passing = starts_with('area'), starts_with('proportion'))) %>% 
   bind_rows(.id = 'strict_level')
 
 # We do want some of the other info about the catchments, so add back on
 centipedadf_cat <- centipedadf$fullCycle_anae_centipeda %>% 
   dplyr::select(date, ValleyName, waterYear, center, latpos, wetland_area, habitat_area, catchment_area) %>%
   left_join(centipedadfsimple)
   
 
 
 return(lst(lippia = lst(lippiayr, lippiadf, lippiadf_cat),
             centipeda = lst(centipedayr, centipedadf, centipedadf_cat)))
}


