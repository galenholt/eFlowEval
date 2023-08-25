setup_local_veg <- function(sub_poly, catchment = 'auto', species = NULL,
                            whichcrs = 3577, interDates = 'default',
                            catchsf = NULL, datOut = 'datOut') {
  
  # Top bit same as metabPlotSetup_local- should be its own function.
  if (interDates == 'default') {
    interDates <- c("2014-06-30", "2015-06-30", "2016-06-30", "2017-06-30", "2018-06-30", "2019-06-30")
  }
  
  # If the sub_poly is actually a bunch of polys, make it one
  sub_poly <- dplyr::summarise(sub_poly) %>% 
    crscheck(whichcrs)
  
  # allow auto-catchment from the sub_poly
  if (catchment == 'auto') {
    # this is dangerous- we should send in the path without relying on knowing datOut
    # Now we can at least send an sf
    if (is.null(catchsf)) {
      catchpath <- file.path(datOut, 'ANAEprocessed', 'ltimNoNorth.rdata')
      catchsf <- load_rename(catchpath, returnOne ='ltimNoNorth')
    }
    
    catchsf <- catchsf %>% 
      crscheck(whichcrs)
    
    catchment <- catchsf[which(st_intersects(sub_poly, 
                                             catchsf, sparse = FALSE)),
                         'ValleyName'] %>% 
      st_drop_geometry() %>% 
      stringr::str_remove_all(' ')
    
    if (length(catchment) > 1) {
      # Choose the biggest intersection
      inter <- st_intersection(sub_poly, catchsf) %>% 
        dplyr::mutate(area = st_area(geometry)) %>% 
        filter(area == max(area))
      
      rlang::warn(glue::glue("sub_poly overlaps with multiple catchments
                              ({paste0(catchment, collapse = ', ')}).
                              At present there is no capacity to process multiple catchments,
                              so using the one with largest overlap 
                             ({inter$ValleyName})"))
      
      catchment <- inter$ValleyName %>% 
        stringr::str_remove_all(' ')
    }
  }
  
  # We'll crop data as it comes in, so set up a little function here. Defining it here means it will just find the crs and sub_poly
  # stars and sf need to be cropped differently
  crop_to_sub <- function(x) {
    x <- crscheck(x, whichcrs)
    if (inherits(x, 'stars')) {
      x <- x[sub_poly]
    }
    if (inherits(x, 'sf')) {
      x <- st_crop(x, sub_poly)
    }
    return(x)
  }
  
  # Now copying from the basin but hitting the local data
  
  
  # only load for the relevant catchment
  
  # This is different than metabolism- now these are
  # in lists of stars, and there are two species I want in together
  lippiaIn <- file.path(datOut, 'Strictures', 'lippia', 
                        paste0(catchment, '_lippia_strictures.rdata'))
  centipedaIn <- file.path(datOut, 'Strictures', 'centipeda', 
                           paste0(catchment, '_centipeda_strictures.rdata'))
  
  # Read in the data from those folders 
  
  # Should make a function
  # The stricture lists
  # These are NOT named by catchment, which makes it MUCH easier to be catchment-agnostic.
  load(lippiaIn) # name `lippiastricts`
  load(centipedaIn) # name `centipedastricts`
  
  # crop before we do anything else
  lippiastricts <- purrr::map(lippiastricts, crop_to_sub)
  centipedastricts <- purrr::map(centipedastricts, crop_to_sub)
  
  # Make hour times into dates, as in basin version
  remove_hours <- function(x) {
    x <- st_set_dimensions(x, which = 'time',
                           values = as.Date(st_get_dimension_values(x, which = 'time')))
  }
  
  # only want to do this for those with dates (e.g. not the anaes).
  whichanaeL <- which(str_detect(names(lippiastricts), '^anae') )
  whichanaeC <- which(str_detect(names(centipedastricts), '^anae') )
  
  lippiastricts[-whichanaeL] <- purrr::map(lippiastricts[-whichanaeL], remove_hours)
  centipedastricts[-whichanaeC] <- purrr::map(centipedastricts[-whichanaeC], remove_hours)
  
  ### AGG TO WATER YEAR ###
  # Can I make simple annual reporting?
  interDates <- as.POSIXct(c("2014-06-30", "2015-06-30", "2016-06-30", "2017-06-30", "2018-06-30", "2019-06-30", "2020-06-30"))
  
  # when everything being summed is NA, sum(na.rm = TRUE) gives 0, but I need it
  # to be NA. So those functions (meanna, sumna, maxna) are defined in helpers.R
  
  # Use max for everything here, since the bimonthly data is defined at max extent
  
  # define fun for purring. It's just a bit too complicated to do anonymously
  aggandflip <- function(x) {
    y <- tempaggregate(x, by_t = as.Date(interDates), FUN = maxna, na.rm = TRUE) %>% 
      aperm(c('geometry', 'time'))
  }
  
  # Leave the ANAEs of the stars at this point- year-agging them doesn't make sense
  lippiayr <- lippiastricts[-whichanaeL] %>% 
    purrr::map(aggandflip)
  
  centipedayr <- centipedastricts[-whichanaeC] %>% 
    purrr::map(aggandflip)
  
  # The basin scale goes on to make a bunch of dataframes. I think I'll do that
  # here too- they'll be easier to plot with.
  
  # Do I want to do this yearly? Or for everything?
  # I think both, actually. I should do it with functions.
  lippiadf <- make_veg_df(lippiastricts, whichanaeL) %>% 
    clean_lippia_levels()
  
  centipedadf <- make_veg_df(centipedastricts, whichanaeC) %>% 
    clean_centipeda_levels()
  
  lippiayrdf <- make_veg_df(lippiayr, whichanaeL) %>% 
    clean_lippia_levels()
  
  centipedayrdf <- make_veg_df(centipedayr, whichanaeC) %>% 
    clean_centipeda_levels()
  
  # lippiadf <- lippiastricts[-whichanaeL] %>% 
  #   purrr::map(\(x) sfandcatch(x, newname = names(x)))
  
  # centipedadf <- centipedastricts[-whichanaeC] %>% 
  #   purrr::map(\(x) sfandcatch(x, newname = names(x)))
  
  # lippiaanae <- lippiastricts[[whichanaeL]]
  
  # centipedaanae <- centipedastricts[[whichanaeC]]
  
  # TO join I'll need to give them unique names.
  # Put a binary pass/fail on there too
  # lippiadf <- purrr::imap(lippiadf, \(x, idx) dplyr::mutate(x, strict_level = idx) %>% 
  #                           select(strict_level, date, area_passing = starts_with('area'))) %>% 
  #   bind_rows() %>% 
  #   dplyr::mutate(passed = area_passing > 0,
  #                 area = st_area(geometry)) %>% 
  #   filter(strict_level != 'surv_Lippia') %>% # Remove this here, since it's out of sequence. Still available in the stars if we want it.
  #   # factors with good names- from basin
  #   mutate(strict_level_F = case_when(strict_level == 'germ_Lippia' ~ "Germ",
  #                                     strict_level == 'germSurv_Lippia' ~ "Germ and surv",
  #                                     strict_level == 'fullCycle_anae_lippia' ~ "Germ, surv, habitat")) %>% 
  #   mutate(strict_level_F = factor(strict_level_F, 
  #                                  levels = c('Germ', 
  #                                             'Germ and surv',
  #                                             'Germ, surv, habitat') ))
  # 
  # centipedadf <- purrr::imap(centipedadf, \(x, idx) dplyr::mutate(x, strict_level = idx) %>% 
  #                              select(strict_level, date, area_passing = starts_with('area'))) %>% 
  #   bind_rows() %>% 
  #   dplyr::mutate(passed = area_passing > 0,
  #                 area = st_area(geometry)) %>%
  #   mutate(strict_level_F = case_when(strict_level == 'seedsurv_centipeda' ~ "Seed surv",
  #                                     strict_level == 'seedsurv_germ_centipeda' ~ 'Seed surv and germ',
  #                                     strict_level == 'seedsurv_germ_fruit_centipeda' ~ 'Seed surv, germ, fruit',
  #                                     strict_level == 'fullCycle_anae_centipeda' ~ 'Seed surv, germ, fruit, habitat',
  #                                     strict_level == 'fullCycle_lippiaLimit_centipeda' ~ 'All and lippia competition')) %>% 
  #   mutate(strict_level_F = factor(strict_level_F, 
  #                                  levels = c('Seed surv',
  #                                             'Seed surv and germ',
  #                                             'Seed surv, germ, fruit',
  #                                             'Seed surv, germ, fruit, habitat',
  #                                             'All and lippia competition') ))
  
  # Almost there3....
  
  return(tibble::lst(lippiastricts,
                     centipedastricts,
                     
                     lippiayr,
                     centipedayr,
                     
                     lippiadf,
                     centipedadf,
                     
                     lippiayrdf,
                     centipedayrdf))
  
}


make_veg_df <- function(starsObj, removeidx) {
  sfObj <- starsObj[-removeidx] %>% 
    purrr::map(\(x) sfandcatch(x, newname = names(x))) %>% 
    purrr::imap(\(x, idx) dplyr::mutate(x, strict_level = idx) %>% 
                  select(strict_level, date, area_passing = starts_with('area'))) %>% 
    bind_rows() %>% 
    dplyr::mutate(passed = area_passing > 0,
                  area = st_area(geometry))
  return(sfObj)
}

clean_lippia_levels <- function(lippiadf) {
  lippiadf <- lippiadf %>% 
    filter(strict_level != 'surv_Lippia') %>% # Remove this here, since it's out of sequence. Still available in the stars if we want it.
    # factors with good names- from basin
    mutate(strict_level_F = case_when(strict_level == 'germ_Lippia' ~ "Germ",
                                      strict_level == 'germSurv_Lippia' ~ "Germ and surv",
                                      strict_level == 'fullCycle_anae_lippia' ~ "Germ, surv, habitat")) %>% 
    mutate(strict_level_F = factor(strict_level_F, 
                                   levels = c('Germ', 
                                              'Germ and surv',
                                              'Germ, surv, habitat') ))
  return(lippiadf)
}

clean_centipeda_levels <- function(centipedadf) {
  centipedadf <- centipedadf %>% 
    mutate(strict_level_F = case_when(strict_level == 'seedsurv_centipeda' ~ "Seed surv",
                                      strict_level == 'seedsurv_germ_centipeda' ~ 'Seed surv and germ',
                                      strict_level == 'seedsurv_germ_fruit_centipeda' ~ 'Seed surv, germ, fruit',
                                      strict_level == 'fullCycle_anae_centipeda' ~ 'Seed surv, germ, fruit, habitat',
                                      strict_level == 'fullCycle_lippiaLimit_centipeda' ~ 'All and lippia competition')) %>% 
    mutate(strict_level_F = factor(strict_level_F, 
                                   levels = c('Seed surv',
                                              'Seed surv and germ',
                                              'Seed surv, germ, fruit',
                                              'Seed surv, germ, fruit, habitat',
                                              'All and lippia competition') ))
  return(centipedadf)
           
}