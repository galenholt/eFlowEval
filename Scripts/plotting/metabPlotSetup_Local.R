# local plots of metabolism (and maybe a few other static plots)


setup_local_metab <- function(sub_poly, catchment = 'auto', whichcrs = 3577, interDates = 'default',
                              catchsf = NULL, datOut = 'datOut') {
  
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
  crop_to_sub <- function(x) {
    x <- crscheck(x, whichcrs)
    x[sub_poly]
  }
  
  ### Load driver data
  tempdir <- file.path(datOut, 'Tempprocessed', 'weightedMean', 'bimonth')
  climdir <- file.path(datOut, 'Climateprocessed', 'weightedMeanCLIM', 'bimonth')
  inundir <- file.path(datOut, 'Inundationprocessed', 'volInun')
  inun10pdir <- file.path(datOut, 'Inundationprocessed', 'vol10p')
  
  # Read in. This depends on our standard naming conventions
    # This should be loopable or purrable or something, but for now i just want to get it general.
  # We do some standard processing here:
  # standardize name
  # set crs and crop to the subpoly
  # set dimensions to dates instead of times
  temp_anae <- file.path(tempdir, paste0(catchment, '_TempBimonthMean.rdata')) |>
    load_rename(returnOne = paste0(catchment, '_TempBimonthMean')) %>% 
    crop_to_sub() %>% 
    st_set_dimensions(which = 'time',
                      values = as.Date(st_get_dimension_values(., which = 'time')))
  
  clim_anae <- file.path(climdir, paste0(catchment, '_ClimateBimonthMean.rdata')) |>
    load_rename(returnOne = paste0(catchment, '_ClimateBimonthMean')) %>% 
    crop_to_sub() %>% 
    st_set_dimensions(which = 'time',
                      values = as.Date(st_get_dimension_values(., which = 'time')))
  
  # The inundation has an index, but we never use it
  inun_anae <- file.path(inundir, paste0(catchment, '_volInun.rdata')) |>
    load_rename(returnOne = paste0(catchment, '_volInun')) %>% 
    crop_to_sub() %>% 
    st_set_dimensions(which = 'time',
                      values = as.Date(st_get_dimension_values(., which = 'time')))
  
  inun10p_anae <- file.path(inun10pdir, paste0(catchment, '_vol10p.rdata')) |>
    load_rename(returnOne = paste0(catchment, '_vol10p')) %>% 
    crop_to_sub() %>% 
    st_set_dimensions(which = 'time',
                      values = as.Date(st_get_dimension_values(., which = 'time')))
  
  
  ### Load responses
  # baseline
  preddirER <- file.path(datOut, 'TempAndProduction', 'Predictions', 'logERdaysvalleys',
                         'bimonth', 'predictxvol')
  preddirGPP <- file.path(datOut, 'TempAndProduction', 'Predictions', 'logGPPdaysvalleys',
                          'bimonth', 'predictxvol')
  # 10p inundation
  preddirER_10p <- file.path(datOut, 'TempAndProduction', 'Predictions', 'logERdaysvalleys',
                             'bimonth', 'predictxvol10p')
  preddirGPP_10p <- file.path(datOut, 'TempAndProduction', 'Predictions', 'logGPPdaysvalleys',
                              'bimonth', 'predictxvol10p')
  # climate plus 2C
  preddirER_clim <- file.path(datOut, 'ClimateAndProduction', 'Predictions', 'logERdaysvalleys',
                              'bimonth', 'predictxvol')
  preddirGPP_clim <- file.path(datOut, 'ClimateAndProduction', 'Predictions', 'logGPPdaysvalleys',
                               'bimonth', 'predictxvol')
  # climate plus 2C AND the 10p inundation
  preddirER_clim10p <- file.path(datOut, 'ClimateAndProduction', 'Predictions', 'logERdaysvalleys',
                                 'bimonth', 'predictxvol10p')
  preddirGPP_clim10p <- file.path(datOut, 'ClimateAndProduction', 'Predictions', 'logGPPdaysvalleys',
                                  'bimonth', 'predictxvol10p')
  
  ## The loading
  predER <- file.path(preddirER, paste0(catchment, '_logERdaysvalleys_PredictxVol.rdata')) |>
    load_rename(returnOne = paste0(catchment, '_logERdaysvalleys_PredictxVol')) %>% 
    crop_to_sub() %>% 
    st_set_dimensions(which = 'time',
                      values = as.Date(st_get_dimension_values(., which = 'time')))
  
  predGPP <- file.path(preddirGPP, paste0(catchment, '_logGPPdaysvalleys_PredictxVol.rdata')) |>
    load_rename(returnOne = paste0(catchment, '_logGPPdaysvalleys_PredictxVol')) %>% 
    crop_to_sub() %>% 
    st_set_dimensions(which = 'time',
                      values = as.Date(st_get_dimension_values(., which = 'time')))
  
  predER_10p <- file.path(preddirER_10p, paste0(catchment, '_logERdaysvalleys_PredictxVol10p.rdata')) |>
    load_rename(returnOne = paste0(catchment, '_logERdaysvalleys_PredictxVol10p')) %>% 
    crop_to_sub() %>% 
    st_set_dimensions(which = 'time',
                      values = as.Date(st_get_dimension_values(., which = 'time')))
  
  predGPP_10p <- file.path(preddirGPP_10p, paste0(catchment, '_logGPPdaysvalleys_PredictxVol10p.rdata')) |>
    load_rename(returnOne = paste0(catchment, '_logGPPdaysvalleys_PredictxVol10p')) %>% 
    crop_to_sub() %>% 
    st_set_dimensions(which = 'time',
                      values = as.Date(st_get_dimension_values(., which = 'time')))
  
  predER_clim <- file.path(preddirER_clim, paste0(catchment, '_logERdaysvalleys_PredictxVol.rdata')) |>
    load_rename(returnOne = paste0(catchment, '_logERdaysvalleys_PredictxVol')) %>% 
    crop_to_sub() %>% 
    st_set_dimensions(which = 'time',
                      values = as.Date(st_get_dimension_values(., which = 'time')))
  
  predGPP_clim <- file.path(preddirGPP_clim, paste0(catchment, '_logGPPdaysvalleys_PredictxVol.rdata')) |>
    load_rename(returnOne = paste0(catchment, '_logGPPdaysvalleys_PredictxVol')) %>% 
    crop_to_sub() %>% 
    st_set_dimensions(which = 'time',
                      values = as.Date(st_get_dimension_values(., which = 'time')))
  
  predER_10p_clim <- file.path(preddirER_10p, paste0(catchment, '_logERdaysvalleys_PredictxVol10p.rdata')) |>
    load_rename(returnOne = paste0(catchment, '_logERdaysvalleys_PredictxVol10p')) %>% 
    crop_to_sub() %>% 
    st_set_dimensions(which = 'time',
                      values = as.Date(st_get_dimension_values(., which = 'time')))
  
  predGPP_10p_clim <- file.path(preddirGPP_clim10p, paste0(catchment, '_logGPPdaysvalleys_PredictxVol10p.rdata')) |>
    load_rename(returnOne = paste0(catchment, '_logGPPdaysvalleys_PredictxVol10p')) %>% 
    crop_to_sub() %>% 
    st_set_dimensions(which = 'time',
                      values = as.Date(st_get_dimension_values(., which = 'time')))
  
  # UNITS
  
  # Temps are in C
  
  # Volumes are in m^3.
  # GPP and ER have been multiplied by volume. So any shifts in
  # vol units should be mirrored in those units
  #
  # GPP is in mg O_2 L^{-1} day^{-1} . but the predictions have been multiplied by vol, but at the per liter rate, yielding mg02/D but 1,000x too little
  # ER is in mg O_2 L^{-1} day^{-1} 
  
  # That per day part is tricky. I could multiply by days in the period, but the
  # inundation is a max extent. SO maybe I'll just say it's the daily rate at the
  # max extent. Without knowing filling patterns that'll work out to just be
  # exactly the same as 1/60th the value of multiplying it out
  
  # a m3 is 0.001 of a megaliter (1,000 vs 1,000,000 liters)
  inun_anae <- inun_anae * 0.001
  names(inun_anae) <- 'volumeML'
  
  inun10p_anae <- inun10p_anae * 0.001
  names(inun10p_anae) <- 'volumeML'
  
  # Now the predictions
  
  # Should really list these up and purrr or for or something. But first let's just get everything to work again
  predER <- predER *
    1000 * # because it had multiplied per L estimates by m3, and there are 1,000 L. Now we're in mg02/day
    0.000001 # convert from mg to kg. If convert to g would be 0.001 and effectively reverse the above
  
  names(predER) <- 
    str_replace(names(predER),
                pattern = 'predict_x_vol_ERdaysvalleys', 
                replacement = 'predicted_ER_kg02perday_at_max_inun')
  
  predGPP <- predGPP *
    1000 * # because it had multiplied per L estimates by m3, and there are 1,000 L. Now we're in mg02/day
    0.000001 # convert from mg to kg. If convert to g would be 0.001 and effectively reverse the above
  
  names(predGPP) <- 
    str_replace(names(predGPP),
                pattern = 'predict_x_vol_GPPdaysvalleys', 
                replacement = 'predicted_GPP_kg02perday_at_max_inun')
  
  # 2C warming
  predER_clim <- predER_clim *
    1000 * # because it had multiplied per L estimates by m3, and there are 1,000 L. Now we're in mg02/day
    0.000001 # convert from mg to kg. If convert to g would be 0.001 and effectively reverse the above
  
  names(predER_clim) <- 
    str_replace(names(predER_clim),
                pattern = 'predict_x_vol_ERdaysvalleys', 
                replacement = 'predicted_ER_kg02perday_at_max_inun')
  
  predGPP_clim <- predGPP_clim *
    1000 * # because it had multiplied per L estimates by m3, and there are 1,000 L. Now we're in mg02/day
    0.000001 # convert from mg to kg. If convert to g would be 0.001 and effectively reverse the above
  
  names(predGPP_clim) <- 
    str_replace(names(predGPP_clim),
                pattern = 'predict_x_vol_GPPdaysvalleys', 
                replacement = 'predicted_GPP_kg02perday_at_max_inun')
  
  # PLUS 10 PERCENT
  predER_10p <- predER_10p *
    1000 * # because it had multiplied per L estimates by m3, and there are 1,000 L. Now we're in mg02/day
    0.000001 # convert from mg to kg. If convert to g would be 0.001 and effectively reverse the above
  
  names(predER_10p) <- 
    str_replace(names(predER_10p),
                pattern = 'predict_x_vol_ERdaysvalleys', 
                replacement = 'predicted_ER_kg02perday_at_max_inun')
  
  predGPP_10p <- predGPP_10p *
    1000 * # because it had multiplied per L estimates by m3, and there are 1,000 L. Now we're in mg02/day
    0.000001 # convert from mg to kg. If convert to g would be 0.001 and effectively reverse the above
  
  names(predGPP_10p) <- 
    str_replace(names(predGPP_10p),
                pattern = 'predict_x_vol_GPPdaysvalleys', 
                replacement = 'predicted_GPP_kg02perday_at_max_inun')
  
  # 2C and 10p
  predER_10p_clim <- predER_10p_clim *
    1000 * # because it had multiplied per L estimates by m3, and there are 1,000 L. Now we're in mg02/day
    0.000001 # convert from mg to kg. If convert to g would be 0.001 and effectively reverse the above
  
  names(predER_10p_clim) <- 
    str_replace(names(predER_10p_clim),
                pattern = 'predict_x_vol_ERdaysvalleys', 
                replacement = 'predicted_ER_kg02perday_at_max_inun')
  
  predGPP_10p_clim <- predGPP_10p_clim *
    1000 * # because it had multiplied per L estimates by m3, and there are 1,000 L. Now we're in mg02/day
    0.000001 # convert from mg to kg. If convert to g would be 0.001 and effectively reverse the above
  
  names(predGPP_10p_clim) <- 
    str_replace(names(predGPP_10p_clim),
                pattern = 'predict_x_vol_GPPdaysvalleys', 
                replacement = 'predicted_GPP_kg02perday_at_max_inun')
  
  # annual reporting --------------------------------------------------------
  
  # interDates <- as.POSIXct(c("2014-06-30", "2015-06-30", "2016-06-30", "2017-06-30", "2018-06-30", "2019-06-30"))
  
  # Need aperms to get geometry and time badk in the right order
  
  tempannual <- tempaggregate(temp_anae, by_t = as.Date(interDates), 
                              FUN = mean, na.rm = TRUE) %>%
    aperm(c(2,1))
  inunannual <- tempaggregate(inun_anae, by_t = as.Date(interDates), 
                              FUN = max, na.rm = TRUE)  %>%   aperm(c(2,1))
  
  climannual <- tempaggregate(clim_anae, by_t = as.Date(interDates), 
                              FUN = mean, na.rm = TRUE)  %>%   aperm(c(2,1))
  inun10pannual <- tempaggregate(inun10p_anae, by_t = as.Date(interDates), 
                                 FUN = max, na.rm = TRUE)  %>%   aperm(c(2,1))
  
  # Baseline predictions
  gppannual <- tempaggregate(predGPP[1,,], by_t = as.Date(interDates), 
                             FUN = max, na.rm = TRUE)  %>%   aperm(c(2,1))
  erannual <- tempaggregate(predER[1,,], by_t = as.Date(interDates), 
                            FUN = max, na.rm = TRUE)  %>%   aperm(c(2,1))
  
  # 2C
  gppannual_clim <- tempaggregate(predGPP_clim[1,,], by_t = as.Date(interDates), 
                                  FUN = max, na.rm = TRUE)  %>%   aperm(c(2,1))
  erannual_clim <- tempaggregate(predER_clim[1,,], by_t = as.Date(interDates), 
                                 FUN = max, na.rm = TRUE)  %>%   aperm(c(2,1))
  
  # 10% inundation
  gppannual_10p <- tempaggregate(predGPP_10p[1,,], by_t = as.Date(interDates), 
                                FUN = max, na.rm = TRUE)  %>%   aperm(c(2,1))
  erannual_10p <- tempaggregate(predER_10p[1,,], by_t = as.Date(interDates), 
                               FUN = max, na.rm = TRUE)  %>%   aperm(c(2,1))
  
  # both
  gppannual_10p_clim <- tempaggregate(predGPP_10p_clim[1,,], by_t = as.Date(interDates), 
                                     FUN = max, na.rm = TRUE)  %>%   aperm(c(2,1))
  erannual_10p_clim <- tempaggregate(predER_10p_clim[1,,], by_t = as.Date(interDates), 
                                    FUN = max, na.rm = TRUE)  %>%   aperm(c(2,1))
  
  return(tibble::lst(temp_anae,
                     inun_anae,
                     clim_anae,
                     inun10p_anae,
                     predGPP,
                     predER,
                     predGPP_10p,
                     predER_10p,
                     predGPP_clim,
                     predER_clim,
                     predGPP_10p_clim,
                     predER_10p_clim,
                     
                     tempannual,
                     inunannual,
                     climannual,
                     inun10pannual,
                     gppannual,
                     erannual,
                     gppannual_10p,
                     erannual_10p,
                     gppannual_clim,
                     erannual_clim,
                     gppannual_10p_clim,
                     erannual_10p_clim))
  
}

