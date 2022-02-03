# a set of functions for creating a standard set of plots

# Libraries and system setup
# source('directorySet.R')

# Let's get libraries here, then sort out git then sort out making this a library so we don't have to deal with all the library crap
# not sure how many of these need to be read in here. Maybe none.
# library(here)
# library(tidyverse)
# library(sf)
# library(stars)
# library(tmap)
# library(transformr)
# library(gganimate)
# library(viridis)
# library(colorspace)
# library(doFuture)



# Set up standard breaks across the full range of data --------------------

# I *think* I'll still do separate for temp, inundation, etc so can specify standard palettes

# Temp
tempsetup <- function(data, attnum = 1, forcemin = NA, forcemax = NA) {
  # if feed it a stars, use the chosen attribute. If fed an attribute directly, use that
  if (class(data) == 'stars') {
    thisdata <- data[[attnum]]
  } else if ('matrix' %in% class(data) | 'array' %in% class(data)) {
    thisdata <- data
  } else {
    stop('data type not supported')
  }
  
  if (!is.na(forcemin)) {
    thismin <- forcemin
  } else {
    thismin <- min(thisdata, na.rm = TRUE)
  }
  
  if (!is.na(forcemax)) {
    thismax <- forcemax
  } else {
    thismax <- max(thisdata, na.rm = TRUE)
  }
  
  
  tempbreaks <- labeling::extended(m = 10,
                                   dmin = thismin,
                                   dmax = thismax)
  temppal <- colorspace::divergingx_hcl(length(tempbreaks)-1, palette = 'Spectral',
                            rev = TRUE)
  midtemp <- median(thisdata, na.rm = TRUE)
  
  tempbreaktext <- format(tempbreaks)
  tempstart <- tempbreaktext[1:(length(tempbreaktext)-1)]
  templabels <- paste0(tempstart, ' to ', tempbreaktext[2:length(tempbreaktext)])
  
  return(tibble::lst(tempbreaks, temppal, midtemp, tempbreaktext, templabels))
  
}

# Inundation
inunsetup <- function(data, attnum, logscale = TRUE, forcemin = NA, forcemax = NA) {
  
  # if feed it a stars, use the chosen attribute. If fed an attribute directly, use that
  if (class(data) == 'stars') {
    thisdata <- data[[attnum]]
  } else if ('matrix' %in% class(data) | 'array' %in% class(data)) {
    thisdata <- data
  } else {
    stop('data type not supported')
  }
  
  if (!is.na(forcemin)) {
    thismin <- ifelse(logscale, log10(1+forcemin), forcemin)
  } else {
    thismin <- ifelse(logscale, 
                      ceiling(min(log10(1 + thisdata), na.rm = TRUE)),
                      min(thisdata, na.rm = TRUE))
  }
  
  if (!is.na(forcemax)) {
    thismax <- ifelse(logscale, log10(1+forcemax), forcemax)
  } else {
    thismax <- ifelse(logscale, 
                      ceiling(max(log10(1 + thisdata), na.rm = TRUE)),
                      max(thisdata, na.rm = TRUE))
  }
  
  
  # inundation
  inunbreaks <- labeling::extended(m = 10,
                                   dmin = thismin,
                                   dmax = thismax)
  inunpal <- colorspace::divergingx_hcl(length(inunbreaks)-1, palette = 'Earth',
                            rev = FALSE)
  midinun <- median(thisdata, na.rm = TRUE)
  
  inunbreaks_log <- labeling::extended(m = 10,
                                       dmin = thismin,
                                       dmax = thismax)
  
  inunpal_log <- colorspace::divergingx_hcl(length(inunbreaks_log)-1, palette = 'Earth',
                                rev = FALSE)
  midinun_log <- median(log10(1+thisdata), na.rm = TRUE)
  
  # Make pretty labels. Breaks CONTAIN the endpoints
  inunlabels_log <- 10^inunbreaks_log
  inunlabels_log <- format(inunlabels_log, big.mark=",", 
                           scientific=FALSE, trim = TRUE, digits = 0)
  inunstart <- inunlabels_log[1:(length(inunlabels_log)-1)]
  inunstart[1] <- "0" # instead of 1
  inunlabels_log <- paste0(inunstart, ' to ', inunlabels_log[2:length(inunlabels_log)])
  inunlabels_log
  
  if (logscale) {
    return(lst(inunbreaks = inunbreaks_log, 
               inunpal = inunpal_log,
               midinun = midinun_log, 
               inunlabels = inunlabels_log))
  } else {
    return(lst(inunbreaks,
               inunpal,
               midinun, 
               inunlabels))
  }
}

# ER
ersetup <- function(data, attnum, logscale = TRUE, forcemin = NA, forcemax = NA) {
  
  # if feed it a stars, use the chosen attribute. If fed an attribute directly, use that
  if (class(data) == 'stars') {
    thisdata <- data[[attnum]]
  } else if ('matrix' %in% class(data) | 'array' %in% class(data)) {
    thisdata <- data
  } else {
    stop('data type not supported')
  }
  
  if (!is.na(forcemin)) {
    thismin <- ifelse(logscale, log10(1+forcemin), forcemin)
  } else {
    thismin <- ifelse(logscale, 
                      ceiling(min(log10(1 + thisdata), na.rm = TRUE)),
                      min(thisdata, na.rm = TRUE))
  }
  
  if (!is.na(forcemax)) {
    thismax <- ifelse(logscale, log10(1+forcemax), forcemax)
  } else {
    thismax <- ifelse(logscale, 
                      ceiling(max(log10(1 + thisdata), na.rm = TRUE)),
                      max(thisdata, na.rm = TRUE))
  }
  
  # ER Use the mean estimate to set the values? Or should I use the min and max?
  # Might depend on what I want to show. If min and max, will need to change to
  # [[2]] for the dmin and [[3]] for the dmax
  erbreaks <- labeling::extended(m = 10,
                                 dmin = thismin,
                                 dmax = thismax)
  erbreaks_log <- labeling::extended(m = 10, 
                                     dmin = thismin, 
                                     dmax = thismax)
  
  # and those breaks might not quite yield 10, so maximise the palette differences
  erpal_log <- sequential_hcl(length(erbreaks_log)-1, palette = 'Purples', rev = TRUE)
  
  # Make pretty labels. Breaks CONTAIN the endpoints
  erlabels_log <- 10^erbreaks_log
  erlabels_log <- format(erlabels_log, big.mark=",", 
                         scientific=FALSE, trim = TRUE, digits = 0)
  erstart <- erlabels_log[1:(length(erlabels_log)-1)]
  erstart[1] <- "0" # instead of 1
  erlabels_log <- paste0(erstart, ' to ', erlabels_log[2:length(erlabels_log)])
  erlabels_log
  
  if (logscale) {
    return(lst(erbreaks = erbreaks_log, 
               erpal = erpal_log, 
               erlabels = erlabels_log))
  } else {
    return(lst(erbreaks, 
               mider, 
               erlabels))
  }
  
}

# GPP
gppsetup <- function(data, attnum, logscale = TRUE, forcemin = NA, forcemax = NA) {
  
  # if feed it a stars, use the chosen attribute. If fed an attribute directly, use that
  if (class(data) == 'stars') {
    thisdata <- data[[attnum]]
  } else if ('matrix' %in% class(data) | 'array' %in% class(data)) {
    thisdata <- data
  } else {
    stop('data type not supported')
  }
  
  if (!is.na(forcemin)) {
    thismin <- ifelse(logscale, log10(1+forcemin), forcemin)
  } else {
    thismin <- ifelse(logscale, 
                      ceiling(min(log10(1 + thisdata), na.rm = TRUE)),
                      min(thisdata, na.rm = TRUE))
  }
  
  if (!is.na(forcemax)) {
    thismax <- ifelse(logscale, log10(1+forcemax), forcemax)
  } else {
    thismax <- ifelse(logscale, 
                      ceiling(max(log10(1 + thisdata), na.rm = TRUE)),
                      max(thisdata, na.rm = TRUE))
  }
  
  
  gppbreaks <- labeling::extended(m = 10,
                                  dmin = thismin,
                                  dmax = thismax)
  gppbreaks_log <- labeling::extended(m = 10, 
                                      dmin = thismin, 
                                      # adding ceiling because this misses the actual max
                                      dmax = ceiling(max(log10(1 + thisdata), na.rm = TRUE)))
  
  # and those breaks might not quite yield 10, so maximise the palette differences
  gpppal_log <- sequential_hcl(length(gppbreaks_log)-1, palette = 'Emrld', rev = TRUE)
  
  # Make pretty labels. Breaks CONTAIN the endpoints
  gpplabels_log <- 10^gppbreaks_log
  gpplabels_log <- format(gpplabels_log, big.mark=",", 
                          scientific=FALSE, trim = TRUE, digits = 0)
  gppstart <- gpplabels_log[1:(length(gpplabels_log)-1)]
  gppstart[1] <- "0" # instead of 1
  gpplabels_log <- paste0(gppstart, ' to ', gpplabels_log[2:length(gpplabels_log)])
  gpplabels_log
  
  if (logscale) {
    return(lst(gppbreaks = gppbreaks_log, 
               gpppal = gpppal_log,
               gpplabels = gpplabels_log))
  } else {
    return(lst(gppbreaks,
               gpppal,
               midgpp, 
               gpplabels))
  }
}


# PLOT GENERATING FUNCTIONS -----------------------------------------------


## TEMP
tempfun <- function(starsObj, attributeNum = 1, datewanted, 
                    forcelegend = NULL,
                    titled = TRUE, titlePrefix = NULL, titleSuffix = NULL, 
                    plotPkg = 'tmap', ...) {
  
  # Title prefix and suffix lets us add bits around the date
  if (titled) {
    thistitle <- paste0(titlePrefix, datewanted, titleSuffix)
  }
  
  
  # Process the data
  temp_sf <- starsObj[attributeNum,,] %>%
    st_as_sf() %>%
    select(all_of(datewanted)) %>%
    pivot_longer(cols = all_of(datewanted), names_to = 'date', values_to = 'Temp')
  
  # get plot controls
  tempControl <- tempsetup(starsObj, attributeNum, ...)
  
  legendlabel <- ifelse(is.null(forcelegend), 
                        'Temp C',
                        forcelegend)
  
  if (plotPkg == 'tmap') {
    temp_tm <- temp_sf %>%
      tm_shape() + 
      tm_fill(col = 'Temp', 
              palette = tempControl$temppal,
              midpoint = tempControl$midtemp,
              breaks = tempControl$tempbreaks,
              title = legendlabel) 
    
    if (length(datewanted) > 1) {
      tem_tm <- temp_tm + tm_facets(by = 'date')  
    }
    
    if (titled) {
      temp_tm <- temp_tm + 
        tm_layout(title = thistitle)
    }
    
    return(temp_tm)
  }
  
  if (plotPkg == 'ggplot') {
    temp_gg <- ggplot() +
      geom_sf(data = temp_sf, mapping = aes(fill = Temp), color = NA) +
      # geom_sf_label(data = ltimNoNorth, mapping = aes(label = ValleyName)) +
      coord_sf() +
      # Closest to the tmap
      scale_fill_stepsn(colors = tempControl$temppal,
                        breaks = tempControl$tempbreaks[2:length(tempControl$tempbreaks)],
                        limits = c(min(tempControl$tempbreaks), max(tempControl$tempbreaks)),
                        labels = tempControl$templabels,
                        guide = 'legend', 
                        name = legendlabel)
    # some other scale options
    # scale_fill_stepsn(colors = temppal, breaks = tempbreaks,
    #                   limits = c(min(tempbreaks), max(tempbreaks)),
    #                   guide = 'legend')
    # scale_fill_binned_divergingx(palette = 'Spectral',rev = TRUE, 
    #                              mid = median(weraiCropTemp[[1]], na.rm = TRUE),
    #                              breaks = tempbreaks, 
    #                              limits = c(min(tempbreaks), max(tempbreaks)))
    
    if (length(datewanted) > 1) {
      temp_gg <- temp_gg + facet_wrap(vars(date))  
    }
    
    if (titled) {
      temp_gg <- temp_gg + 
        ggtitle(thistitle)
    }
    return(temp_gg)
  }
  
}

# Inundation
inunfun <- function(starsObj, attributeNum = 1, datewanted, units = 'Ml',
                    forcelegend = NULL,
                    titled = TRUE, titlePrefix = NULL, titleSuffix = NULL, 
                    plotPkg = 'tmap', ...) {
  
  # Title prefix and suffix lets us add bits around the date
  if (titled) {
    thistitle <- paste0(titlePrefix, datewanted, titleSuffix)
  }
  
  inun_sf <- starsObj[attributeNum,,] %>%
    st_as_sf() %>%
    select(all_of(datewanted)) %>%
    pivot_longer(cols = all_of(datewanted), names_to = 'date', values_to = 'InundationVolume') %>%
    mutate(logVolume = log10(1+InundationVolume))
  
  # get plot controls
  inunControl <- inunsetup(starsObj, attributeNum, ...)
  
  legendlabel <- ifelse(is.null(forcelegend), 
                        paste0(units, ' Inundation\nat max extent'),
                        forcelegend)

  if (plotPkg == 'tmap') {
    inun_tm <- inun_sf %>%
      tm_shape() + 
      tm_fill(col = 'logVolume', 
              palette = inunControl$inunpal,
              midpoint = inunControl$midinun, 
              breaks = inunControl$inunbreaks,
              labels = inunControl$inunlabels,
              title = legendlabel) 
    
    if (length(datewanted) > 1) {
      inun_tm <- inun_tm + tm_facets(by = 'date') 
    }
    
    
    if (titled) {
      inun_tm <- inun_tm + 
        tm_layout(title = thistitle)
    }
    return(inun_tm)  
  }
  
  if (plotPkg == 'ggplot') {
    inun_gg <- ggplot() +
      geom_sf(data = inun_sf, mapping = aes(fill = logVolume), color = NA) +
      # geom_sf_label(data = ltimNoNorth, mapping = aes(label = ValleyName)) +
      coord_sf() +
      # Closest to the tmap
      scale_fill_stepsn(colors = inunControl$inunpal,
                        breaks = inunControl$inunbreaks[2:length(inunControl$inunbreaks)],
                        limits = c(min(inunControl$inunbreaks), max(inunControl$inunbreaks)),
                        labels = inunControl$inunlabels,
                        guide = 'legend',
                        name = legendlabel)
    # inun_gg
    
    if (length(datewanted) > 1) {
      inun_gg <- inun_gg + facet_wrap(vars(date))  
    }
    
    
    if (titled) {
      inun_gg <- inun_gg + 
        ggtitle(thistitle)
    }
    return(inun_gg)
  }
}

# GPP
# I guess for the moment without uncertainty
gppfun <- function(starsObj, attributeNum = 1, datewanted, units = 'kg',
                   forcelegend = NULL,
                   titled = TRUE, titlePrefix = NULL, titleSuffix = NULL, 
                   plotPkg = 'tmap', ...) {
  
  # Title prefix and suffix lets us add bits around the date
  if (titled) {
    thistitle <- paste0(titlePrefix, datewanted, titleSuffix)
  }
  
  # here, attributes are estimates and CI,PI
  gpp_sf <- starsObj[attributeNum,,] %>%
    st_as_sf() %>%
    select(all_of(datewanted)) %>%
    pivot_longer(cols = all_of(datewanted), names_to = 'date', values_to = 'GPP') %>%
    mutate(logGPP = log10(1+GPP))
  
  # get plot controls
  gppControl <- gppsetup(starsObj, attributeNum, ...)
  
  
  
  # getting the 02 subscripted and linebreaks to work in both tmap and ggplot is
  # a huge pain, so I'm giving up and just doing something that kind of works
  legendlabel <- ifelse(is.null(forcelegend), 
                        paste0('GPP (', units, ' 02/day)\nat max extent'),
                        forcelegend)
  
  if (plotPkg == 'tmap') {
    gpp_tm <- gpp_sf %>%
      tm_shape() +
      tm_fill(col = 'logGPP', palette = gppControl$gpppal,
              breaks = gppControl$gppbreaks,
              labels = gppControl$gpplabels,
              title = legendlabel) 
    
    if (length(datewanted) > 1) {
      gpp_tm <- gpp_tm + tm_facets(by = 'date')  
    }
    
    if (titled) {
      gpp_tm <- gpp_tm + 
        tm_layout(title = thistitle)
    }
    return(gpp_tm)
  }
  
  
  if (plotPkg == 'ggplot') {
    gpp_gg <- ggplot() +
      geom_sf(data = gpp_sf, mapping = aes(fill = logGPP), color = NA) +
      # geom_sf_label(data = ltimNoNorth, mapping = aes(label = ValleyName)) +
      coord_sf() +
      # Closest to the tmap
      scale_fill_stepsn(colors = gppControl$gpppal,
                        breaks = gppControl$gppbreaks[2:length(gppControl$gppbreaks)],
                        limits = c(min(gppControl$gppbreaks), max(gppControl$gppbreaks)),
                        labels = gppControl$gpplabels,
                        guide = 'legend',
                        name = legendlabel)
    # gpp_gg
    if (length(datewanted) > 1) {
      gpp_gg <- gpp_gg + facet_wrap(vars(date))  
    }
    
    if (titled) {
      gpp_gg <- gpp_gg + 
        ggtitle(thistitle)
    }
    return(gpp_gg)
  }
  
}

# ER plot
erfun <- function(starsObj, attributeNum = 1, datewanted, units = 'kg',
                  forcelegend = NULL, 
                  titled = TRUE, titlePrefix = NULL, titleSuffix = NULL, 
                  plotPkg = 'tmap', ...) {
  
  # Title prefix and suffix lets us add bits around the date
  if (titled) {
    thistitle <- paste0(titlePrefix, datewanted, titleSuffix)
  }
  
  
  er_sf <- starsObj[attributeNum,,] %>%
    st_as_sf() %>%
    select(all_of(datewanted)) %>%
    pivot_longer(cols = all_of(datewanted), names_to = 'date', values_to = 'ER') %>%
    mutate(logER = log10(1+ER))
  
  # er_sf <- starsObj[attributeNum,,] %>% 
  #   st_as_sf() %>% 
  #   select(all_of(datewanted)) %>%
  #   rename(ER = 1) %>%
  #   mutate(logER = log10(1+ER))
  
  # get plot controls
  erControl <- ersetup(starsObj, attributeNum, ...)
  
  legendlabel <- ifelse(is.null(forcelegend), 
                        paste0('ER (', units, ' 02/day)\nat max extent'),
                        forcelegend)
  
  
  if (plotPkg == 'tmap') {
    er_tm <- er_sf %>%
      tm_shape() +
      tm_fill(col = 'logER', palette = erControl$erpal,
              breaks = erControl$erbreaks,
              labels = erControl$erlabels,
              title = legendlabel)
    if (titled) {
      er_tm <- er_tm + 
        tm_layout(title = thistitle)
    }
    
    if (length(datewanted) > 1) {
      er_tm <- er_tm + tm_facets(by = 'date')  
    }
    
    return(er_tm)
  }
  
  
  if (plotPkg == 'ggplot') {
    er_gg <- ggplot() +
      geom_sf(data = er_sf, mapping = aes(fill = logER), color = NA) +
      # geom_sf_label(data = ltimNoNorth, mapping = aes(label = ValleyName)) +
      coord_sf() +
      # Closest to the tmap
      scale_fill_stepsn(colors = erControl$erpal,
                        breaks = erControl$erbreaks[2:length(erControl$erbreaks)],
                        limits = c(min(erControl$erbreaks), max(erControl$erbreaks)),
                        labels = erControl$erlabels,
                        guide = 'legend',
                        name = legendlabel)
    
    if (length(datewanted) > 1) {
      er_gg <- er_gg + facet_wrap(vars(date))  
    }
    
    # er_gg
    if (titled) {
      er_gg <- er_gg + 
        ggtitle(thistitle)
    }
    return(er_gg)
  }
}


# COMBO PLOTS -------------------------------------------------------------

# 
inputsfun <- function(tempObj, tempAtt = 1,
                      inunObj, inunAtt = 1,
                      datewanted) {
  tmap_arrange(tempfun(tempObj, tempAtt, datewanted), 
               inunfun(inunObj, inunAtt, datewanted))
}

predictfun <- function(gppObj, gppAtt = 1,
                       erObj, erAtt = 1,
                       datewanted) {
  tmap_arrange(gppfun(gppObj, gppAtt, datewanted),
               erfun(erObj, erAtt, datewanted))
}

allfun <- function(tempObj, tempAtt = 1,
                   inunObj, inunAtt = 1,
                   gppObj, gppAtt = 1,
                   erObj, erAtt = 1,
                   datewanted) {
  tmap_arrange(tempfun(tempObj, tempAtt, datewanted),
               inunfun(inunObj, inunAtt, datewanted),
               gppfun(gppObj, gppAtt, datewanted),
               erfun(erObj, erAtt, datewanted))
}


# TESTING -----------------------------------------------------------------


# # TEST against the original local and static using some date
# availDays <- st_get_dimension_values(weraiCropTemp, which = 'time')
# datewanted <- as.character(availDays[19])
# 
# # Temps
# tempfun(weraiCropTemp, 
#         attributeNum = 1, 
#         datewanted = datewanted, 
#         titled = FALSE, plotPkg = 'tmap')
# 
# tempfun(weraiCropTemp, 
#         attributeNum = 1, 
#         datewanted = datewanted, 
#         titled = FALSE, plotPkg = 'ggplot')
# 
# # Inun
# inunfun(weraiCropInun, 
#         attributeNum = 1, 
#         datewanted = datewanted, 
#         titled = FALSE, plotPkg = 'tmap')
# 
# inunfun(weraiCropInun, 
#         attributeNum = 1, 
#         datewanted = datewanted, 
#         titled = FALSE, plotPkg = 'ggplot')
# 
# # GPP
# gppfun(weraiCropPredGPP, 
#         attributeNum = 1, 
#         datewanted = datewanted, 
#         titled = FALSE, plotPkg = 'tmap')
# 
# gppfun(weraiCropPredGPP, 
#         attributeNum = 1, 
#         datewanted = datewanted, 
#         titled = FALSE, plotPkg = 'ggplot')
# 
# # ER
# erfun(weraiCropPredER, 
#         attributeNum = 1, 
#         datewanted = datewanted, 
#         titled = FALSE, plotPkg = 'tmap')
# 
# erfun(weraiCropPredER, 
#         attributeNum = 1, 
#         datewanted = datewanted, 
#         titled = FALSE, plotPkg = 'ggplot')
