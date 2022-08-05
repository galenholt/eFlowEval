# Functions developed for another project
huefinder <- function(hueval, minhue, maxhue, n = Inf, palname = NULL, reverse = FALSE) {
  
  # If continuous, use the value
  # If binned, find the value of the bin the value is in
  if (is.infinite(n)) {
    matchH <- (maxhue-minhue)*hueval + minhue
  } else if (!is.infinite(n)) {
    
    nvec <- seq(from = 0, to = 1, length.out = n)
    
    # The nvecs need to choose the COLOR, but the last one gets dropped in
    # findInterval, so need an n+1
    whichbin <- findInterval(hueval,
                             seq(from = 0, to = 1, length.out = n+1),
                             rightmost.closed = TRUE)
    
    
    # Don't build if using named palette because won't have min and max
    if (is.null(palname)) {
      binhue <- nvec[whichbin]
      matchH <- (maxhue-minhue)*binhue + minhue
    }
    
  }
  
  if (is.null(palname)) {
    h <- cbind(50, max_chroma(h = matchH, l = 50, floor = TRUE),
               matchH)
    h <- hex(polarLUV(h))
  } else {
    h <- sequential_hcl(n, palname, rev = reverse)[whichbin]
  }
  
  return(h)
}

# Sort of a short-circuit of huefinder for binned data using the old setup
# functions so I don't have to re-write all the labeling
hueassign <- function(hueval, huepal, huebreaks) {
  # use the palette and breaks calculated from gppsetup
  whichbin <- findInterval(hueval,
                           huebreaks,
                           rightmost.closed = TRUE)
  h <- huepal[whichbin]
}

fadefinder <- function(fadeval, basehue, n = Inf) {
  
  # If n is infinite, just use fadeval. Otherwise, bin, dropping the all-white level
  if (is.infinite(n)) {
    fadeval <- fadeval
  } else {
    # The +1 drops the white level
    fadevec <- seq(from = 0, to = 1, length.out = n + 1)
    
    # Rightmost closed fixes an issue right at 1
    fadeval <- fadevec[findInterval(fadeval, fadevec, rightmost.closed = TRUE) + 1]
  }
  
  fadedcol <- lighten(basehue, amount = 1-fadeval) %>%
    desaturate(amount = 1-fadeval)
}

relpos <- function(vec) {
  (vec - min(vec))/(max(vec)-min(vec))
}


# generate a matrix of color values
col2dmat <- function(pal, n1, n2 = 2, dropwhite = TRUE, fadevals = NULL) {
  # pal can be either a palette name or a vector of hex colors (or single hex color)
  # dropwhite is there to by default knock off the bottom row that's all white
  # fadevals is a way to bypass the n2 and specify specific fade levels (ie if nonlinear)
  
  if (all(str_detect(pal, '#'))) {
    baseramp <- pal
  } else {
    baseramp <- sequential_hcl(n1, pal)
  }
  
  if (is.null(fadevals)) {
    if (dropwhite) {n2 = n2+1}
    
    fadesteps <- rev(seq(0,1, length.out = n2))
    
    if (dropwhite) {fadesteps <- fadesteps[2:length(fadesteps)]}
    
  }
  
  if (!is.null(fadevals)) {
    fadesteps <- sort(fadevals, decreasing = TRUE)
  }
  
  colormat <- matrix(rep(baseramp, length(fadesteps)), nrow = length(fadesteps), byrow = TRUE)
  
  
  for(i in 1:length(fadesteps)) {
    colormat[i, ] <- lighten(colormat[i, ], amount = fadesteps[i]) %>%
      desaturate(amount = fadesteps[i])
  }
  
  return(colormat)
}

# generate a plot from the color value matrix
plot2dcols <- function(colmat) {
  coltib <- as_tibble(colmat, rownames = 'row') %>%
    pivot_longer(cols = starts_with('V'), names_to = 'column') %>%
    mutate(row = as.numeric(row), column = as.numeric(str_remove(column, 'V')))
  
  colplot <- ggplot(coltib, aes(y = row, x = column, fill = value)) +
    geom_tile() + scale_fill_identity()
  
  return(colplot)
}


# new funs ----------------------------------------------------------------



# Function to make a fade column, either fixed (default) or calculated
# This needs to work for a single fadeAtt or two and find the difference
# (possibly relative to calue)
getfadecol <- function(starsObj, attributeNum = 1, datewanted, 
                       fixfade = 1, fadeAtts = NULL, fadeRel = TRUE) {
  # There's probably a rearrange that
  # could be done here to swap attributes and dims? usiung merge() doesn't work
  # easily, anyway, so ignore for now.
  sfF <- starsObj[attributeNum,,] %>%
    st_as_sf() %>%
    select(all_of(datewanted)) %>%
    pivot_longer(cols = all_of(datewanted), names_to = 'date', values_to = 'plotValues')
  
  # We need a column of fade values. It can be set (maxfade and fadeval = NULL) or
  # calculated from a single column or a comparison. Assumes the comparison is a
  # difference- ie width of a ci or something.That might be a bad idea- it should
  # be relative to the mean or it'll always fade larger things more
  
  if (!is.null(fadeAtts)) {
    if (length(fadeAtts == 1)) {
      starsFade = starsObj[fadeAtts,,]
    } else if (length(fadeAtts == 2)) {
      starsFade = abs(starsObj[fadeAtts[1],,] - starsObj[fadeAtts[2],,])
    } else {
      stop("No way to handle more than two fade attributes. Write some code if you want to do this.")
    }
    
    fade_sf <- starsFade %>%
      st_as_sf() %>%
      select(all_of(datewanted)) %>%
      pivot_longer(cols = all_of(datewanted), names_to = 'date', values_to = 'fadeVal')
    
    sfF <- bind_cols(sfF, st_drop_geometry(select(fade_sf, fadeVal)))
    
    if (fadeRel) {
      sfF <- sfF %>% 
        mutate(fadeVal = fadeVal/plotValues)
    }
    
    
  } else {
    sfF <- mutate(sfF, fadeVal = fixfade)
  }
  
  return(sfF)
}

# Function to get the sf to plot - either just return the sf of the stars, or if
# there are two combine them based on uncertainty 
sf_to_plot <- function(starsObj, attributeNum = 1, datewanted, 
                       lessCertainStars = NULL, maxFade = 0.5, fadeAtts = NULL, fadeRel = TRUE) {
  
  sfC <- getfadecol(starsObj, attributeNum = attributeNum, datewanted, 
                    fixfade = 1, fadeAtts = fadeAtts, fadeRel = fadeRel)
  
  if (!is.null(lessCertainStars)) {
    sfU <- getfadecol(lessCertainStars, attributeNum = attributeNum, datewanted, 
                      fixfade = maxFade, fadeAtts = fadeAtts, fadeRel = fadeRel)
    
    # check it matches- fail if not
    matchrows <- all(diag(st_equals_exact(sfC, sfU, par = 2, sparse = FALSE)))
    if(!matchrows) {
      stop('certain and uncertain stars do not have matching polygons')
    }
    
    # assume the values in the more certain dataframe are the ones to use
    uncertainvals <- which(is.na(sfC$plotValues))
    
    sfC <- bind_rows(sfC[-uncertainvals, ], sfU[uncertainvals, ])
    
  }
  
  # Deal with the situation where the fade values are outside 0-1
  if (any(sfC$fadeVal > 1 | sfC$fadeVal < 0)) {
    sfC <- sfC %>%
      mutate(origFade = fadeVal,
             fadeVal = relpos(fadeVal))
  }
  
  # enforce maxfade
  if (any(sfC$fadeVal < maxFade)) {
    sfC <- sfC %>% 
      mutate(fadeVal = fadeVal*(1-maxFade)+maxFade)
  }
  
  return(sfC)
  
}



# GPP
# Major re-write of gppfun to make more general- binned vs continuous, better labeling, and fades
gppfade <- function(starsObj, attributeNum = 1, datewanted, units = 'kg',
                    forcelegend = NULL, colorchoice = NA,
                    titled = TRUE, titlePrefix = NULL, titleSuffix = NULL, 
                    plotPkg = 'tmap', logscale = TRUE, 
                    binOrCon = 'bin', palette = 'ag_Sunset', reverse = FALSE,
                    lessCertainStars = NULL, fadeAmount = 0.5,
                    fadeAtts = NULL, fadeRel = TRUE, ...) {
  
  # Title prefix and suffix lets us add bits around the date
  if (titled) {
    thistitle <- paste0(titlePrefix, datewanted, titleSuffix)
  }
  
  gpp_sf <- sf_to_plot(starsObj, attributeNum, datewanted, 
                       lessCertainStars = lessCertainStars, 
                       maxFade = fadeAmount, 
                       fadeAtts = fadeAtts, fadeRel = fadeRel)
  

  
  # get plot controls
  
  if (binOrCon == 'bin') {
    gppControl <- gppsetup(gpp_sf, 'plotValues', logscale, pal = palette, reverse = reverse, ...)
  } else {
    gppControl <- gppsetup(gpp_sf, 'plotValues', logscale, 
                           pal = palette, reverse = reverse, 
                           continuous = TRUE,...)
  }
  
  # log AFTER the plot controls, or end up double-logging
  if (logscale) {
    gpp_sf <- gpp_sf %>%
      mutate(plotValues = log10(1+plotValues))
  }
  
  # Make some final plot color columns 
  # huefinder does a good job with the bins, but gppsetup() also sets labels
  # etc. so use that I guess. Kind of annoying to do two ways
  
  if (binOrCon == 'bin') {
    gpp_sf <- gpp_sf %>%
      mutate(relvals = relpos(plotValues),
             hue = hueassign(plotValues, gppControl$gpppal, gppControl$gppbreaks),
             # conhue = huefinder(relvals, n = 1000, palname = palette, reverse = reverse),
             plotcolor = fadefinder(fadeVal, hue)) #,
             # confade = fadefinder(fadeVal, conhue))
    # gpp_sf <- gpp_sf %>%
    #   mutate(plotcolor = binfade) 
  } else {
    gpp_sf <- gpp_sf %>%
      mutate(relvals = relpos(plotValues),
             # binhue = hueassign(plotValues, gppControl$gpppal, gppControl$gppbreaks),
             hue = huefinder(relvals, n = 1000, palname = palette, reverse = reverse),
             # binfade = fadefinder(fadeVal, binhue),
             plotcolor = fadefinder(fadeVal, hue))
    # gpp_sf <- gpp_sf %>%
    #   mutate(plotcolor = confade)
  }
  
  # getting the 02 subscripted and linebreaks to work in both tmap and ggplot is
  # a huge pain, so I'm giving up and just doing something that kind of works
  legendlabel <- ifelse(is.null(forcelegend), 
                        paste0('GPP (', units, ' 02/day)\nat max extent'),
                        forcelegend)
  
  if (plotPkg == 'tmap') {
    gpp_tm <- gpp_sf %>%
      tm_shape() +
      tm_fill(col = 'plotValues', palette = gppControl$gpppal,
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
  
  # plotvar <- ifelse(logscale, 'logGPP', 'GPP')
  # plotvar <- rlang::data_sym(plotvar)
  
  
  if (plotPkg == 'ggplot') {
    # gpp_sf <- mutate(gpp_sf, faccolor = factor(plotcolor, levels = gppControl$gpppal))

    # Need to handle fill differently depending on whether binned or continuous,
    # so first just make the base plot
    gpp_gg <- ggplot() +
      geom_sf(data = gpp_sf, mapping = aes(fill = plotcolor), color = colorchoice, size = 0.1) +
      # geom_sf_label(data = ltimNoNorth, mapping = aes(label = ValleyName)) +
      coord_sf() 
    
    # If there's no fade, we can use ggplot legends. Though it is tricky even
    # between binned and colorbarred to get everything on it. It's likely
    # possible to use legends for some fades, but getting it to work was hard
    # enough without any missing values (see initial development work). So if
    # there's fade, just make a plot for the legend and move on with life.
    if (all(gpp_sf$hue == gpp_sf$plotcolor, na.rm = TRUE)) {
      # If binned, make sure we have a legend for all bins
      if (binOrCon == 'bin') {
        # "Name" the colors with their names, so we get ALL of the possible colors
        
        allcols <- setNames(gppControl$gpppal,
                            gppControl$gpppal)
        
        # scale_color_manual for binned- _identity drops those that don't exist
        gpp_gg <- gpp_gg +
          scale_fill_manual(values = allcols, 
                            # breaks = gppControl$gppbreaks[2:length(gppControl$gppbreaks)],
                            limits = gppControl$gpppal,
                            labels = gppControl$gpplabels,
                            na.value = 'grey50',
                            name = legendlabel)
        # gpp_gg
      } else {
        # Where color is continuous we can't use _identity and colorbar, because
        # it's not a real scale. Which means it works best to just use the main
        # palette on the correct column
        
        # still want to sort out pretty breaks and labels
        theselims <- c(min(gppControl$gppbreaks), max(gppControl$gppbreaks))
        # # Clean this up, likely in gppsetup
        # thismin <- theselims[1]#log10(1+theselims[1])
        # thismax <- theselims[2]#log10(1+theselims[2])
        # 
        # thesebreaks <- labeling::extended(m = 4,dmin = thismin, dmax = thismax)
        # theselabels <- if(logscale)  10^thesebreaks else  thesebreaks
        # 
        # theselabels <- format(theselabels, big.mark=",", 
        #                     scientific=FALSE, trim = TRUE, digits = 0)
        # 
        gpp_gg <- ggplot() +
          geom_sf(data = gpp_sf, mapping = aes(fill = plotValues), color = colorchoice, size = 0.1) +
          # geom_sf_label(data = ltimNoNorth, mapping = aes(label = ValleyName)) +
          coord_sf() +
          scale_fill_continuous_sequential(palette = palette, rev = reverse,
                                           limits = theselims,
                                           breaks = gppControl$gppbreaks,
                                           labels = gppControl$gpplabels,
                                           name = legendlabel)
        
        
        # gpp_gg <- gpp_gg +
        #   scale_fill_identity(guide = 'legend')
        # gpp_gg
      }
    } else {
      # Let's build a plot as a legend and leave it at that.
      
      # Main plot
      gpp_gg <- gpp_gg +
        scale_fill_identity()
      gpp_gg
      
      # legend plot
      # I don't like hardcoding that n2. Think about that for future
      # argh that was complicated, but now it works
      # NEEDS CLEANUP
      # all(gpp_sf$plotcolor %in% matfade)
      fadevals <- 1-unique(gpp_sf$fadeVal)
      # col2dmat takes a palette name or a color vector. 
      # fadevals are fades, so need to 1- the ones we specify above. That's kind of annoying
      # Will need to do this a bit differently to have continuous (or even just >2) of fades
      if (binOrCon == 'bin') {
        
        if (is.null(fadeAtts)) {
          matfade <- col2dmat(gppControl$gpppal, n1 = length(gppControl$gpppal), n2 = length(fadevals),
                              fadevals = fadevals)
          ybreaks <- 1:nrow(matfade)
          ylabels <- c('Less', 'More')
        } else {
          fadebins <- 4
          # including fadevals in matfade bypasses n2. We aren't doing an exact match, so use n2 instead
          matfade <- col2dmat(gppControl$gpppal, n1 = length(gppControl$gpppal), n2 = fadebins)
          
          ybreaks <- 1:nrow(matfade)
          ylabels <- seq(from = min(gpp_sf$origFade), to = max(gpp_sf$origFade), length.out = fadebins) %>%
            round(digits = 2)
        }
        
        xbreaks <- 1:ncol(matfade)
        xlabs <- gppControl$gpplabels
      } else {
        
        # again, sort out the fade axis
        if (is.null(fadeAtts)) {
          matfade <- col2dmat(palette, n1 = 100, n2 = length(fadevals),
                              fadevals = fadevals)
          ybreaks <- 1:nrow(matfade)
          ylabels <- c('Less', 'More')
        } else {
          fadebins <- 100
          # including fadevals in matfade bypasses n2. We aren't doing an exact match, so use n2 instead
          matfade <- col2dmat(palette, n1 = 100, n2 = fadebins)
          
          nybreaks <- 4
          ybreaksraw <- labeling::extended(m = nybreaks, 
                                        dmin = log10(min(gpp_sf$origFade)), 
                                        dmax = log10(max(gpp_sf$origFade)))
          
          minmaxF <- c(min(gpp_sf$origFade), max(gpp_sf$origFade))
          internalindexF <- which(10^ybreaksraw >= minmaxF[1] & 10^ybreaksraw <= minmaxF[2])
          
          internalbreaksF <- ybreaksraw[internalindexF]
          # That gives the values, but we still need to sort out where those are on an axis with n1 values.
          relbreaksF <- relpos(c(log10(minmaxF), internalbreaksF))
          # Get them on the 1-100 scale of the axis bins
          ybreaks <- round(relbreaksF[3:length(relbreaksF)]*100)
  
          
          ylabels <- format(10^internalbreaksF, big.mark=",", 
                            scientific=FALSE, trim = TRUE, digits = 0)
        }
        
        # breaks are tricky. they're set pretty from the data, so can be a bit
        # outside. But for the hue col, they get set from 0-1 on the relpos. So,
        # need to figure out where the gppbreaks are on that same relpos axis.
        # we DON'T want to go outside the axis, because that would mean
        # recalculating huefinder for a different set of values. So toss the breaks outside the data
        minmax <- c(min(gpp_sf$plotValues), max(gpp_sf$plotValues))
        internalindex <- which(gppControl$gppbreaks >= minmax[1] & gppControl$gppbreaks <= minmax[2])

        internalbreaks <- gppControl$gppbreaks[internalindex]
        # That gives the values, but we still need to sort out where those are on an axis with n1 values.
        relbreaks <- relpos(c(minmax, internalbreaks))
        xbreaks <- round(relbreaks[3:length(relbreaks)]*100)
        xlabs <- gppControl$gpplabels[internalindex]
      }
      
      
      bin_leg <- plot2dcols(matfade) +
        # Breaks aren't centered on the values for this geom, so instead of 0.5 and 1, need to shift
        theme_void() +
        scale_y_continuous(breaks = ybreaks, labels = ylabels, name = 'Confidence') +
        scale_x_continuous(breaks = xbreaks, labels = xlabs, name = NULL) +
        ggtitle(legendlabel) + # name = legendlabel in the scale_x works too but puts it on the bottom
        theme(axis.text.y = element_text(),
              axis.title.y = element_text(angle = 90),
              axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
              axis.title.x = element_text())
      bin_leg
      
      # I almost wonder if we should return a plot and the legend in a list. This is going to be really hard to always place correctly.
      # Example 1: the position needs to differ between binned and continuous
      bpos = ifelse(binOrCon == 'bin', 0.65, 0.75)
      
      gpp_gg <- (gpp_gg + theme_bw() + theme(legend.position = 'none')) +
        inset_element((bin_leg + theme(axis.text = element_text(size = 8),
                                            title = element_text(size = 8))),
                      left = 0.01, bottom = bpos, right = 0.48, top = 1)
      
    }

    gpp_gg
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

