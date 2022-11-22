# Testing faded uncertainty

# # Functions developed for another project
# huefinder <- function(hueval, minhue, maxhue, n = Inf, palname = NULL) {
#   
#   # If continuous, use the value
#   # If binned, find the value of the bin the value is in
#   if (is.infinite(n)) {
#     matchH <- (maxhue-minhue)*hueval + minhue
#   } else if (!is.infinite(n)) {
#     
#     nvec <- seq(from = 0, to = 1, length.out = n)
#     
#     # The nvecs need to choose the COLOR, but the last one gets dropped in
#     # findInterval, so need an n+1
#     whichbin <- findInterval(hueval,
#                              seq(from = 0, to = 1, length.out = n+1),
#                              rightmost.closed = TRUE)
#     
#     
#     # Don't build if using named palette because won't have min and max
#     if (is.null(palname)) {
#       binhue <- nvec[whichbin]
#       matchH <- (maxhue-minhue)*binhue + minhue
#     }
#     
#   }
#   
#   if (is.null(palname)) {
#     h <- cbind(50, max_chroma(h = matchH, l = 50, floor = TRUE),
#                matchH)
#     h <- hex(polarLUV(h))
#   } else {
#     h <- sequential_hcl(n, palname)[whichbin]
#   }
#   
#   return(h)
# }
# 
# # Sort of a short-circuit of huefinder for binned data using the old setup
# # functions so I don't have to re-write all the labeling
# hueassign <- function(hueval, huepal, huebreaks) {
#   # use the palette and breaks calculated from gppsetup
#   whichbin <- findInterval(hueval,
#                            huebreaks,
#                            rightmost.closed = TRUE)
#   h <- huepal[whichbin]
# }
# 
# fadefinder <- function(fadeval, basehue, n = Inf) {
#   
#   # If n is infinite, just use fadeval. Otherwise, bin, dropping the all-white level
#   if (is.infinite(n)) {
#     fadeval <- fadeval
#   } else {
#     # The +1 drops the white level
#     fadevec <- seq(from = 0, to = 1, length.out = n + 1)
#     
#     # Rightmost closed fixes an issue right at 1
#     fadeval <- fadevec[findInterval(fadeval, fadevec, rightmost.closed = TRUE) + 1]
#   }
#   
#   fadedcol <- lighten(basehue, amount = 1-fadeval) %>%
#     desaturate(amount = 1-fadeval)
# }
# 
# relpos <- function(vec) {
#   (vec - min(vec))/(max(vec)-min(vec))
# }

# First, not a function


starsObjU <- logGPPdaysannual
starsObjC <- logGPPdaysvalleysannual
attributeNum = 1
colorchoice = 'grey50'
logscale = TRUE

gpp_sfC <- starsObjC[attributeNum,,] %>%
  st_as_sf() %>%
  select(all_of(datewanted)) %>%
  pivot_longer(cols = all_of(datewanted), names_to = 'date', values_to = 'plotValues') %>%
  mutate(certainty = 'best',
         fade = 1)

gpp_sfU <- starsObjU[attributeNum,,] %>%
  st_as_sf() %>%
  select(all_of(datewanted)) %>%
  pivot_longer(cols = all_of(datewanted), names_to = 'date', values_to = 'plotValues') %>%
  mutate(certainty = 'low',
         fade = 0.5)

# Do I trust that those are matched orders? they should be.
uncertainvals <- which(is.na(gpp_sfC$plotValues))

# Really should check that the rows match
all(diag(st_equals_exact(gpp_sfC, gpp_sfU, par = 2, sparse = FALSE)))

gpp_sfCU <- bind_rows(gpp_sfC[-uncertainvals, ], gpp_sfU[uncertainvals, ])


gpp_sfCU <- gpp_sfCU %>%
  mutate(relvals = relpos(plotValues),
    binhue = huefinder(relvals, n = 8, palname = 'ag_Sunset'),
         conhue = huefinder(relvals, n = 1000, palname = 'ag_Sunset'),
         binfade = fadefinder(fade, binhue),
         confade = fadefinder(fade, conhue))
# gppControl <- gppsetup(gpp_sfCU, 'plotValues', logscale)

gpp_gg <- ggplot() +
  geom_sf(data = gpp_sfCU, mapping = aes(fill = binfade), color = colorchoice, size = 0.1) +
  # geom_sf_label(data = ltimNoNorth, mapping = aes(label = ValleyName)) +
  coord_sf() +
  scale_fill_identity()
  # scale_fill_stepsn(colors = gppControl$gpppal)
  # Closest to the tmap
  # scale_fill_stepsn(colors = gppControl$gpppal,
  #                   breaks = gppControl$gppbreaks[2:length(gppControl$gppbreaks)],
  #                   limits = c(min(gppControl$gppbreaks), max(gppControl$gppbreaks)),
  #                   labels = gppControl$gpplabels,
  #                   guide = 'legend',
  #                   name = 'test')
gpp_gg

gpp_ggC <- ggplot() +
  geom_sf(data = gpp_sfCU, mapping = aes(fill = confade), color = colorchoice, size = 0.1) +
  # geom_sf_label(data = ltimNoNorth, mapping = aes(label = ValleyName)) +
  coord_sf() +
  scale_fill_identity()
# scale_fill_stepsn(colors = gppControl$gpppal)
# Closest to the tmap
# scale_fill_stepsn(colors = gppControl$gpppal,
#                   breaks = gppControl$gppbreaks[2:length(gppControl$gppbreaks)],
#                   limits = c(min(gppControl$gppbreaks), max(gppControl$gppbreaks)),
#                   labels = gppControl$gpplabels,
#                   guide = 'legend',
#                   name = 'test')
gpp_ggC



## Figuring out quantitative uncertainty

fadeAtts <- c(2,3)
# here, attributes are estimates and CI,PI 
# There's probably a rearrange that
# could be done here to swap attributes and dims? usiung merge() doesn't work
# easily, anyway, so ignore for now.
gpp_sfC <- starsObjC[attributeNum,,] %>%
  st_as_sf() %>%
  select(all_of(datewanted)) %>%
  pivot_longer(cols = all_of(datewanted), names_to = 'date', values_to = 'plotValues')

# We need a column of fade values. It can be set (maxfade and fadeval = NULL) or
# calculated from a single column or a comparison. Assumes the comparison is a
# difference- ie width of a ci or something.That might be a bad idea- it should
# be relative to the mean or it'll always fade larger things more

if (!is.null(fadeAtts)) {
  if (length(fadeAtts == 1)) {
    starsFade = starsObjC[fadeAtts,,]
  } else if (length(fadeAtts == 2)) {
    starsFade = abs(starsObjC[fadeAtts[1],,] - starsObjC[fadeAtts[2],,])
  } else {
    stop("No way to handle more than two fade attributes. Write some code if you want to do this.")
  }
  
  gpp_fade <- starsFade %>%
    st_as_sf() %>%
    select(all_of(datewanted)) %>%
    pivot_longer(cols = all_of(datewanted), names_to = 'date', values_to = 'fadeVal')
  
  gpp_sfC <- bind_cols(gpp_sfC, select(gpp_fade, fadeVal))
  
  if (fadeRel) {
    gpp_sfC <- gpp_sfC %>% 
      mutate(fadeVal = fadeVal/plotValues)
  }
  
  
} else {
  gpp_sfC <- mutate(gpp_sfC, fadeVal = 1)
}



# change the way we do this entirely- make the plot function take an sf dataframe. Do all the stars sorting elsewhere.




# # Function to make a fade column, either fixed (default) or calculated
# # This needs to work for a single fadeAtt or two and find the difference
# # (possibly relative to calue)
# getfadecol <- function(starsObj, attributeNum = 1, datewanted, 
#                        fixfade = 1, fadeAtts = NULL, fadeRel = TRUE) {
#   # There's probably a rearrange that
#   # could be done here to swap attributes and dims? usiung merge() doesn't work
#   # easily, anyway, so ignore for now.
#   sfF <- starsObj[attributeNum,,] %>%
#     st_as_sf() %>%
#     select(all_of(datewanted)) %>%
#     pivot_longer(cols = all_of(datewanted), names_to = 'date', values_to = 'plotValues')
#   
#   # We need a column of fade values. It can be set (maxfade and fadeval = NULL) or
#   # calculated from a single column or a comparison. Assumes the comparison is a
#   # difference- ie width of a ci or something.That might be a bad idea- it should
#   # be relative to the mean or it'll always fade larger things more
#   
#   if (!is.null(fadeAtts)) {
#     if (length(fadeAtts == 1)) {
#       starsFade = starsObj[fadeAtts,,]
#     } else if (length(fadeAtts == 2)) {
#       starsFade = abs(starsObj[fadeAtts[1],,] - starsObj[fadeAtts[2],,])
#     } else {
#       stop("No way to handle more than two fade attributes. Write some code if you want to do this.")
#     }
#     
#     fade_sf <- starsFade %>%
#       st_as_sf() %>%
#       select(all_of(datewanted)) %>%
#       pivot_longer(cols = all_of(datewanted), names_to = 'date', values_to = 'fadeVal')
#     
#     sfF <- bind_cols(sfF, select(fade_sf, fadeVal))
#     
#     if (fadeRel) {
#       sfF <- sfF %>% 
#         mutate(fadeVal = fadeVal/plotValues)
#     }
#     
#     
#   } else {
#     sfF <- mutate(sfF, fadeVal = fixfade)
#   }
#   
#   return(sfF)
# }
# 
# # Function to get the sf to plot - either just return the sf of the stars, or if
# # there are two combine them based on uncertainty 
# sf_to_plot <- function(starsObj, attributeNum = 1, datewanted, 
#                        lessCertainStars = NULL, maxFade = 0.5, fadeAtts = NULL, fadeRel = TRUE) {
#   
#   sfC <- getfadecol(starsObj, attributeNum = attributeNum, datewanted, 
#                         fixfade = 1, fadeAtts = fadeAtts, fadeRel = fadeRel)
#   
#   if (!is.null(lessCertainStars)) {
#     sfU <- getfadecol(lessCertainStars, attributeNum = attributeNum, datewanted, 
#                         fixfade = maxFade, fadeAtts = fadeAtts, fadeRel = fadeRel)
#     
#     # check it matches- fail if not
#     matchrows <- all(diag(st_equals_exact(sfC, sfU, par = 2, sparse = FALSE)))
#     if(!matchrows) {
#       stop('certain and uncertain stars do not have matching polygons')
#     }
#     
#     # assume the values in the more certain dataframe are the ones to use
#     uncertainvals <- which(is.na(sfC$plotValues))
#     
#     sfC <- bind_rows(sfC[-uncertainvals, ], sfU[uncertainvals, ])
#     
#   }
#   
#   return(sfC)
#   
# }
# 
# 
# 
# # GPP
# # Major re-write of gppfun to make more general- binned vs continuous, better labeling, and fades
# gppfade <- function(starsObj, attributeNum = 1, datewanted, units = 'kg',
#                    forcelegend = NULL, colorchoice = NA,
#                    titled = TRUE, titlePrefix = NULL, titleSuffix = NULL, 
#                    plotPkg = 'tmap', logscale = TRUE, 
#                    binOrCon = 'bin', palette = 'ag_Sunset',
#                    lessCertainStars = NULL, fadeAmount = 0.5,
#                    fadeAtts = NULL, fadeRel = TRUE, ...) {
#   
#   # Title prefix and suffix lets us add bits around the date
#   if (titled) {
#     thistitle <- paste0(titlePrefix, datewanted, titleSuffix)
#   }
#   
#   gpp_sf <- sf_to_plot(starsObj, attributeNum, datewanted, 
#                        lessCertainStars = lessCertainStars, 
#                        maxFade = fadeAmount, 
#                        fadeAtts = fadeAtts, fadeRel = fadeRel)
#   
#   # log
#   if (logscale) {
#     gpp_sf <- gpp_sf %>%
#       mutate(plotValues = log10(1+plotValues))
#   }
#   
#   # get plot controls
#   gppControl <- gppsetup(starsObj, attributeNum, logscale, pal = palette, ...)
#   
#   # Make some final plot color columns 
#   # huefinder does a good job with the bins, but gppsetup() also sets labels
#   # etc. so use that I guess. Kind of annoying to do two ways
#   gpp_sf <- gpp_sf %>%
#     mutate(relvals = relpos(plotValues),
#            binhue = hueassign(plotValues, gppControl$gpppal, gppControl$gppbreaks),
#            conhue = huefinder(relvals, n = 1000, palname = palette),
#            binfade = fadefinder(fade, binhue),
#            confade = fadefinder(fade, conhue))
#   
#   if (binOrCon == 'bin') {
#     gpp_sf <- gpp_sf %>%
#       mutate(plotcolor = binfade) 
#   } else {
#     gpp_sf <- gpp_sf %>%
#       mutate(plotcolor = binfade)
#   }
#   
#   # getting the 02 subscripted and linebreaks to work in both tmap and ggplot is
#   # a huge pain, so I'm giving up and just doing something that kind of works
#   legendlabel <- ifelse(is.null(forcelegend), 
#                         paste0('GPP (', units, ' 02/day)\nat max extent'),
#                         forcelegend)
#   
#   if (plotPkg == 'tmap') {
#     gpp_tm <- gpp_sf %>%
#       tm_shape() +
#       tm_fill(col = 'plotValues', palette = gppControl$gpppal,
#               breaks = gppControl$gppbreaks,
#               labels = gppControl$gpplabels,
#               title = legendlabel) 
#     
#     if (length(datewanted) > 1) {
#       gpp_tm <- gpp_tm + tm_facets(by = 'date')  
#     }
#     
#     if (titled) {
#       gpp_tm <- gpp_tm + 
#         tm_layout(title = thistitle)
#     }
#     return(gpp_tm)
#   }
#   
#   # plotvar <- ifelse(logscale, 'logGPP', 'GPP')
#   # plotvar <- rlang::data_sym(plotvar)
#   
#   
#   if (plotPkg == 'ggplot') {
#     gpp_gg <- ggplot() +
#       geom_sf(data = gpp_sf, mapping = aes(fill = plotcolor), color = colorchoice, size = 0.1) +
#       # geom_sf_label(data = ltimNoNorth, mapping = aes(label = ValleyName)) +
#       coord_sf() +
#       scale_fill_identity()
#       # scale_fill_stepsn(colors = gppControl$gpppal,
#       #                   breaks = gppControl$gppbreaks[2:length(gppControl$gppbreaks)],
#       #                   limits = c(min(gppControl$gppbreaks), max(gppControl$gppbreaks)),
#       #                   labels = gppControl$gpplabels,
#       #                   guide = 'legend',
#       #                   name = legendlabel)
#     # gpp_gg
#     if (length(datewanted) > 1) {
#       gpp_gg <- gpp_gg + facet_wrap(vars(date))  
#     }
#     
#     if (titled) {
#       gpp_gg <- gpp_gg + 
#         ggtitle(thistitle)
#     }
#     return(gpp_gg)
#   }
#   
# }

# scale_fill_identity(breaks = gppControl$gppbreaks[2:length(gppControl$gppbreaks)],
#                     labels = gppControl$gpplabels,
#                     guide = 'legend')

# Tests -------------------------------------------------------------------

## First, replicate what we already have
# Simple- just the best values
gppfade(starsObjC/1000, attributeNum = 1, datewanted, units = 'tonnes',
        forcelegend = 'Maximum yearly GPP\n(tonnes O2)', colorchoice = NA,
        titled = TRUE, titlePrefix = NULL, titleSuffix = NULL, 
        plotPkg = 'ggplot', logscale = TRUE, 
        binOrCon = 'bin', palette = 'ag_Sunset', reverse = TRUE,
        lessCertainStars = NULL, fadeAmount = 0.5,
        fadeAtts = NULL, fadeRel = TRUE)

# Simple- just the best values, with legend force
gppfade(starsObjC/1000, attributeNum = 1, datewanted, units = 'tonnes',
        forcelegend = 'Maximum yearly GPP\n(tonnes O2)', colorchoice = NA,
        titled = TRUE, titlePrefix = NULL, titleSuffix = NULL, 
        plotPkg = 'ggplot', logscale = TRUE, 
        binOrCon = 'bin', palette = 'ag_Sunset', reverse = TRUE,
        lessCertainStars = NULL, fadeAmount = 0.5,
        fadeAtts = NULL, fadeRel = TRUE,
        forcemin = 1000, forcemax = 10000000)


# Simple- just the uncertain vals for all catchments
gppfade(starsObjU/1000, attributeNum = 1, datewanted, units = 'tonnes',
        forcelegend = 'Maximum yearly GPP\n(tonnes O2)', colorchoice = NA,
        titled = TRUE, titlePrefix = NULL, titleSuffix = NULL, 
        plotPkg = 'ggplot', logscale = TRUE, 
        binOrCon = 'bin', palette = 'ag_Sunset', reverse = TRUE,
        lessCertainStars = NULL, fadeAmount = 0.5,
        fadeAtts = NULL, fadeRel = TRUE)


# Same, but continuous
gppfade(starsObjU/1000, attributeNum = 1, datewanted, units = 'tonnes',
        forcelegend = 'Maximum yearly GPP\n(tonnes O2)', colorchoice = NA,
        titled = TRUE, titlePrefix = NULL, titleSuffix = NULL, 
        plotPkg = 'ggplot', logscale = TRUE, 
        binOrCon = 'con', palette = 'ag_Sunset', reverse = TRUE,
        lessCertainStars = NULL, fadeAmount = 0.5,
        fadeAtts = NULL, fadeRel = TRUE)




## Faded
# Titling is goofing up the inset fig. Ignore for now.

# Binned, binary fade (certain/uncertain)
gppfade(starsObjC/1000, attributeNum = 1, datewanted, units = 'tonnes',
        forcelegend = 'Maximum yearly GPP\n(tonnes O2)', colorchoice = NA,
        titled = FALSE, titlePrefix = NULL, titleSuffix = NULL, 
        plotPkg = 'ggplot', logscale = TRUE, 
        binOrCon = 'bin', palette = 'ag_Sunset', reverse = TRUE,
        lessCertainStars = starsObjU/1000, fadeAmount = 0.75,
        fadeAtts = NULL, fadeRel = TRUE)

# Continuous, binary fade (certain/uncertain)
gppfade(starsObjC/1000, attributeNum = 1, datewanted, units = 'tonnes',
        forcelegend = 'Maximum yearly GPP\n(tonnes O2)', colorchoice = NA,
        titled = FALSE, titlePrefix = NULL, titleSuffix = NULL, 
        plotPkg = 'ggplot', logscale = TRUE, 
        binOrCon = 'con', palette = 'ag_Sunset', reverse = TRUE,
        lessCertainStars = starsObjU/1000, fadeAmount = 0.75,
        fadeAtts = NULL, fadeRel = TRUE)

  # annoying that the sizing is off between those two.

## 2-col bins. These now have MORE uncertainty as bigger numbers, so should probably reverse the fade scale
# Binned, only uncertain, fade by 2 cols (CI, for ex) (though that's actually going to be the same everywhere if it's the same regression. so...)
gppfade(starsObjU/1000, attributeNum = 1, datewanted, units = 'tonnes',
        forcelegend = 'Maximum yearly GPP\n(tonnes O2)', colorchoice = NA,
        titled = FALSE, titlePrefix = NULL, titleSuffix = NULL, 
        plotPkg = 'ggplot', logscale = TRUE, 
        binOrCon = 'bin', palette = 'ag_Sunset', reverse = TRUE,
        lessCertainStars = NULL, fadeAmount = 0.5,
        fadeAtts = c(2,3), fadeRel = TRUE)

# same, more extreme since Rel makes everything the same on log. looks silly, but a good test
gppfade(starsObjU/1000, attributeNum = 1, datewanted, units = 'tonnes',
        forcelegend = 'Maximum yearly GPP\n(tonnes O2)', colorchoice = NA,
        titled = FALSE, titlePrefix = NULL, titleSuffix = NULL, 
        plotPkg = 'ggplot', logscale = TRUE, 
        binOrCon = 'bin', palette = 'ag_Sunset', reverse = TRUE,
        lessCertainStars = NULL, fadeAmount = 0.5,
        fadeAtts = c(2,3), fadeRel = FALSE)

# Continuous, only uncertain, fade by 2 cols (CI, for ex)
gppfade(starsObjU/1000, attributeNum = 1, datewanted, units = 'tonnes',
        forcelegend = 'Maximum yearly GPP\n(tonnes O2)', colorchoice = NA,
        titled = FALSE, titlePrefix = NULL, titleSuffix = NULL, 
        plotPkg = 'ggplot', logscale = TRUE, 
        binOrCon = 'con', palette = 'ag_Sunset', reverse = TRUE,
        lessCertainStars = NULL, fadeAmount = 0.5,
        fadeAtts = c(2,3), fadeRel = FALSE)

  # **TODO** NEED TO sort out the legend size for these where it's getting bigger.

# Binned, only uncertain, fade by a single col (will need to just make this data externally- it's just a check of the fader functionality)

# same, continuous

# Binned, both, but joined and then faded by CI
# Same, continuous


## **TODO** NEED TO CHECK datewanted longer than 1- does that screw up the legend plot method?

## **TODO** new colors for gpp and er

## **TODO** can we make this universal? ie use the xxcontrol functions to do
## things differently for temp, inun, er, gpp, and then use one single plotting
## function? this does not seem gpp-specific.

## TODO use dummy data put the sortin in a markdown




