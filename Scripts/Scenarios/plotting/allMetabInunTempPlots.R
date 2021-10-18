# Plot metabolism and its drivers across the basin through time
# Plot inundation
# Libraries and system setup
source('directorySet.R')

# Let's get libraries here, then sort out git then sort out making this a library so we don't have to deal with all the library crap
# library(sp)
# library(rgeos)
library(here)
library(tidyverse)
library(sf)
library(stars)
library(tmap)
library(transformr)
library(gganimate)
library(viridis)
library(colorspace)


# Set the crs
whichcrs <- 3577
# directory


# This is all based on inundationCatchmentPlots, but modified to focus on the outcomes (GPP and ER)

# Setup -------------------------------------------------------------------
# Directory to export TO
scriptOut <- file.path('strictOut', 'inundation')
if (!dir.exists(scriptOut)) {dir.create(scriptOut, recursive = TRUE)}

# Catchments
basinRef <- file.path(datOut, 'ANAEprocessed', 'ltimNoNorth.rdata')

# Directory with inundation
# Use volume; it's most relevant
inunIn <- file.path(datOut, 'Inundationprocessed', 'volInun', 'basinConcat')

# Temperature directory
tempIn <- file.path(datOut, 'Tempprocessed', 'weightedMean', 'bimonth', 'basinConcat')

# directory with metabolism predictions
metabIn <- file.path(datOut, 'TempAndProduction', 'Predictions', 'bimonth', 'predictxvol', 'basinConcat')
# Also have the potential metab (ie ignoring water) but I think won't use
metabPotentialIn <- file.path(datOut, 'TempAndProduction', 'Predictions', 'bimonth', 'basinConcat')



# Read in data ------------------------------------------------------------

# # Really could be a function
# for(sfun in 1:length(summaryFuns)) {
#   summaryFun <- summaryFuns[sfun]
#   
#   # There are some that were NOT chunked- leave them alone, and just look in the chunked folder
#   inunIn <- paste0(datOut, '/Inundationprocessed/', summaryFun)
#   anaeIn <- file.path(datOut, 'ANAEprocessed')
#   
#   scriptFigOut <- file.path('strictOut', 'inundation')
#   scriptDatOut <- file.path(inunIn, 'basinConcat')
#   
  # Read in the data
    # I gave these uninformative names, annoyingly
load(file.path(inunIn, 'inunCatchConcat_volInun.rdata'))
inundation <- get('inunBasin') 
rm(list = paste0('inunBasin'))
# The inundation script changes the name to area for both area and volume. Need to fix that
names(inundation) <- 'volumeinundated'

load(file.path(tempIn, 'catchmentAggregated.rdata'))
temperature <- get('catchmentBasin') 
rm(list = paste0('catchmentBasin'))

load(file.path(metabIn, 'catchmentAggregated.rdata'))
metabolism <- get('catchmentBasin') 
rm(list = paste0('catchmentBasin'))

# mostly as a check, load the potentials
load(file.path(metabPotentialIn, 'catchmentAggregated.rdata'))
metabolismPotential <- get('catchmentBasin') 
rm(list = paste0('catchmentBasin'))
 
load(basinRef)
st_transform(ltimNoNorth, st_crs(metabolism))

# wuick look
inundation
temperature  
metabolism
metabolismPotential

# need to cut the inundation to the times that match
inuntimes <- st_get_dimension_values(inundation, which = 'time')
temptimes <- st_get_dimension_values(temperature, which = 'time')
metabtimes <- st_get_dimension_values(metabolism, which = 'time')

whichoverlapit <- which(inuntimes %in% temptimes)
overlaptimesit <- inuntimes[whichoverlapit]
whichoverlapim <- which(inuntimes %in% metabtimes)
overlaptimesim <- inuntimes[whichoverlapim]
all(overlaptimesit == overlaptimesim)

matchtimes <- overlaptimesim

# Cut to those
# Get the relevant volumes
inundation <- inundation %>% slice('time', index = whichoverlapim)

# And, to make the plots cleaner, cut the hours off the times
inundation <- st_set_dimensions(inundation, which = 'time', 
                                values = as.Date(st_get_dimension_values(inundation, which = 'time')))
temperature <- st_set_dimensions(temperature, which = 'time', 
                                values = as.Date(st_get_dimension_values(temperature, which = 'time')))
metabolism <- st_set_dimensions(metabolism, which = 'time', 
                                values = as.Date(st_get_dimension_values(metabolism, which = 'time')))
metabolismPotential <- st_set_dimensions(metabolismPotential, which = 'time', 
                                values = as.Date(st_get_dimension_values(metabolismPotential, which = 'time')))



# static plots
  plotInunBasin <- catchAggPlot(inundation[,,1:9], title = 'Total Volume Inundated')
  plotTempBasin <- catchAggPlot(temperature[,,1:9], title = 'Average wetland temperature')
  plotGPPBasinV <- catchAggPlot(metabolism[1,,1:9], title = 'Total GPP (valley term)')
  plotGPPBasin <- catchAggPlot(metabolism[2,,1:9], title = 'Total GPP (no valley term)')
  
  plotInunBasin
  plotTempBasin
  plotGPPBasinV
  plotGPPBasin
  

# Some testing and static plots- uncomment for quick looks at thin --------

  
  # # Plot the basin to try to sort out what's missing where things are missing
  # basinPlot <- ggplot() +
  #   geom_sf(data = ltimNoNorth, mapping = aes(fill = ValleyName)) +
  #   geom_sf_label(data = ltimNoNorth, mapping = aes(label = ValleyName)) +
  #   coord_sf() +
  #   theme_void()  +
  #   # scale_fill_gradient(low = 'firebrick', high = 'forestgreen' ) +
  #   # scale_fill_brewer(palette = 'Dark2') +
  #   ggtitle('Catchments')
  # basinPlot
  # 
  # # Try to id the missing basins by overlaying
  # # inundation checks
  # ggplot() + 
  #   geom_sf(data = ltimNoNorth, mapping = aes(color = ValleyName)) +
  #   geom_stars(data = inundation[,,4:5]) +
  #   geom_sf_label(data = ltimNoNorth, mapping = aes(label = ValleyName)) +
  #   scale_fill_viridis(option = 'mako')
  #   
  # # Temps are fine
  # 
  # # GPP (valley term)
  # ggplot() + 
  #   geom_stars(data = metabolism[1,,5]) +
  #   geom_sf(data = ltimNoNorth, mapping = aes(color = ValleyName), fill = NA) +
  #   geom_sf_label(data = ltimNoNorth, mapping = aes(label = ValleyName), size = 3) # +
  #   # scale_fill_viridis(option = 'mako')
  # # GPP (no valley term)
  # ggplot() + 
  #   geom_stars(data = metabolism[2,,5]) +
  #   geom_sf(data = ltimNoNorth, mapping = aes(color = ValleyName), fill = NA) +
  #   geom_sf_label(data = ltimNoNorth, mapping = aes(label = ValleyName), size = 3)
  # # ER (valley term)
  # ggplot() + 
  #   geom_stars(data = metabolism[3,,5]) +
  #   geom_sf(data = ltimNoNorth, mapping = aes(color = ValleyName), fill = NA) +
  #   geom_sf_label(data = ltimNoNorth, mapping = aes(label = ValleyName), size = 3) # +
  # # scale_fill_viridis(option = 'mako')
  # # ER (no valley term)
  # ggplot() + 
  #   geom_stars(data = metabolism[4,,5]) +
  #   geom_sf(data = ltimNoNorth, mapping = aes(color = ValleyName), fill = NA) +
  #   geom_sf_label(data = ltimNoNorth, mapping = aes(label = ValleyName), size = 3)
  # 
  

# Print an example scaled up plot -----------------------------------------
  gppNoValley <- ggplot() +
    geom_stars(data = metabolism[2,,15]) +
    coord_sf() +
    # facet_wrap(~as.character(time)) +
    theme_void()  +
    # scale_fill_gradient(low = 'firebrick', high = 'forestgreen' ) +
    scale_fill_continuous_sequential(palette = 'Emrld') +
    labs(fill = 'Total wetland\nGPP')
gppNoValley
  
  
png(file.path(scriptOut, 'GPPflatexampleplot.png'), height = 12/2.54, width = 12/2.54, units = 'in', res = 300)
gppNoValley
dev.off()



gppNoValley2 <- ggplot() +
  geom_stars(data = metabolism[2,,20]) +
  coord_sf() +
  # facet_wrap(~as.character(time)) +
  theme_void()  +
  # scale_fill_gradient(low = 'firebrick', high = 'forestgreen' ) +
  scale_fill_continuous_sequential(palette = 'Emrld') +
  labs(fill = 'Total wetland\nGPP')
gppNoValley2


png(file.path(scriptOut, 'GPPflatexampleplot2.png'), height = 12/2.54, width = 12/2.54, units = 'in', res = 300)
gppNoValley2
dev.off()


# Can I make simple annual reporting?
interDates <- as.POSIXct(c("2014-06-30", "2015-06-30", "2016-06-30", "2017-06-30", "2018-06-30", "2019-06-30"))

metannual <- tempaggregate(metabolism, by_t = as.Date(interDates), FUN = sum, na.rm = TRUE)

gppNoValleyTimeCompare <- ggplot() +
  geom_stars(data = metannual[2,,]) +
  coord_sf() +
  facet_wrap(~as.character(time)) +
  theme_void()  +
  # scale_fill_gradient(low = 'firebrick', high = 'forestgreen' ) +
  scale_fill_continuous_sequential(palette = 'Emrld') +
  labs(fill = 'Yearly wetland\nGPP')
gppNoValleyTimeCompare


png(file.path(scriptOut, 'GPPflatexampleplot.png'), height = 12/2.54, width = 12/2.54, units = 'in', res = 300)
gppNoValley
dev.off()




# Animations --------------------------------------------------------------

  
  # Inundation animation
  # The smooth version looks better, but is less accurate
  inunAnimSmooth <- ggplot() +
    geom_stars(data = log(inundation)) +
    coord_sf() +
    # facet_wrap(~as.character(time)) +
    theme_void()  +
    # scale_fill_gradient(low = 'firebrick', high = 'forestgreen' ) +
    # scale_fill_viridis(option = 'mako') +
    scale_fill_continuous_sequential(palette = 'Blue-Yellow') +
    # gganimate bits
    # only the times that exist, but less smooth
    # gganimate bits
    # transition_states(time) +
    # labs(title = 'Time: {closest_state}', fill = 'log(Volume)') +
    # # Smooth time, but has lots of extra 'times' that aren't in the data
    transition_time(time) +
    labs(title = 'Volume wetlands inundated {as.Date(frame_time)}', fill = 'log(Volume)') + #,
         # fill = paste0('log(', statname, 'inundated)') +
    ease_aes('linear')
  
  # Sort of tempted to change the color ramp here to have more 'dirt', but the
  # idea really is that everything has SOME water
    # Shifting th top down a bit helped. Still needs some work though
  inunAnim <- ggplot() +
    geom_stars(data = log(inundation)) +
    coord_sf() +
    # facet_wrap(~as.character(time)) +
    theme_void(base_size = 22)  +
    # scale_fill_gradient(low = 'firebrick', high = 'forestgreen' ) +
    # scale_fill_viridis(option = 'mako') +
    # scale_fill_continuous_sequential(palette = 'YLGnBu', end = 0.7) +
    scale_fill_continuous_divergingx(palette = 'Earth', 
                                     mid = 0,
                                     rev = FALSE) +
    # scale_fill_continuous_sequential(palette = 'Blue-Yellow') +
    # gganimate bits
    # only the times that exist, but less smooth
    # gganimate bits
    transition_states(time) +
    labs(title = 'Volume wetlands inundated\n{closest_state}', fill = 'log(Volume)') +
    # # Smooth time, but has lots of extra 'times' that aren't in the data
    # transition_time(time) +
    # labs(title = 'Volume inundated {as.Date(frame_time)}', fill = log(Volume)) + #,
    # fill = paste0('log(', statname, 'inundated)') +
    ease_aes('linear')
  # catchAnim
  
  animate(inunAnim, duration = 41*0.5)
  # animate(inunAnimSmooth, duration = 41*0.5)
  
  # I think even though the smooth looks better, we should stick to the actual data
  
  # Temperature animation
  # colorspace's red-blue isn't super great. Try creating my own
  tempcolors <- c("blue", "green", "yellow", "red")
  
  tempAnim <- ggplot() +
    geom_stars(data = temperature) +
    coord_sf() +
    # facet_wrap(~as.character(time)) +
    theme_void(base_size = 22)  +
    # scale_fill_gradient(low = 'firebrick', high = 'forestgreen' ) +
    # scale_fill_viridis(option = 'plasma') + # Need to change the color
    # scale_fill_gradientn(colours = tempcolors) +
    # scale_fill_continuous_sequential(palette = 'Red-Blue') +
  #   https://colorspace.r-forge.r-project.org/reference/scale_colour_continuous_divergingx.html
  # https://colorspace.r-forge.r-project.org/articles/approximations.html#overview-1
    # 'Temps' doesn't quite get cold enough- sort of stops at green. Use rev = FALSE for temps
    # 'Roma' is pretty good, but dark
    # 'RdYlBu' is similar to Roma, but brighter. Reds a bit too bright?
    # Going with Spectral. I like that.
    scale_fill_continuous_divergingx(palette = 'Spectral', 
                                     mid = median(temperature[[1]], na.rm = TRUE),
                                     rev = TRUE) + # RdYlBu, 'Spectral', 'Geyser', 'Temps', 'Roma'
    # gganimate bits
    # only the times that exist, but less smooth
    # gganimate bits
    transition_states(time) +
    labs(title = 'Mean wetland temperature\n{closest_state}', fill = 'Temp') +
    # # Smooth time, but has lots of extra 'times' that aren't in the data
    # transition_time(time) +
    # labs(title = 'Temperature {as.Date(frame_time)}') + #,
    # fill = paste0('log(', statname, 'inundated)')) +
    ease_aes('linear')
  # catchAnim
  
  # animate(tempAnim, duration = 41*0.5)
  
  # GPP with a Valley term animation
  gppValleyAnim <- ggplot() +
    geom_stars(data = log(metabolism[1,,])) +
    coord_sf() +
    # facet_wrap(~as.character(time)) +
    theme_void(base_size = 22)  +
    # scale_fill_gradient(low = 'firebrick', high = 'forestgreen' ) +
    scale_fill_continuous_sequential(palette = 'Emrld') + # Mint and Green-Yellow probabyl work too
    # gganimate bits
    # only the times that exist, but less smooth
    # gganimate bits
    transition_states(time) +
    labs(title = 'Total wetland GPP\n{closest_state}', fill = 'log(GPP)') +
    # # Smooth time, but has lots of extra 'times' that aren't in the data
    # transition_time(time) +
    # labs(title = 'Total Aquatic GPP {as.Date(frame_time)}', fill = 'GPP') + #,
    # fill = paste0('log(', statname, 'inundated)')) +
    ease_aes('linear')
  # catchAnim
  
  # animate(gppValleyAnim, duration = 41*0.5)
  
  # GPP without a Valley term animation
  gppNoValleyAnim <- ggplot() +
    geom_stars(data = log(metabolism[2,,])) +
    coord_sf() +
    # facet_wrap(~as.character(time)) +
    theme_void(base_size = 22)  +
    # scale_fill_gradient(low = 'firebrick', high = 'forestgreen' ) +
    scale_fill_continuous_sequential(palette = 'Emrld') +
    # gganimate bits
    # only the times that exist, but less smooth
    # gganimate bits
    transition_states(time) +
    labs(title = 'Total wetland GPP\n{closest_state}', fill = 'log(GPP)') +
    # # Smooth time, but has lots of extra 'times' that aren't in the data
    # transition_time(time) +
    # labs(title = 'Total Aquatic GPP {as.Date(frame_time)}') + #,
    # fill = paste0('log(', statname, 'inundated)')) +
    ease_aes('linear')
  # catchAnim
  
  # animate(gppNoValleyAnim, duration = 41*0.5)
  
  
  # ER with a Valley term animation
  erValleyAnim <- ggplot() +
    geom_stars(data = log(metabolism[3,,])) +
    coord_sf() +
    # facet_wrap(~as.character(time)) +
    theme_void(base_size = 22)  +
    # scale_fill_gradient(low = 'firebrick', high = 'forestgreen' ) +
    scale_fill_continuous_sequential(palette = 'Purples') +
    # gganimate bits
    # only the times that exist, but less smooth
    # gganimate bits
    transition_states(time) +
    labs(title = 'Total wetland ER\n{closest_state}', fill = 'log(ER)') +
    # # Smooth time, but has lots of extra 'times' that aren't in the data
    # transition_time(time) +
    # labs(title = 'ER {as.Date(frame_time)}') + #,
    # fill = paste0('log(', statname, 'inundated)')) +
    ease_aes('linear')
  # catchAnim
  
  # animate(erValleyAnim, duration = 41*0.5)
  
  # er without a Valley term animation
  erNoValleyAnim <- ggplot() +
    geom_stars(data = log(metabolism[4,,])) +
    coord_sf() +
    # facet_wrap(~as.character(time)) +
    theme_void(base_size = 22)  +
    # scale_fill_gradient(low = 'firebrick', high = 'forestgreen' ) +
    scale_fill_continuous_sequential(palette = 'Purples') +
    # gganimate bits
    # only the times that exist, but less smooth
    # gganimate bits
    transition_states(time) +
    labs(title = 'Total wetland ER\n{closest_state}', fill = 'log(ER)') +
    # # Smooth time, but has lots of extra 'times' that aren't in the data
    # transition_time(time) +
    # labs(title = 'ER {as.Date(frame_time)}') + #,
    # fill = paste0('log(', statname, 'inundated)')) +
    ease_aes('linear')
  # catchAnim
  
  # animate(erNoValleyAnim, duration = 41*0.5)
  
  ## Can I plot a 'degree of autotrophy'?
    # have to log the autotroph index since it's divided
  autotroph <- metabolism[2,,]/metabolism[4,,]
  autotroph <- log(autotroph)
  names(autotroph) <- 'logAutotroph'
  
  autotrophAnim <- ggplot() +
    geom_stars(data = autotroph) +
    coord_sf() +
    # facet_wrap(~as.character(time)) +
    theme_void(base_size = 22)  +
    # scale_fill_gradient(low = 'firebrick', high = 'forestgreen' ) +
    # scale_fill_continuous_sequential(palette = 'PRGn') +
    scale_fill_continuous_divergingx(palette = 'PRGn', 
                                     mid = 0,
                                     rev = FALSE) +
    # gganimate bits
    # only the times that exist, but less smooth
    # gganimate bits
    transition_states(time) +
    labs(title = 'Autotrophic index\n{closest_state}', fill = 'log(GPP/ER)') +
    # # Smooth time, but has lots of extra 'times' that aren't in the data
    # transition_time(time) +
    # labs(title = 'ER {as.Date(frame_time)}') + #,
    # fill = paste0('log(', statname, 'inundated)')) +
    ease_aes('linear')
  # catchAnim
  
  # animate(autotrophAnim, duration = 41*0.5)
  # Works, but it doesn't ever go green. Which I guess I expected. Still, would be nice to occasionally flash green
  
# Would be nice to match them up. can't facet, or I lose the ability to set color scales. 
  
  # If I just save them and dump in a ppt, do they go simultaneously?
  
  # ggpubr::ggarrange(inunAnim, tempAnim)
  
  # save all the animate plots, put in a ppt
  # Check their labs, etc
  # Save a couple examples of static tmaps
  
  # Save code- write a bit later
  anim_save(filename = file.path(scriptOut, paste0('volumeAnim.gif')), 
            animation = animate(inunAnim, duration = 41*0.5))
  
  anim_save(filename = file.path(scriptOut, paste0('tempAnim.gif')), 
            animation = animate(tempAnim, duration = 41*0.5))
  
  anim_save(filename = file.path(scriptOut, paste0('gppValleyAnim.gif')), 
            animation = animate(gppValleyAnim, duration = 41*0.5))
  
  anim_save(filename = file.path(scriptOut, paste0('gppNoValleyAnim.gif')), 
            animation = animate(gppNoValleyAnim, duration = 41*0.5))
  
  anim_save(filename = file.path(scriptOut, paste0('erValleyAnim.gif')), 
            animation = animate(erValleyAnim, duration = 41*0.5))
  
  anim_save(filename = file.path(scriptOut, paste0('erNoValleyAnim.gif')), 
            animation = animate(erNoValleyAnim, duration = 41*0.5))
  
  anim_save(filename = file.path(scriptOut, paste0('autotrophAnim.gif')), 
            animation = animate(autotrophAnim, duration = 41*0.5))
  
  
# Can I make tmaps with those? --------------------------------------------
  # Tmap seems to need sf, not stars
  # Not actually any better, and harder to control. It's cool for static maps,
  # but the animate loses the nice backgrounds. 
  # and WAY less control over colors
  tmap_mode("view")
  inunsf <- inundation %>%
    st_as_sf(long = TRUE) %>%
    mutate(logVolInundated = log(volumeinundated))

  inunTMA <- tm_shape(inunsf) + tm_fill('logVolInundated', palette = 'cividis') + 
    tm_facets(along = 'time') 
  
  tmap_animation(inunTMA)
  
  # Compare that to a static
  tm_shape(filter(inunsf, time < ymd('20140201'))) + tm_fill('logVolInundated', palette = 'cividis')
  # Where the base map really adds a lot.But losing it makes the animate less interesting
  
  # tm_shape(singlepoints) + tm_symbols('gpp')
  
  # The 'veiw' versions would be cool online. But for printing, I'm not seeing an advantage.
  tmap_mode("view")
  tm_shape(filter(inunsf, time < ymd('20140201'))) + tm_fill('logVolInundated', palette = 'cividis')
  
  
  tmap_mode("plot")
  tm_shape(filter(inunsf, time < ymd('20140201'))) + tm_fill('logVolInundated', palette = 'cividis')
  
  
  ## TODO:
  