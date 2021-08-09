# Plot the inundation at the basin and through time
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
library(transformr)
library(gganimate)
library(viridis)
library(foreach)
library(doFuture)

# Set up parallel backend
registerDoFuture()
# plan(multisession)
plan(sequential) # dopar doesn't work, so no sense setting up a multisession

# This is almost EXACTLY a library load at this point. Just need to actually wrap it up and split the git
basicfuns <- list.files(here('Functions'))
basicfuns <- file.path('Functions', basicfuns)
# read in those functions
sapply(basicfuns, source)

# Set the crs
whichcrs <- 3577
# directory
summaryFun <- 'areaInun'
# There are some that were NOT chunked- leave them alone, and just look in the chunked folder
inunIn <- paste0(datOut, '/Inundationprocessed/', summaryFun)
anaeIn <- file.path(datOut, 'ANAEprocessed')

scriptFigOut <- file.path('strictOut', 'inundation')
scriptDatOut <- file.path(inunIn, 'basinConcat')

load(file.path(scriptDatOut, paste0('inunCatchConcat_', summaryFun, '.rdata')))


# and plot
plotInunBasin <- catchAggPlot(inunBasin, title = 'Total Area Inundated')
# plotInunBasin

# # Ugh. scales are wonky- handbuild
# catchPlot <- ggplot() +
#   geom_stars(data = log(inunBasin)) +
#   coord_sf() +
#   facet_wrap(~as.character(time)) +
#   theme_void()  +
#   # scale_fill_gradient(low = 'firebrick', high = 'forestgreen' ) +
#   scale_fill_viridis(option = 'mako')
# catchPlot

# Can i use gganimate with geom_stars()
# library(gganimate)
# library(transformr)
catchAnim <- ggplot() +
  geom_stars(data = log(inunBasin)) +
  coord_sf() +
  # facet_wrap(~as.character(time)) +
  theme_void()  +
  # scale_fill_gradient(low = 'firebrick', high = 'forestgreen' ) +
  scale_fill_viridis(option = 'mako') +
  # gganimate bits
  # only the times that exist, but less smooth
  # gganimate bits
  # transition_states(time) +
  # labs(title = 'Time: {closest_state}', fill = 'log(Area inundated)') +
  # # Smooth time, but has lots of extra 'times' that aren't in the data
  transition_time(time) +
  labs(title = 'Time: {as.Date(frame_time)}', fill = 'log(Area inundated)') +
  ease_aes('linear')
# catchAnim

# animate(catchAnim, duration = 197*0.5)

anim_save(filename = file.path(scriptFigOut, 'inundationTime.gif'), 
          animation = animate(catchAnim, duration = 197*0.25))
