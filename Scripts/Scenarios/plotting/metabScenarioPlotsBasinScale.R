# script to look at the metabolism scenarios (increase temp, increase volume)

# Based largely on allMetabInunTempPlots and metabolismLocalAndStatic but
# modifying to sort out how best to present the scenario outcomes (and with some
# tsting)


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

# Setup -------------------------------------------------------------------
# Directory to export TO
scriptOut <- file.path('strictOut', 'metabolism')
if (!dir.exists(scriptOut)) {dir.create(scriptOut, recursive = TRUE)}

# Catchments
basinRef <- file.path(datOut, 'ANAEprocessed', 'ltimNoNorth.rdata')

# Directory with inundation
# Use volume; it's most relevant
inunIn <- file.path(datOut, 'Inundationprocessed', 'volInun', 'basinConcat')

# Directory with inundation
# Use volume; it's most relevant
inunIn10p <- file.path(datOut, 'Inundationprocessed', 'vol10p', 'basinConcat')

# Temperature directory
tempIn <- file.path(datOut, 'Tempprocessed', 'weightedMean', 'bimonth', 'basinConcat')

# Climate scenario directory
climIn <- file.path(datOut, 'Climateprocessed', 'weightedMeanCLIM', 'bimonth', 'basinConcat')

# directory with metabolism predictions
# This one's a bit different, since it accesses subfolders with the different metabolisms and the volume scenarios
metabIn <- file.path(datOut, 'TempAndProduction', 'Predictions')
# The climate change directory has the same structure
metclimIn <- file.path(datOut, 'ClimateAndProduction', 'Predictions')
metabStats <- c('logERdays', 'logERdaysvalleys', 'logGPPdays', 'logGPPdaysvalleys')

# i want the bimonth, at least for now to keep tiungs small there's the
# predcictions given water in bimonth itself, but right now I only want the
# outcome multiplied by water, to keep things simple. For a walkthrough, maybe
# we will want that intermediate, but not right now

metabsTemp <- file.path(metabIn, metabStats, 'bimonth', 'predictxvol')
metabsTemp10p <- file.path(metabIn, metabStats, 'bimonth', 'predictxvol10p')

metabsClim<- file.path(metclimIn, metabStats, 'bimonth', 'predictxvol')
metabsClim10p <- file.path(metclimIn, metabStats, 'bimonth', 'predictxvol10p')


# Read in the data from those folders -------------------------------------

# Should make a function, but I have a feeling I've named everything the same.
# going to do this stupidly one at a time to be safe.
# and weirdly, why AREN'T the inundations named catchmentAggregated>?

# The catchment boundaries
load(basinRef)
# Need to re-process the anaes
ltimNoNorth <- st_transform(ltimNoNorth, 3577)

## Inundation
load(file.path(inunIn, 'inunCatchConcat_volInun.rdata'))
inundation <- get('inunBasin') 
rm(list = paste0('inunBasin'))

# Inundation +10%
load(file.path(inunIn10p, 'inunCatchConcat_vol10p.rdata'))
inundation10p <- get('inunBasin') 
rm(list = paste0('inunBasin'))

# Temperatures
load(file.path(tempIn, 'catchmentAggregated.rdata'))
temperature <- get('catchmentBasin') 
rm(list = paste0('catchmentBasin'))

# climate change +2 degrees
load(file.path(climIn, 'catchmentAggregated.rdata'))
climate <- get('catchmentBasin') 
rm(list = paste0('catchmentBasin'))

# metabolism actually is easier if I don't make the vector of paths above but build it in a looop
metabsClim

# I could at least nested loop this for climate, but I'm going to do it ugly
for (i in 1:length(metabStats)) {
  # baseline temps and volumes
  load(file.path(metabIn, metabStats[i], 'bimonth', 'predictxvol', 
                 'basinConcat', 'catchmentAggregated.rdata'))
  assign(metabStats[i], catchmentBasin)
  rm(list = paste0('catchmentBasin'))
  # baseline temps and vol10p
  load(file.path(metabIn, metabStats[i], 'bimonth', 'predictxvol10p',
                 'basinConcat', 'catchmentAggregated.rdata'))
  assign(paste0(metabStats[i], '10p'), catchmentBasin)
  rm(list = paste0('catchmentBasin'))
  
  # climate and baseline vols
  load(file.path(metclimIn, metabStats[i], 'bimonth', 'predictxvol', 
                 'basinConcat', 'catchmentAggregated.rdata'))
  assign(paste0(metabStats[i], 'CLIM'), catchmentBasin)
  rm(list = paste0('catchmentBasin'))
  
  load(file.path(metclimIn, metabStats[i], 'bimonth', 'predictxvol10p',
                 'basinConcat', 'catchmentAggregated.rdata'))
  assign(paste0(metabStats[i], '10p', 'CLIM'), catchmentBasin)
  rm(list = paste0('catchmentBasin'))
}

# # wuick look
inundation
temperature  
inundation10p
climate
logGPPdays
logERdays10pCLIM


# Data cleaning -----------------------------------------------------------

# inundation is bimontyhly, but goes back much further in time than the temps
# (and therefore the metab predictions)

# # need to cut the inundation to the times that match
inuntimes <- st_get_dimension_values(inundation, which = 'time')
inuntimes10p <- st_get_dimension_values(inundation10p, which = 'time')
temptimes <- st_get_dimension_values(temperature, which = 'time')

# Same but worth checking
whichoverlapit <- which(inuntimes %in% temptimes)
whichoverlapit10p <- which(inuntimes %in% temptimes)

# Cut to those
inundation <- inundation %>% slice('time', index = whichoverlapit)
inundation10p <- inundation10p %>% slice('time', index = whichoverlapit10p)

# check
all(st_get_dimension_values(inundation, which = 'time') == 
      st_get_dimension_values(temperature, which = 'time'))

all(st_get_dimension_values(inundation10p, which = 'time') == 
      st_get_dimension_values(temperature, which = 'time'))

# change all the stars to not include hours
# from https://stackoverflow.com/questions/5158830/identify-all-objects-of-given-class-for-further-processing
starFilter <- function(x) inherits(get(x), 'stars')
starobjs <- Filter( starFilter, ls() )

for (i in 1:length(starobjs)) {
  assign(starobjs[i], 
         st_set_dimensions(get(starobjs[i]), which = 'time',
                           values = as.Date(st_get_dimension_values(get(starobjs[i]), which = 'time'))))
}


# quick plots before getting serious --------------------------------------

# static plots
plotInunBasin <- catchAggPlot(inundation[,,1:9], title = 'Total Volume Inundated')
plotTempBasin <- catchAggPlot(temperature[,,1:9], title = 'Average wetland temperature')
plotInun10pBasin <- catchAggPlot(inundation10p[,,1:9], title = 'Total Volume Inundated +10%')
plotClimBasin <- catchAggPlot(climate[,,1:9], title = 'Average wetland temperature +2')
# hard to see differences without fixing the scales. get to that later
plotInunBasin
plotInun10pBasin
plotTempBasin
plotClimBasin

# Just trying to hit various co,bos here

plotGPPBasin <- catchAggPlot(logGPPdays[1,,1:9], title = 'Total GPP')
plotGPPBasinPU <- catchAggPlot(logGPPdays[2,,1:9], title = 'GPP upper PI')
plotGPPBasinPL <- catchAggPlot(logGPPdays[3,,1:9], title = 'GPP lower PI')

plotGPPBasin
plotGPPBasinPU
plotGPPBasinPL

plotERBasinTC <- catchAggPlot(logERdays10pCLIM[1,,1:9], title = 'Total ER')
plotERBasinCUTC <- catchAggPlot(logERdays10pCLIM[4,,1:9], title = 'ER upper CI')
plotERBasinCLTC <- catchAggPlot(logERdays10pCLIM[5,,1:9], title = 'ER lower CI')

plotERBasinTC
plotERBasinCUTC
plotERBasinCLTC

# OK, things seem to have worked. NOW, need to sort out exactly what we want to show and build that up.
# almost wonder if this should be an Rmarkdown so we can write text as we go. Does rmd handle gifs? what about the cool tmaps?
# suppose lets build the plots and go from there.