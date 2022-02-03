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
# library(tmap)
# library(transformr)
# library(gganimate)
# library(viridis)
# library(colorspace)


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



# Make yearly aggregations ------------------------------------------------

# WHY DOES TEMPAGGREGATE FLIP DIMS?????

# when everything being summed is NA, sum(na.rm = TRUE) gives 0, but I need it to be NA. So define a function

# when everything being summed is NA, sum(na.rm = TRUE) gives 0, but I need it to be NA. So define a function
# on the backend, some of the code expects a na.rm so pass it I guess
# really, should make these generic and accept the FUN, but not now
sumna <- function(x, na.rm = TRUE) {
  ifelse(all(is.na(x)), NA, sum(x, na.rm = TRUE))
}

meanna <- function(x, na.rm = TRUE) {
  ifelse(all(is.na(x)), NA, mean(x, na.rm = TRUE))
}

#
# This is super ugly. should have looped. Oh well.

# Can I make simple annual reporting?
interDates <- as.POSIXct(c("2014-06-30", "2015-06-30", "2016-06-30", "2017-06-30", "2018-06-30", "2019-06-30"))

# Mean is the appropriate stat for temps
climateannual <- tempaggregate(climate, by_t = as.Date(interDates), 
                               FUN = meanna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time'))

temperatureannual <- tempaggregate(temperature, by_t = as.Date(interDates), 
                                   FUN = meanna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time'))

# Sum to get the total inundation
inundationannual <- tempaggregate(inundation, by_t = as.Date(interDates), 
                                  FUN = sumna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time'))
inundation10pannual <- tempaggregate(inundation10p, by_t = as.Date(interDates), 
                                     FUN = sumna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time'))

# sum to get total ER
logERdaysannual <- tempaggregate(logERdays, by_t = as.Date(interDates), 
                                  FUN = sumna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time'))

logERdays10pannual <- tempaggregate(logERdays10p, by_t = as.Date(interDates), 
                                  FUN = sumna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time'))

logERdays10pCLIMannual <- tempaggregate(logERdays10pCLIM, by_t = as.Date(interDates), 
                                  FUN = sumna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time'))

logERdaysCLIMannual <- tempaggregate(logERdaysCLIM, by_t = as.Date(interDates), 
                                        FUN = sumna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time'))

logERdaysvalleysannual <- tempaggregate(logERdaysvalleys, by_t = as.Date(interDates), 
                                 FUN = sumna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time'))

logERdaysvalleys10pannual <- tempaggregate(logERdaysvalleys10p, by_t = as.Date(interDates), 
                                    FUN = sumna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time'))

logERdaysvalleys10pCLIMannual <- tempaggregate(logERdaysvalleys10pCLIM, by_t = as.Date(interDates), 
                                        FUN = sumna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time'))

logERdaysvalleysCLIMannual <- tempaggregate(logERdaysvalleysCLIM, by_t = as.Date(interDates), 
                                     FUN = sumna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time'))

# sum to get total ER
logGPPdaysannual <- tempaggregate(logGPPdays, by_t = as.Date(interDates), 
                                 FUN = sumna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time'))

logGPPdays10pannual <- tempaggregate(logGPPdays10p, by_t = as.Date(interDates), 
                                    FUN = sumna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time'))

logGPPdays10pCLIMannual <- tempaggregate(logGPPdays10pCLIM, by_t = as.Date(interDates), 
                                        FUN = sumna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time'))

logGPPdaysCLIMannual <- tempaggregate(logGPPdaysCLIM, by_t = as.Date(interDates), 
                                     FUN = sumna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time'))

logGPPdaysvalleysannual <- tempaggregate(logGPPdaysvalleys, by_t = as.Date(interDates), 
                                        FUN = sumna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time'))

logGPPdaysvalleys10pannual <- tempaggregate(logGPPdaysvalleys10p, by_t = as.Date(interDates), 
                                           FUN = sumna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time'))

logGPPdaysvalleys10pCLIMannual <- tempaggregate(logGPPdaysvalleys10pCLIM, by_t = as.Date(interDates), 
                                               FUN = sumna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time'))

logGPPdaysvalleysCLIMannual <- tempaggregate(logGPPdaysvalleysCLIM, by_t = as.Date(interDates), 
                                            FUN = sumna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time'))

