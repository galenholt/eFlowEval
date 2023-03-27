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
library(lubridate)
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
scriptOut <- file.path('strictOut', 'metabolism', 'basin')
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

# i want the bimonth, at least for now to keep things small there's the
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
maxna <- function(x, na.rm = TRUE) {
  ifelse(all(is.na(x)), NA, sum(x, na.rm = TRUE))
}

meanna <- function(x, na.rm = TRUE) {
  ifelse(all(is.na(x)), NA, mean(x, na.rm = TRUE))
}

maxna <- function(x, na.rm = TRUE) {
  ifelse(all(is.na(x)), NA, max(x, na.rm = TRUE))
}

#
# This is super ugly. should have looped. Oh well.

# Can I make simple annual reporting?
interDates <- as.POSIXct(c("2014-06-30", "2015-06-30", "2016-06-30", "2017-06-30", "2018-06-30", "2019-06-30", "2020-06-30"))

# Mean is the appropriate stat for temps
climateannual <- tempaggregate(climate, by_t = as.Date(interDates), 
                               FUN = meanna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time'))

temperatureannual <- tempaggregate(temperature, by_t = as.Date(interDates), 
                                   FUN = meanna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time'))

# Sum to get the total inundation
inundationannual <- tempaggregate(inundation, by_t = as.Date(interDates), 
                                  FUN = maxna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time'))
inundation10pannual <- tempaggregate(inundation10p, by_t = as.Date(interDates), 
                                     FUN = maxna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time'))

# sum to get total ER
logERdaysannual <- tempaggregate(logERdays, by_t = as.Date(interDates), 
                                  FUN = maxna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time'))

logERdays10pannual <- tempaggregate(logERdays10p, by_t = as.Date(interDates), 
                                  FUN = maxna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time'))

logERdays10pCLIMannual <- tempaggregate(logERdays10pCLIM, by_t = as.Date(interDates), 
                                  FUN = maxna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time'))

logERdaysCLIMannual <- tempaggregate(logERdaysCLIM, by_t = as.Date(interDates), 
                                        FUN = maxna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time'))

logERdaysvalleysannual <- tempaggregate(logERdaysvalleys, by_t = as.Date(interDates), 
                                 FUN = maxna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time'))

logERdaysvalleys10pannual <- tempaggregate(logERdaysvalleys10p, by_t = as.Date(interDates), 
                                    FUN = maxna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time'))

logERdaysvalleys10pCLIMannual <- tempaggregate(logERdaysvalleys10pCLIM, by_t = as.Date(interDates), 
                                        FUN = maxna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time'))

logERdaysvalleysCLIMannual <- tempaggregate(logERdaysvalleysCLIM, by_t = as.Date(interDates), 
                                     FUN = maxna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time'))

# sum to get total ER
logGPPdaysannual <- tempaggregate(logGPPdays, by_t = as.Date(interDates), 
                                 FUN = maxna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time'))

logGPPdays10pannual <- tempaggregate(logGPPdays10p, by_t = as.Date(interDates), 
                                    FUN = maxna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time'))

logGPPdays10pCLIMannual <- tempaggregate(logGPPdays10pCLIM, by_t = as.Date(interDates), 
                                        FUN = maxna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time'))

logGPPdaysCLIMannual <- tempaggregate(logGPPdaysCLIM, by_t = as.Date(interDates), 
                                     FUN = maxna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time'))

logGPPdaysvalleysannual <- tempaggregate(logGPPdaysvalleys, by_t = as.Date(interDates), 
                                        FUN = maxna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time'))

logGPPdaysvalleys10pannual <- tempaggregate(logGPPdaysvalleys10p, by_t = as.Date(interDates), 
                                           FUN = maxna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time'))

logGPPdaysvalleys10pCLIMannual <- tempaggregate(logGPPdaysvalleys10pCLIM, by_t = as.Date(interDates), 
                                               FUN = maxna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time'))

logGPPdaysvalleysCLIMannual <- tempaggregate(logGPPdaysvalleysCLIM, by_t = as.Date(interDates), 
                                            FUN = maxna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time'))




# a 5-year aggregation ----------------------------------------------------

# over the dailies, not the yearlies

# what are some limits?

td <- st_get_dimension_values(temperature, which = 'time')
td
by_t <- as.Date(interDates)
rbind(td, findInterval(td, by_t, rightmost.closed = TRUE))
temperatureannual # so there's 8 intervals, but we only get 6 out of the aggregation- those below the lower number and above the upper are discarded
# SO, the dates are the STARTING values of the interval, NOT the ending
st_get_dimension_values(temperatureannual, which = 'time')

# What I really want is a way to get the interval for a 5-year reporting period. 
# Let's say that runs from 2014-06 to 2019-06. is that in inundation?
st_get_dimension_values(inundation, which = 'time') # yes

by_t <- as.Date(interDates)[c(1,6)]
by_t
rbind(td, findInterval(td, by_t, rightmost.closed = TRUE))
rbind(st_get_dimension_values(inundation, which = 'time'), findInterval(td, by_t, rightmost.closed = TRUE))

# now, let's test I'm right about how the intervals work. this should give 1 slice from by_t[1] to 2].
# argh it's dropping that dimension, but shouldn't. It ignores drop = FALSE, even though I'm sure it's using aperm under the hood.

# rather than re-write the stars.aggregate, or try to fix it in tempaggregate,
# which would involce adding a dimension which is super annoying,
# add a throwaway to by_t
by_t <- c(as.Date("2000-06-30"), by_t)
rbind(td, findInterval(td, by_t, rightmost.closed = TRUE))
rbind(st_get_dimension_values(inundation, which = 'time'), findInterval(td, by_t, rightmost.closed = TRUE))

# So now, that's saving from 2000-2014 and 2014-2019. We only want the latter,
# but need to save the former to get the time dimension

climate5_test <- tempaggregate(climate, by_t = by_t, # "5 years"
                          FUN = meanna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time')) %>%
  slice("time", 2, drop = FALSE)
climate5_test
plot(climate5_test)

# OK, done testing. Let's make those


# 5-year period from 2014-06 to 2019-06 -----------------------------------
# have to do a stupid thing here to keep > 1 slice or the array dims collapse
by_t_5 <- c(as.Date("2000-06-30"), as.Date(interDates)[c(1,6)])

temperature5 <- tempaggregate(temperature, by_t = by_t_5, # "5 years"
                          FUN = meanna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time')) %>%
  slice("time", 2, drop = FALSE)
# temperature5

climate5 <- tempaggregate(climate, by_t = by_t_5, # "5 years"
                               FUN = meanna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time')) %>%
  slice("time", 2, drop = FALSE)
# climate5

# Sum to get the total inundation
inundation5 <- tempaggregate(inundation, by_t = by_t_5, 
                                  FUN = maxna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time')) %>% 
  slice('time', 2, drop = FALSE) 

inundation10p5 <- tempaggregate(inundation10p, by_t = by_t_5, 
                                     FUN = maxna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time')) %>% 
  slice('time', 2, drop = FALSE) 

# sum to get total ER
logERdays5 <- tempaggregate(logERdays, by_t = by_t_5, 
                                 FUN = maxna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time')) %>% 
  slice('time', 2, drop = FALSE) 

logERdays10p5 <- tempaggregate(logERdays10p, by_t = by_t_5, 
                                    FUN = maxna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time')) %>% 
  slice('time', 2, drop = FALSE) 

logERdays10pCLIM5 <- tempaggregate(logERdays10pCLIM, by_t = by_t_5, 
                                        FUN = maxna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time')) %>% 
  slice('time', 2, drop = FALSE) 

logERdaysCLIM5 <- tempaggregate(logERdaysCLIM, by_t = by_t_5, 
                                     FUN = maxna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time')) %>% 
  slice('time', 2, drop = FALSE) 

logERdaysvalleys5 <- tempaggregate(logERdaysvalleys, by_t = by_t_5, 
                                        FUN = maxna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time')) %>% 
  slice('time', 2, drop = FALSE) 

logERdaysvalleys10p5 <- tempaggregate(logERdaysvalleys10p, by_t = by_t_5, 
                                           FUN = maxna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time')) %>% 
  slice('time', 2, drop = FALSE) 

logERdaysvalleys10pCLIM5 <- tempaggregate(logERdaysvalleys10pCLIM, by_t = by_t_5, 
                                               FUN = maxna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time')) %>% 
  slice('time', 2, drop = FALSE) 

logERdaysvalleysCLIM5 <- tempaggregate(logERdaysvalleysCLIM, by_t = by_t_5, 
                                            FUN = maxna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time')) %>% 
  slice('time', 2, drop = FALSE) 

# sum to get total ER
logGPPdays5 <- tempaggregate(logGPPdays, by_t = by_t_5, 
                                  FUN = maxna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time')) %>% 
  slice('time', 2, drop = FALSE) 

logGPPdays10p5 <- tempaggregate(logGPPdays10p, by_t = by_t_5, 
                                     FUN = maxna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time')) %>% 
  slice('time', 2, drop = FALSE) 

logGPPdays10pCLIM5 <- tempaggregate(logGPPdays10pCLIM, by_t = by_t_5, 
                                         FUN = maxna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time')) %>% 
  slice('time', 2, drop = FALSE) 

logGPPdaysCLIM5 <- tempaggregate(logGPPdaysCLIM, by_t = by_t_5, 
                                      FUN = maxna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time')) %>% 
  slice('time', 2, drop = FALSE) 

logGPPdaysvalleys5 <- tempaggregate(logGPPdaysvalleys, by_t = by_t_5, 
                                         FUN = maxna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time')) %>% 
  slice('time', 2, drop = FALSE) 

logGPPdaysvalleys10p5 <- tempaggregate(logGPPdaysvalleys10p, by_t = by_t_5, 
                                            FUN = maxna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time')) %>% 
  slice('time', 2, drop = FALSE) 

logGPPdaysvalleys10pCLIM5 <- tempaggregate(logGPPdaysvalleys10pCLIM, by_t = by_t_5, 
                                                FUN = maxna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time')) %>% 
  slice('time', 2, drop = FALSE) 

logGPPdaysvalleysCLIM5 <- tempaggregate(logGPPdaysvalleysCLIM, by_t = by_t_5, 
                                             FUN = maxna, na.rm = TRUE) %>%
  aperm(c('Shape', 'time')) %>% 
  slice('time', 2, drop = FALSE) 



# Make 5-yearly dfs for histograms ----------------------------------------
# sfandcatch turns a stars into sf, stacks with a date col, and attaches the catchment name

# inputs
temp5df  <- sfandcatch(temperature5, catches = ltimNoNorth, newname = 'Temp')  %>%
  mutate(center = st_centroid(Shape), latpos = st_coordinates(center)[,2])

clim5df  <- sfandcatch(climate5, catches = ltimNoNorth, newname = 'Temp')  %>% 
  mutate(center = st_centroid(Shape), latpos = st_coordinates(center)[,2])

inun5df  <- sfandcatch(inundation5/1000, catches = ltimNoNorth, newname = 'inundation')  %>%  
  mutate(center = st_centroid(Shape), latpos = st_coordinates(center)[,2],
         logInun = log10(inundation + 1))

inun10p5df  <- sfandcatch(inundation10p5/1000, catches = ltimNoNorth, newname = 'inundation')  %>% 
  mutate(center = st_centroid(Shape), latpos = st_coordinates(center)[,2])

# outputs- don't do the uncertainty here yet.
er5df <- sfandcatch(logERdays5[1,,]/1000, catches = ltimNoNorth, newname = 'ER')  %>% 
  mutate(center = st_centroid(Shape), latpos = st_coordinates(center)[,2],
         logER = log10(ER + 1))
 
er510pdf <- sfandcatch(logERdays10p5[1,,]/1000, catches = ltimNoNorth, newname = 'ER')  %>%  
  mutate(center = st_centroid(Shape), latpos = st_coordinates(center)[,2],          
         logER = log10(ER + 1))
 
er510pCLIMdf <- sfandcatch(logERdays10pCLIM5[1,,]/1000, catches = ltimNoNorth, newname = 'ER')  %>%  
  mutate(center = st_centroid(Shape), latpos = st_coordinates(center)[,2],          
         logER = log10(ER + 1))

er5CLIMdf <- sfandcatch(logERdaysCLIM5[1,,]/1000, catches = ltimNoNorth, newname = 'ER')  %>% 
  mutate(center = st_centroid(Shape), latpos = st_coordinates(center)[,2],         
         logER = log10(ER + 1))

er5_vdf <- sfandcatch(logERdaysvalleys5[1,,]/1000, catches = ltimNoNorth, newname = 'ER')  %>%  
  mutate(center = st_centroid(Shape), latpos = st_coordinates(center)[,2],          
         logER = log10(ER + 1))

er5_v10pdf <- sfandcatch(logERdaysvalleys10p5[1,,]/1000, catches = ltimNoNorth, newname = 'ER')  %>%   
  mutate(center = st_centroid(Shape), latpos = st_coordinates(center)[,2],          
         logER = log10(ER + 1))

er5_v10pCLIMdf <- sfandcatch(logERdaysvalleys10pCLIM5[1,,]/1000, catches = ltimNoNorth, newname = 'ER')  %>%   mutate(center = st_centroid(Shape), latpos = st_coordinates(center)[,2],          logER = log10(ER + 1))

er5_vCLIMdf <- sfandcatch(logERdaysvalleysCLIM5[1,,]/1000, catches = ltimNoNorth, newname = 'ER')  %>%   
  mutate(center = st_centroid(Shape), latpos = st_coordinates(center)[,2],         
         logER = log10(ER + 1))

gpp5df <- sfandcatch(logGPPdays5[1,,]/1000,  catches = ltimNoNorth, newname = 'GPP')  %>%   
  mutate(center = st_centroid(Shape), latpos = st_coordinates(center)[,2],         
         logGPP = log10(GPP + 1))

gpp510pdf <- sfandcatch(logGPPdays10p5[1,,]/1000,  catches = ltimNoNorth, newname = 'GPP')  %>%   
  mutate(center = st_centroid(Shape), latpos = st_coordinates(center)[,2],        
         logGPP = log10(GPP + 1))

gpp510pCLIMdf <- sfandcatch(logGPPdays10pCLIM5[1,,]/1000,  catches = ltimNoNorth, newname = 'GPP')  %>%   
  mutate(center = st_centroid(Shape), latpos = st_coordinates(center)[,2],         
         logGPP = log10(GPP + 1))

gpp5CLIMdf <- sfandcatch(logGPPdaysCLIM5[1,,]/1000,  catches = ltimNoNorth, newname = 'GPP')  %>%   
  mutate(center = st_centroid(Shape), latpos = st_coordinates(center)[,2],         
         logGPP = log10(GPP + 1))

gpp5_vdf <- sfandcatch(logGPPdaysvalleys5[1,,]/1000,  catches = ltimNoNorth, newname = 'GPP')  %>%   
  mutate(center = st_centroid(Shape), latpos = st_coordinates(center)[,2],         
         logGPP = log10(GPP + 1))

gpp5_v10pdf <- sfandcatch(logGPPdaysvalleys10p5[1,,]/1000,  catches = ltimNoNorth, newname = 'GPP')  %>%  
  mutate(center = st_centroid(Shape), latpos = st_coordinates(center)[,2],          
         logGPP = log10(GPP + 1))

gpp5_v10pCLIMdf <- sfandcatch(logGPPdaysvalleys10pCLIM5[1,,]/1000,  catches = ltimNoNorth, newname = 'GPP')  %>% 
  mutate(center = st_centroid(Shape), latpos = st_coordinates(center)[,2],         
         logGPP = log10(GPP + 1)) 

gpp5_vCLIMdf <- sfandcatch(logGPPdaysvalleysCLIM5[1,,]/1000,  catches = ltimNoNorth, newname = 'GPP')  %>%  
  mutate(center = st_centroid(Shape), latpos = st_coordinates(center)[,2],       
         logGPP = log10(GPP + 1))
                                        

# Make bimonhtly timeseries ---------------------------------------------------------


# inputs
tempdf  <- sfandcatch(temperature, catches = ltimNoNorth, newname = 'Temp')  %>%
  mutate(center = st_centroid(Shape), latpos = st_coordinates(center)[,2])

climdf  <- sfandcatch(climate, catches = ltimNoNorth, newname = 'Temp')  %>% 
  mutate(center = st_centroid(Shape), latpos = st_coordinates(center)[,2])

inundf  <- sfandcatch(inundation, catches = ltimNoNorth, newname = 'inundation')  %>%  
  mutate(center = st_centroid(Shape), latpos = st_coordinates(center)[,2],
         logInun = log10(inundation + 1))

inun10pdf  <- sfandcatch(inundation10p, catches = ltimNoNorth, newname = 'inundation')  %>% 
  mutate(center = st_centroid(Shape), latpos = st_coordinates(center)[,2])

# outputs- don't do the uncertainty here yet.
erdf <- sfandcatch(logERdays[1,,]/1000,  catches = ltimNoNorth, newname = 'ER')  %>% 
  mutate(center = st_centroid(Shape), latpos = st_coordinates(center)[,2],
         logER = log10(ER + 1))

er10pdf <- sfandcatch(logERdays10p[1,,]/1000,  catches = ltimNoNorth, newname = 'ER')  %>%  
  mutate(center = st_centroid(Shape), latpos = st_coordinates(center)[,2],          
         logER = log10(ER + 1))

er10pCLIMdf <- sfandcatch(logERdays10pCLIM[1,,]/1000,  catches = ltimNoNorth, newname = 'ER')  %>%  
  mutate(center = st_centroid(Shape), latpos = st_coordinates(center)[,2],          
         logER = log10(ER + 1))

erCLIMdf <- sfandcatch(logERdaysCLIM[1,,]/1000,  catches = ltimNoNorth, newname = 'ER')  %>% 
  mutate(center = st_centroid(Shape), latpos = st_coordinates(center)[,2],         
         logER = log10(ER + 1))

er_vdf <- sfandcatch(logERdaysvalleys[1,,]/1000,  catches = ltimNoNorth, newname = 'ER')  %>%  
  mutate(center = st_centroid(Shape), latpos = st_coordinates(center)[,2],          
         logER = log10(ER + 1))

er_v10pdf <- sfandcatch(logERdaysvalleys10p[1,,]/1000,  catches = ltimNoNorth, newname = 'ER')  %>%   
  mutate(center = st_centroid(Shape), latpos = st_coordinates(center)[,2],          
         logER = log10(ER + 1))

er_v10pCLIMdf <- sfandcatch(logERdaysvalleys10pCLIM[1,,]/1000,  catches = ltimNoNorth, newname = 'ER')  %>%   mutate(center = st_centroid(Shape), latpos = st_coordinates(center)[,2],          logER = log10(ER + 1))

er_vCLIMdf <- sfandcatch(logERdaysvalleysCLIM[1,,]/1000,  catches = ltimNoNorth, newname = 'ER')  %>%   
  mutate(center = st_centroid(Shape), latpos = st_coordinates(center)[,2],         
         logER = log10(ER + 1))

gppdf <- sfandcatch(logGPPdays[1,,]/1000,  catches = ltimNoNorth, newname = 'GPP')  %>%   
  mutate(center = st_centroid(Shape), latpos = st_coordinates(center)[,2],         
         logGPP = log10(GPP + 1))

gpp10pdf <- sfandcatch(logGPPdays10p[1,,]/1000,  catches = ltimNoNorth, newname = 'GPP')  %>%   
  mutate(center = st_centroid(Shape), latpos = st_coordinates(center)[,2],        
         logGPP = log10(GPP + 1))

gpp10pCLIMdf <- sfandcatch(logGPPdays10pCLIM[1,,]/1000,  catches = ltimNoNorth, newname = 'GPP')  %>%   
  mutate(center = st_centroid(Shape), latpos = st_coordinates(center)[,2],         
         logGPP = log10(GPP + 1))

gppCLIMdf <- sfandcatch(logGPPdaysCLIM[1,,]/1000,  catches = ltimNoNorth, newname = 'GPP')  %>%   
  mutate(center = st_centroid(Shape), latpos = st_coordinates(center)[,2],         
         logGPP = log10(GPP + 1))

gpp_vdf <- sfandcatch(logGPPdaysvalleys[1,,]/1000,  catches = ltimNoNorth, newname = 'GPP')  %>%   
  mutate(center = st_centroid(Shape), latpos = st_coordinates(center)[,2],         
         logGPP = log10(GPP + 1))

gpp_v10pdf <- sfandcatch(logGPPdaysvalleys10p[1,,]/1000,  catches = ltimNoNorth, newname = 'GPP')  %>%  
  mutate(center = st_centroid(Shape), latpos = st_coordinates(center)[,2],          
         logGPP = log10(GPP + 1))

gpp_v10pCLIMdf <- sfandcatch(logGPPdaysvalleys10pCLIM[1,,]/1000,  catches = ltimNoNorth, newname = 'GPP')  %>% 
  mutate(center = st_centroid(Shape), latpos = st_coordinates(center)[,2],         
         logGPP = log10(GPP + 1)) 

gpp_vCLIMdf <- sfandcatch(logGPPdaysvalleysCLIM[1,,]/1000,  catches = ltimNoNorth, newname = 'GPP')  %>%  
  mutate(center = st_centroid(Shape), latpos = st_coordinates(center)[,2],       
         logGPP = log10(GPP + 1))


# Make annual timeseries --------------------------------------------------

# The dates are the start of the intervals, and so the wateryear is just the year() value of those dates
# inputs
tempannualdf  <- sfandcatch(temperatureannual, catches = ltimNoNorth, newname = 'Temp')  %>%
  mutate(waterYear = year(date), center = st_centroid(Shape), latpos = st_coordinates(center)[,2])

climannualdf  <- sfandcatch(climateannual, catches = ltimNoNorth, newname = 'Temp')  %>% 
  mutate(waterYear = year(date), center = st_centroid(Shape), latpos = st_coordinates(center)[,2])

inunannualdf  <- sfandcatch(inundationannual/1000, catches = ltimNoNorth, newname = 'inundation')  %>%  
  mutate(waterYear = year(date), center = st_centroid(Shape), latpos = st_coordinates(center)[,2],
         logInun = log10(inundation + 1))

inun10pannualdf  <- sfandcatch(inundation10pannual/1000, catches = ltimNoNorth, newname = 'inundation')  %>% 
  mutate(waterYear = year(date), center = st_centroid(Shape), latpos = st_coordinates(center)[,2])

# outputs- don't do the uncertainty here yet.
erannualdf <- sfandcatch(logERdaysannual[1,,]/1000,  catches = ltimNoNorth, newname = 'ER')  %>% 
  mutate(waterYear = year(date), center = st_centroid(Shape), latpos = st_coordinates(center)[,2],
         logER = log10(ER + 1))

erannual10pdf <- sfandcatch(logERdays10pannual[1,,]/1000,  catches = ltimNoNorth, newname = 'ER')  %>%  
  mutate(waterYear = year(date), center = st_centroid(Shape), latpos = st_coordinates(center)[,2],          
         logER = log10(ER + 1))

erannual10pCLIMdf <- sfandcatch(logERdays10pCLIMannual[1,,]/1000,  catches = ltimNoNorth, newname = 'ER')  %>%  
  mutate(waterYear = year(date), center = st_centroid(Shape), latpos = st_coordinates(center)[,2],          
         logER = log10(ER + 1))

erannualCLIMdf <- sfandcatch(logERdaysCLIMannual[1,,]/1000,  catches = ltimNoNorth, newname = 'ER')  %>% 
  mutate(waterYear = year(date), center = st_centroid(Shape), latpos = st_coordinates(center)[,2],         
         logER = log10(ER + 1))

erannual_vdf <- sfandcatch(logERdaysvalleysannual[1,,]/1000,  catches = ltimNoNorth, newname = 'ER')  %>%  
  mutate(waterYear = year(date), center = st_centroid(Shape), latpos = st_coordinates(center)[,2],          
         logER = log10(ER + 1))

erannual_v10pdf <- sfandcatch(logERdaysvalleys10pannual[1,,]/1000,  catches = ltimNoNorth, newname = 'ER')  %>%   
  mutate(waterYear = year(date), center = st_centroid(Shape), latpos = st_coordinates(center)[,2],          
         logER = log10(ER + 1))

erannual_v10pCLIMdf <- sfandcatch(logERdaysvalleys10pCLIMannual[1,,]/1000,  catches = ltimNoNorth, newname = 'ER')  %>%   mutate(waterYear = year(date), center = st_centroid(Shape), latpos = st_coordinates(center)[,2],          logER = log10(ER + 1))

erannual_vCLIMdf <- sfandcatch(logERdaysvalleysCLIMannual[1,,]/1000,  catches = ltimNoNorth, newname = 'ER')  %>%   
  mutate(waterYear = year(date), center = st_centroid(Shape), latpos = st_coordinates(center)[,2],         
         logER = log10(ER + 1))

gppannualdf <- sfandcatch(logGPPdaysannual[1,,]/1000,  catches = ltimNoNorth, newname = 'GPP')  %>%   
  mutate(waterYear = year(date), center = st_centroid(Shape), latpos = st_coordinates(center)[,2],         
         logGPP = log10(GPP + 1))

gppannual10pdf <- sfandcatch(logGPPdays10pannual[1,,]/1000,  catches = ltimNoNorth, newname = 'GPP')  %>%   
  mutate(waterYear = year(date), center = st_centroid(Shape), latpos = st_coordinates(center)[,2],        
         logGPP = log10(GPP + 1))

gppannual10pCLIMdf <- sfandcatch(logGPPdays10pCLIMannual[1,,]/1000,  catches = ltimNoNorth, newname = 'GPP')  %>%   
  mutate(waterYear = year(date), center = st_centroid(Shape), latpos = st_coordinates(center)[,2],         
         logGPP = log10(GPP + 1))

gppannualCLIMdf <- sfandcatch(logGPPdaysCLIMannual[1,,]/1000,  catches = ltimNoNorth, newname = 'GPP')  %>%   
  mutate(waterYear = year(date), center = st_centroid(Shape), latpos = st_coordinates(center)[,2],         
         logGPP = log10(GPP + 1))

gppannual_vdf <- sfandcatch(logGPPdaysvalleysannual[1,,]/1000,  catches = ltimNoNorth, newname = 'GPP')  %>%   
  mutate(waterYear = year(date), center = st_centroid(Shape), latpos = st_coordinates(center)[,2],         
         logGPP = log10(GPP + 1))

gppannual_v10pdf <- sfandcatch(logGPPdaysvalleys10pannual[1,,]/1000,  catches = ltimNoNorth, newname = 'GPP')  %>%  
  mutate(waterYear = year(date), center = st_centroid(Shape), latpos = st_coordinates(center)[,2],          
         logGPP = log10(GPP + 1))

gppannual_v10pCLIMdf <- sfandcatch(logGPPdaysvalleys10pCLIMannual[1,,]/1000,  catches = ltimNoNorth, newname = 'GPP')  %>% 
  mutate(waterYear = year(date), center = st_centroid(Shape), latpos = st_coordinates(center)[,2],         
         logGPP = log10(GPP + 1)) 

gppannual_vCLIMdf <- sfandcatch(logGPPdaysvalleysCLIMannual[1,,]/1000,  catches = ltimNoNorth, newname = 'GPP')  %>%  
  mutate(waterYear = year(date), center = st_centroid(Shape), latpos = st_coordinates(center)[,2],       
         logGPP = log10(GPP + 1))




# uncertainty dfs ---------------------------------------------------------

# These assume that everything is at the limits at the same time (perfect correlation)
gppdfUPI <- sfandcatch(logGPPdays[2,,]/1000,  catches = ltimNoNorth, newname = 'GPP')  %>%   
  mutate(center = st_centroid(Shape), latpos = st_coordinates(center)[,2],         
         logGPP = log10(GPP + 1)) %>%
  st_drop_geometry() %>% # because going to join
  rename(upperPI = logGPP) %>%
  select(date, ValleyName, upperPI)

gppdfLPI <- sfandcatch(logGPPdays[3,,]/1000,  catches = ltimNoNorth, newname = 'GPP')  %>%   
  mutate(center = st_centroid(Shape), latpos = st_coordinates(center)[,2],         
         logGPP = log10(GPP + 1)) %>%
  st_drop_geometry() %>% # because going to join
  rename(lowerPI = logGPP)  %>%
  select(date, ValleyName, lowerPI)

# TO make this easier to gppplot, combine the datasets

gppUncertaintyLimits <- left_join(gppdf, gppdfUPI) %>% left_join(gppdfLPI)



# These assume that everything is at the limits at the same time (perfect correlation)
erdfUPI <- sfandcatch(logERdays[2,,]/1000,  catches = ltimNoNorth, newname = 'ER')  %>%   
  mutate(center = st_centroid(Shape), latpos = st_coordinates(center)[,2],         
         logER = log10(ER + 1)) %>%
  st_drop_geometry() %>% # because going to join
  rename(upperPI = logER) %>%
  select(date, ValleyName, upperPI)

erdfLPI <- sfandcatch(logERdays[3,,]/1000,  catches = ltimNoNorth, newname = 'ER')  %>%   
  mutate(center = st_centroid(Shape), latpos = st_coordinates(center)[,2],         
         logER = log10(ER + 1)) %>%
  st_drop_geometry() %>% # because going to join
  rename(lowerPI = logER)  %>%
  select(date, ValleyName, lowerPI)

# TO make this easier to erplot, combine the datasets

erUncertaintyLimits <- left_join(erdf, erdfUPI) %>% left_join(erdfLPI)




# Law of large numbers says that the other version (perfectly uncorrelated)
# should match the mean. BUT I guess we should demonstrate that? How big is that
# call going to be? and how do we do it?

# We would need to read in the versions with each ANAE, then loop over each anae
# at its limits with all others at their means and get the catchment mean. and
# then average THOSE for each ANAE

# Do we actually need to do this? we KNOW what it is in the limit, so we can just say that.

# Come back to this- get the fig done first