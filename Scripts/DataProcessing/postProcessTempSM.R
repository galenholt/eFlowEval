# script to clean up the demo sm and temp data. Much of this is to make the two datasets match up

# Quick strict development

library(here)
library(tidyverse)
library(sf)
library(stars)

# also loads internal functions
source('directorySet.R')


# Read in soil moisture in ANAEs--------------------------------------------------

load(file.path(datOut, 'ANAEprocessed', 'lachSoilprocessedAllOut.rdata'))

# Read in soil temp in ANAEs --------------------------------------------

load(file.path(datOut, 'lachSoilTempprocessedAllOut.rdata'))
# 

# Temp; cut after pull out of the data processing script 
rm(soilMoistMin5, soilMoistMin42, soilTempMax28)


# # So, soil moist goes to Oct 2020 (currently)
# max(st_get_dimension_values(dailyPolySMavg, which = 'time'))
# # While temp goes to Dec 28 2019 (not sure why I didn't get the last few days of 2019)
# max(st_get_dimension_values(dailyPolyTempavg, which = 'time'))

# So, need to cut moist back to match temp or the matched logicals barfs
# Re-write into self, or end up using a ton of memory
tempmax <- max(st_get_dimension_values(dailyPolyTempavg, which = 'time'))
dailyPolySMavg <- filter(dailyPolySMavg, time <= tempmax) # Check safety, had trouble with filter previously, but this SHOULD be ok since contiguous
# 
# dailyPolySMavg
# max(st_get_dimension_values(dailyPolySMavg, which = 'time'))
# max(st_get_dimension_values(dailyPolyTempavg, which = 'time'))

moistmin <- min(st_get_dimension_values(dailyPolySMavg, which = 'time'))
# And, they're off by 12 hours. argh. for now, just toss noon on dec 31 2013 in the temp too
dailyPolyTempavg <- filter(dailyPolyTempavg, time >= moistmin) # Check safety, had trouble with filter previously, but this SHOULD be ok since contiguous
# dailyPolyTempavg
# testlogic <- dailyPolySMavg < dailyPolyTempavg
# testlogic
# 
# max(st_get_dimension_values(dailyPolySMavg, which = 'time'))
# max(st_get_dimension_values(dailyPolyTempavg, which = 'time'))
# 
# min(st_get_dimension_values(dailyPolySMavg, which = 'time'))
# min(st_get_dimension_values(dailyPolyTempavg, which = 'time'))

save(dailyPolyTempavg,
     file = file.path(datOut, 'lachTempMatched.rdata'))

save(dailyPolySMavg,
     file = file.path(datOut, 'lachSMMatched.rdata'))




