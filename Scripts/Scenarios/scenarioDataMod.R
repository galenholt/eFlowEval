# script to modify the underlying data to reflect an intervention

# I THINK THE MAIN WAY TO DO SCENARIO COMPARISONS WILL BE TO ACTUALLY PROVIDE
# MODIFIED DATA, *NOT* DO IT PROGRAMATICALLY
  # That said, it is likely that these will modify a very small subset of the
  # data, and that modification may be done programatically elsewhere



# Basic outline of scenario structures ------------------------------------

# HERE
  # Read in daily data
  # Modify to reflect watering
  # Save a new set of dailies

# THEN
  # Run the strictures scripts twice: once for new, once for old
    # so, will need to make that programmatic
  # Comparison analysis


# Read in base data ------------------------------------------------------

# Quick strict development

library(here)
library(tidyverse)
library(sf)
library(stars)
# library(cubelyr)
# library(viridis)

# # Argh. sort all this directory crap out later
# # Trying to at least separate scripts and functions, looking towards library
# source(here('Functions', 'rastPolyJoin.R'))
# source(here('Functions', 'timeRoll.R'))
# source(here('Functions', 'helpers.R'))
# source(here('Functions', 'unevenTimeMult.R'))


myhome <- str_remove(path.expand("~"), "/Documents")
datDir <- file.path(myhome, "Deakin University/QAEL - MER/Model/dataBase") # "C:/Users/Galen/Deakin University/QAEL - MER/Model/dataBase"

datOut <- "datOut"

# Read in just the ANAEs
load(file.path(datOut, 'lachAll.rdata'))
lachAll <- st_transform(lachAll, 4326) # WHY ISN"T THIS LIKE THIS ALREADY?

# Read in soil moisture in ANAEs--------------------------------------------------

load(file.path(datOut, 'lachSMMatched.rdata'))

# Read in soil temp in ANAEs --------------------------------------------

load(file.path(datOut, 'lachTempMatched.rdata'))


# Now, modify the data to simulate some watering- totally made up ---------

  # Potential to get super fancy here, but really, this should be coming from
  # hydrology or specific water managers, etc. The point is that we CAN take
  # different data, not that we are experts at making those scenarios. Could
  # encompass a million different parameters (topping up, preventing drying,
  # location, etc), and that's not the point here: we just need a mock-up of the
  # outcome

# I think what I'll try to do is increase inundation more or less uniformly.
# This is super-simple and unrealistic, but easy to do.
  # Will leave soil temps alone; they are less likely to be altered by
  # management. though asking about climate, maybe?

# Let's say that if the soil moisture is > 50%, it gets topped-up to 95
  # sort of roughly has the effect of adding rainfall and keeping inundation topped up
# And, let's have a temp + 2 version, as a VERY crude sim of climate change

### Threshold the soil moisture
# # Some checks
# 
# mean(dailyPolySMavg[[1]])
# max(dailyPolySMavg[[1]])
# min(dailyPolySMavg[[1]])
# sum(dailyPolySMavg[[1]] > 0.8)
# sum(dailyPolySMavg[[1]] > 0.5)

# Make the changes
# Just indexing and changing is fastest
# can overwrite here, but careful later

# And, I think for safety, probably want to set this up with a different name.
# But using the same name DOES make the code more modular
dailyPolySMavg[[1]][which(dailyPolySMavg[[1]] > 0.5)] <- 0.95


### Increase temp 2 degrees
dailyPolyTempavg <- dailyPolyTempavg + 2

save(dailyPolyTempavg,
     file = file.path(datOut, 'lachTempModified.rdata'))

save(dailyPolySMavg,
     file = file.path(datOut, 'lachSMModified.rdata'))

# Benchmarking other methods of making the changes ------------------------
# 
# dpsmTop <- dailyPolySMavg
# dpsmTop[[1]][which(dpsmTop[[1]] > 0.5)] <- 0.95

# sum(dailyPolySMavg[[1]] > 0.8)
# sum(dpsmTop$sm_pct > 0.8)
# sum(dpsmTop$sm_pct > 0.5 & dpsmTop$sm_pct < 0.9)
# 
# # Or, alternatively this might be simpler?
# topup <- function(x) {
#   ifelse(x > 0.5, 0.95, x)
# }
# 
# # dpsmTop2 <- topup(dailyPolySMavg)
# 
# # Can't do it directly, needs need another indexing step to get at the numbers, or use dplyr
# 
# # What about dplyr?
# dpsmTop3 <- dailyPolySMavg %>% mutate(topupsm = topup(sm_pct))
# 
# sum(dailyPolySMavg[[1]] > 0.8)
# sum(dpsmTop3$sm_pct > 0.8)
# sum(dpsmTop3$topupsm > 0.8)
# sum(dpsmTop3$topupsm > 0.5 & dpsmTop3$topupsm < 0.9)
# 
# # Should just do that in one go, overwriting sm_pct
# dpsmTop2 <- dailyPolySMavg %>% mutate(sm_pct = topup(sm_pct))
# 
# sum(dailyPolySMavg[[1]] > 0.8)
# sum(dpsmTop2$sm_pct > 0.8)
# sum(dpsmTop2$sm_pct > 0.5 & dpsmTop2$sm_pct < 0.9)
# 
# # benchmark the two methods
# 
# thbench <- microbenchmark('index' = {dpsmTop <- dailyPolySMavg
#                dpsmTop[[1]][which(dpsmTop[[1]] > 0.5)] <- 0.95},
#                'dply' = {dpsmTop <- dailyPolySMavg %>% mutate(sm_pct = topup(sm_pct))},
#                times = 20L)
# thbench
# autoplot(thbench)
# 
# # So, use the index method: commented out everything else