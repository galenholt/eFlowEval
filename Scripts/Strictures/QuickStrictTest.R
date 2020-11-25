# Quick strict testing

# Script to build on the ANAE without having to reprocess it
library(here)
library(tidyverse)
library(sf)
library(stars)

# Argh. sort all this directory crap out later
# Trying to at least separate scripts and functions, looking towards library
source(here('Functions', 'rastPolyJoin.R'))
source(here('Functions', 'timeRoll.R'))


myhome <- str_remove(path.expand("~"), "/Documents")
datDir <- file.path(myhome, "Deakin University/QAEL - MER/Model/dataBase") # "C:/Users/Galen/Deakin University/QAEL - MER/Model/dataBase"

datOut <- "datOut"


# Read in some test data --------------------------------------------------

load(file.path(datOut, 'lachSoilprocessedAllOut.rdata'))


# A couple logical stricts ------------------------------------------------

# To meet the "dead if not moist > 10% for 6 weeks" condition, we need to check if min(last 6 weeks) is below 10
# To meet the "needs inundation (>80%) for 5 days" condition, we need to check if min(last 5 days) is below 80

moist10 <- soilMoistMin42 > 0.1
moist80 <- soilMoistMin5 > 80

# Are BOTH met? (at same time. Lag will be a bit harder, but likely not too much)
  # But aggregating over time could help
moist10_80 <- moist10 & moist80


# Aggregate to timescale (years?) -----------------------------------------

# How MANY days in each year met each condition?
  # to water year
m10y <- aggregate(moist10, by = "years", FUN = sum, na.rm = TRUE)

# Could easily imagine aggregating something like the 80 condition as a logical
# over the whole year using max, and then the sum of the survival. Or something

# But for now, let's do a yes/no for both and sum to get the yearly number of conditions met
  # Again, there's no true interactions here, but that should be doable by writing something conditional or lagged

m10yL <- aggregate(moist10, by = "years", FUN = max, na.rm = TRUE)
m80yL <- aggregate(moist80, by = "years", FUN = max, na.rm = TRUE)

conditionsmet <- m10yL + m80yL


# Test plot ---------------------------------------------------------------
whichcrs <- 4326
bb <- st_bbox(c(xmin = 144.4, ymin = -33.8, xmax = 144.6, ymax = -33.6), crs = whichcrs)
condsub <- conditionsmet[st_as_sfc(bb)]
plot(condsub)
# Would be good to do a testplot somewhere with actual 1s, but whatever.
