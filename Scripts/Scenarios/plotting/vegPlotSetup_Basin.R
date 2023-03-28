# script for the veg plots at catchment/basin scale
# Libraries and system setup
source('directorySet.R')

library(here)
library(tidyverse)
library(lubridate)
library(sf)
library(stars)

# Set the crs
whichcrs <- 3577

# Setup -------------------------------------------------------------------
# Directory to export TO
scriptOut <- file.path('strictOut', 'vegetation', 'basin')
if (!dir.exists(scriptOut)) {dir.create(scriptOut, recursive = TRUE)}

# Catchments
basinRef <- file.path(datOut, 'ANAEprocessed', 'ltimNoNorth.rdata')

# For the metabolism, I brought in the driver data. I think I'll skip it here,
# at least for now

# directories with strictures This is different than metabolism- now these are
# in lists of stars, and there are two species I want in together
lippiaIn <- file.path(datOut, 'Strictures', 'lippia', 'basinConcat', 'catchmentAggregated.rdata')
centipedaIn <- file.path(datOut, 'Strictures', 'centipeda', 'basinConcat', 'catchmentAggregated.rdata')

# Read in the data from those folders -------------------------------------

# Should make a function

# The catchment boundaries
load(basinRef)
# Need to re-process the anaes
ltimNoNorth <- st_transform(ltimNoNorth, 3577)

# The stricture lists
load(lippiaIn)
load(centipedaIn)


# Time aggregate ----------------------------------------------------------

# The time steps and limits are all over the place, depending on what data is
# available. We have daily for some things and bimonthly for others, but the
# bimonthly goes back much further than the daily

# I could bring everything to bimonthly with the short limits? Or I could just
# go straight to years?

# What metric should I use? As before (metabolism), the bimonth sort of enforces
# a max, since the mean potentailly misses big events, and the sum isn't right.

# I'm just going to do year, but I think doing bimonth would be pretty much the
# same.

# as in metabolism, kill the hours part of the time dim, but do it differnetly
# here since they're in a list
remove_hours <- function(x) {
  x <- st_set_dimensions(x, which = 'time',
                  values = as.Date(st_get_dimension_values(x, which = 'time')))
}

lippia_stricts_catchment <- purrr::map(lippia_stricts_catchment, remove_hours)
centipeda_stricts_catchment <- purrr::map(centipeda_stricts_catchment, remove_hours)

### AGG TO WATER YEAR AS IN METAB ###
# Can I make simple annual reporting?
interDates <- as.POSIXct(c("2014-06-30", "2015-06-30", "2016-06-30", "2017-06-30", "2018-06-30", "2019-06-30", "2020-06-30"))

# when everything being summed is NA, sum(na.rm = TRUE) gives 0, but I need it
# to be NA. So those functions (meanna, sumna, maxna) are defined in helpers.R

# Use max for everything here, since the bimonthly data is defined at max extent

# define fun for purring. It's just a bit too complicated to do anonymously
aggandflip <- function(x) {
  y <- tempaggregate(x, by_t = as.Date(interDates), FUN = maxna, na.rm = TRUE) %>% 
    aperm(c('Shape', 'time'))
}

lippiayr <- lippia_stricts_catchment %>% 
  purrr::map(aggandflip)

centipedayr <- lippia_stricts_catchment %>% 
  purrr::map(aggandflip)

## make dataframes for bar/line etc plots
# kinda weird to pipe in the purrr, but it works.
lippiadf <- lippiayr %>% 
  purrr::map(\(x) sfandcatch(x, catches = ltimNoNorth, newname = names(x)) %>% 
               mutate(waterYear = year(date), 
                      center = st_centroid(Shape), 
                      latpos = st_coordinates(center)[,2]))

centipedadf <- centipedayr %>% 
  purrr::map(\(x) sfandcatch(x, catches = ltimNoNorth, newname = names(x)) %>% 
               mutate(waterYear = year(date), 
                      center = st_centroid(Shape), 
                      latpos = st_coordinates(center)[,2]))
