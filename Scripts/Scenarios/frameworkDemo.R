# Demo strict wrapper

# Abstracting a level, so we can run the actual stricture tests as functions

# NAMING CONVENTIONS TO TRY TO STICK TO -----------------------------------

# Environmental variables should be varName_StatNumDays or similar, because they may be used in different places
# If no stats or rolls, just name: i.e. soilTemp
# If a check and then a roll, append check_statroll, ie. soilTempG60_Max28 for max over 28 days of whether soiltemp is > 60
# STRICTURES (ie the logicals) should be _Taxon
# probably don't really need more than lifestage_taxon, but in case we
# represent the stage differently, will leave the little suffixes for now, I
# guess, i.e. germ2535_Lippia
# sequential, ie have to germ then survive: stage1Stage2Stage3_Taxon, ie germSurv_Lippia
# With/without ANAE limits just stick ANAE on the end
# Summaries: 
# by year(or wateryear, etc): Yr on end
# by area: _Area (ie. _lachlan
# If/when we move to a function, the naming will be a bit less important. But
# then we'd likely only return the final output, so worth thinking about
# whether/how important the intermediate steps are.And if they're needed
# elsewhere, we will still have the issue, ie. if lippia GERM rather than full
# cycle blocked centipeda, would need to get at that earlier one anyway. 
# Suppose the answer might be to return them as lists with a standard set of
# names, so would ask for lippia$germ or lippia$full, etc


# Quick strict development

library(here)
library(tidyverse)
library(sf)
library(stars)
library(cubelyr)
library(viridis)

# Argh. sort all this directory crap out later
# Trying to at least separate scripts and functions, looking towards library

# This is almost EXACTLY a library load at this point. Just need to actually wrap it up and split the git
basicfuns <- list.files(here('Functions'))
basicfuns <- here('Functions', basicfuns)
strictfuns <- list.files(here('Strictures'), pattern = '.R')
strictfuns <- here('Strictures', strictfuns)

# read in those functions
sapply(basicfuns, source)
sapply(strictfuns, source)


myhome <- str_remove(path.expand("~"), "/Documents")
datDir <- file.path(myhome, "Deakin University/QAEL - MER/Model/dataBase") # "C:/Users/Galen/Deakin University/QAEL - MER/Model/dataBase"

datOut <- "datOut"



# Shared data -------------------------------------------------------------

# Assume at least at present that everything will need this
# Read in just the ANAEs
load(file.path(datOut, 'lachAll.rdata'))
lachAll <- st_transform(lachAll, 4326) # WHY ISN"T THIS LIKE THIS ALREADY?

# Get the catchments in (out of place; move)
LTIM_Valleys <- read_sf(dsn = file.path(datDir, 'ANAE/MDB_ANAE.gdb'), layer = 'LTIM_Valleys') %>%
  st_cast("MULTIPOLYGON") # cleans up an issue with multisurfaces

# LTIM areas, useful for plotting
ltimCut <- LTIM_Valleys %>%
  select(ValleyName) # Three different ways to reference, basically

ltimCut <- st_transform(ltimCut, st_crs(lachAll))

# Get just the lachlan for plotting
lachOnly <- filter(ltimCut, ValleyName == "Lachlan")

# Need to get the areas for area-weighting 
lachArea <- st_area(lachAll)


# Date breaks -------------------------------------------------------------

interDates <- as.POSIXct(c("2014-06-30", "2015-06-30", "2016-06-30", "2017-06-30", "2018-06-30", "2019-06-30"))

# could also be 'years', 'months', etc, see the help for some of the sf aggregation functions

# Set up baseline scenario ---------------------------------------------
# Naming them as variables avoids screwing up and having the spp respond to different environments
baseSM <- 'lachSMMatched.rdata'
baseTemp <- 'lachTempMatched.rdata'
baseLip <- 'fullCycle_Lippia_yr_base.rdata' # This is made in lippia_analysis.R, not lippiastricts()
# TODO: make that full year summary less dependent on running a script

# Lippia baseline
lippia_base <- lippiastricts(smFile = baseSM, 
                             tempFile = baseTemp)
# yearly summary
lippia_baseYr <- yearsummary(lippia_base, 
                             whichSave = 'fullCycle', 
                             outsuffix = 'base', 
                             datebreaks = interDates)

# Centipeda baseline
centipeda_base <- centipedastricts(smFile = baseSM, 
                                   tempFile = baseTemp, 
                                   lippiaFile = baseLip)

# yearly summary
centipeda_baseYr <- yearsummary(centipeda_base,
                                datebreaks = interDates)




