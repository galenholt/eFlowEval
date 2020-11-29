# lippia strictures



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
source(here('Functions', 'rastPolyJoin.R'))
source(here('Functions', 'timeRoll.R'))
source(here('Functions', 'helpers.R'))
source(here('Functions', 'unevenTimeMult.R'))

myhome <- str_remove(path.expand("~"), "/Documents")
datDir <- file.path(myhome, "Deakin University/QAEL - MER/Model/dataBase") # "C:/Users/Galen/Deakin University/QAEL - MER/Model/dataBase"

datOut <- "datOut"


# Read in soil moisture in ANAEs--------------------------------------------------

load(file.path(datOut, 'lachSoilprocessedAllOut.rdata'))

# Read in soil temp in ANAEs --------------------------------------------

load(file.path(datOut, 'lachSoilTempprocessedAllOut.rdata'))
# 
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
# -------------------------------------------------------------------------

# Make stars objects to test against relevant to the strictures -----------
# These are not the stricture TESTS, but processed data to test the strictures against

# -------------------------------------------------------------------------


# These are actually in the data we just read (as of Nov 24 2020), but in
# future should be made in these scripts, so doing it here again

# Soil Temp ---------------------------------------------------------------
#
# "Germination was highest (range 50 to 62 per cent) in temperatures ranging
# from 25 to 35 °C by day, to 10 to 20 °C by night: a minimum day–night
# difference of 5 °C was required to trigger germination"

# We don't have night temp currently, but let's just look for day temp in a band from 25-35
# Do we want to roll this? Maybe not? Maybe a 2-day just to smooth a bit?
# I'm going to do not, just to demo something where we don't need to roll

# So, here, we'll just ask if the daily temp is >= 25 & <= 35
# Don't really even need another object, but I'll make one to de-kelvin (and for consistency)
soilTemp <- dailyPolyTempavg - 273

# Nothing more to do with that until we test the strictures


# Soil moisture -----------------------------------------------------------

# Stricture 2: extended floods (>4 wks) of >30cm depth kill adult plants
# We don't have inundation data yet, so demo-ing this as soil moisture > 80%
# The period of time inundation is required is 4 weeks, then they die

# To meet the "can't have continuous inundation (>80%) for 4 weeks" condition, 
# Passing the stricutre would then be if the min over the last 4 weeks is < 80 (ie die if it never drops below 80)

# Get the min soil moisture of the last 4 weeks as a rolling min for each polygon
soilMoist_Min28 <- dailyPolySMavg # initialize

system.time(soilMoist_Min28[[1]] <- timeRoll(soilMoist_Min28[[1]], 
                                           FUN = RcppRoll::roll_min, 
                                           rolln = 28, 
                                           align = 'right',
                                           na.rm = TRUE))
# This is crude; likely 1 day < 80 isn't sufficient, and we should ask whether
# there are at least x days. But we demonstrate that method with centipeda, and
# this should be inundation as soon as that data is available, so ignore for now


# ANAE type ---------------------------------------------------------------

# same as for centipeda for now. Stolen from Ash's script
lippiaANAE <- c("Pt1.2.1","Pt1.8.1")



# -------------------------------------------------------------------------


# INDEPENDENT STRICTURE TESTS ---------------------------------------------------------


# -------------------------------------------------------------------------

# Check each one separately, first (no interaction)


# Germination requires temp 25-35 -----------------------------------------

# Is the temp in the germ band?
germ2535_Lippia <- (soilTemp >= 25) & (soilTemp <= 35)

# These multi-checks mean it does 3 logical tests. If need be later on, could
# speed up with Rcpp. It's available in dplyr, but doesn't seem to work on soilTemp because it ses a list, not a vector
  # Can get it to work on the [[1]] array, but not sure it's worth it, at least for now
# system.time(germ2535_Lippia[[1]] <- between(soilTemp[[1]], 25, 35))

# https://stackoverflow.com/questions/34519811/what-is-the-fastest-way-to-perform-multiple-logical-comparisons-in-r


# Die if inundated > 4 weeks ----------------------------------------------

# Is the minumum soil moisture over the preceding 4 weeks less than 80%, indicating drying occurred, and so survival?
surv4_Lippia <- soilMoist_Min28 < 0.8


# ANAE classification -----------------------------------------------------

# True/False. Could also be a which() if we want index numbers
isANAE_Lippia <- lachAll$ANAE_CODE %in% lippiaANAE


# -------------------------------------------------------------------------


# DEPENDENT STRICTURE TESTS ---------------------------------------------------------


# -------------------------------------------------------------------------

# Going to build this up step-by-step

# If the species  requires ANAE zones, not clear where to put: could lead with
# it or finish. Or in the middle if they're most relevant to a certain life
# stage
# I think I'll end with it, because it might be interesting to see how many
# OTHER zones look like they should work?


# Germination not dependent on anything here ------------------------------


# adult survival requires germination -------------------------------------

# Unlike centipeda, where we were interested in fruiting, this is just survival,
# and so instead of needing a growing period between germ and this test (i.e.
# option 2 in centip), here we can just ask about germination at any time in the
# past x days (up to a generation). This is more like option1 in centip. No idea growing period, but let's say 2 months (60 days)?

germ_Lippia_Sum60 <- germ2535_Lippia # initialize

system.time(germ_Lippia_Sum60[[1]] <- timeRoll(germ2535_Lippia[[1]], 
                                        FUN = RcppRoll::roll_sum, 
                                        rolln = 60, 
                                        align = 'right',
                                        na.rm = TRUE))
# Then the stricture test is whether there was germ and soil moist
germSurv_Lippia <- (germ_Lippia_Sum60 > 0) & surv4_Lippia



# -------------------------------------------------------------------------


# Restrict all to ANAE ----------------------------------------------------


# -------------------------------------------------------------------------

germANAE_Lippia <- germ2535_Lippia * isANAE_Lippia
survANAE_Lippia <- surv4_Lippia * isANAE_Lippia
fullCycleANAE_Lippia <- germSurv_Lippia * isANAE_Lippia



# ############################ --------------------------------------------

# I think the stuff below here should probably be separated into a plotting
# script, and the stuff above should be saved, to enable
# comparisons/interactions across taxa/themes


# ############################################ ----------------------------





# -------------------------------------------------------------------------


# Summary outputs ---------------------------------------------------------


# -------------------------------------------------------------------------


# Set up time summary breakpoints
# Calendar years, months, other things set with character vectors can facet on
# time itself in the plotting, but giving it dates can't. No idea why
# by_t <- 'years' # calendar years

# let's just pick some cut points manually to see how this works, then maybe
# lubridate to make them programatically (e.g. every april 30 or whatever)
startdate <- min(min(st_get_dimension_values(dailyPolySMavg, which = 'time')),  
                 min(st_get_dimension_values(dailyPolyTempavg, which = 'time')))
enddate <-  max(max(st_get_dimension_values(dailyPolySMavg, which = 'time')),  
                max(st_get_dimension_values(dailyPolyTempavg, which = 'time')))

interDates <- as.POSIXct(c("2014-06-30", "2015-06-30", "2016-06-30", "2017-06-30", "2018-06-30", "2019-06-30"))

# intervals
# this works, but it causes the scale_id error in ggplot (see facet_scale_id_error.R)
# That's fixable by faceting by as.character(time). Do we want to do that? Maybe? Should be OK, I think?
by_t <- c(startdate, interDates,  enddate)

# Independent strictures --------------------------------------------------

# Proportion of days each stricture was met each year
germYr_Lippia <- tempaggregate(germ2535_Lippia, by = by_t, FUN = propor, na.rm = TRUE)

survYr_Lippia <- tempaggregate(surv4_Lippia, by = by_t, FUN = propor, na.rm = TRUE)


# Dependent strictures ----------------------------------------------------

# Just do the full thing, since there aren't really mulitple steps here
fullYr_Lippia <- tempaggregate(fullCycleANAE_Lippia, by = by_t, FUN = propor, na.rm = TRUE)

# Let's spit this out and save it, so centipeda can read JUST this in, and not
# have to source() this whole file

save(fullYr_Lippia, file = file.path('strictOut', 'fullYr_Lippia.rdata'))
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


# Spatial aggregation at the catchment ------------------------------------


# -------------------------------------------------------------------------


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



# -------------------------------------------------------------------------


# Independent ---------------------------------------------------------------


# -------------------------------------------------------------------------


# Germination  ------------------------------------------------------
germCatch_Lippia <- catchAggW(strict = germYr_Lippia, strictWeights = lachArea, FUN = sum, summaryPoly = lachOnly)

germPlot_Lippia <- catchAggPlot(germCatch_Lippia, title = 'Germ Lippia')
germPlot_Lippia


# Survival ----------------------------------------------------------------
survCatch_Lippia <- catchAggW(strict = survYr_Lippia, strictWeights = lachArea, FUN = sum, summaryPoly = lachOnly)

survPlot_Lippia <- catchAggPlot(survCatch_Lippia, title = 'Adult Survival Lippia')
survPlot_Lippia


# -------------------------------------------------------------------------


# Dependent (full cycle) --------------------------------------------------


# -------------------------------------------------------------------------

fullCatch_Lippia <- catchAggW(strict = fullYr_Lippia, strictWeights = lachArea, FUN = sum, summaryPoly = lachOnly)

fullPlot_Lippia <- catchAggPlot(fullCatch_Lippia, title = 'Full Cycle Lippia')
fullPlot_Lippia


