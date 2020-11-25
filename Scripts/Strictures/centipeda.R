# Centipeda strictures

# Quick strict testing

# Script to build on the ANAE without having to reprocess it
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


# Soil moisture -----------------------------------------------------------

# Stricture 1: Germination requires inundation
  # We don't have inundation data yet, so demo-ing this as soil moisture > 80%
  # The period of time inundation is required is ??, but let's say 5 days as an example

  # To meet the "needs inundation (>80%) for 5 days" condition, we need to check if min(last 5 days) is below 80
  
# Get the min soil moisture of the last 5 days as a rolling min for each polygon
soilMoistMin5 <- dailyPolySMavg # initialize

system.time(soilMoistMin5[[1]] <- timeRoll(soilMoistMin5[[1]], 
                                           FUN = RcppRoll::roll_min, 
                                           rolln = 5, 
                                           align = 'right',
                                           na.rm = TRUE))

# Stricture 2: Soil moisture needs to be maintained for a certain period of time to enable fruiting and seed-set
  # As above, no numbers for the moisture level or period of time.
  # let's say they die if soil moisture is < 10% in the preceding 6-week growing period

  # To meet the "dead if not moist > 10% for 6 weeks" condition, we need to check if min(last 6 weeks) is below 10
  
  # First, get the min soil moisture over the last 6 weeks as a rolling min
  soilMoistMin42 <- dailyPolySMavg # initialize

  system.time(soilMoistMin42[[1]] <- timeRoll(soilMoistMin42[[1]], 
                                              FUN = RcppRoll::roll_min, 
                                              rolln = 42, 
                                              align = 'right',
                                              na.rm = TRUE))


# Soil Temp ---------------------------------------------------------------

# Loss of seed viability at temps > 60
  # Let's say the 60 degree condition can't have occurred in the last month (28 days)? No idea why I'm choosing that, but just because
  # To meet the "dead if temp > 60 in last month" condition, we need to check if max(last 28 days) is 60
    # An alternative would be to ask whether > x days were above 60 (ie one day
    # might be fine, but a week straight might not be). This would be a two-step process of checking each day > 60, then rolling over THAT for a month with a sum
    # Let's do that, actually, since it's a different way to do it.
  
  # Testing, data explore
  # range(dailyPolyTempavg[[1]], na.rm = T)-273
  # hist(dailyPolyTempavg[[1]])
  # sum((dailyPolyTempavg[[1]]-273) > 60, na.rm = TRUE)
  
  # So, let's say the stricture is < 4 out of last 28 days can have soil temp > 60
  soilTempMax28 <- dailyPolyTempavg # Initialize
  
  # Put in C instead of Kelvin
  soilTempMax28 <- soilTempMax28-273
  
  # Logical; ask if the day is > 60 (no rolling at this point)
  soilTempMax28 <- soilTempMax28 > 60

  # How many of the last 28 days are > 60 (rolling sum of the logicals)
  system.time(soilTempMax28[[1]] <- timeRoll(soilTempMax28[[1]], 
                                             FUN = RcppRoll::roll_sum, 
                                             rolln = 28, 
                                             align = 'right',
                                             na.rm = TRUE))
  # More data explore to make sure the stricture is reasonable
  # max(soilTempMax28[[1]], na.rm = T)
  # sum(soilTempMax28[[1]] > 5, na.rm = T)
  

# ANAE type ---------------------------------------------------------------

  # Stolen from Ash's script
  centipANAE <- c("Pt1.2.1","Pt1.8.1")
  
 #  # Which are those?
 # antest <- lachAll[which(lachAll$ANAE_CODE %in% centipANAE), 'ANAE_DESC']
 # unique(antest$ANAE_DESC)
  # taxa1ind <- which(lachAll$ANAE_CODE == Taxa1ANAE)
 

# -------------------------------------------------------------------------


# INDEPENDENT STRICTURE TESTS ---------------------------------------------------------


# -------------------------------------------------------------------------

 # Check each one separately, first (no interaction)
  # In future, will only do the necessary ones...

# Seed survival requires <60 ----------------------------------------------
  # Say no more than 4 days out of previous month > 60
 seed60 <- soilTempMax28 <= 4

 
# Germination requires inundation -----------------------------------------
 
 # To meet the "needs inundation (>80%) for 5 days" condition, we need to check if min(last 5 days) is below 80
germ80 <- soilMoistMin5 > 0.8
 

# Fruiting requires consistent moisture -----------------------------------

 # To meet the "dead if not moist > 10% for 6 weeks" condition, we need to check if min(last 6 weeks) is below 10
 fruit10 <- soilMoistMin42 > 0.1
 
 

# ANAE classification -----------------------------------------------------

 # True/False. Could also be a which() if we want index numbers
rightANAE <- lachAll$ANAE_CODE %in% centipANAE
 
 
 # -------------------------------------------------------------------------
 
 
 # DEPENDENT STRICTURE TESTS ---------------------------------------------------------
 
 
 # -------------------------------------------------------------------------

 # Going to fully build this up step-by-step
 
 # If the species  requires ANAE zones, not clear where to put: could lead with
 # it or finish. Or in the middle if they're most relevant to a certain life
 # stage
  # I think I'll end with it, because it might be interesting to see how many
  # OTHER zones look like they should work?
 
 # germination requires seed survival
 # relatively simple for now; if seed60 == 1, seeds have survived to that day, and then can do the moist check
  # So this is just a simple &
 germIntSurv <- seed60 & germ80
 
 
  # Fruiting requires germination within the last ?? time period and soil moisture condition
    # there are a few ways to do this; I'll demo them here
    # use germIntSurv to keep going with the lice-cycle logic from the start
 
 # 1. Simplest: germination and soil moisture check over the same time span: ie
 # has it germinated in last 6 weeks, and has soil moist been above 10%?
  # This is crude, since germ could have happened day before, for ex
 
 # To get germ in last 6 weeks, roll whether germination happened in prev. 6 weeks as the sum of days with germ
 germ6weeks <- germIntSurv # initialize
 
 system.time(germ6weeks[[1]] <- timeRoll(germIntSurv[[1]], 
                                             FUN = RcppRoll::roll_sum, 
                                             rolln = 42, 
                                             align = 'right',
                                             na.rm = TRUE))
 # Then the stricture test is whether there was germ and soil moist
 fruitG_10 <- (germ6weeks > 0) & fruit10
 
 # 2. Need some growth period; ie germination needs to have occurred 3 months to
 # 6 weeks ago, then, soil moist needs to have remained above 10% over those 6
 # weeks
  # Prevents germ and immediate fruiting, but kludgy
 
 # likely a way to do this sort of disjoint window in a single function, but for now, do two and subtract
  # The three-month germ roll (90 days) will give the total days in the last 3
  # months with germ, and the 6-week roll will give the number of those in the
  # last 6 weeks. 3 month - 6 week will give the number of germs between 3 month
  # and 6 weeks, and then we can ask if > 0
    # This is roughly the "can't fruit in Feb without germ in Jan" with more
    # flexibility on timing, and some safety to ensure growing period
 
 # Going to just keep filling same object to save space
 germ3mo6 <- germIntSurv # initialize
 
 system.time(germ3mo6[[1]] <- timeRoll(germIntSurv[[1]], 
                                         FUN = RcppRoll::roll_sum, 
                                         rolln = 90, 
                                         align = 'right',
                                         na.rm = TRUE))
 
 # Get the germ days in the interval
 germ3mo6 <- germ3mo6 - germ6weeks
 
 # Then, the stricture test is whether there was germ in the interval, followed by soil moisture
 fruitGint_10 <- (germ3mo6 > 0) & fruit10
 
 # 3. The best way to do this is to ask if there has been sufficient soil
 # moisture since the last germination event that was long enough ago for the
 # plant to grow (i.e. again, we don't want germ that happened 2 days ago)
  # We want to look back a minimum growing period, then find the most recent
  # germ, then ask if soil moist has been > 10 since then
 
 # This will take a slightly different approach I think, but should be doable.
 # Suppose the question is whether to do it now or produce outputs first?
  # It will take some thinking, because now each location and time will have a
  # different lookback for soil moist. So will need to a) find that lookback, b)
  # do the soil moist check. Could get expensive if there's not something written already
    # I think since the above is already a bit further than we'd planned, I'll
    # move on to summarizing and plotting etc, and come back to this
 # The #2 method is also pretty good, in that it IS reasonable to set a start
 # window on the germ; it may not be relevant to have a 2-year growing period,
 # for example. The only catch is that if germ was three months ago, then at 2
 # months it dried out, but the subsequent 6 weeks were wet, we'd miss that
 
 # But maybe we could do a which() on germ3mo6 to get when germ happened in the
 # window, and if we did max(which()), it'd give us the last germ in the window.
 # Then we'd just need to do the min check on the window. This is likely all
 # more expensive than the time roll, but maybe not too bad? Will just have to try
  # Might be able to build it on cumsum: https://stackoverflow.com/questions/54570330/counting-number-of-rows-since-last-observation-that-fulfills-condition
  # And maybe cummin, starting over at germ date. That SHOULD be pretty fast, I think.
 


# And, restrict to the required ANAE --------------------------------------

 # Need germ in the interval, and fruit, and ANAE
 fullCycleANAE <- fruitGint_10 & rightANAE
 

# -------------------------------------------------------------------------


# Summary outputs ---------------------------------------------------------


# -------------------------------------------------------------------------

# Processing can occur at small time steps where possible, but presentation will
# necessarily need to be summarized over both time and space
 # There are a few different ways to do that, so I'll demo a few
 
# Some things I'll need
 # Proportion function
 propor <- function(x, na.rm = FALSE) {
   sum(x, na.rm = na.rm)/length(x)
 }
 
 # Water year breakpoints
  # TBD for now
 # See `by` in ?aggregate.stars
 # Left-closed, right open time intervals
 # or cut.Posix
 
 # Calendar years can facet on time itslef in the plotting. No idea why
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
 seedYr <- aggregate(seed60, by = by_t, FUN = propor, na.rm = TRUE)
 # test the prop makes sense (did my fun work?)
  # Yes
 # seedYrS <- aggregate(seed60, by = by_t, FUN = sum, na.rm = TRUE)
 
 germYr <- aggregate(germ80, by = by_t, FUN = propor, na.rm = TRUE)
 
 fruitYr <- aggregate(fruit10, by = by_t, FUN = propor, na.rm = TRUE)

 # Those are all just for those strictures (not cut by ANAE)
 # If we want to ONLY look within the right ANAE zones,
 seedANAEYr <- seedYr*rightANAE
 germANAEYr <- germYr*rightANAE
 fruitANAEYr <- fruitYr*rightANAE
  
 # 
 

# Dependent strictures ----------------------------------------------------

# So, if we go dependent, is there much reason to do subsets of it? ie, got from survival to germ, but not fruiting? Or just do it for the final step?
 # Suppose there might be a point to the intermediates. Make them, I guess
 # but not going to go with/without ANAE, or too many options
 
 # survival already above as independent
 survGermYrANAE <- aggregate(germIntSurv, by = by_t, FUN = propor, na.rm = TRUE)*rightANAE
 
 fullCycleYr <- aggregate(fullCycleANAE, by = by_t, FUN = propor, na.rm = TRUE)
 
 # how to present any of this? Could make a map, but lotsa white space. Still, might be fine, depending on the goals

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

 # Need to get the areas for area-weighting 
 lachArea <- st_area(lachAll)
 
 
 # Summing/averaging proportions is kind of weird. So maybe as a first pass, go with the areal sum of success (ie area-days of passing)
  # Easy to do this differently later if we want
 
 # Work our way up again
 
 # All the t(t(array)) stuff is hacky and should be able to be cleaned up wiht a bit more time with the help files

# -------------------------------------------------------------------------


# Independent strictures --------------------------------------------------


# -------------------------------------------------------------------------

 
 

# Seed bank survival ------------------------------------------------------
 seedArea <- seedANAEYr
 # get area-proportion of success (same as area-days, but divided by days in year)
 seedArea[[1]] <- t(t(seedArea[[1]])*lachArea)
 
 # Now aggregate over space
 seedCatch <- aggregate(seedArea, by = filter(ltimCut, ValleyName == "Lachlan"), FUN = sum, na.rm = TRUE)
 
 # Plot. Color ramp traffic light, because we can
 ggplot() +
   geom_stars(data = seedCatch) +
   coord_sf() +
   facet_wrap(~as.character(time)) +
   theme_void()  +
   scale_fill_gradient(low = 'firebrick', high = 'forestgreen' )
 

# Germination -------------------------------------------------------------

 germArea <- germANAEYr
 # get area-proportion of success (same as area-days, but divided by days in year)
 germArea[[1]] <- t(t(germArea[[1]])*lachArea)
 
 # Now aggregate over space
 germCatch <- aggregate(germArea, by = filter(ltimCut, ValleyName == "Lachlan"), FUN = sum, na.rm = TRUE)
 
 # Plot. Color ramp traffic light, because we can
 ggplot() +
   geom_stars(data = germCatch) +
   coord_sf() +
   facet_wrap(~as.character(time)) +
   theme_void()  +
   scale_fill_gradient(low = 'firebrick', high = 'forestgreen' ) 
 
 

# Fruiting ----------------------------------------------------------------

 fruitArea <- fruitANAEYr
 # get area-proportion of success (same as area-days, but divided by days in year)
 fruitArea[[1]] <- t(t(fruitArea[[1]])*lachArea)
 
 # Now aggregate over space
 fruitCatch <- aggregate(fruitArea, by = filter(ltimCut, ValleyName == "Lachlan"), FUN = sum, na.rm = TRUE)
 
 # Plot. Color ramp traffic light, because we can
 ggplot() +
   geom_stars(data = fruitCatch) +
   coord_sf() +
   facet_wrap(~as.character(time)) +
   theme_void()  +
   scale_fill_gradient(low = 'firebrick', high = 'forestgreen' ) 
 
 

# -------------------------------------------------------------------------


# Dependent ---------------------------------------------------------------


# -------------------------------------------------------------------------


# survival and germination ------------------------------------------------

 
 survGermArea <- survGermYrANAE
 # get area-proportion of success (same as area-days, but divided by days in year)
 survGermArea[[1]] <- t(t(survGermArea[[1]])*lachArea)
 
 # Now aggregate over space
 survGermCatch <- aggregate(survGermArea, by = filter(ltimCut, ValleyName == "Lachlan"), FUN = sum, na.rm = TRUE)
 
 # Plot. Color ramp traffic light, because we can
 ggplot() +
   geom_stars(data = survGermCatch) +
   coord_sf() +
   facet_wrap(~as.character(time)) +
   theme_void()  +
   scale_fill_gradient(low = 'firebrick', high = 'forestgreen' ) 
 

# Full cycle --------------------------------------------------------------

 fullCycleArea <- fullCycleYr
 # get area-proportion of success (same as area-days, but divided by days in year)
 fullCycleArea[[1]] <- t(t(fullCycleArea[[1]])*lachArea)
 
 # Now aggregate over space
 fullCycleCatch <- aggregate(fullCycleArea, by = filter(ltimCut, ValleyName == "Lachlan"), FUN = sum, na.rm = TRUE)
 
 # Plot. Color ramp traffic light, because we can
 ggplot() +
   geom_stars(data = fullCycleCatch) +
   coord_sf() +
   facet_wrap(~as.character(time)) +
   theme_void()  +
   scale_fill_gradient(low = 'firebrick', high = 'forestgreen' )  
 