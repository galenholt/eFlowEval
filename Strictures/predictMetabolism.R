# Script to use regressions to predict metabolism

# Header from the temperature file to retain all the directories,  --------
source('directorySet.R')

# Let's get libraries here, then sort out git then sort out making this a
# library so we don't have to deal with all the library crap
library(here)
library(tidyverse)
library(sf)
library(stars)


# TODO
# For each temp on each day, get the predicted GPP and ER
# For both the catchment regression and non-catchment regression
# That'll generate ANOTHER dataset- export it
# and make an animation
# animate the temps while I'm at it
# THEN, re-import and aggregate to the bimonthly sequence
# How? I do it somewhere else but can't remember where. I think in one of the demos I break things down to water year
# Then, read in the volume
# Multiply the predicted GPP by volume
# Aggregate up to catchments, make a cool animation
# Oof. That's a lot for one day.


# Read in data ------------------------------------------------------------

## TEMPS
# Need to wrap this over catchments
tempsIn <- file.path(datOut, 'Tempprocessed', 'weightedMean')
catchNames <- list.files(file.path(tempsIn, 'chunked')) # Get the catchment names from the folders
catchFiles <- list.files(tempsIn, pattern = '.rdata')
# And test with and without a catchment in the set of catchments from metabolism
thisCatch <- 'Murrumbidgee' # For testing- needs to be grabeed from catchNames in a loop
load(file = file.path(tempsIn, paste0(thisCatch, '_weightedMean.rdata')))

## Regressions
regIn <- file.path(datOut, 'TempAndProduction', 'tempMetabolismRegressions.rdata')
load(regIn)


# NOW, sort out how to predict(). Or do it directly -----------------------
# First, grab a subset of the bidgee data so I can work with it
subCatch <- Murrumbidgee_weightedMean %>% slice('geometry', 1:10) %>% slice('time', 500:515)
subCatch
subIndex <- Murrumbidgee_weightedMean_index[1:10, ] # This is just the geometries, so match above

names(subCatch)

# I need to get the variables here to match the variables in the model
  # That's going to be a pain for things that operate on the time dimension. But
  # doable. Might have to switch to SF? Hope not
# what do I need?
summary(gppDaysValleys)
# tempC, daysAwayFromWaterYear, ValleyName, wateryear

# First, can I turn the kelvins into tempC?
subCatch[[1]] <- as.numeric(subCatch[[1]])-272.15
names(subCatch) <- 'tempC'

# I think *probably* the way to do the variables that depend on time is to get them into the attributes:
# From ?predict.stars: separate predictors in object need to be separate
# attributes in object; in case they are e.g. in a band dimension, use
# 'split(object)'

# So, what does split give us?
subSplit <- split(subCatch, f = 'time')
# Why is this failing?
subCatch %>% split('time')
st_as_stars(subCatch) # seemed to be the error, but works
subSplit <- split(st_as_stars(subCatch), 'time') # forcing the st_as_stars in the call doesn't help

# Unclear what the issue is, and I could make a reproducible example, but it'd
# take a while. Try that later. For now, let's do the sf thing. Which sucks. Is
# it possible that there's a better solution?
subDF <- as.data.frame(subCatch)
subDF
str(subDF)
# yeah, that'll do. probably better, actually. If I can turn it back into a star. But should be able to.

# Now, I need to transform the times as in the regression script
   # which I think means turning the water year and days from start of water year into functions in helpers
subDF <- subDF %>% 
  mutate(daysAwayFromWaterYear = daysfromWY(time), 
         bimonthFactor = as.factor(getBimonth(time)), # Get both time variables, I guess
         ValleyName = thisCatch,
         wateryear = getWaterYear(time)) # For prediction outside the initial range, this will need to be done differently, I think. I had a way to choose a median value of this somehow in the coexistence work

 #NEGATIVES ARE FINE. THIS IS LOG(GPP)
# predict(gppDaysValleys, newdata = subDF)

# Does it make sense to add ALL the predictions? Rather than carry multiple
# objects, carry one with multiple attributes? Sure would be slick if that works
  # Though, I think I'll skip bimonth here- that makes more sense if I average to the bimonth first

subDF <- subDF %>% 
  modelr::add_predictions(gppDaysValleys, var = 'logGPPdaysvalleys') %>% 
  modelr::add_predictions(gppDays, var = 'logGPPdays') %>%
  modelr::add_predictions(erDaysValleys, var = 'logERdaysvalleys') %>% 
  modelr::add_predictions(erDays, var = 'logERdays')
  
subDF

plot(subDF[,'logGPPdays'])

# uh oh. that's not spatial
crudesf <- st_as_sf(subDF) # I think this just assumes it's WGS84. would need to reset the crs
plot(crudesf[,'logGPPdays'])
# It's super fast though. and should be fine. OTOH, if we can KEEP it an sf all the way through...
# Try to mirror the above, with an sf

subSF <- st_as_sf(subCatch, long = TRUE) # THe long argument keeps times
subSF
str(subSF)

subSF <- subSF %>% 
  mutate(daysAwayFromWaterYear = daysfromWY(time), 
         bimonthFactor = as.factor(getBimonth(time)), # Get both time variables, I guess
         ValleyName = thisCatch,
         wateryear = getWaterYear(time))

subSF <- subSF %>% 
  modelr::add_predictions(gppDaysValleys, var = 'logGPPdaysvalleys') %>% 
  modelr::add_predictions(gppDays, var = 'logGPPdays') %>%
  modelr::add_predictions(erDaysValleys, var = 'logERdaysvalleys') %>% 
  modelr::add_predictions(erDays, var = 'logERdays')

subSF
plot(subSF[,'logGPPdays'])
# YEAH, DO THAT

### A check for predicting out of scope: 
  # Water year is a random factor, and is ALSO the thing that will be out of scope. 
  # ?predict.merMod suggests that newdata needs COLUMNS for the variables,
  # butwill predict on the mean if they're outside scope

subSFnewyear <- st_as_sf(subCatch, long = TRUE) # THe long argument keeps times
subSFnewyear
str(subSFnewyear)

subSFnewyear <- subSFnewyear %>% 
  mutate(daysAwayFromWaterYear = daysfromWY(time), 
         bimonthFactor = as.factor(getBimonth(time)), # Get both time variables, I guess
         ValleyName = thisCatch,
         wateryear = 2100)

subSFnewyear <- subSFnewyear %>% 
  modelr::add_predictions(gppDaysValleys, var = 'logGPPdaysvalleys', type = 'merMod', allow.new.levels = TRUE) # %>% 
  # modelr::add_predictions(gppDays, var = 'logGPPdays', allow.new.levels = TRUE) %>%
  # modelr::add_predictions(erDaysValleys, var = 'logERdaysvalleys', allow.new.levels = TRUE) %>% 
  # modelr::add_predictions(erDays, var = 'logERdays', allow.new.levels = TRUE)
# argh. does it really break add_predictions? There's no way to pass arguments???
# The raw predict() function works fine.
predict(gppDaysValleys, newdata = subSFnewyear, allow.new.levels = TRUE)

# add_predictions is pretty slick, but seems to also not be very full-featured.
# can I write my own?
add_preds <- function(newdata, mod, predname = NULL) {
  if (is.null(predname)) {
    predname <- deparse(substitute(mod))
  }
  preds <- predict(mod, newdata = newdata, allow.new.levels = TRUE)
  predf <- bind_cols(newdata, tempname = preds)
  names(predf)[which(names(predf) == 'tempname')] <- predname # avoiding rlang to sort out the name programatically
  return(predf)
}
# that won't work in a mutate call. I could probably write it in purrr, but honestly probably easier to bind_cols and call it a day

subSFnewyear2 <- subSFnewyear %>% add_preds(mod = gppDaysValleys, predname = 'logGPPdaysvalleys')
subSFnewyear2

# Ok, let's roll that whole thing out
subSFnewyear <- subSFnewyear %>% 
  add_preds(gppDaysValleys, predname = 'logGPPdaysvalleys')  %>% 
  add_preds(gppDays, predname = 'logGPPdays') %>%
  add_preds(erDaysValleys, predname = 'logERdaysvalleys') %>%
  add_preds(erDays, predname = 'logERdays')

subSFnewyear
plot(subSFnewyear[,'logGPPdays'])
plot(subSFnewyear[,'logGPPdaysvalleys'])
plot(subSFnewyear[,'logERdays'])
plot(subSFnewyear[,'logERdaysvalleys'])

# TODO: Sort out turning back into stars, since that's what everything expects
  # and, a quick check, is WAY cheaper for RAM
object.size(subCatch)
object.size(st_as_sf(subCatch, long = TRUE)) # same call as above to make subSF

# Looks like st_as_stars, possibly followed by merge is the go, but will need to be careful
# First, select down to the things we actually want
subSFnewyear <- subSFnewyear %>%
  select(time, starts_with('log')) # Don't save the other stuff; we have a temp stars already, and the rest was just for prediction
subSFnewyear
st_as_stars(subSFnewyear) # only geometry as a dimension
merge(st_as_stars(subSFnewyear)) # All attributes as an attribute dimension
  # This would be OK, except that time just ends up jumbled in with everything else
?st_as_stars
# st_as_stars(subSFnewyear, dimensions = st_dimensions(subCatch)) # nope
?merge.stars
merge(st_as_stars(subSFnewyear), y = 'time') 

# not seeing anything obvious. Does this work for a single attribute?
subSFnewyear %>% select(time, logGPPdays) %>%
  st_as_stars() %>% merge()

# Ah. This is failing here where it worked in rastPolyJoin because there the times were columns?
  # THERE, there are ONLY time columns, with the values in them being the
  # attribute values. And then I just overwrite the time dimension

# what about st_redimension
redim <- subSFnewyear %>% select(time, logGPPdays) %>%
  st_as_stars() %>%
  # merge() %>%
  st_redimension(new_dims = st_dimensions(subCatch))
redim
# that *seems* like it worked. But is it RIGHT? The times are confusing me a bit- did it put the data where it needs to be?
redim[[1]] # Huh. the times are a vector
redim[[2]] # but the values are a matrix with 10 cols (geometry) and 15 rows(times)
# that redim SHOUlD match 
subSFnewyear %>% as.data.frame() %>% 
  select(time, geometry, logGPPdays) %>% 
  pivot_wider(id_cols = c('time', 'geometry'), values_from = logGPPdays, names_from = time)

# SEEMS TO HAVE WORKED. bUILD FROM THERE/
# Does THIS work for all variables?
redimall <- subSFnewyear %>% # select(time, logGPPdays) %>%
  st_as_stars() %>%
  # merge() %>%
  st_redimension(new_dims = st_dimensions(subCatch))
redimall
redimall[[1]]
redimall[[2]]
redimall[[3]]
redimall[[4]]
redimall[[5]]
# Cool. Works. need to remove the time attribute though
redimall
redimall[[1]] <- NULL
redimall

plot(redimall['logGPPdaysvalleys'])
plot(redimall['logGPPdays'])
plot(redimall['logERdaysvalleys'])

# Let's clean that up start to finish
# Then roll out to whole area. might need to chunk up? Hope not, but maybe.
# First check will be whether I can st_as_sf the whole catchment

# Worth thinking about how to throw NA for valleys that shouldn't be included in the valleys model
  # would do it automatically for predict(), except I want to predict for new years


# Clean up for subCatch ---------------------------------------------------
# add_predictions is pretty slick, but seems to also not be very full-featured.
# can I write my own?
add_preds <- function(newdata, mod, predname = NULL) {
  if (is.null(predname)) {
    predname <- deparse(substitute(mod))
  }
  preds <- predict(mod, newdata = newdata, allow.new.levels = TRUE)
  predf <- bind_cols(newdata, tempname = preds)
  names(predf)[which(names(predf) == 'tempname')] <- predname # avoiding rlang to sort out the name programatically
  return(predf)
}
# 

subCatch <- Murrumbidgee_weightedMean %>% slice('geometry', 1:10) %>% slice('time', 500:515)
# Turn the kelvins into tempC?
subCatch[[1]] <- as.numeric(subCatch[[1]])-272.15
names(subCatch) <- 'tempC'

# Make into an sf dataframe so we can do the predictions
subSFnewyear <- st_as_sf(subCatch, long = TRUE) # THe long argument keeps times

# create the columns we need
subSFnewyear <- subSFnewyear %>% 
  mutate(daysAwayFromWaterYear = daysfromWY(time), 
         bimonthFactor = as.factor(getBimonth(time)), # Get both time variables, I guess
         ValleyName = thisCatch,
         wateryear = 2100)

# predict new values
subSFnewyear <- subSFnewyear %>% 
  add_preds(gppDaysValleys, predname = 'logGPPdaysvalleys')  %>% 
  add_preds(gppDays, predname = 'logGPPdays') %>%
  add_preds(erDaysValleys, predname = 'logERdaysvalleys') %>%
  add_preds(erDays, predname = 'logERdays')

# throw out all the extra variables, leaving just the predictions
subSFnewyear <- subSFnewyear %>%
  select(time, starts_with('log'))

# Turn into stars
  # This version for all model predictions. might as well (unless the resulting object is too big)
starpredictions <- subSFnewyear %>% # select(time, logGPPdays) %>%
  st_as_stars() %>%
  # st_redimension is more flexible than merge.stars()
  st_redimension(new_dims = st_dimensions(subCatch))
starpredictions[[1]] <- NULL


# Test a different catchment without the right valley ---------------------

# # add_predictions is pretty slick, but seems to also not be very full-featured.
# # can I write my own?
# add_preds <- function(newdata, mod, predname = NULL) {
#   if (is.null(predname)) {
#     predname <- deparse(substitute(mod))
#   }
#   
#   if (!checklevels(newdata, mod)) {
#     predf <- mutate(newdata, tempname = NA)
#   } else {
#     preds <- predict(mod, newdata = newdata, allow.new.levels = TRUE)
#     predf <- bind_cols(newdata, tempname = preds)
#   }
#   
#   names(predf)[which(names(predf) == 'tempname')] <- predname # avoiding rlang to sort out the name programatically
#   return(predf)
# }


subCatch <- Murrumbidgee_weightedMean %>% slice('geometry', 1:10) %>% slice('time', 500:515)
# Turn the kelvins into tempC?
subCatch[[1]] <- as.numeric(subCatch[[1]])-272.15
names(subCatch) <- 'tempC'

# Make into an sf dataframe so we can do the predictions
subSFnewyearvalley <- st_as_sf(subCatch, long = TRUE) # THe long argument keeps times

# create the columns we need
subSFnewyearvalley <- subSFnewyearvalley %>% 
  mutate(daysAwayFromWaterYear = daysfromWY(time), 
         bimonthFactor = as.factor(getBimonth(time)), # Get both time variables, I guess
         ValleyName = 'FAKECATCH',
         wateryear = 2100)



# I need to sort out how to identify fixed effect factors that are new
# # Get the factor levels
# mf <- model.frame(gppBimonthValleys)
# 
# mf <- model.frame(gppDays)
# faclevs <- mf %>% select_if(is.factor) %>%
#   map(unique)
# # and the ones that are fixed factors (not random effects)
# fixedvars <- attributes(attributes(mf)$terms)$varnames.fixed
# # throw out any potential random effects
# faclevs[!(names(faclevs) %in% fixedvars)] <- NULL
# # get the levels available in the data
# datalevs <- subSFnewyearvalley %>% st_drop_geometry() %>% select(any_of(names(faclevs))) %>% map(unique)
# # function to check if they are in
# checkin <- function(x,y) {
#   tf <- (x %in% y)
# }
# # use map-reduce to ask if all levels are available for all factors.
# facsinmodel <- map2(datalevs, faclevs, checkin) %>% reduce(all)

# # Turn that into a checker function
# checklevels <- function(newdata, mod) {
#   # Get the factor levels
#   faclevs <- model.frame(mod) %>% select_if(is.factor) %>% 
#     map(unique)
#   # and the ones that are fixed factors (not random effects)
#   fixedvars <- attributes(attributes(model.frame(mod))$terms)$varnames.fixed
#   # throw out any potential random effects
#   faclevs[!(names(faclevs) %in% fixedvars)] <- NULL
#   
#   # get the levels available in the data
#   datalevs <- newdata %>% 
#     st_drop_geometry() %>% # only relevant if sf
#     select(any_of(names(faclevs))) %>% 
#     map(unique)
#   
#   # function to check if they are in
#   checkin <- function(x,y) {
#     tf <- (x %in% y)
#   }
#   
#   # use map-reduce to ask if all levels are available for all factors across
#   # what might be an uneven list.
#   facsinmodel <- map2(datalevs, faclevs, checkin) %>% reduce(all)
#   return(facsinmodel)
# }
checklevels(subSFnewyearvalley, gppBimonthValleys)
checklevels(subSFnewyearvalley, gppBimonth)
checklevels(subSFnewyearvalley, gppDays)



# predict new values
subSFnewyearvalley <- subSFnewyearvalley %>% 
  add_preds(gppDaysValleys, predname = 'logGPPdaysvalleys')  %>% 
  add_preds(gppDays, predname = 'logGPPdays') %>%
  add_preds(erDaysValleys, predname = 'logERdaysvalleys') %>%
  add_preds(erDays, predname = 'logERdays')

# throw out all the extra variables, leaving just the predictions
subSFnewyearvalley <- subSFnewyearvalley %>%
  select(time, starts_with('log'))
subSFnewyearvalley
# Turn into stars
# This version for all model predictions. might as well (unless the resulting object is too big)
starpredictions <- subSFnewyearvalley %>% # select(time, logGPPdays) %>%
  st_as_stars() %>%
  # st_redimension is more flexible than merge.stars()
  st_redimension(new_dims = st_dimensions(subCatch))
starpredictions[[1]] <- NULL
starpredictions



# TODO: -------------------------------------------------------------------


# Expand to the whole basin- can I do this in one go? or is it too --------

# Delete everything above and re-read in so start with clean slate
rm(list = ls())
# Header from the temperature file to retain all the directories,  --------
source('directorySet.R')

# Let's get libraries here, then sort out git then sort out making this a
# library so we don't have to deal with all the library crap
library(here)
library(tidyverse)
library(sf)
library(stars)


# TODO
# For each temp on each day, get the predicted GPP and ER
# For both the catchment regression and non-catchment regression
# That'll generate ANOTHER dataset- export it
# and make an animation
# animate the temps while I'm at it
# THEN, re-import and aggregate to the bimonthly sequence
# How? I do it somewhere else but can't remember where. I think in one of the demos I break things down to water year
# Then, read in the volume
# Multiply the predicted GPP by volume
# Aggregate up to catchments, make a cool animation
# Oof. That's a lot for one day.


# Read in data ------------------------------------------------------------

## TEMPS
# Need to wrap this over catchments
tempsIn <- file.path(datOut, 'Tempprocessed', 'weightedMean')
catchNames <- list.files(file.path(tempsIn, 'chunked')) # Get the catchment names from the folders
catchFiles <- list.files(tempsIn, pattern = '.rdata')
# And test with and without a catchment in the set of catchments from metabolism
thisCatch <- 'Murrumbidgee' # For testing- needs to be grabeed from catchNames in a loop
load(file = file.path(tempsIn, paste0(thisCatch, '_weightedMean.rdata')))

## Regressions
regIn <- file.path(datOut, 'TempAndProduction', 'tempMetabolismRegressions.rdata')
load(regIn)



# Whole basin predictions -------------------------------------------------

# Somehow going for the sf creates something 524Gb. that's bad. will need to chunk. which means the question is how much to chunk
# subCatch <- Murrumbidgee_weightedMean %>% slice('geometry', 1:10) %>% slice('time', 500:515)
# Turn the kelvins into tempC?
Murrumbidgee_weightedMean[[1]] <- as.numeric(Murrumbidgee_weightedMean[[1]])-272.15
names(Murrumbidgee_weightedMean) <- 'tempC'

# Make into an sf dataframe so we can do the predictions
# THIS IS A BIT ABSURD- BEEN GOING 30 MINS AND NOT FINISHED. SO THINK I'LL NEED
# TO WRAP ALL THIS IN A FOREACH WITH A CHUNKMAKER BIT ON IT.
# Question is how big to make the chunks.
# Probably should write the whole smash as a function, then do a test.
# Suppose another question is whether the HPC can just do it quickly.
# AND, do I want to chunk over space, time, or both? Would be nice if I only chunked one dimension
catchSF <- st_as_sf(Murrumbidgee_weightedMean, long = TRUE) # THe long argument keeps times

# create the columns we need
catchSF <- catchSF %>% 
  mutate(daysAwayFromWaterYear = daysfromWY(time), 
         bimonthFactor = as.factor(getBimonth(time)), # Get both time variables, I guess
         ValleyName = thisCatch,
         wateryear = 2100)

# predict new values
catchSF <- catchSF %>% 
  add_preds(gppDaysValleys, predname = 'logGPPdaysvalleys')  %>% 
  add_preds(gppDays, predname = 'logGPPdays') %>%
  add_preds(erDaysValleys, predname = 'logERdaysvalleys') %>%
  add_preds(erDays, predname = 'logERdays')

# throw out all the extra variables, leaving just the predictions
catchSF <- catchSF %>%
  select(time, starts_with('log'))
catchSF

# Turn into stars
# This version for all model predictions. might as well (unless the resulting object is too big)
starpredictions <- catchSF %>% # select(time, logGPPdays) %>%
  st_as_stars() %>%
  # st_redimension is more flexible than merge.stars()
  st_redimension(new_dims = st_dimensions(Murrumbidgee_weightedMean))
starpredictions[[1]] <- NULL
starpredictions


# Wrap around basins to produce one per basin and save

# LONGER TERM: INCLUDE PREDICTION INTERVALS. predict.merMod doesnt do them; bootMer might
  # Honestly, might just be easier to scrap the random effects and use lm predict
  # looks like library(merTools) is the way to go
  # https://stats.stackexchange.com/questions/147836/prediction-interval-for-lmer-mixed-effects-model-in-r

## CAREFUL WHEN USING THESE STARS ELSEWHERE- SOME FUNCTIONS (YEARSUMMARY,
## ROLLINGTIMEFUNCTIONS) EXPECT ONLY ONE ATTRIBUTE AND SO OPERATE ON STARS[[1]]
