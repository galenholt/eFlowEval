# Script to read in temp data and sensor data, match it up and do a regression

source('directorySet.R')

# Let's get libraries here, then sort out git then sort out making this a
# library so we don't have to deal with all the library crap
library(here)
library(tidyverse)
library(lubridate)
library(sf)
library(stars)
library(foreach)
library(doFuture)
library(patchwork)

# Set up directory --------------------------------------------------------

scriptOut <- file.path(datOut, 'TempAndProduction')
if (!dir.exists(scriptOut)) {dir.create(scriptOut, recursive = TRUE)}

# Read in data (generated in joinTempProduction) ----------------------------------------------------
load(file.path(scriptOut, 'joinedTempProd.rdata'))


# CUT OUT THE MIS-GPSed Point in bidgee that maps to Loddon ---------------------------------------
  # Hopefully temporary and can get correct point location soon
joinedTempProd <- filter(joinedTempProd, ValleyName != "Loddon")


# Bit of data cleanup -----------------------------------------------------

# Get rid of the units in the temp column
joinedTempProd <- joinedTempProd %>% 
  mutate(tempC = as.numeric(tempK)-272.15) %>%
  mutate(month = lubridate::month(sampledate, label = TRUE)) %>%
  mutate(passesCriteria = ifelse(meetsusecriteria %in% c('Y', 'Yes'), TRUE, FALSE)) %>% # Don't check the c('N', 'No'), because if there are missing, they should code as no anyway
  mutate(ValleyName = str_remove_all(ValleyName, ' ')) # Match the way we have to deal with valleyname everywhere else

joinedTempPasses <- filter(joinedTempProd, passesCriteria)

# Create logged variables
joinedTempPasses <- joinedTempPasses %>%
  mutate(logGPP = log(gpp), logER = log(er))

# -------------------------------------------------------------------------


# PLOTS -------------------------------------------------------------------


# -------------------------------------------------------------------------



# All data, linear scale --------------------------------------------------


# Quickly, does the daily water temp from the loggers correlate to surface?
watervssurftemp <- ggplot(joinedTempProd, aes(x = avedailytemp, y = tempC, 
                           color = month)) + 
  geom_point() + labs(x = 'Water temp', y = 'Surface temp', color = 'Month')
# watervssurftemp
watervssurftemp + geom_smooth(method = 'lm') + facet_wrap(~ValleyName)


# GPP to surface temps
surfvgpp <- ggplot(joinedTempProd, aes(x = tempC, y = gpp, 
                                              color = month)) + 
  geom_point() 
  # labs(x = 'Water temp', y = 'Surface temp', color = 'Month')
surfvgpp + geom_smooth(method = 'lm') + facet_wrap(~ValleyName)
# Not really. Need to sort that out

# GPP to water temps
watervgpp <- ggplot(joinedTempProd, aes(x = avedailytemp, y = gpp, 
                                       color = month)) + 
  geom_point() 
# labs(x = 'Water temp', y = 'Surface temp', color = 'Month')
watervgpp + geom_smooth(method = 'lm') + facet_wrap(~ValleyName)

# ER to surface
surfver <- ggplot(joinedTempProd, aes(x = tempC, y = er, 
                                       color = month)) + 
  geom_point() 
# labs(x = 'Water temp', y = 'Surface temp', color = 'Month')
surfver + geom_smooth(method = 'lm') + facet_wrap(~ValleyName)
# Not really. Need to sort that out

# ER to water
waterver <- ggplot(joinedTempProd, aes(x = avedailytemp, y = er, 
                                        color = month)) + 
  geom_point() 
# labs(x = 'Water temp', y = 'Surface temp', color = 'Month')
waterver + geom_smooth(method = 'lm') + facet_wrap(~ValleyName)

# So, good news is surface is mapping to water temp in the crude relationships
# Bad news is those are unimodal, and so shouldn't be fit with linear fits.
# Though maybe logged?


# Plots on log scale ------------------------------------------------------

# GPP to surface
surfvlogGPP <- ggplot(joinedTempProd, aes(x = tempC, y = log(gpp), 
                                       color = month)) + 
  geom_point() 
# labs(x = 'Water temp', y = 'Surface temp', color = 'Month')
surfvlogGPP + geom_smooth(method = 'lm') + facet_wrap(~ValleyName)
# less modal, but less of a relationship too

# GPP to water
watervlogGPP <- ggplot(joinedTempProd, aes(x = avedailytemp, y = log(gpp), 
                                        color = month)) + 
  geom_point() 
# labs(x = 'Water temp', y = 'Surface temp', color = 'Month')
watervlogGPP + geom_smooth(method = 'lm') + facet_wrap(~ValleyName)

# ER to surface
surfvlogER <- ggplot(joinedTempProd, aes(x = tempC, y = log(er), 
                                      color = month)) + 
  geom_point() 
# labs(x = 'Water temp', y = 'Surface temp', color = 'Month')
surfvlogER + geom_smooth(method = 'lm') + facet_wrap(~ValleyName)
# not much of anything, though season matters

# Water to ER
watervlogER <- ggplot(joinedTempProd, aes(x = avedailytemp, y = log(er), 
                                       color = month)) + 
  geom_point() 
# labs(x = 'Water temp', y = 'Surface temp', color = 'Month')
watervlogER + geom_smooth(method = 'lm')+ facet_wrap(~ValleyName)




# -------------------------------------------------------------------------


# Log plots using passing data --------------------------------------------
   # only to surface temps

# GPP to surface
surfvlogGPPpass <- ggplot(joinedTempPasses, aes(x = tempC, y = log(gpp), 
                                          color = month)) + 
  geom_point() 
# labs(x = 'Water temp', y = 'Surface temp', color = 'Month')
surfvlogGPPpass + geom_smooth(method = 'lm') + facet_wrap(~ValleyName)
# less modal, but less of a relationship too

# ER to surface
surfvlogERpass <- ggplot(joinedTempPasses, aes(x = tempC, y = log(er), 
                                         color = month)) + 
  geom_point() 
# labs(x = 'Water temp', y = 'Surface temp', color = 'Month')
surfvlogERpass + geom_smooth(method = 'lm') + facet_wrap(~ValleyName)

# That actually tightens up a fair amount. How terrible are they liear?
surfvGPPpass <- ggplot(joinedTempPasses, aes(x = tempC, y = gpp, 
                                                color = month)) + 
  geom_point() 
# labs(x = 'Water temp', y = 'Surface temp', color = 'Month')
surfvGPPpass + geom_smooth(method = 'lm') # + facet_wrap(~ValleyName)
# vs
surfvlogGPPpass + geom_smooth(method = 'lm')

# Yeah, stick with logs


# Save some more readable plots --------------------------------------------------

# GPP to surface
surfvlogGPPPretty <- ggplot(joinedTempPasses, aes(x = tempC, y = log(gpp), 
                                                color = ValleyName)) + 
  geom_point() +
  scale_color_brewer(palette = 'Dark2') +
  labs(y = 'log(GPP)',
       x = 'Temperature',
       color = 'Catchment')
# labs(x = 'Water temp', y = 'Surface temp', color = 'Month')
surfvlogGPPPretty + geom_smooth(method = 'lm') + theme_bw()
# less modal, but less of a relationship too

# ER to surface
surfvlogERPretty <- ggplot(joinedTempPasses, aes(x = tempC, y = log(er), 
                                               color = ValleyName)) + 
  geom_point() +
  scale_color_brewer(palette = 'Dark2') +
  labs(y = 'log(ER)',
       x = 'Temperature',
       color = 'Catchment')
# labs(x = 'Water temp', y = 'Surface temp', color = 'Month')
surfvlogERPretty + geom_smooth(method = 'lm') + theme_bw()


# Those are actually pretty ugly
# GPP to surface
surfvlogGPPPrettyL <- ggplot(joinedTempPasses, aes(x = tempC, y = log(gpp), 
                                                  color = ValleyName)) + 
  geom_smooth(method = 'lm') +
  scale_color_brewer(palette = 'Dark2') +
  labs(y = 'log(GPP)',
       x = 'Temperature',
       color = '')
# labs(x = 'Water temp', y = 'Surface temp', color = 'Month')
surfvlogGPPPrettyL + theme_bw()
# less modal, but less of a relationship too

# ER to surface
surfvlogERPrettyL <- ggplot(joinedTempPasses, aes(x = tempC, y = log(er), 
                                                 color = ValleyName)) + 
  geom_smooth(method = 'lm') +
  scale_color_brewer(palette = 'Dark2') +
  labs(y = 'log(ER)',
       x = 'Temperature',
       color = '')
# labs(x = 'Water temp', y = 'Surface temp', color = 'Month')
surfvlogERPrettyL + theme_bw()

png(file.path(scriptOut, 'regressionPlot.png'), height = 8/2.54, width = 12/2.54, units = 'in', res = 300)
ggpubr::ggarrange(surfvlogGPPPrettyL + theme_bw(),
                  surfvlogERPrettyL + theme_bw(),
                  common.legend = TRUE)
dev.off()



# TODO --------------------------------------------------------------------

# See if I can put a location effect in? DONE
# Maybe year as a random effect?
# How to do seasonality?
  # Month as random effect? (still won't be linear, but because random less of an issue)
  # days since water year start (July 1)
  # Days since... won't be linear
  # Maybe days FROM NEAREST July 1? ie some sort of absolute distance from the water year start?
  # is it even necessary? Will be highly corr with temp
# I guess just go with it, and say there's lots of noise
# Check notes from Darren, make sure I'm not missing anything NOPE- just temp
# and season. Once I figure out how to do season...



# Let's see if we can pick apart a decent way to do season --------
  # and check its corr with temp
tempmonth <- ggplot(joinedTempPasses, aes(x = month, y = tempC, 
                                             color = ValleyName)) + 
  geom_point() 
# labs(x = 'Water temp', y = 'Surface temp', color = 'Month')
tempmonth + geom_smooth(method = 'lm')

tempday <- ggplot(joinedTempPasses, aes(x = sampledate, y = tempC, 
                                          color = ValleyName)) + 
  geom_point() 
# labs(x = 'Water temp', y = 'Surface temp', color = 'Month')
tempday # + geom_smooth(method = 'lm')

# Day of year
tempDOY <- ggplot(joinedTempPasses, aes(x = yday(sampledate), y = tempC, 
                                        color = ValleyName)) + 
  geom_point() 
# labs(x = 'Water temp', y = 'Surface temp', color = 'Month')
tempDOY # + geom_smooth(method = 'lm')

# Let's try making an abs(distance from July 1 start of water year) variable.
# It'll still be sinusoid, but close enough to linear for our purposes

# FUNCTIONS USED IN THIS SECTION MOVED TO HELPERS.R

# Ah, but the months ARE quanitized, at least with hydrology. But is it always the same two months?
# Created in inundationCatchmentAggregate
inunconcatpath <- file.path(datOut, 'Inundationprocessed',
                            'areaInun', 'basinConcat',
                            'inunCatchConcat_areaInun.rdata')
load(inunconcatpath)

inundates <- round_date(st_get_dimension_values(inunBasin, which = 'time'), unit = 'day')
table(day(inundates)) # always on the first day
table(month(inundates)) # always the same months

# The following function is clearer to me for this situation
  # WHEN WE GO TO JOIN, BE VERY CAREFUL- THE MONTHS HERE WON'T MATCH THE MONTHS
  # IN THE INUNDATION, BECAUSE THOSE ARE THE 1ST OF THE NEXT MONTH
# case_when(
#   month(sampledate) %in% c(1, 2) ~ 1, # Maps to 03-01
#   month(sampledate) %in% c(3, 4) ~ 2, # Map to 05-01
#   month(sampledate) %in% c(5, 6) ~ 4, # Map to 07-01
#   month(sampledate) %in% c(7, 8) ~ 5, # Map to 09-01
#   month(sampledate) %in% c(9, 10) ~ 6, # Map to 11-01  
#   month(sampledate) %in% c(11, 12) ~ 7, # Maps to Jan 01 because this is the preceding
# )

# AND I need a water year indicator to assign water years for random variables
  # Starts July 1
# And the min date in the temp data is August 11 2014
min(joinedTempPasses$sampledate)
# I think establish a duration of a year
d <- duration(1, units = 'year')
interval(start = dmy('01072014'), end = dmy('01072014') + d)
# then, generate all of those (I guess), and do a %within% check

# but is this easier to do with a logical on the fly?
exampledate <- dmy('01082015')
month(exampledate)
# if the month is >= 7, the wateryear is the current year, ie
year(exampledate)

# But if hte month is < 7, e.g.
exd2 <- dmy('07052015')
# then the wateryear is the previous
year(exd2-dyears()) # the construction here is funny, but works



joinedTempPasses <- joinedTempPasses %>%
  # will be off by 1 for leap years, don't care
  mutate(daysAwayFromWaterYear = abs(yday(sampledate)-yday(dmy('01072019')))) %>%
  mutate(season = getSeason(sampledate)) %>%
  mutate(inunBimonthGroup = getBimonth(sampledate),
  bimonthFactor = as.factor(inunBimonthGroup)) %>%
  mutate(wateryear = getWaterYear(sampledate))



  
# Look at those with plots
tempDOWY <- ggplot(joinedTempPasses, aes(x = daysAwayFromWaterYear, y = tempC, 
                                        color = ValleyName)) + 
  geom_point() 
# labs(x = 'Water temp', y = 'Surface temp', color = 'Month')
tempDOWY # + geom_smooth(method = 'lm')


# NOW, that's pretty collinear with temp, but does it add information? 
  # Probably does. BUt this plot isn't easy to read. 
tempWYGPP <- ggplot(joinedTempPasses, aes(x = daysAwayFromWaterYear, y = tempC, 
                                         color = log(gpp))) + 
  geom_point(alpha = 0.2) 
# labs(x = 'Water temp', y = 'Surface temp', color = 'Month')
tempWYGPP + viridis::scale_color_viridis()

tempSeason <- ggplot(joinedTempPasses, aes(x = season, y = tempC, 
                                         color = ValleyName)) + 
  geom_point(position = position_jitter()) 
# labs(x = 'Water temp', y = 'Surface temp', color = 'Month')
tempSeason


tempSeason <- ggplot(joinedTempPasses, aes(x = season, y = tempC, 
                                           color = log(gpp))) + 
  geom_point(alpha = 0.2, position = position_jitter()) 
# labs(x = 'Water temp', y = 'Surface temp', color = 'Month')
tempSeason + viridis::scale_color_viridis()

# and the bimonthly groupings defined by inundation
tempBimonth <- ggplot(joinedTempPasses, aes(x = inunBimonthGroup, y = tempC, 
                                           color = log(gpp))) + 
  geom_point(alpha = 0.2, position = position_jitter()) 
# labs(x = 'Water temp', y = 'Surface temp', color = 'Month')
tempBimonth + viridis::scale_color_viridis()
# If we use this, two options- could still do the absolute difference from winter, OR could treat as factors. 
  # Inclination is to treat as factors- then we can get differences between spring and fall (ascending/descending)

# I guess the question here is whether there's a season effect on top of th temp effect. Probably is.
  # but, we can check that with anova/AIC. I think just start building some models and pick one

# So, let's build models as best we can
# But, let's ALSO build models targeted at the data we're going to be fitting this to. 
  # That said, there are two different ways to fit.
  # 1. average the temps into the bimonthlies, fit to temp and multiply by inundation
  # 2. fit to temp, THEN average production into bimonthlies, THEN multiply by inundation
    # 2 does a better job with nonlinear temp-production relationships, and lets us use what I expect will be the best fits
  


# GPP MODELS --------------------------------------------------------------
# Some of these are throwing errors because there are 9 0 gpps that yield inf log(gpp)
# sum(joinedTempPasses$gpp == 0)
min(joinedTempPasses$gpp[joinedTempPasses$gpp > 0])
min(joinedTempPasses$gpp[joinedTempPasses$gpp > 0])
# I COULD go add 0.01 to evertthing, but that does exist, so I'm inclined to
# just throw out those 9 points
jtp <- filter(joinedTempPasses, gpp > 0 & !is.na(tempC))


# Temp, from daysAwayFromWaterYear, catchment, year as random
  # subtest: should I use lme4 or TMB
system.time(tempdaysvalleyRwgpp <- lme4::lmer(logGPP ~ tempC + daysAwayFromWaterYear + ValleyName + 
                                                (1|wateryear),
           data = jtp)) # I think I don't need any fancy families or anything
# system.time(tempdaysvalleyRwgppTMB <- glmmTMB::glmmTMB(logGPP ~ tempC + daysAwayFromWaterYear + ValleyName + 
#                                                          (1|wateryear),
#                                 data = jtp)) # I think I don't need any fancy families or anything
# # library(glmm)
# # library(nlme)
# # aaaa different syntax
#   # system.time(tempdaysvalleyRwgppGLMM <- glmm::glmm(fixed = logGPP ~ tempC + daysAwayFromWaterYear + ValleyName,
#   #                                                   random = random = logGPP ~ wateryear,
#   #                                                      data = jtp))
# # so is nlme. This is frustrating
# # TMB much slower, but gives p values, but not single-value anovas
# summary(tempdaysvalleyRwgpp)
# summary(tempdaysvalleyRwgppTMB)
# AIC(tempdaysvalleyRwgpp)
# AIC(tempdaysvalleyRwgppTMB)
# anova(tempdaysvalleyRwgpp)
# anova(tempdaysvalleyRwgppTMB)
# 
# I think I'm going to stick with lmer, since it's more common, faster, and gives anova tables.

# but predictions don't have intervals. do they for glmmTMB?
# no

# # code found here:https://www.r-bloggers.com/2015/06/confidence-intervals-for-prediction-in-glmms/
# # says #first CI and PI using predict-like method, using code posted here: http://glmm.wikidot.com/faq
#   # Which is Ben Bolker's website, since moved to http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html
# # modifying the terms, but
# ## UUUUNNNGGGGHHHH. Newdata is a pain. I might actually be better off sorting
# ## this out in predictMetabolism where I've generated the newdatas
# newdat<-data.frame(logGPP = seq(-4,4,length=20)) # ROughly the range(jtp$logGPP)
# mm <- model.matrix(~logGPP, newdat)
# newdat$y <- mm %*% lme4::fixef(tempdaysvalleyRwgpp) 
# # predict(m,newdat, re.form=NA) would give the same results
# pvar1 <- diag(mm %*% tcrossprod(vcov(m),mm))
# tvar1 <- pvar1+VarCorr(m)$f[1] # must be adapted for more complex models
# newdat <- data.frame(
#   newdat
#   , plo = newdat$y-1.96*sqrt(pvar1)
#   , phi = newdat$y+1.96*sqrt(pvar1)
#   , tlo = newdat$y-1.96*sqrt(tvar1)
#   , thi = newdat$y+1.96*sqrt(tvar1)
# )

# For the days away from water year ---------------------------------------

# Before we get into randoms or not, let's build some that we can anova
# Temp, from daysAwayFromWaterYear, catchment, year as random
tempdaysvalleyRwgpp <- lme4::lmer(logGPP ~ tempC + daysAwayFromWaterYear + ValleyName + 
                                 (1|wateryear),
                               data = jtp) # I think I don't need any fancy families or anything

# Temp, from daysAwayFromWaterYear, NO CATCHMENT year as random
tempdaysRwgpp <- lme4::lmer(logGPP ~ tempC + daysAwayFromWaterYear + 
                                 (1|wateryear),
                               data = jtp) # I think I don't need any fancy families or anything

# Temp alone, year as random
tempRwgpp <- lme4::lmer(logGPP ~ tempC + 
                                 (1|wateryear),
                               data = jtp) # I think I don't need any fancy families or anything

# Temp, NO DAYS, catchment, year as random
tempvalleyRwgpp <- lme4::lmer(logGPP ~ tempC + ValleyName + 
                                 (1|wateryear),
                               data = jtp) # I think I don't need any fancy families or anything

# Not going to look at no temp case, because that's the data we have
anova(tempRwgpp, tempvalleyRwgpp, tempdaysvalleyRwgpp)
anova(tempRwgpp, tempdaysRwgpp, tempdaysvalleyRwgpp)
# In both cases, adding both helps. so stick with that.

# interaction terms?
# Temp, from daysAwayFromWaterYear, catchment, both interactions
tempdaysvalleyInteractRwgpp <- lme4::lmer(logGPP ~ tempC + daysAwayFromWaterYear + ValleyName + 
                                 tempC*daysAwayFromWaterYear + tempC*ValleyName +
                                 (1|wateryear),
                               data = jtp)

# Temp, from daysAwayFromWaterYear, catchment, days interaction
tempdaysInteractRwgpp <- lme4::lmer(logGPP ~ tempC + daysAwayFromWaterYear + ValleyName + 
                                        tempC*daysAwayFromWaterYear +
                                        (1|wateryear),
                                      data = jtp)

# Temp, from daysAwayFromWaterYear, catchment, catch interaction
tempvalleyInteractRwgpp <- lme4::lmer(logGPP ~ tempC + daysAwayFromWaterYear + ValleyName + 
                                       tempC*ValleyName +
                                        (1|wateryear),
                                      data = jtp)
anova(tempRwgpp, tempvalleyRwgpp, tempdaysvalleyRwgpp, tempvalleyInteractRwgpp, tempdaysvalleyInteractRwgpp)
anova(tempRwgpp, tempdaysRwgpp, tempdaysvalleyRwgpp, tempdaysInteractRwgpp, tempdaysvalleyInteractRwgpp)
# in both cases, the whole thing is best. Suppose go with that

# Is the random effect needed?
# Temp, from daysAwayFromWaterYear, catchment, both interactions, NO random effect
tempdaysvalleyInteractNORgpp <- lm(logGPP ~ tempC + daysAwayFromWaterYear + ValleyName + 
                                         tempC*daysAwayFromWaterYear + tempC*ValleyName,
                                       data = jtp)
AIC(tempdaysvalleyInteractRwgpp, tempdaysvalleyInteractNORgpp)

# Same set of analyses, but for the bimonthly chunks ----------------------
   # the ones without binmonth have already been made, don't do that again
# Before we get into randoms or not, let's build some that we can anova
# Temp, from bimonthFactor, catchment, year as random
tempbimonthvalleyRwgpp <- lme4::lmer(logGPP ~ tempC + bimonthFactor + ValleyName + 
                                 (1|wateryear),
                               data = jtp) # I think I don't need any fancy families or anything

# Temp, from bimonthFactor, NO CATCHMENT year as random
tempbimonthRwgpp <- lme4::lmer(logGPP ~ tempC + bimonthFactor + 
                           (1|wateryear),
                         data = jtp) # I think I don't need any fancy families or anything


# Not going to look at no temp case, because that's the data we have
anova(tempRwgpp, tempvalleyRwgpp, tempbimonthvalleyRwgpp)
anova(tempRwgpp, tempbimonthRwgpp, tempbimonthvalleyRwgpp)
# In both cases, adding both helps. so stick with that.

# interaction terms?
# Temp, from bimonthFactor, catchment, both interactions
tempbimonthvalleyInteractRwgpp <- lme4::lmer(logGPP ~ tempC + bimonthFactor + ValleyName + 
                                         tempC*bimonthFactor + tempC*ValleyName +
                                         (1|wateryear),
                                       data = jtp)

# Temp, from bimonthFactor, catchment, bimonth interaction
tempbimonthInteractRwgpp <- lme4::lmer(logGPP ~ tempC + bimonthFactor + ValleyName + 
                                   tempC*bimonthFactor +
                                   (1|wateryear),
                                 data = jtp)

# Temp, from bimonthFactor, catchment, catch interaction
tempvalleyInteractRwgpp <- lme4::lmer(logGPP ~ tempC + bimonthFactor + ValleyName + 
                                     tempC*ValleyName +
                                     (1|wateryear),
                                   data = jtp)
anova(tempRwgpp, tempvalleyRwgpp, tempbimonthvalleyRwgpp, tempvalleyInteractRwgpp, tempbimonthvalleyInteractRwgpp)
anova(tempRwgpp, tempbimonthRwgpp, tempbimonthvalleyRwgpp, tempbimonthInteractRwgpp, tempbimonthvalleyInteractRwgpp)


# So, consistently the full model, though the valley interaction seems less important here.

# Temp, from daysAwayFromWaterYear, catchment, both interactions, NO random effect
# Temp, from bimonthFactor, catchment, both interactions
tempbimonthvalleyInteractNORgpp <- lm(logGPP ~ tempC + bimonthFactor + ValleyName + 
                                            tempC*bimonthFactor + tempC*ValleyName,
                                          data = jtp)
AIC(tempbimonthvalleyInteractRwgpp, tempbimonthvalleyInteractNORgpp)
# Less of an effect of random year here, but still does improve things a bit



# ER MODELS ---------------------------------------------------------------
# Some of these are throwing errors because ther are 9 0 gpps that yield inf log(gpp)
# sum(joinedTempPasses$gpp == 0)
min(joinedTempPasses$er[joinedTempPasses$er > 0])
sum(joinedTempPasses$er == 0) # interesting. 9, but not the SAME 9
# I COULD go add 0.01 to evertthing, but that does exist, so I'm inclined to
# just throw out those 9 points
jtpER <- filter(joinedTempPasses, er > 0 & !is.na(tempC))


# For the days away from water year ---------------------------------------

# Before we get into randoms or not, let's build some that we can anova
# Temp, from daysAwayFromWaterYear, catchment, year as random
tempdaysvalleyRwer <- lme4::lmer(logER ~ tempC + daysAwayFromWaterYear + ValleyName + 
                                    (1|wateryear),
                                  data = jtpER) # I think I don't need any fancy families or anything

# Temp, from daysAwayFromWaterYear, NO CATCHMENT year as random
tempdaysRwer <- lme4::lmer(logER ~ tempC + daysAwayFromWaterYear + 
                              (1|wateryear),
                            data = jtpER) # I think I don't need any fancy families or anything

# Temp alone, year as random
tempRwer <- lme4::lmer(logER ~ tempC + 
                          (1|wateryear),
                        data = jtpER) # I think I don't need any fancy families or anything

# Temp, NO DAYS, catchment, year as random
tempvalleyRwer <- lme4::lmer(logER ~ tempC + ValleyName + 
                                (1|wateryear),
                              data = jtpER) # I think I don't need any fancy families or anything

# Not going to look at no temp case, because that's the data we have
anova(tempRwer, tempvalleyRwer, tempdaysvalleyRwer)
anova(tempRwer, tempdaysRwer, tempdaysvalleyRwer)

# Adding days doesn't matter. (though it DOES matter if we add it to valley? which is weird).
# does adding temp? YES
nullRwer <- lme4::lmer(logER ~ 1 + 
                               (1|wateryear),
                             data = jtpER) # I think I don't need any fancy families or anything
anova(nullRwer, tempRwer)
# and
valleyRwer <- lme4::lmer(logER ~ ValleyName + 
                               (1|wateryear),
                             data = jtpER) # I think I don't need any fancy families or anything
anova(valleyRwer, tempvalleyRwer)

# interaction terms?
# Temp, from daysAwayFromWaterYear, catchment, both interactions
tempdaysvalleyInteractRwer <- lme4::lmer(logER ~ tempC + daysAwayFromWaterYear + ValleyName + 
                                            tempC*daysAwayFromWaterYear + tempC*ValleyName +
                                            (1|wateryear),
                                          data = jtpER)

# Temp, from daysAwayFromWaterYear, catchment, days interaction
tempdaysInteractRwer <- lme4::lmer(logER ~ tempC + daysAwayFromWaterYear + ValleyName + 
                                      tempC*daysAwayFromWaterYear +
                                      (1|wateryear),
                                    data = jtpER)

# Temp, from daysAwayFromWaterYear, catchment, catch interaction
tempvalleyInteractRwer <- lme4::lmer(logER ~ tempC + daysAwayFromWaterYear + ValleyName + 
                                        tempC*ValleyName +
                                        (1|wateryear),
                                      data = jtpER)
anova(tempRwer, tempvalleyRwer, tempdaysvalleyRwer, tempvalleyInteractRwer, tempdaysvalleyInteractRwer)
anova(tempRwer, tempdaysRwer, tempdaysvalleyRwer, tempdaysInteractRwer, tempdaysvalleyInteractRwer)
# in both cases, the whole thing is best. Suppose go with that

# Since the days term wasn't useful alone, how does the non-valley case work out?
tempdaysOnlyInteractRwer <- lme4::lmer(logER ~ tempC + daysAwayFromWaterYear +
                                     tempC*daysAwayFromWaterYear +
                                     (1|wateryear),
                                   data = jtpER)
anova(tempRwer, tempdaysRwer, tempdaysOnlyInteractRwer)
# interaction still helps, so leave it in

# Is the random effect needed?
# Temp, from daysAwayFromWaterYear, catchment, both interactions, NO random effect
tempdaysvalleyInteractNORer <- lm(logER ~ tempC + daysAwayFromWaterYear + ValleyName + 
                                     tempC*daysAwayFromWaterYear + tempC*ValleyName,
                                   data = jtpER)
AIC(tempdaysvalleyInteractRwer, tempdaysvalleyInteractNORer)

# Same set of analyses, but for the bimonthly chunks ----------------------
# the ones without binmonth have already been made, don't do that again
# Before we get into randoms or not, let's build some that we can anova
# Temp, from bimonthFactor, catchment, year as random
tempbimonthvalleyRwer <- lme4::lmer(logER ~ tempC + bimonthFactor + ValleyName + 
                                       (1|wateryear),
                                     data = jtpER) # I think I don't need any fancy families or anything

# Temp, from bimonthFactor, NO CATCHMENT year as random
tempbimonthRwer <- lme4::lmer(logER ~ tempC + bimonthFactor + 
                                 (1|wateryear),
                               data = jtpER) # I think I don't need any fancy families or anything


# Not going to look at no temp case, because that's the data we have
anova(tempRwer, tempvalleyRwer, tempbimonthvalleyRwer)
anova(tempRwer, tempbimonthRwer, tempbimonthvalleyRwer)
# Now, bimonth is more useful than days

# interaction terms?
# Temp, from bimonthFactor, catchment, both interactions
tempbimonthvalleyInteractRwer <- lme4::lmer(logER ~ tempC + bimonthFactor + ValleyName + 
                                               tempC*bimonthFactor + tempC*ValleyName +
                                               (1|wateryear),
                                             data = jtpER)

# Temp, from bimonthFactor, catchment, bimonth interaction
tempbimonthInteractRwer <- lme4::lmer(logER ~ tempC + bimonthFactor + ValleyName + 
                                         tempC*bimonthFactor +
                                         (1|wateryear),
                                       data = jtpER)

# Temp, from bimonthFactor, catchment, catch interaction
tempvalleyInteractRwer <- lme4::lmer(logER ~ tempC + bimonthFactor + ValleyName + 
                                        tempC*ValleyName +
                                        (1|wateryear),
                                      data = jtpER)
anova(tempRwer, tempvalleyRwer, tempbimonthvalleyRwer, tempvalleyInteractRwer, tempbimonthvalleyInteractRwer)
anova(tempRwer, tempbimonthRwer, tempbimonthvalleyRwer, tempbimonthInteractRwer, tempbimonthvalleyInteractRwer)


# So, consistently the full model.

# Temp, from daysAwayFromWaterYear, catchment, both interactions, NO random effect
# Temp, from bimonthFactor, catchment, both interactions
tempbimonthvalleyInteractNORer <- lm(logER ~ tempC + bimonthFactor + ValleyName + 
                                        tempC*bimonthFactor + tempC*ValleyName,
                                      data = jtpER)
AIC(tempbimonthvalleyInteractRwer, tempbimonthvalleyInteractNORer)
# Less of an effect of random year here, but still does improve things a bit

# SO, although the days term fell over in a couple places, it was useful in the
# full model (though not as an interaction). So let's take that one out

# FINAL MODELS ------------------------------------------------------------

# DAYS, with valleys
gppDaysValleys <- lme4::lmer(logGPP ~ tempC + daysAwayFromWaterYear + ValleyName + 
                                         tempC*daysAwayFromWaterYear + tempC*ValleyName +
                                         (1|wateryear),
                                       data = jtp)

# BIMONTH, with valleys
gppBimonthValleys <- lme4::lmer(logGPP ~ tempC + bimonthFactor + ValleyName + 
                                    tempC*bimonthFactor + tempC*ValleyName +
                                    (1|wateryear),
                                  data = jtp)

# DAYS, without valleys (so can do whole basin)
gppDays <- lme4::lmer(logGPP ~ tempC + daysAwayFromWaterYear + 
                                 tempC*daysAwayFromWaterYear +
                                 (1|wateryear),
                               data = jtp)

# BIMONTH, without valleys (so can do whole basin)
gppBimonth <- lme4::lmer(logGPP ~ tempC + bimonthFactor +
                                    tempC*bimonthFactor +
                                    (1|wateryear),
                                  data = jtp)

## ER models
# DAYS, with valleys
  # No day*temp interaction here
erDaysValleys <- lme4::lmer(logER ~ tempC + daysAwayFromWaterYear + ValleyName + 
                                tempC*ValleyName +
                               (1|wateryear),
                             data = jtpER)

# BIMONTH, with valleys
erBimonthValleys <- lme4::lmer(logER ~ tempC + bimonthFactor + ValleyName + 
                                  tempC*bimonthFactor + tempC*ValleyName +
                                  (1|wateryear),
                                data = jtpER)

# DAYS, without valleys (so can do whole basin)
  # here, the temp*days is important enough to include
erDays <- lme4::lmer(logER ~ tempC + daysAwayFromWaterYear + 
                        tempC*daysAwayFromWaterYear +
                        (1|wateryear),
                      data = jtpER)

# BIMONTH, without valleys (so can do whole basin)
erBimonth <- lme4::lmer(logER ~ tempC + bimonthFactor +
                           tempC*bimonthFactor +
                           (1|wateryear),
                         data = jtpER)


# Create stats tables -----------------------------------------------------
  # don't use the more-precise ps, they crash computer
sjPlot::tab_model(gppDaysValleys, wrap.labels = 500) # , p.val = "kr", show.df = TRUE
sjPlot::tab_model(erDaysValleys, wrap.labels = 500) # , p.val = "kr", show.df = TRUE
# huge wrap_labels values put everything on one line helps prevent breaking wehn
# pasting into word tables

# I could generate the plots myself from predict(), but if sjPlot::plot_model()
# does it for me, that'd sure be easier. Though I *have* already written
# add_preds()- would just need to feed it newdata

# plots each at the mean of the others. Guess I need to do this myself.
predplot <- sjPlot::plot_model(gppDaysValleys, type = 'pred')
predplot
# Why does it only allow two colors?
predcolorplot <- sjPlot::plot_model(gppDaysValleys, type = 'int') # showing data doesn't really work well , show.data = TRUE
predcolorplot


eachvalley <- unique(jtp$ValleyName)
temps <- seq(from = min(jtp$tempC)-1, to = max(jtp$tempC)+1, length.out = 100)
dwy <- range(jtp$daysAwayFromWaterYear) # Get the end and middle of water year. It's just linear between. and the middle (day 184) is the end of the range because it's folded

predgrid <- expand_grid(ValleyName = eachvalley, tempC = temps, daysAwayFromWaterYear = dwy) %>%
  mutate(wateryear = 'WY')

predsGPP <- predgrid %>%
  add_preds(gppDaysValleys, predname = 'logGPPdaysvalleys', interval = 'confidence')

# quick test
ggplot(predsGPP, aes(x = logGPPdaysvalleys, y = logGPPdaysvalleys_cfit)) + geom_point()

# plot with data

# Days away is tricky to visualize

rawplot0 <- ggplot() +
  geom_point(data = jtp, aes(x = tempC, y = logGPP, 
                             color = ValleyName), alpha = 0.2) +
  geom_ribbon(data = filter(predsGPP, daysAwayFromWaterYear == 0),
              aes(x = tempC, ymin = logGPPdaysvalleys_clwr, ymax = logGPPdaysvalleys_cupr,
                  color = ValleyName, fill = ValleyName), alpha = 0.2) +
  geom_line(data = filter(predsGPP, daysAwayFromWaterYear == 0),
            aes(x = tempC, y = logGPPdaysvalleys, 
                color = ValleyName)) 

rawplot0

rawplotend <- ggplot() +
  geom_point(data = jtp, aes(x = tempC, y = logGPP, 
                             color = ValleyName), alpha = 0.2) +
  geom_ribbon(data = filter(predsGPP, daysAwayFromWaterYear == 184),
              aes(x = tempC, ymin = logGPPdaysvalleys_clwr, ymax = logGPPdaysvalleys_cupr,
                  color = ValleyName, fill = ValleyName), alpha = 0.2) +
  geom_line(data = filter(predsGPP, daysAwayFromWaterYear == 184),
            aes(x = tempC, y = logGPPdaysvalleys, 
                color = ValleyName)) 

rawplotend

  # facet_grid(.~daysAwayFromWaterYear) 

# what to do with daysAwayFromWaterYear? It is continuous, and so not easy to
# include in the plot. Facetting doesn't work because the points are all across
# the range. And it DOES make the points sit closer to the fits. One option
# would be to adjust each data point by the predicted shift from
# daysFromWaterYear. That's reasonably straightforward, but ignores error in
# that relationship

predjtp <- jtp %>%
  add_preds(gppDaysValleys, predname = 'predicted', interval = 'none')

predjtpzero <- jtp %>%
  mutate(daysAwayFromWaterYear = 0) %>%
  add_preds(gppDaysValleys, predname = 'predictedzero', interval = 'none')

predjtpend <- jtp %>%
  mutate(daysAwayFromWaterYear = 184) %>%
  add_preds(gppDaysValleys, predname = 'predictedend', interval = 'none')

predjtp <- bind_cols(predjtp, 
                     predictedzero = predjtpzero$predictedzero, 
                     predictedend = predjtpend$predictedend) %>%
  mutate(dayadjust0 = predicted-predictedzero, # get the difference between predicted at the relevant day and what it would be at day 0
         gppadjust0 = logGPP-dayadjust0, # shift the value by the amount we just calculated
         dayadjustend = predicted-predictedend,
         gppadjustend = logGPP-dayadjustend) # shift the value by the amount we just calculated

adjplot0 <- ggplot() +
  geom_point(data = predjtp, aes(x = tempC, y = gppadjust0, 
                             color = ValleyName), alpha = 0.2) +
  geom_ribbon(data = filter(predsGPP, daysAwayFromWaterYear == 0),
              aes(x = tempC, ymin = logGPPdaysvalleys_clwr, ymax = logGPPdaysvalleys_cupr,
                  color = ValleyName, fill = ValleyName), alpha = 0.2) +
  geom_line(data = filter(predsGPP, daysAwayFromWaterYear == 0),
            aes(x = tempC, y = logGPPdaysvalleys, 
                color = ValleyName))
adjplot0

adjplotend <- ggplot() +
  geom_point(data = predjtp, aes(x = tempC, y = gppadjustend, 
                                 color = ValleyName), alpha = 0.2) +
  geom_ribbon(data = filter(predsGPP, daysAwayFromWaterYear == 184),
              aes(x = tempC, ymin = logGPPdaysvalleys_clwr, ymax = logGPPdaysvalleys_cupr,
                  color = ValleyName, fill = ValleyName), alpha = 0.2) +
  geom_line(data = filter(predsGPP, daysAwayFromWaterYear == 184),
            aes(x = tempC, y = logGPPdaysvalleys, 
                color = ValleyName))

adjplotend


# Combine- will need to clean up.- fix axes, for ex. and colors
(rawplotend + coord_cartesian(ylim = c(-4, 4.5))) + 
  (adjplot0 + coord_cartesian(ylim = c(-4, 4.5))) + 
  (adjplotend + coord_cartesian(ylim = c(-4, 4.5))) + 
  plot_layout(guides = 'collect')


# I really don't like the way this is working.
# The data just doesn't really match the figures
# Can I find at least sections of days close to the ends? maybe a month?
ggplot(jtp, aes(x = daysAwayFromWaterYear)) + geom_histogram()


rawplot0_chunk <- ggplot() +
  geom_point(data = filter(jtp, daysAwayFromWaterYear < 30), aes(x = tempC, y = logGPP, 
                             color = ValleyName), alpha = 0.2) +
  geom_ribbon(data = filter(predsGPP, daysAwayFromWaterYear == 0),
              aes(x = tempC, ymin = logGPPdaysvalleys_clwr, ymax = logGPPdaysvalleys_cupr,
                  color = ValleyName, fill = ValleyName), alpha = 0.2) +
  geom_line(data = filter(predsGPP, daysAwayFromWaterYear == 0),
            aes(x = tempC, y = logGPPdaysvalleys, 
                color = ValleyName)) 

rawplot0_chunk

rawplotend_chunk <- ggplot() +
  geom_point(data = filter(jtp, daysAwayFromWaterYear > (184-30)), aes(x = tempC, y = logGPP, 
                             color = ValleyName), alpha = 0.2) +
  geom_ribbon(data = filter(predsGPP, daysAwayFromWaterYear == 184),
              aes(x = tempC, ymin = logGPPdaysvalleys_clwr, ymax = logGPPdaysvalleys_cupr,
                  color = ValleyName, fill = ValleyName), alpha = 0.2) +
  geom_line(data = filter(predsGPP, daysAwayFromWaterYear == 184),
            aes(x = tempC, y = logGPPdaysvalleys, 
                color = ValleyName)) 

rawplotend_chunk

# I think after all that, let's just plot the fits at the ends, and attach a different way of doing the data?
# these are the same as the rawplots but without the data

## GPP First
predplot0GPP <- ggplot() +
  geom_ribbon(data = filter(predsGPP, daysAwayFromWaterYear == 0),
              aes(x = tempC, ymin = logGPPdaysvalleys_clwr, ymax = logGPPdaysvalleys_cupr,
                  color = ValleyName, fill = ValleyName), alpha = 0.2) +
  geom_line(data = filter(predsGPP, daysAwayFromWaterYear == 0),
            aes(x = tempC, y = logGPPdaysvalleys, 
                color = ValleyName)) +
  colorspace::scale_color_discrete_qualitative(palette = 'Dark2') +
  colorspace::scale_fill_discrete_qualitative(palette = 'Dark2') +
  labs(y = 'log(GPP)', x =  'Temperature', color = 'Catchment', fill = 'Catchment') +
  # annotate(geom = 'text', x = 15, y = 2.5, label = '**Day 0**<br>(start/end)') + 
  ggtext::geom_richtext(aes(x = 18, y = 2.5, label = '**Day 0**<br>(start/end)'),
                        fill = NA, label.color = NA) 
predplot0GPP

predplotendGPP <- ggplot() +
  geom_ribbon(data = filter(predsGPP, daysAwayFromWaterYear == 184),
              aes(x = tempC, ymin = logGPPdaysvalleys_clwr, ymax = logGPPdaysvalleys_cupr,
                  color = ValleyName, fill = ValleyName), alpha = 0.2) +
  geom_line(data = filter(predsGPP, daysAwayFromWaterYear == 184),
            aes(x = tempC, y = logGPPdaysvalleys, 
                color = ValleyName)) +
  colorspace::scale_color_discrete_qualitative(palette = 'Dark2') +
  colorspace::scale_fill_discrete_qualitative(palette = 'Dark2') +
  labs(y = 'log(GPP)', x =  'Temperature', color = 'Catchment', fill = 'Catchment') + 
  ggtext::geom_richtext(aes(x = 15, y = 2.75, label = '**Day 184**<br>(middle)'),
                        fill = NA, label.color = NA)

predplotendGPP

dataplotGPP <- ggplot(data = jtp, aes(x = tempC, y = logGPP, # shape = ValleyName,
                                   color = daysAwayFromWaterYear)) +
  geom_point(alpha = 0.2) +
  # stat_density2d() +
  # stat_ellipse() +
  colorspace::scale_color_continuous_sequential(palette = 'YLGnBu') +
  facet_wrap(~ValleyName, nrow = 2) +
  labs(y = 'log(GPP)', x =  'Temperature', color = 'Days From\nWater Year')
dataplotGPP

dataandpredsGPP_simple <- (dataplotGPP + pubtheme) / 
  ((predplot0GPP + pubtheme) + 
     (predplotendGPP + pubtheme)) + 
  plot_layout(guides = 'collect') #& theme(legend.position = 'bottom')
dataandpredsGPP_simple

# Close but not ideal legends
legends <- ggpubr::ggarrange(ggpubr::get_legend(predplotendGPP + pubtheme), ggpubr::get_legend(dataplotGPP + pubtheme))


dataandpredsGPP <- (dataplotGPP + pubtheme + 
                   guides(color = guide_colorbar(title.position = 'top')) +
                   theme(legend.position = c(0.9, 0.15), 
                         legend.direction = 'horizontal')) / 
  ((predplot0GPP + pubtheme +  theme(legend.position = 'none')) + 
     (predplotendGPP + pubtheme + theme(legend.position = 'none')) +
     (ggpubr::ggarrange(ggpubr::get_legend(predplotendGPP + pubtheme))))
dataandpredsGPP


# Save Plots
pdf(file.path(scriptOut, 'gppDataPreds.pdf'), onefile = FALSE, 
    height = 12/2.54, width = 16/2.54, useDingbats = FALSE)
dataandpredsGPP
dev.off()

png(file.path(scriptOut, 'gppDataPreds.png'), 
    height = 12/2.54, width = 16/2.54, units = 'in', res = 300)
dataandpredsGPP
dev.off()

## ER

predsER <- predgrid %>%
  add_preds(erDaysValleys, predname = 'logERdaysvalleys', interval = 'confidence')


predplot0ER <- ggplot() +
  geom_ribbon(data = filter(predsER, daysAwayFromWaterYear == 0),
              aes(x = tempC, ymin = logERdaysvalleys_clwr, ymax = logERdaysvalleys_cupr,
                  color = ValleyName, fill = ValleyName), alpha = 0.2) +
  geom_line(data = filter(predsER, daysAwayFromWaterYear == 0),
            aes(x = tempC, y = logERdaysvalleys, 
                color = ValleyName)) +
  colorspace::scale_color_discrete_qualitative(palette = 'Dark2') +
  colorspace::scale_fill_discrete_qualitative(palette = 'Dark2') +
  labs(y = 'log(ER)', x =  'Temperature', color = 'Catchment', fill = 'Catchment') +
  # annotate(geom = 'text', x = 15, y = 2.5, label = '**Day 0**<br>(start/end)') + 
  ggtext::geom_richtext(aes(x = 18, y = 2.5, label = '**Day 0**<br>(start/end)'),
                        fill = NA, label.color = NA) 
predplot0ER

predplotendER <- ggplot() +
  geom_ribbon(data = filter(predsER, daysAwayFromWaterYear == 184),
              aes(x = tempC, ymin = logERdaysvalleys_clwr, ymax = logERdaysvalleys_cupr,
                  color = ValleyName, fill = ValleyName), alpha = 0.2) +
  geom_line(data = filter(predsER, daysAwayFromWaterYear == 184),
            aes(x = tempC, y = logERdaysvalleys, 
                color = ValleyName)) +
  colorspace::scale_color_discrete_qualitative(palette = 'Dark2') +
  colorspace::scale_fill_discrete_qualitative(palette = 'Dark2') +
  labs(y = 'log(ER)', x =  'Temperature', color = 'Catchment', fill = 'Catchment') + 
  ggtext::geom_richtext(aes(x = 20, y = 3.5, label = '**Day 184**<br>(middle)'),
                        fill = NA, label.color = NA)

predplotendER

dataplotER <- ggplot(data = jtp, aes(x = tempC, y = logER, # shape = ValleyName,
                                      color = daysAwayFromWaterYear)) +
  geom_point(alpha = 0.2) +
  # stat_density2d() +
  # stat_ellipse() +
  colorspace::scale_color_continuous_sequential(palette = 'YLGnBu') +
  facet_wrap(~ValleyName, nrow = 2) +
  labs(y = 'log(ER)', x =  'Temperature', color = 'Days From\nWater Year')
dataplotER

dataandpredsER_simple <- (dataplotER + pubtheme) / 
  ((predplot0ER + pubtheme) + 
     (predplotendER + pubtheme)) + 
  plot_layout(guides = 'collect') #& theme(legend.position = 'bottom')
dataandpredsER_simple

# Close but not ideal legends

dataandpredsER <- (dataplotER + pubtheme + 
                      guides(color = guide_colorbar(title.position = 'top')) +
                      theme(legend.position = c(0.9, 0.15), 
                            legend.direction = 'horizontal')) / 
  ((predplot0ER + pubtheme +  theme(legend.position = 'none')) + 
     (predplotendER + pubtheme + theme(legend.position = 'none')) +
     (ggpubr::ggarrange(ggpubr::get_legend(predplotendER + pubtheme))))
dataandpredsER


# Save Plots
pdf(file.path(scriptOut, 'ERDataPreds.pdf'), onefile = FALSE, 
    height = 12/2.54, width = 16/2.54, useDingbats = FALSE)
dataandpredsER
dev.off()

png(file.path(scriptOut, 'ERDataPreds.png'), 
    height = 12/2.54, width = 16/2.54, units = 'in', res = 300)
dataandpredsER
dev.off()

# season vs temp ----------------------------------------------------------
# A quick check of the correlation between temp and season. Made the plots above, really, but
ggplot(jtp, aes(x = tempC, y = daysAwayFromWaterYear, color = ValleyName)) + geom_point()
cor(jtp$daysAwayFromWaterYear, jtp$tempC)

# Checking in-stream temps ------------------------------------------------
gppDaysValleys_IS <- lme4::lmer(logGPP ~ avedailytemp + daysAwayFromWaterYear + ValleyName + 
                               tempC*daysAwayFromWaterYear + tempC*ValleyName +
                               (1|wateryear),
                             data = jtp)

erDaysValleys_IS <- lme4::lmer(logER ~ avedailytemp + daysAwayFromWaterYear + ValleyName + 
                              tempC*ValleyName +
                              (1|wateryear),
                            data = jtpER)

# R2 are tricky from GLMMs, but r.squaredGLMM seems to be reasonably well supported
# And, later, sjstats::r2 is as well, and has been superseded by performance::r2.
# All three give the same answer
# There are arguments for wanting both the marginal (fixed effects only) or conditional (entire model).

# GPP
# For the version with surface temps
MuMIn::r.squaredGLMM(gppDaysValleys)
sjstats::r2(gppDaysValleys)
performance::r2(gppDaysValleys)
# and water temps does do a bit better
MuMIn::r.squaredGLMM(gppDaysValleys_IS)
sjstats::r2(gppDaysValleys_IS)
performance::r2(gppDaysValleys_IS)

# ER
performance::r2(erDaysValleys)
performance::r2(erDaysValleys_IS)

# How about RMSE? 
# lower is better

# GPP
merTools::RMSE.merMod(gppDaysValleys)
performance::rmse(gppDaysValleys)

merTools::RMSE.merMod(gppDaysValleys_IS)
performance::rmse(gppDaysValleys_IS)

# ER
performance::rmse(erDaysValleys)

performance::rmse(erDaysValleys_IS)

# what if I leave out water year (since that is common between the two), and use
# the better-defined R2 in a linear model?
gdvLin <- lm(logGPP ~ tempC + daysAwayFromWaterYear + ValleyName + 
                 tempC*daysAwayFromWaterYear + tempC*ValleyName,
               data = jtp)

gdvLin_IS <- lm(logGPP ~ avedailytemp + daysAwayFromWaterYear + ValleyName + 
                 tempC*daysAwayFromWaterYear + tempC*ValleyName,
               data = jtp)

summary(gdvLin)
summary(gdvLin_IS)
# Very similar finding, RMSE is lower for the in-stream and R2 is 3-4% higher (out of 30%).

# Saving ------------------------------------------------------------------


save(gppDaysValleys,
     gppBimonthValleys,
     gppDays,
     gppBimonth,
     erDaysValleys,
     erBimonthValleys,
     erDays,
     erBimonth,
     file = file.path(scriptOut, 'tempMetabolismRegressions.rdata'))

