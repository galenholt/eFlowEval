# predictMetabolsim modifications to try to save uncertainty

# Header from the temperature file to retain all the directories,  --------
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
registerDoFuture()
# plan(multicore)

# Local testing setup
plan(multisession)
args <- c('blah', 'blah', 'blah', 'blah', 'blah', 'blah', 'Avoca')

# Setup -------------------------------------------------------------------

# Make a sub-directory for the subchunk
scriptOut <- file.path(datOut, 'TempAndProduction', 'PredictionsTESTING')
if (!dir.exists(scriptOut)) {dir.create(scriptOut, recursive = TRUE)}

# chunksize in n anae polygons
chunksize <- 1000

# I htink rather than a slurm array, pass a catchment name?
# could do either, really. Sort out arg orders.
# arraynum <- as.numeric(args[8])
# Going to be easiest I think to base on names rather than arraynums and name indices.
thisCatch <-  args[7]  # "EdwardWakool" # args[7] # 'Murrumbidgee' # For testing- needs to be grabbed from catchNames in a loop

print(thisCatch)

# stop('testing end here to make sure passing catchment name')
# Read in data ------------------------------------------------------------

## TEMPS
# Need to wrap this over catchments
tempsIn <- file.path(datOut, 'Tempprocessed', 'weightedMean')
# catchNames <- list.files(file.path(tempsIn, 'chunked')) # Get the catchment names from the folders
catchFiles <- list.files(tempsIn, pattern = '.rdata')
# Get the catchment names from the files not folders so we don't have to retain the chunks
catchNames <- str_extract(catchFiles, pattern = '^[A-z][^_]*')
# And test with and without a catchment in the set of catchments from metabolism
thisWMname <- paste0(thisCatch, '_weightedMean')
load(file = file.path(tempsIn, paste0(thisWMname, '.rdata')))

## Regressions
  # These are the regressions themselves, not predictions or inputs
regIn <- file.path(datOut, 'TempAndProduction', 'tempMetabolismRegressions.rdata')
load(regIn)

# change its name to something generic
# This is annoying, but I guess not too bad
weightedMeanTemps <- get(thisWMname)
rm(list = thisWMname)


# Whole basin predictions -------------------------------------------------

# Somehow going for the sf creates something 524Gb. that's bad. will need to chunk. which means the question is how much to chunk
# subCatch <- weightedMeanTemps %>% slice('geometry', 1:10) %>% slice('time', 500:515)
# Turn the kelvins into tempC?
weightedMeanTemps[[1]] <- as.numeric(weightedMeanTemps[[1]])-272.15
names(weightedMeanTemps) <- 'tempC'

# Let's ignore time and just loop over the anaes. doing it that way simplifies
# things and has worked elsewhere (the subchunk files and rastPolyjoin)
chunkpred <- function(bottom, top) {
  subCatch <- weightedMeanTemps %>% slice('geometry', bottom:top) # %>% slice('time', 500:515)
  
  
  # Make into an sf dataframe so we can do the predictions
  # THIS IS A BIT ABSURD- BEEN GOING 30 MINS AND NOT FINISHED. SO THINK I'LL NEED
  # TO WRAP ALL THIS IN A FOREACH WITH A CHUNKMAKER BIT ON IT.
  # Question is how big to make the chunks.
  # Probably should write the whole smash as a function, then do a test.
  # Suppose another question is whether the HPC can just do it quickly.
  # AND, do I want to chunk over space, time, or both? Would be nice if I only chunked one dimension
  catchSF <- st_as_sf(subCatch, long = TRUE) # THe long argument keeps times
  
  # create the columns we need
  catchSF <- catchSF %>% 
    mutate(daysAwayFromWaterYear = daysfromWY(time), 
           bimonthFactor = as.factor(getBimonth(time)), # Get both time variables, I guess
           ValleyName = thisCatch,
           wateryear = getWaterYear(time))
  
  # predict new values
  catchSF <- catchSF %>%
    add_preds(gppDaysValleys, predname = 'logGPPdaysvalleys', interval = 'both')  %>%
    add_preds(gppDays, predname = 'logGPPdays', interval = 'both') %>%
    add_preds(erDaysValleys, predname = 'logERdaysvalleys', interval = 'both') %>%
    add_preds(erDays, predname = 'logERdays', interval = 'both')
  
  # test plot
  ggplot(catchSF, aes(x = tempC, y = logGPPdays, color = daysAwayFromWaterYear)) +
    geom_point() +
    geom_linerange(aes(ymin = logGPPdays_pupr, ymax = logGPPdays_plwr)) + 
    geom_linerange(aes(ymin = logGPPdays_cupr, ymax = logGPPdays_clwr), color = 'grey30')
    
  
  # 
  # test <- predict(gppDays, newdata = catchSF, allow.new.levels = TRUE) # works
  
  # try to get the intervals following bolker
# What does it actually need? the formula object?
  
  # Example from https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#predictions-andor-confidence-or-prediction-intervals-on-predictions
  data("Orthodont",package="MEMSS")
  fm1 <- lmer(
    formula = distance ~ age*Sex + (age|Subject)
    , data = Orthodont
  )
  newdat <- expand.grid(
    age=c(8,10,12,14)
    , Sex=c("Female","Male")
    , distance = 0
  )
  newdat$distance <- predict(fm1,newdat,re.form=NA)
  mm <- model.matrix(terms(fm1),newdat)
  ## or newdat$distance <- mm %*% fixef(fm1)
  pvar1 <- diag(mm %*% tcrossprod(vcov(fm1),mm))
  tvar1 <- pvar1+VarCorr(fm1)$Subject[1]  ## must be adapted for more complex models
  cmult <- 2 ## could use 1.96
  newdat <- data.frame(
    newdat
    , plo = newdat$distance-cmult*sqrt(pvar1)
    , phi = newdat$distance+cmult*sqrt(pvar1)
    , tlo = newdat$distance-cmult*sqrt(tvar1)
    , thi = newdat$distance+cmult*sqrt(tvar1)
  )
  #plot confidence
  g0 <- ggplot(newdat, aes(x=age, y=distance, colour=Sex))+geom_point()
  g0 + geom_pointrange(aes(ymin = plo, ymax = phi))+
    labs(title="CI based on fixed-effects uncertainty ONLY")
  
  #plot prediction
  g0 + geom_pointrange(aes(ymin = tlo, ymax = thi))+
    labs(title="CI based on FE uncertainty + RE variance")
  
  

# try to translate --------------------------------------------------------
# gppDays = fm1
# catchSF = newdat
  # 
  
  # Get the predictions (really, as before)
    # sort out the allow.new.levels in a bit- will need the version with catchment
  preds <- predict(gppDays, newdata = catchSF, re.form = NA)
  # to more closely match add_preds do this with a bind- will need to deal with colu,nm naming
  catchSF2 <- bind_cols(catchSF, logGPP = preds)
  
  # Arrgh I'm losing nans but want to keep them as nans.
  # doesn't appear to be an easy argument to the function, so let's cut them off, do the predict, then glue them back on with Nan predictions
  
  # Probably most general to cut off the predicted Nans, because those will be the ones with nan anywhere in the predictor variables
  catchSFnonan <- filter(catchSF2, !is.nan(logGPP))
  catchSFnan <- filter(catchSF2, is.nan(logGPP)) # save to glue back

  mm2 <- model.matrix(terms(gppDays), catchSFnonan)
  
  pvar2 <- diag(mm2 %*% tcrossprod(vcov(gppDays),mm2))
  ## must be adapted for more complex models- this is the intercept-intercept #
  #cell of the varcorr matrix of the random effect Can I get it programaticaly
  #without having to know its name? 
  #
  #
  # but this is the variance from the random
  #effect, NOT the residual variance, and so this is still just a CI, accounting
  #for the random effect, I think. I need to add that as well to get prediction
  #intervals. but some of that is eaten by the random varianve. so ... what to
  #do
  tvar2 <- pvar2+VarCorr(gppDays)$wateryear[1]  
  cmult2 <- 1.96 ## could use 1.96
  
  # Translate the data.frame Ben uses to dplyr
  catchSFnonan2 <- catchSFnonan %>%
    mutate(plo = logGPP-cmult2*sqrt(pvar2),
           phi = logGPP+cmult2*sqrt(pvar2),
           tlo = logGPP-cmult2*sqrt(tvar2),
           thi = logGPP+cmult2*sqrt(tvar2)
  )
  
  ### GLUE THE NANS BACK ON
  
  
  #plot confidence
  g0 <- ggplot(catchSFnonan2, aes(x=tempC, y=logGPP, colour=daysAwayFromWaterYear))+geom_point()
  g0 + geom_pointrange(aes(ymin = plo, ymax = phi))+
    labs(title="CI based on fixed-effects uncertainty ONLY")
  
  #plot prediction
  g0 + geom_pointrange(aes(ymin = tlo, ymax = thi))+
    labs(title="CI based on FE uncertainty + RE variance")
  
  # I want to actually plot the data. Where is that?
  # I can do it on the back end, but for testing, I need jtp in tempProdRespRegressions
  
  gdata <- ggplot(jtp, aes(x=tempC, y=logGPP, colour=daysAwayFromWaterYear))+geom_point() + 
    geom_point(data = catchSFnonan2, aes(x=tempC, y=logGPP), color = 'black')
  gdata
  gdataCI <- gdata + 
    geom_pointrange(data = catchSFnonan2, 
                    aes(ymin = plo, ymax = phi), 
                    color = 'black')
  gdataCI
  gdataPI <- gdata + 
    geom_pointrange(data = catchSFnonan2, 
                    aes(ymin = tlo, ymax = thi), 
                    color = 'black')
  gdataPI
  
  # That sure doesn't look like a prediction interval. More like a CI accounting for random effet varianve. 
  
  gdataK <- ggplot(jtp, aes(x=tempC, y=logGPP, colour=daysAwayFromWaterYear)) +
    geom_density2d_filled(alpha = 0.5, contour_var = 'ndensity')
  gdataK # ok, so 90% is pretty easy to see there
  
  gdataK +
    geom_pointrange(data = catchSFnonan2, 
                          aes(ymin = tlo, ymax = thi), 
                          color = 'black')
    # That's pretty clearly nopt picking up the full spread
  
  # ARGH
  
  # So, looks like Ben has methods for lme and glmmTMB, so could try them
  # First, let's try merTools::predictInterval
  
  # does include.resid.var switvh between CI and PI>?
  testPI <- merTools::predictInterval(gppDays, catchSFnonan, level = 0.9)
  catchPItest <- bind_cols(catchSFnonan, testPI)

  # does that look right?
  gdataK +
    geom_point(data = catchPItest, aes(y = fit)) +
    geom_pointrange(data = catchPItest, 
                    aes(ymin = lwr, ymax = upr), 
                    color = 'black')
  # I think that's better, but let's flop the overlay
  gpi <- ggplot(catchPItest, aes(x = tempC, y = fit)) +
    geom_point(color = 'black') +
    geom_pointrange(aes(ymin = lwr, ymax = upr), 
                    color = 'black') +
    geom_density2d_filled(data = jtp, aes(x=tempC, y=logGPP), 
                          alpha = 0.5, contour_var = 'ndensity')
  gpi
  
  # Cool. It doesn't do the mean-var relationship, but that's because it's not complex enough model to
  
  # can I get the CI from this too?
  testPIC <- merTools::predictInterval(gppDays, catchSFnonan, level = 0.9, include.resid.var = FALSE)
  catchPItestC <- bind_cols(catchSFnonan, testPIC)
  
  gpiC <- ggplot(catchPItestC, aes(x = tempC, y = fit)) +
    geom_point(color = 'black') +
    geom_pointrange(aes(ymin = lwr, ymax = upr), 
                    color = 'black') +
    geom_density2d_filled(data = jtp, aes(x=tempC, y=logGPP), 
                          alpha = 0.5, contour_var = 'ndensity')
  gpiC
  
  # how does that compare to the homebrew I did above?
  gH <- ggplot(catchSFnonan2, aes(x=tempC, y=logGPP), color = 'black')+geom_point() 
  gH
  gdataPI <- gH + 
    geom_pointrange(data = catchSFnonan2, 
                    aes(ymin = tlo, ymax = thi), 
                    color = 'black')
  gHK <- gdataPI +
    geom_density2d_filled(data = jtp, aes(x=tempC, y=logGPP), 
                          alpha = 0.5, contour_var = 'ndensity')
  
  gHK
  
 ggpubr::ggarrange(gpiC, gHK)

 # pretty similar. Let's go wtih that as the way to do this. 
 
 # NOW, TO WRITE THE CODE.. Finally
 
 # back to old -------------------------------------------------------------

  
  
  # throw out all the extra variables, leaving just the predictions
  catchSF <- catchSF %>%
    select(time, starts_with('log'), -contains('pfit'), -contains('cfit'))
  # catchSF
  
  # Turn into stars
  # This version for all model predictions. might as well (unless the resulting object is too big)
  starpredictions <- catchSF %>% # select(time, logGPPdays) %>%
    st_as_stars() %>%
    # st_redimension is more flexible than merge.stars()
    st_redimension(new_dims = st_dimensions(subCatch))
  starpredictions[[1]] <- NULL
  
  
 # break up, but do that elsewhere- inside the loop is a mess to return
  return(starpredictions)
}
# # Sort out the dims
# a <- chunkpred(bottom = 1, top = 2)
# b <- chunkpred(bottom = 1, top = 2)
# c(a,b, along = 1)

# # Step 1: sort out an optimal chunksize
# benchChunk <- microbenchmark::microbenchmark("a2" = { b <- chunkpred(bottom = 1, top = 2)},
#                                              "a10" = { b <- chunkpred(bottom = 1, top = 10)},
#                                              "a100" = { b <- chunkpred(bottom = 1, top = 100)},
#                                              "a200" = { b <- chunkpred(bottom = 1, top = 200)},
#                                              "a1000" = { b <- chunkpred(bottom = 1, top = 1000)},
#                                            times = 1, unit = 's')
# # benchS_S
# print('chunk speeds')
# print(benchChunk)
# # benchChunk$mean
# 
# # on HPC, 2 was
# 245/2
# # 10 
# 1526/10
# # 100
# 15273/100
# # 200
# 29206/200
# # 1000 
# 142448/1000
# # I think I'll stop there

# so, the above are ms, in seconds that's 142/1000

# 142*27/60 minutes expected
# Try to fire one off for 4 hours and see how long it takes


# Testing for intervals and rearrange -------------------------------------

bottom <- 1
top <- 10
testpreds <- chunkpred(bottom, top)
testpreds

# step 2: modify the function to allow a bottom and top, then with the optimal
# chunksize, run and c() together as in the subchunk scripts
# Stolen from rastPolyJoin, need to modify the chunksizes, and confirm this makes sense

startbig <- proc.time()
ngeoms <- dim(weightedMeanTemps)['geometry']
nbreaks <- ceiling(ngeoms/chunksize) + 1
breaks <- round(seq(from = 0, to = ngeoms, length.out = nbreaks))
starpreds <- foreach(l = 1:(length(breaks)-1),
                     .combine=function(...) c(..., along = 1), # Pass dimension argument to c.stars
                     .multicombine=TRUE) %do% {
                       bottom <- breaks[l]+1
                       top <- breaks[l+1]
                       chunkpred(bottom, top) # NEED TO MODIFY CHUNKPRED
                     }

endbig <- proc.time()

print('Time taken for loop')
print(endbig-startbig)

## TODO IF COPYING FOR NEW VERSIONS:
# save the name of the outputs, instead of starpreds (see other sorts of scripts for the assign() code)
# SAVE THE INDICES sf- they aren't the same coming out of the processing script, which translates to here

### Break up into fit and intervals separately
gppdv <- starpreds %>%
  select(contains('logGPPdaysvalleys'))

gppd <- starpreds %>%
  select(contains('logGPPdays'), -contains('valleys'))

erdv <- starpreds %>%
  select(contains('logERdaysvalleys'))

erd <- starpreds %>%
  select(contains('logERdays'), -contains('valleys'))
# 3578 seconds total for bidgee (27000 anaes). Ends up just under 1GB. So that's not terrible

save(starpreds, file = file.path(scriptOut, paste0(thisCatch, '_predictedGPPER.rdata')))