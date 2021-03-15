# analysis of centipeda outputs
# -------------------------------------------------------------------------


# Summary outputs ---------------------------------------------------------


# -------------------------------------------------------------------------

# Processing can occur at small time steps where possible, but presentation will
# necessarily need to be summarized over both time and space
# There are a few different ways to do that, so I'll demo a few

# Some things I'll need

# Water year breakpoints
# TBD for now
# See `by` in ?aggregate.stars
# Left-closed, right open time intervals
# or cut.Posix

# Calendar years can facet on time itslef in the plotting. No idea why
# by_t <- 'years' # calendar years

# let's just pick some cut points manually to see how this works, then maybe
# lubridate to make them programatically (e.g. every april 30 or whatever)
startdate <- min(st_get_dimension_values(centipeda_base$fullCycleLippia_Centipeda, which = 'time'))
enddate <-  max(st_get_dimension_values(centipeda_base$fullCycleLippia_Centipeda, which = 'time'))

interDates <- as.POSIXct(c("2014-06-30", "2015-06-30", "2016-06-30", "2017-06-30", "2018-06-30", "2019-06-30"))
# intervals
# this works, but it causes the scale_id error in ggplot (see facet_scale_id_error.R)
# That's fixable by faceting by as.character(time). Do we want to do that? Maybe? Should be OK, I think?
by_t <- c(startdate, interDates,  enddate)

# Independent strictures --------------------------------------------------

# Proportion of days each stricture was met each year
seedYr_Centipeda <- tempaggregate(centipeda_base$seed60_Centipeda, by = by_t, FUN = propor, na.rm = TRUE)
# test the prop makes sense (did my fun work?)
names(seedYr_Centipeda) <- 'propDaysPassed'

# seedYrS <- tempaggregate(seed60, by = by_t, FUN = sum, na.rm = TRUE)

germYr_Centipeda <- tempaggregate(centipeda_base$germ_Centipeda, by = by_t, FUN = propor, na.rm = TRUE)
names(germYr_Centipeda) <- 'propDaysPassed'

fruitYr_Centipeda <- tempaggregate(centipeda_base$fruit10_Centipeda, by = by_t, FUN = propor, na.rm = TRUE)
names(fruitYr_Centipeda) <- 'propDaysPassed'
# 


# Dependent strictures ----------------------------------------------------

# So, if we go dependent, is there much reason to do subsets of it? ie, got from survival to germ, but not fruiting? Or just do it for the final step?
# Suppose there might be a point to the intermediates. Make them, I guess
# but not going to go with/without ANAE, or too many options. Since conditional, use wtih ANAE

# survival already above as independent
seedGermYrANAE_Centipeda <- tempaggregate(centipeda_base$seedGerm_Centipeda*centipeda_base$isANAE_Centipeda, 
                                          by = by_t, FUN = propor, na.rm = TRUE)
names(seedGermYrANAE_Centipeda) <- 'propDaysPassed'

fullYr_Centipeda <- tempaggregate((centipeda_base$seedGermFruit_Centipeda & centipeda_base$isANAE_Centipeda), 
                                  by = by_t, FUN = propor, na.rm = TRUE)
names(fullYr_Centipeda) <- 'propDaysPassed'

fullYrLippia_Centipeda <- tempaggregate(centipeda_base$fullCycleLippia_Centipeda, 
                                        by = by_t, FUN = propor, na.rm = TRUE)
names(fullYrLippia_Centipeda) <- 'propDaysPassed'

# how to present any of this? Could make a map, but lotsa white space. Still, might be fine, depending on the goals
# Can easily do the zoom in to a bounding box thing if there are areas of interest
# -------------------------------------------------------------------------


# Spatial aggregation at the catchment ------------------------------------


# ------------------------------------------------------------------------


# Summing/averaging proportions is kind of weird. So maybe as a first pass, go with the areal sum of success (ie area-days of passing)
# Easy to do this differently later if we want

# Work our way up again

# All the t(t(array)) stuff is hacky and should be able to be cleaned up wiht a bit more time with the help files

# -------------------------------------------------------------------------


# Independent strictures --------------------------------------------------


# -------------------------------------------------------------------------


# Seed bank survival ------------------------------------------------------
seedCatch_Centipeda <- catchAggW(strict = seedYr_Centipeda, strictWeights = lachArea, FUN = sum, summaryPoly = lachOnly)
names(seedCatch_Centipeda) <- 'areaDaysPassed'

seedPlot_Centipeda <- catchAggPlot(seedCatch_Centipeda, title = 'Seed Surv Centipeda')
seedPlot_Centipeda

# Germination -------------------------------------------------------------

germCatch_Centipeda <- catchAggW(strict = germYr_Centipeda, strictWeights = lachArea, FUN = sum, summaryPoly = lachOnly)
names(germCatch_Centipeda) <- 'areaDaysPassed'

germPlot_Centipeda <- catchAggPlot(germCatch_Centipeda, title = 'Germ Centipeda')
germPlot_Centipeda

# Fruiting ----------------------------------------------------------------

fruitCatch_Centipeda <- catchAggW(strict = fruitYr_Centipeda, strictWeights = lachArea, FUN = sum, summaryPoly = lachOnly)
names(fruitCatch_Centipeda) <- 'areaDaysPassed'
fruitPlot_Centipeda <- catchAggPlot(fruitCatch_Centipeda, title = 'Fruiting Centipeda')
fruitPlot_Centipeda

# -------------------------------------------------------------------------


# Dependent ---------------------------------------------------------------


# -------------------------------------------------------------------------


# survival and germination ------------------------------------------------

seedGermCatch_Centipeda <- catchAggW(strict = seedGermYrANAE_Centipeda, strictWeights = lachArea, FUN = sum, summaryPoly = lachOnly)
names(seedGermCatch_Centipeda) <- 'areaDaysPassed'
seedGermPlot_Centipeda <- catchAggPlot(seedGermCatch_Centipeda, title = 'Seed Surv + Germ Centipeda')
seedGermPlot_Centipeda

# Full cycle, no lippia --------------------------------------------------------------

fullCatch_Centipeda <- catchAggW(strict = fullYr_Centipeda, strictWeights = lachArea, 
                                 FUN = sum, summaryPoly = lachOnly)
names(fullCatch_Centipeda) <- 'areaDaysPassed'
fullPlot_Centipeda <- catchAggPlot(fullCatch_Centipeda, title = 'Full Life Cycle Success')
fullPlot_Centipeda


# FullCycle, including lippia ---------------------------------------------

fullCatchLippia_Centipeda <- catchAggW(strict = fullYrLippia_Centipeda, strictWeights = lachArea, FUN = sum, summaryPoly = lachOnly)
names(fullCatchLippia_Centipeda) <- 'areaDaysPassed'
fullPlotLippia_Centipeda <- catchAggPlot(fullCatchLippia_Centipeda, title = 'Full Cycle Centipeda (dependent on Lippia)')
fullPlotLippia_Centipeda
