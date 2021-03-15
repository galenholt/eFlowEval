# Lippia output summary and figures

# -------------------------------------------------------------------------


# Summary outputs ---------------------------------------------------------


# -------------------------------------------------------------------------


# Set up time summary breakpoints
# Calendar years, months, other things set with character vectors can facet on
# time itself in the plotting, but giving it dates can't. No idea why
# by_t <- 'years' # calendar years

# let's just pick some cut points manually to see how this works, then maybe
# lubridate to make them programatically (e.g. every april 30 or whatever)

startdate <- min(st_get_dimension_values(lippia_base$fullCycle_Lippia, which = 'time'))
enddate <-  max(st_get_dimension_values(lippia_base$fullCycle_Lippia, which = 'time'))

interDates <- as.POSIXct(c("2014-06-30", "2015-06-30", "2016-06-30", "2017-06-30", "2018-06-30", "2019-06-30"))


# Old, to handle differences between sm and temp
# startdate <- min(min(st_get_dimension_values(dailyPolySMavg, which = 'time')),  
#                  min(st_get_dimension_values(dailyPolyTempavg, which = 'time')))
# enddate <-  max(max(st_get_dimension_values(dailyPolySMavg, which = 'time')),  
#                 max(st_get_dimension_values(dailyPolyTempavg, which = 'time')))
# 

# intervals
# this works, but it causes the scale_id error in ggplot (see facet_scale_id_error.R)
# That's fixable by faceting by as.character(time). Do we want to do that? Maybe? Should be OK, I think?
by_t <- c(startdate, interDates,  enddate)

# Independent strictures --------------------------------------------------

# Proportion of days each stricture was met each year
germYr_Lippia <- tempaggregate(lippia_base$germ2535_Lippia, by = by_t, FUN = propor, na.rm = TRUE)
# Would be nice if temaggregate could do the rename internally dependent on FUN
names(germYr_Lippia) <- 'propDaysPassed'

survYr_Lippia <- tempaggregate(lippia_base$surv4_Lippia, by = by_t, FUN = propor, na.rm = TRUE)
names(survYr_Lippia) <- 'propDaysPassed'

# Dependent strictures ----------------------------------------------------

# Just do the full thing, since there aren't really mulitple steps here
fullYr_Lippia <- tempaggregate(lippia_base$fullCycle_Lippia, by = by_t, FUN = propor, na.rm = TRUE)
names(fullYr_Lippia) <- 'propDaysPassed'
# Let's spit this out and save it, so centipeda can read JUST this in, and not
# have to source() this whole file

save(fullYr_Lippia, file = file.path('strictOut', 'fullYr_Lippia_base.rdata'))

# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


# Spatial aggregation at the catchment ------------------------------------


# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


# Independent ---------------------------------------------------------------


# -------------------------------------------------------------------------


# Germination  ------------------------------------------------------
germCatch_Lippia <- catchAggW(strict = germYr_Lippia, strictWeights = lachArea, FUN = sum, summaryPoly = lachOnly)
names(germCatch_Lippia) <- 'areaDaysPassed'

germPlot_Lippia <- catchAggPlot(germCatch_Lippia, title = 'Germ Lippia', as_sf = FALSE)
germPlot_Lippia


# Survival ----------------------------------------------------------------
survCatch_Lippia <- catchAggW(strict = survYr_Lippia, strictWeights = lachArea, FUN = sum, summaryPoly = lachOnly)
names(survCatch_Lippia) <- 'areaDaysPassed'

survPlot_Lippia <- catchAggPlot(survCatch_Lippia, varname = 'areaDaysPassed', title = 'Adult Survival Lippia', as_sf = TRUE)
survPlot_Lippia # + labs(fill = "Survival")

# -------------------------------------------------------------------------


# Dependent (full cycle) --------------------------------------------------


# -------------------------------------------------------------------------

fullCatch_Lippia <- catchAggW(strict = fullYr_Lippia, strictWeights = lachArea, FUN = sum, summaryPoly = lachOnly)
names(fullCatch_Lippia) <- 'areaDaysPassed'

fullPlot_Lippia <- catchAggPlot(fullCatch_Lippia, title = 'Full Cycle Lippia', as_sf = FALSE)
fullPlot_Lippia # + labs(fill = "Germ &\nSurvival")

