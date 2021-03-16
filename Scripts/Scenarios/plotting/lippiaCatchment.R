# Plots and maps for lippia

# Currently, these expect the strictures to be run and held in memory

# Spatial aggregation at the catchment ------------------------------------

# Independent ---------------------------------------------------------------

# Germination  ------------------------------------------------------
germCatch_Lippia <- catchAggW(strict = lippia_baseYr$germ2535_Lippia_yr, 
                              strictWeights = lachArea, 
                              FUN = sum, summaryPoly = lachOnly)
names(germCatch_Lippia) <- 'areaDaysPassed'

germPlot_Lippia <- catchAggPlot(germCatch_Lippia, title = 'Germ Lippia', as_sf = FALSE)
germPlot_Lippia


# Survival ----------------------------------------------------------------
survCatch_Lippia <- catchAggW(strict = lippia_baseYr$surv4_Lippia_yr, strictWeights = lachArea, FUN = sum, summaryPoly = lachOnly)
names(survCatch_Lippia) <- 'areaDaysPassed'

survPlot_Lippia <- catchAggPlot(survCatch_Lippia, varname = 'areaDaysPassed', title = 'Adult Survival Lippia', as_sf = TRUE)
survPlot_Lippia # + labs(fill = "Survival")

# -------------------------------------------------------------------------


# Dependent (full cycle) --------------------------------------------------


# -------------------------------------------------------------------------

fullCatch_Lippia <- catchAggW(strict = lippia_baseYr$fullCycle_Lippia_yr, strictWeights = lachArea, FUN = sum, summaryPoly = lachOnly)
names(fullCatch_Lippia) <- 'areaDaysPassed'

fullPlot_Lippia <- catchAggPlot(fullCatch_Lippia, title = 'Full Cycle Lippia', as_sf = FALSE)
fullPlot_Lippia # + labs(fill = "Germ &\nSurvival")
