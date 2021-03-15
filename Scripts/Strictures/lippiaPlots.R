# Plots and maps for lippia


# Spatial aggregation at the catchment ------------------------------------

# Independent ---------------------------------------------------------------

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
