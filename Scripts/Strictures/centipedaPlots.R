# Mapping centipeda



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
