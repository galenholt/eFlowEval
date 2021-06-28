# lippia strictures

lippiastricts <- function(smFile, tempFile) {
  
  # Currently specifying the sort of file to read in to remind what's needed.
  # BUT, could just have a list of filenames, and read them all in whatever they
  # are
  
  # Read in soil moisture in ANAEs--------------------------------------------------
  
  load(file.path(datOut, 'ANAEprocessed', smFile))
  
  # Read in soil temp in ANAEs --------------------------------------------
  
  load(file.path(datOut, tempFile))
  # 
  
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
  # The attribute needs a name for what it really is, not inherit from data
  names(germ2535_Lippia) <- 'passedStricts'
  # These multi-checks mean it does 3 logical tests. If need be later on, could
  # speed up with Rcpp. It's available in dplyr, but doesn't seem to work on soilTemp because it ses a list, not a vector
  # Can get it to work on the [[1]] array, but not sure it's worth it, at least for now
  # system.time(germ2535_Lippia[[1]] <- between(soilTemp[[1]], 25, 35))
  
  # https://stackoverflow.com/questions/34519811/what-is-the-fastest-way-to-perform-multiple-logical-comparisons-in-r
  
  
  # Die if inundated > 4 weeks ----------------------------------------------
  
  # Is the minumum soil moisture over the preceding 4 weeks less than 80%, indicating drying occurred, and so survival?
  surv4_Lippia <- soilMoist_Min28 < 0.8
  # The attribute needs a name for what it really is, not inherit from data
  names(surv4_Lippia) <-'passedStricts'
  
  # ANAE classification -----------------------------------------------------
  
  # True/False. Could also be a which() if we want index numbers
  isANAE_Lippia <- LachlanANAE$ANAE_CODE %in% lippiaANAE
  
  
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
  # The attribute needs a name for what it really is, not inherit from data
  names(germSurv_Lippia) <- 'passedStricts'
  
  # -------------------------------------------------------------------------
  
  
  # Restrict all to ANAE for final strictures -------------------------------
  
  
  # -------------------------------------------------------------------------
  
  germ_Lippia <- germ2535_Lippia * isANAE_Lippia
  surv_Lippia <- surv4_Lippia * isANAE_Lippia
  fullCycle_Lippia <- germSurv_Lippia * isANAE_Lippia
  
  # Not renaming these, they are still whether there was germ, surv, or both. just now that depends on anae
  
  # Save those as a list, along with some of the constituents
  # this is fairly redundant. Do I actually want to do this? Or should I save
  # germ2535, surv4 and isANAE, and calculate the finals elsewhere?
  # Decide later. It's useful to have the ultimate thing as an output, I think
  # maybe the answer is to have the final output with everything, and then the
  # base, but skip stuff in the middle, because we can always re-calc that as needed
  lippiastricts <- tibble::lst(fullCycle_Lippia, 
                               germ2535_Lippia,
                               surv4_Lippia, 
                               isANAE_Lippia)
  
  return(lippiastricts)
}


