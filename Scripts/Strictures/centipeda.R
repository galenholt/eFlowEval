centipedastricts <- function(smFile, tempFile, lippiaFile) {
  # Currently specifying the sort of file to read in to remind what's needed.
  # BUT, could just have a list of filenames, and read them all in whatever they
  # are
  
  # Read in soil moisture in ANAEs--------------------------------------------------
  
  load(file.path(datOut, smFile))
  
  # Read in soil temp in ANAEs --------------------------------------------
  
  load(file.path(datOut, tempFile))
  
  # Read in yearly Lippia success -------------------------------------------
  
  load(file.path("strictOut", lippiaFile))
  
  
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
  soilMoist_Min5 <- dailyPolySMavg # initialize
  
  system.time(soilMoist_Min5[[1]] <- timeRoll(soilMoist_Min5[[1]], 
                                              FUN = RcppRoll::roll_min, 
                                              rolln = 5, 
                                              align = 'right',
                                              na.rm = TRUE))
  
  # Stricture 2: Soil moisture needs to be maintained for a certain period of time to enable fruiting and seed-set
  # As above, no numbers for the moisture level or period of time.
  # let's say they die if soil moisture is < 10% in the preceding 6-week growing period
  
  # To meet the "dead if not moist > 10% for 6 weeks" condition, we need to check if min(last 6 weeks) is below 10
  
  # First, get the min soil moisture over the last 6 weeks as a rolling min
  soilMoist_Min42 <- dailyPolySMavg # initialize
  
  system.time(soilMoist_Min42[[1]] <- timeRoll(soilMoist_Min42[[1]], 
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
  
  # Put in C instead of Kelvin
  soilTemp <- dailyPolyTempavg - 273
  
  # So, let's say the stricture is < 4 out of last 28 days can have soil temp > 60
  soilTempG60_Max28 <- soilTemp # Intialize
  
  # Logical; ask if the day is > 60 (no rolling at this point)
  # This isn't done below in the stricture testing section, because it's not
  # actually testing the stricture (whihc is about number of days). It's
  # prepping the data to calculate number of days
  soilTempG60_Max28 <- soilTempG60_Max28 > 60
  
  # How many of the last 28 days are > 60 (rolling sum of the logicals)
  system.time(soilTempG60_Max28[[1]] <- timeRoll(soilTempG60_Max28[[1]], 
                                                 FUN = RcppRoll::roll_sum, 
                                                 rolln = 28, 
                                                 align = 'right',
                                                 na.rm = TRUE))
  # More data explore to make sure the stricture is reasonable
  # max(soilTempG60_Max28[[1]], na.rm = T)
  # sum(soilTempG60_Max28[[1]] > 5, na.rm = T)
  
  # The attribute needs a name for what it really is, not inherit from data
  names(soilTempG60_Max28) <- 'days'
  
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
  seed60_Centipeda <- soilTempG60_Max28 <= 4
  # The attribute needs a name for what it really is, not inherit from data
  names(seed60_Centipeda) <- 'passedStricts'
  
  # Germination requires inundation -----------------------------------------
  
  # To meet the "needs inundation (>80%) for 5 days" condition, we need to check if min(last 5 days) is below 80
  # 80 yields no germinaton in Cumbung. Ever. Bump it down to 50, see what happens
  germ_Centipeda <- soilMoist_Min5 > 0.2
  # The attribute needs a name for what it really is, not inherit from data
  names(germ_Centipeda) <- 'passedStricts'
  
  # Fruiting requires consistent moisture -----------------------------------
  
  # To meet the "dead if not moist > 10% for 6 weeks" condition, we need to check if min(last 6 weeks) is below 10
  fruit10_Centipeda <- soilMoist_Min42 > 0.1
  # The attribute needs a name for what it really is, not inherit from data
  names(fruit10_Centipeda) <- 'passedStricts'
  
  
  # ANAE classification -----------------------------------------------------
  
  # True/False. Could also be a which() if we want index numbers
  isANAE_Centipeda <- lachAll$ANAE_CODE %in% centipANAE
  
  
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
  seedGerm_Centipeda <- seed60_Centipeda & germ_Centipeda
  
  # The attribute needs a name for what it really is, not inherit from data
  names(seedGerm_Centipeda) <- 'passedStricts'
  
  # Fruiting requires germination within the last ?? time period and soil moisture condition
  # there are a few ways to do this; I'll demo them here
  # use seedGerm_Centipeda to keep going with the lice-cycle logic from the start
  
  # 1. Simplest: germination and soil moisture check over the same time span: ie
  # has it germinated in last 6 weeks, and has soil moist been above 10%?
  # This is crude, since germ could have happened day before, for ex
  
  # 2 is better, but this feeds into it
  # To get germ in last 6 weeks, roll whether germination happened in prev. 6 weeks as the sum of days with germ
  seedGerm_Centipeda_Sum42 <- seedGerm_Centipeda # initialize
  
  system.time(seedGerm_Centipeda_Sum42[[1]] <- timeRoll(seedGerm_Centipeda[[1]],
                                                        FUN = RcppRoll::roll_sum,
                                                        rolln = 42,
                                                        align = 'right',
                                                        na.rm = TRUE))
  # Then the stricture test is whether there was germ and soil moist
  # Crap name, probably don't use. in fact, comment out
  # seedGermFruitconnect_Centipeda <- (seedGerm_Centipeda_Sum42 > 0) & fruit10_Centipeda
  
  # The attribute needs a name for what it really is, not inherit from data
  names(seedGerm_Centipeda_Sum42) <- 'days'
  
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
  seedGerm_Centipeda_Sum90 <- seedGerm_Centipeda # initialize
  
  system.time(seedGerm_Centipeda_Sum90[[1]] <- timeRoll(seedGerm_Centipeda[[1]], 
                                                        FUN = RcppRoll::roll_sum, 
                                                        rolln = 90, 
                                                        align = 'right',
                                                        na.rm = TRUE))
  # The attribute needs a name for what it really is, not inherit from data
  names(seedGerm_Centipeda_Sum90) <- 'days'
  
  # Then, the stricture test is whether there was germ in the interval, followed by soil moisture
  # Subtraction gets the number of germ days in the interval from 90 days ago to 42
  seedGermFruit_Centipeda <- ((seedGerm_Centipeda_Sum90 - seedGerm_Centipeda_Sum42) > 0) & fruit10_Centipeda
  
  # The attribute needs a name for what it really is, not inherit from data
  names(seedGermFruit_Centipeda) <- 'passedStricts'
  
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
  
  # But maybe we could do a which() on seedGerm_Centipeda_Sum90m42 to get when germ happened in the
  # window, and if we did max(which()), it'd give us the last germ in the window.
  # Then we'd just need to do the min check on the window. This is likely all
  # more expensive than the time roll, but maybe not too bad? Will just have to try
  # Might be able to build it on cumsum: https://stackoverflow.com/questions/54570330/counting-number-of-rows-since-last-observation-that-fulfills-condition
  # And maybe cummin, starting over at germ date. That SHOULD be pretty fast, I think.
  
  
  
  # And, restrict to the required ANAE --------------------------------------
  
  # Just the individual and the whole, don't look at other subsets for now
  fullCycleANAE_Centipeda <- seedGermFruit_Centipeda & isANAE_Centipeda
  seedANAE_Centipeda <- seed60_Centipeda*isANAE_Centipeda
  germANAE_Centipeda <- germ_Centipeda*isANAE_Centipeda
  fruitANAE_Centipeda <- fruit10_Centipeda*isANAE_Centipeda
  
  
  # Failure if Lippia made it through in the preceding year -----------------
  
  # COULD do this earlier and short-circuit everything, but like ANAE, it will
  # be good to look at the outcomes with and without
  
  # This would be relatively easy to do below where the centipeda is put on the
  # same timescale, then could just loop with t-1, or add a sheet or something.
  # BUT, being able to get it on the daily data will be useful because then it
  # will be available in a more interactive type setup; ie adding it at the end
  # after summarizing years means it really only works as a summarization to
  # present, not as a model of process. While we aren't using it here as a
  # process, really (nothing then depends on it), doing it this way allows that
  # to happen
  # And the function I wrote is fairly generic, so we can do things like monthly
  # etc, as indicated by the biology
  
  # For now, just do it for the final
  # Not worth doing the lippia alone, because that's just !fullCycleANAE_Lippia, so if we want that, just go get it
  
  # Invert the logic on the lippia, since it is whether it existed, and we want the centip stricture to pass if lippia WASN'T there
  fullCycleLippia_Centipeda <- unevenTimeMult(fineStars = fullCycleANAE_Centipeda, coarseStars = fullYr_Lippia, 
                                              lag = 1, invertCoarseLogic = TRUE)
  
  
  # Save those as a list, along with some of the constituents
  # this is fairly redundant. Do I actually want to do this? Or should I save
  # only the bits that other parts are built on?
  # Decide later. It's useful to have the ultimate thing as an output, I think
  # maybe the answer is to have the final output with everything, and then the
  # base, but skip stuff in the middle. Except, some of the stuff in the middle
  # is complex (ie no seeds unless germ wtihin some period)
  
  centipedastricts <- tibble::lst(fullCycleLippia_Centipeda, 
                               seed60_Centipeda, 
                               germ_Centipeda, 
                               fruit10_Centipeda, 
                               seedGerm_Centipeda,
                               seedGermFruit_Centipeda,
                               isANAE_Centipeda)
  
  return(centipedastricts)
  
}
 
 
