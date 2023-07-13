# Royal Spoonbill Strictures.

# Header from the temperature file to retain all the directories,  --------
source('directorySet.R')

# Let's get libraries here, then sort out git then sort out making this a
# library so we don't have to deal with all the library crap


library(Rcpp)
library(RcppRoll)
library(stars)
library(sf)
library(tidyverse)


# Setup -------------------------------------------------------------------

# Make a sub-directory for the subchunk
scriptOut <- file.path(datOut, 'Strictures', 'Spoonbill')
if (!dir.exists(scriptOut)) {dir.create(scriptOut, recursive = TRUE)}

# chunksize in n anae polygons
chunksize <- 1000

# I htink rather than a slurm array, pass a catchment name?
# could do either, really. Sort out arg orders.
# arraynum <- as.numeric(args[8])
# Going to be easiest I think to base on names rather than arraynums and name indices.
thisCatch <-  "EdwardWakool" #'Avoca' #args[7] # 'Murrumbidgee' # For testing- needs to be grabbed from catchNames in a loop

print(thisCatch)
# stop('testing end here to make sure passing catchment name')


# preliminaries --------------------------------------------------

# Read in data ------------------------------------------------------------

# read in Ramsar boundaries
# load(file.path(scriptOut, "ramsarMDB.rdata"))
load(file.path(datOut, 'WetlandBoundaries', "ramsarBoundsMDB.rdata"))
# ramsarBoundsMDB <- sf::st_as_sf(ramsarBoundsMDB)
# ramsarBoundsMDB <- st_transform(ramsarBoundsMDB, whichcrs)



# ANAE 'preferences' ranked from most to least times observed
# McGinness, Langston and Brooks (2020) VEWH Prioritisation Project: Stage 2 Final Report
# Royal Spoonbill (Platalea regia) requirements, distribution and habitat mapping
breedANAEcodes <- read.table(
  file.path(datDir, "dataStrict/BreedingANAERoyalSpoonBill.txt"), 
  skip = 2, sep = ":", header = TRUE)
forageANAEcodes <- read.table(
  file.path(datDir, "dataStrict/ForagingANAERoyalSpoonBill.txt"), 
  skip = 2, sep = ":", header = TRUE)


## Areas of each ANAE meeting depth stricture
# Need to wrap this over catchments
depthInBreed <- file.path(datOut, 'Inundationprocessed', 'areaSpoonbillBreed')
depthInForage <- file.path(datOut, 'Inundationprocessed', 'areaSpoonbillForage')
catchNames <- list.files(file.path(depthInBreed))# Get the catchment names from the folders
# catchNames <- sub("\\_.x", "", catchNames)
catchNames <- substr(catchNames, 1,regexpr("\\_", catchNames)-1 )

catchBreedFiles <- list.files(depthInBreed, pattern = '.rdata')
catchForageFiles <- list.files(depthInForage, pattern = '.rdata')



load(file = file.path(depthInBreed, catchBreedFiles[grepl(thisCatch, catchBreedFiles)])) 
# whichcrs <- st_crs(EdwardWakool_areaSpoonbillBreed) #GDA94  EPSG:3577
load(file = file.path(depthInForage, catchForageFiles[grepl(thisCatch, catchForageFiles)]))

# load ANAE for ANAE code matching on veg type for depth stars 
# anae 

load(file.path(datOut, 'ANAEprocessed', paste0(thisCatch,  'ANAE.rdata'))) #GDA94   4283 
# load("C:/Users/amacq/Source/datOut/ANAEprocessed/EdwardWakoolANAE.rdata")
EdwardWakoolANAE <- st_transform(EdwardWakoolANAE, 3577)

# Inundation for Breeding stricture ---------------------------------------

# Areas of each ANAE meeting Breeding depth stricture. Bimonthly timestep 
# No point having one anae meeting depth strictures is it is not big enough
# to support a colony.


# Identidy TRUE/FALSE which polys have breed habitat
breedANAE <-(EdwardWakoolANAE$ANAE_CODE %in% breedANAEcodes$ANAE_CODE) #class logical i.e. no index, geometry
# breedANAEind <- which(EdwardWakoolANAE$ANAE_CODE %in% breedANAEcodes$ANAE_CODE)


# check and match index of depth stars to EdwardWakoolANAE
EdwardWakool_areaSpoonbillBreed <-  matchStarsIndex(index1 = EdwardWakoolANAE, stars1 = NULL, index2 = EdwardWakool_areaSpoonbillBreed_index, 
                stars2 = EdwardWakool_areaSpoonbillBreed, testfinal = FALSE)
# unpack list
breedDepth_index <- EdwardWakool_areaSpoonbillBreed[[1]]  # TODO this needs to be made generic
breedDepth <- EdwardWakool_areaSpoonbillBreed[[2]]
names(breedDepth) <- "Area"

# double-check index match 
sum(breedDepth_index$UID==EdwardWakoolANAE$UID)
length(EdwardWakoolANAE$UID)

# turn non-breeding-suitable polys to zero, other values still areas
breedANAEstrict <- breedDepth*breedANAE


# limit to wetland boundaries

ramsarTF <- st_covered_by(((breedANAEstrict)), 
                                   (st_buffer(ramsarBoundsMDB,250))) %>% lengths > 0

ramsarStrict <- breedANAEstrict*ramsarTF

dim(breedANAEstrict) - dim(ramsarStrict) # 0 
# constraining to ramsar makes no difference for EdwardWakool

# polygons wet for three consecutive bimonths in breeding season. 

timeStrict <- ramsarStrict >0  #  areas over 0 = TRUE


timeStrict[[1]] <- t(RcppRoll::roll_sum(t(timeStrict[[1]]), n= 3, align = "right", fill = c(NA, NA, NA)))

timeStrict[[1]] <- timeStrict$Area == 3

seasonStrict <- timeStrict

times <- st_get_dimension_values(seasonStrict, "time")

#  for southern basin there is only one 6month period so the 'time' must be march. 
# TODO for wetlands north of macquarie marshes check dates are within breeding season. lubridate interval?
breedTimes <- which(lubridate::month(times)== 03)

seasonStrict[[1]] <-0
seasonStrict[[1]][,breedTimes]<-1 

seasonStrict <- seasonStrict*timeStrict #  Times and polygon combinations where 3 bimonthly and season strictures are TRUE

# get max inundation for each 6 month (3 x bimonthly period)
breedDepth[[1]] <- t(RcppRoll::roll_max(t(breedDepth[[1]]), n= 3, align = "right", fill = c(NA, NA, NA)))

breedStrict <- breedDepth*seasonStrict #max area of inundation across 3 bimos in the breeding season 
# TODO will get rid of intermediate step objects on after testing. 


# sum area up to wetland complex scale

# TODO: redo ramsarBound and leave in valley tag so I can filter

LTIM_Valleys <- read_sf(dsn = file.path(datDir, 'ANAE/MDB_ANAE_Aug2017/MDB_ANAE.gdb'), layer = 'LTIM_Valleys') %>%
  st_cast("MULTIPOLYGON") %>% # cleans up an issue with multisurfaces
  st_make_valid()
LTIM_Valleys <- st_transform(LTIM_Valleys, 3577)

load(file.path(datOut, 'WetlandBoundaries', "ramsarBoundsMDB.rdata"))

ramsarBoundEdwa <- st_intersection(ramsarBoundsMDB, filter(LTIM_Valleys, ValleyName =="Edward Wakool"))

f <- function(x){sum(x, na.rm = TRUE)}
breedAreaByWetland <- aggregate(breedStrict, ramsarBoundEdwa,f )


# test against minimum total area inundated threshold.


# calculate historical max area of inundation 

by_t <- dim(breedAreaByWetland)["time"]*2 #number of months in dataset
maxWetlandInunArea <- aggregate(breedAreaByWetland, by = paste(by_t,"months"), max, na.rm = TRUE ) %>%
  aperm(c('geometry', 'time'))

areaPC <- 0.7 # proportion of max inundation as threshold for breeding

areaStricture <- breedAreaByWetland

areaStricture[[1]] <- breedAreaByWetland[[1]] > 
  areaPC*matrix(rep(maxWetlandInunArea[[1]], 
                    dim(breedAreaByWetland)[2]), 
                ncol = dim(breedAreaByWetland)[2])
dim(areaStricture)
sum(areaStricture$Area)

numBreedEvents <- st_as_sf(areaStricture)
numBreedEvents <- numBreedEvents %>% st_drop_geometry()
numBreedEvents <- numBreedEvents %>% pivot_longer(names_to = "Date", values_to = "Breed", cols = everything())
numBreedEvents <- numBreedEvents %>%  group_by(Date)%>% summarise(NumBreed = sum(Breed)) %>% ungroup()


# --- foraging ----

# Identify TRUE/FALSE which polys have breed habitat
forageANAE <-(EdwardWakoolANAE$ANAE_CODE %in% forageANAEcodes$ANAE_CODE) #class logical i.e. no index, geometry
# forageANAEind <- which(EdwardWakoolANAE$ANAE_CODE %in% forageANAEcodes$ANAE_CODE)


# check and match index of depth stars to EdwardWakoolANAE
EdwardWakool_areaSpoonbillForage <-  
  matchStarsIndex(index1 = EdwardWakoolANAE, stars1 = NULL, index2 = EdwardWakool_areaSpoonbillForage_index, 
                                                    stars2 = EdwardWakool_areaSpoonbillForage, testfinal = FALSE)
# unpack list
forageDepth_index <- EdwardWakool_areaSpoonbillForage[[1]]  # TODO this needs to be made generic
forageDepth <- EdwardWakool_areaSpoonbillForage[[2]]
names(forageDepth) <- "Area"

# double-check index match 
sum(forageDepth_index$UID==EdwardWakoolANAE$UID)
length(EdwardWakoolANAE$UID)

# turn non-foraging-suitable polys to zero, other values still areas
forageANAEstrict <- forageDepth*forageANAE

sum(forageANAEstrict$Area) == sum(forageDepth$Area) #T: no reduction in areas??


# aggregate into ramsarbounds

# forage area available in each wetland (ramsar) boundary over time
forageAreaByWetland <- aggregate(forageANAEstrict, ramsarBoundEdwa, sum, na.rm = TRUE )


forageAreaByWetlandBuffer <- aggregate(forageANAEstrict, st_buffer(ramsarBoundEdwa,10000), sum, na.rm = TRUE )

sum(forageAreaByWetlandBuffer$Area) - sum(forageAreaByWetland$Area) # 198468638


# could calculate the maximum area of forage and use a % threshold like breeding?

# for now just plot


# # For plot put Area from buffered area into original geometry
# 
# forageAreaBufferPlot <- forageAreaByWetland
# forageAreaBufferPlot[[1]] <- forageAreaByWetlandBuffer[[1]]
# 
# 
# forageAreaBufferPlot <- aggregate(forageAreaBufferPlot, "year", sum, na.rm = TRUE)
# # for some reason this changes the order of dimensions 
# forageAreaBufferPlot <- aperm(forageAreaBufferPlot, c(2,1))

# calculate historical max area of inundation matching foraging habitat

by_t <- dim(forageAreaByWetland)["time"]*2 #number of months in dataset
maxWetlandForageArea <- aggregate(forageAreaByWetland, 
                                  by = paste(by_t,"months"), max, na.rm = TRUE ) %>%
  aperm(c('geometry', 'time'))

forageAreaPC <- 0.05 # proportion of max inundation as threshold for foraging

forageAreaStricture <- forageAreaByWetland
forageAreaStricture[[1]] <- forageAreaByWetland[[1]] > 
  forageAreaPC*matrix(rep(maxWetlandInunArea[[1]], 
                    dim(breedAreaByWetland)[2]), 
                ncol = dim(breedAreaByWetland)[2])
# forageAreaStricture[[1]] <- forageAreaByWetland > forageAreaPC*maxWetlandInunArea
dim(forageAreaStricture)
sum(forageAreaStricture$Area) #125

dim(areaStricture)
sum(areaStricture$Area) #56
# so seems like forage stricture will remove some breeding opportunities


# combine breeding and foraging strictures

forageBreedStricture <- areaStricture*forageAreaStricture
sum(forageBreedStricture$Area) #
# was zero so none of the time x polygon combinations line up??
# whoops was using wrong PC mulitplier but still only get 8 making it through?


ggplot()+
  geom_stars(data = areaStricture[,,194]) # 2 true

ggplot()+
  geom_stars(data = forageAreaStricture[,,194]) # all false


# The objects needed for the plots. should be cleaned up into a list and saved as RDS
save(breedStrict, forageAreaByWetland, forageAreaByWetlandBuffer, 
     breedAreaByWetland, numBreedEvents, forageANAEstrict, breedANAEstrict,
     breedDepth, areaStricture, forageBreedStricture,
     file = file.path(scriptOut, 'spoonbill_strictures.rdata'))

# Next steps
# 
# • Tidy up code
# • Make sure main script runs properly.  
# • Save data tables for use in graphics
# • Move plotting/testing from wetlandBoundaries
# 
# • Turn main script into a function(s)
#   • Double-check outputs
# ○ E.g. Ordering of indicies confused?
#   • Address possible mismatch between forage = bimonthly ,or summed to year and Breed  = march
# • 5% seems like a low threshold value, maybe max() isn't the right metric.
# 	• Add functionality for Northern basin i.e. Wider breeding season.
# 	• Solution for matching wetland names to geometry ramsarLabels in W
# 	• Analyse breeding data for actual thresholds or sensitivity analysis
# 	• Check ramsar bounds works for other catchments 






















