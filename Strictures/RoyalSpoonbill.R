# Royal Spoonbill Strictures.

# Header from the temperature file to retain all the directories,  --------
source('directorySet.R')

# Let's get libraries here, then sort out git then sort out making this a
# library so we don't have to deal with all the library crap

library(Rcpp)
library(RcppRoll)

registerDoFuture()
plan(multicore)


# Setup -------------------------------------------------------------------

# Make a sub-directory for the subchunk
scriptOut <- file.path(datOut, 'Spoonbill', 'Breed')
if (!dir.exists(scriptOut)) {dir.create(scriptOut, recursive = TRUE)}

# chunksize in n anae polygons
chunksize <- 1000

# I htink rather than a slurm array, pass a catchment name?
# could do either, really. Sort out arg orders.
# arraynum <- as.numeric(args[8])
# Going to be easiest I think to base on names rather than arraynums and name indices.
thisCatch <-  'Avoca' #args[7] # 'Murrumbidgee' # For testing- needs to be grabbed from catchNames in a loop

print(thisCatch)

# stop('testing end here to make sure passing catchment name')


# preliminaries --------------------------------------------------

myhome <- paste0('C:/Users/', Sys.getenv("USERNAME"))


# Read in data ------------------------------------------------------------

# read in Ramsar boundaries
# load(file.path(scriptOut, "ramsarMDB.rdata"))
load(file.path(datOut, 'WetlandBoundaries', "ramsarBoundsMDB.rdata"))
ramsarBoundsMDB <- st_as_sf(ramsarBoundsMDB)
# ramsarBoundsMDB <- st_transform(ramsarBoundsMDB, whichcrs)



# ANAE 'preferences' ranked from most to least times observed
# McGinness, Langston and Brooks (2020) VEWH Prioritisation Project: Stage 2 Final Report
# Royal Spoonbill (Platalea regia) requirements, distribution and habitat mapping
breedANAEcodes <- read.table(
  file.path(myhome, "Deakin University/QAEL - MER/Model/dataStrict/BreedingANAERoyalSpoonBill.txt"), 
  skip = 2, sep = ":", header = TRUE)
forageANAEcodes <- read.table(
  file.path(myhome, "Deakin University/QAEL - MER/Model/dataStrict/ForagingANAERoyalSpoonBill.txt"), 
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



load(file = file.path(depthInBreed, catchBreedFiles[which(thisCatch %in% catchNames)])) 
whichcrs <- st_crs(Avoca_areaSpoonbillBreed) #GDA94  EPSG:3577
load(file = file.path(depthInForage, catchForageFiles[which(thisCatch %in% catchNames)]))

# load ANAE for ANAE code matching on veg type for depth stars 
# anae 

load(file.path(datOut, 'ANAEprocessed', paste0(thisCatch,  'ANAE.rdata'))) #GDA94   4283 
# load("C:/Users/amacq/Source/datOut/ANAEprocessed/AvocaANAE.rdata")
st_crs(AvocaANAE)
AvocaANAE <- st_transform(AvocaANAE, whichcrs)

# Inundation for Breeding stricture ---------------------------------------

# Areas of each ANAE meeting Breeding depth stricture. Bimonthly timestep 
# No point having one anae meeting depth strictures is it is not big enough
# to support a colony.


# Identidy TRUE/FALSE which polys have breed habitat
breedANAE <-(AvocaANAE$ANAE_CODE %in% breedANAEcodes$ANAE_CODE) #class logical i.e. no index, geometry
# breedANAEind <- which(AvocaANAE$ANAE_CODE %in% breedANAEcodes$ANAE_CODE)


# check and match index of depth stars to AvocaANAE
Avoca_areaSpoonbillBreed <-  matchStarsIndex(index1 = AvocaANAE, stars1 = NULL, index2 = Avoca_areaSpoonbillBreed_index, 
                stars2 = Avoca_areaSpoonbillBreed, testfinal = FALSE)
# unpack list
breedDepth_index <- Avoca_areaSpoonbillBreed[[1]]  # this needs to be made generic
breedDepth <- Avoca_areaSpoonbillBreed[[2]]
names(breedDepth) <- "Area"

# double-check index match 
sum(breedDepth_index$UID==AvocaANAE$UID)
length(AvocaANAE$UID)

# turn non-breeding-suitable polys to zero, other values still areas
breedANAEstrict <- breedDepth*breedANAE

# limit to wetland boundaries

ramsarTF <- st_covered_by(((breedANAEstrict)), 
                                   (st_buffer(ramsarBoundsMDB,250))) %>% lengths > 0

ramsarStrict <- breedANAEstrict*ramsarTF


# polygons wet for three consecutive bimonths in breeding season. 

timeStrict <- ramsarStrict >0  #  areas over 0 = TRUE


timeStrict[[1]] <- t(RcppRoll::roll_sum(t(timeStrict[[1]]), n= 3, align = "right", fill = c(NA, NA, NA)))

timeStrict[[1]] <- timeStrict$Area == 3

seasonStrict <- timeStrict

times <- st_get_dimension_values(seasonStrict, "time")

#  for southern basin there is only one 6month period so the 'time' must be march. 

breedTimes <- which(lubridate::month(times)== 03)

seasonStrict[[1]] <-0
seasonStrict[[1]][,breedTimes]<-1 

seasonStrict <- seasonStrict*timeStrict #  Times and polygon combinations where 3 bimonthly and season strictures are TRUE

# get max inundation for each 6 month (3 x bimonthly period)
breedDepth[[1]] <- t(RcppRoll::roll_max(t(breedDepth[[1]]), n= 3, align = "right", fill = c(NA, NA, NA)))

breedStrict <- breedDepth*seasonStrict #max area of inundation across 3 bimos in the breeding season 
# TODO will get rid of intermediate step objects on after testing. 


# sum area up to wetland complex scale




# test against minimum total area inundated threshold















