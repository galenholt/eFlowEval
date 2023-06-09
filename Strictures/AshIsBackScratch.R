
source('directorySet.R')
# renv::restore()
# 
# a <- renv::status()
# 
# a$library[[1]]
# 
# b <- bind_rows(a)
# renvpackageList28_09_21 <- read.table(
#   "C:/Users/amacq/Source/.Rproj.user/shared/notebooks/renvpackageList28_09_21.txt",
#   quote="\"", comment.char="")
# packlist<- (renvpackageList28_09_21)
# n=6
# for(n in 10:length(packlist)){
#   renv::install(packlist$V1[n])
# }
# library(brew)
# 
# renv::install(packlist$V1[10])
# 
# install.packages("data.table")
# 
# renv::install("readr@2.0.2", type = "binary")
# 
# renv::install("lwgeom@0.2-6")
# renv::install("microbenchmark@1.4-7")
# renv::install("modelr@0.1.8")
# renv::install("ncdf4@1.17")
# renv::install("raster@3.4-10")
# renv::install("sf@1.0-0")
# renv::install("stars@0.5-3")
# renv::install("tidyverse@1.3.1")
# renv::install("tmap@3.3-2")
# renv::install("transformr@0.1.3")
# 
# # preliminaries --------------------------------------------------
# 
# myhome <- paste0('C:/Users/', Sys.getenv("USERNAME"))

# ------- spoonbill strictures ---------------------------------------------------

# read in Ramsar boundaries -----------------

# load(file.path(scriptOut, "ramsarMDB.rdata"))
# load(file.path(scriptOut, "ramsarBoundsMDB.rdata"))
load(file.path(datOut, 'WetlandBoundaries', "ramsarBoundsMDB.rdata"))

# read in point observations for breeding
breedObsRSB <- scan(
  file.path(datDir, "dataStrict/BreedingObservationsRoyalSpoonbill.txt"), 
  skip = 2)
# breedObsRSB <- unlist(breedObsRSB)

# a <- data.frame("Lat" = breedObsRSB[breedObsRSB<0], "Long" =breedObsRSB[breedObsRSB>0])
breedObsRSB <- matrix(data = breedObsRSB, ncol = 2, nrow = length(breedObsRSB)/2, byrow = TRUE)
breedObsRSB <- as.data.frame(breedObsRSB)
names(breedObsRSB) <- c("Lat", "Long")

# do those hit the ramsars?
breedsf <- sf::st_as_sf(breedObsRSB, coords = c('Lat', 'Long'), crs = 4326)


#subset wetland boundaries to those with breeding observations
load(file.path(datOut, 'ANAEprocessed', 'ltimNoNorth.rdata'))
library(ggplot2)
ggplot() + geom_sf(data = ltimNoNorth) + geom_sf(data = breedsf)
# Thos are in the right place, now what ramsars do they hit?

ggplot() + 
  geom_sf(data = ltimNoNorth) + 
  geom_sf(data = ramsarBoundsMDB, mapping = aes(fill = Wetland)) +
  geom_sf(data = breedsf) + theme(legend.position = 'none')

# and a check of the examples
ggplot() +
  geom_sf(data = dplyr::filter(ramsarBoundsMDB, 
                               grepl('werai|perri|mille', Wetland, ignore.case = TRUE)), 
          mapping = aes(fill = Wetland)) +
  geom_sf(data = breedsf) +
  coord_sf(xlim = c(143, 146), ylim = c(-35, -37), crs = 4326)

# read in ANAEv3 

# wetlands <- read_sf(dsn = file.path(datDir, 
#                                     'ANAE/ANAE_Wetlands_v3_24mar2021/Wetlands_ANAE_v3_24mar2021/Wetlands_ANAE_v3_24mar2021.shp')) %>%
#   st_cast("MULTIPOLYGON") %>% # cleans up an issue with multisurfaces
#   st_make_valid()

# And get the LTIM_Valleys to use to subset for toy models at scale, but not enormous scale
LTIM_Valleys <- read_sf(dsn = file.path(datDir, 'ANAE/MDB_ANAE_Aug2017/MDB_ANAE.gdb'), layer = 'LTIM_Valleys') %>%
  st_cast("MULTIPOLYGON") %>% # cleans up an issue with multisurfaces
  st_make_valid()

# and the basin boundary, might be useful, especially for clipping rasters
basin <- read_sf(dsn = file.path(datDir, 'ANAE/MDB_ANAE_Aug2017/MDB_ANAE.gdb'), layer = 'MDB_Boundary') %>%
  st_cast("MULTIPOLYGON") %>% # cleans up an issue with multisurfaces
  st_make_valid() %>%
  select(LEVEL2NAME) # no need for other info




load(file.path(datOut, 'ANAEprocessed/AvocaANAE.rdata')) #GDA94   4283



load(file.path("C:/Users/amacq/Source/datOut/Inundationprocessed/areaSpoonbillBreed", "Avoca_areaSpoonbillBreed.rdata"))
load(file.path("C:/Users/amacq/Source/datOut/Inundationprocessed/areaSpoonbillForage", "Avoca_areaSpoonbillForage.rdata"))

Aforage <- Avoca_areaSpoonbillForage

str(Avoca_areaSpoonbillBreed)
summary(Avoca_areaSpoonbillBreed$depth)

# avoca catchment 197 time slice = every 2 days? Months? X1988.01.01.11.00.00" "X1988.03.01.11.00.00"

(Abreed[,1,196])


plot(Avoca_areaSpoonbillBreed[1])

Avoca_areaSpoonbillBreed %>% slice(time, 5) -> Abreed5

Abreed <- Avoca_areaSpoonbillBreed
dim(Abreed[[1]])
Abreed1 <- Abreed[,,1]
plot(Abreed[,,150], breaks = "equal")
# Abreed[depth, geometry, time]
plot(Aforage[,1:10,150], breaks = "equal")



summary(Abreed["depth",,1])


Abreed==Aforage

max(Abreed1$depth)

st_area(Abreed[,1,1])
geo<- st_geometry(Abreed[,,1])

st_area(geo[1])

st_area(st_geometry(breed_Sbill[1,1,1]))
breed_Sbill[[1]][1]

## ---- Breeding strictures -------
# duration min 6 months = >3 bimos 
# - Southern basin (below Macquarie Marshes): October- March
# - Northern Basin: September-(early) May



# As a test do we want area of polygon non-zero or proportion or absolute value?
# start with non-zero and then do time. 

areaBreedThresh <- 0.3

breed_Sbill <- Avoca_areaSpoonbillBreed
breed_SbillA <- Avoca_areaSpoonbillBreed
breed_SbillA[[1]] <- as.numeric(st_area(st_geometry(Avoca_areaSpoonbillBreed))*areaBreedThresh)
  # as.numeric(st_area(st_geometry(Avoca_areaSpoonbillBreed))*areaBreedThresh)

# breed_Sbill <- Avoca_areaSpoonbillBreed[[1]] > as.numeric(st_area(st_geometry(Avoca_areaSpoonbillBreed))*areaBreedThresh)
breed_Sbill <- Avoca_areaSpoonbillBreed[[1]] > breed_SbillA[[1]]
# 611 of 448569 values greater than 50% area inundated 

breed_Sbill_sum3 <- breed_Sbill

# look for 3 consecutive 'bimos' i.e. 6 months of inundation
system.time(breed_Sbill_sum3[[1]] <- timeRoll(breed_Sbill[[1]],
                                                      FUN = RcppRoll::roll_sum,
                                                      rolln = 3,
                                                      align = 'right',
                                                      na.rm = TRUE))
# RcppRoll might work direct on breedSbill
# rolls down? 
# t()  to transpose 
# timeroll works on 3 dims ie. raster. does a t() too. 







# ANAE 'preferences' ranked from most to least times observed
# McGinness, Langston and Brooks (2020) VEWH Prioritisation Project: Stage 2 Final Report
# Royal Spoonbill (Platalea regia) requirements, distribution and habitat mapping
breedANAE <- read.table(
  file.path(dataBase, "dataStrict/BreedingANAEcolonialBirds.txt"), 
                        skip = 2, sep = ":", header = TRUE)

# Foraging strictures ----------------------------------------------------
# water < 0.4m
# specific ANAE
# within 2km of roosting habitat... Not implemented
# abundant density of small fish, crustaceans or aquatic inverts... No implemented
#     Could be a link to productivity?

# ANAEs : area meeting forage strictures, currently includes other activities but Heather sending me alternative 
load(file.path("C:/Users/amacq/Source/datOut/Inundationprocessed/areaSpoonbillForage", "Avoca_areaSpoonbillForage.rdata"))

# ANAE 'preferences' ranked from most to least times observed
# McGinness, Langston and Brooks (2020) VEWH Prioritisation Project: Stage 2 Final Report
# Royal Spoonbill (Platalea regia) requirements, distribution and habitat mapping
forageEtcANAE <- read.table(
  file.path(dataBase, "dataStrict/ForagingEtcANAEcolonialBirds.txt"), 
                            skip = 2, sep = ":", header = TRUE)

# set a minimum inundated area of ecological significance for foraging by quantile
minForageArea <- Avoca_areaSpoonbillForage %>%
  st_geometry() %>%
  st_area() %>%
  quantile(.,0.2)%>%
  as.numeric()


forage_Sbill <- Avoca_areaSpoonbillForage #initialise 

# apply min area stricture
forage_Sbill <- Avoca_areaSpoonbillForage > minForageArea
names(forage_Sbill) <- "passedStricts"

# constrain to forage ANAE. Do this here vs centipeda at end. 
#  index variable has  UID + geometry. need to map ANAE type onto UID
# [?? this excludes dams and stuff]
#  

forageANAEind <- which(AvocaANAE$ANAE_CODE %in% forageEtcANAE$ANAE_CODE)
forageANAE <-(AvocaANAE$ANAE_CODE %in% forageEtcANAE$ANAE_CODE)

forage_SbillANAE <- forage_Sbill*forageANAE  # keeps all polys turns passed stricts false where not in suitable ANAE

# forage_SbillANAE <- forage_Sbill[forageANAEind]  # this would discard polys of unsuitable ANAE types. 
  
# check anae is subsetting 
sum(forage_Sbill$passedStricts) #4961
sum(forage_SbillANAE$passedStricts)  #3716
length(forage_Sbill$passedStricts)  #448569





















