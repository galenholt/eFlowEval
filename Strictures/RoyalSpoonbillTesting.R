




# plot Breeding stars at each stricture step

# Libraries and system setup
source('directorySet.R')

# Let's get libraries here
library(here)
library(tidyverse)
library(sf)
library(stars)
library(ggrepel)
library(Rcpp)
library(RcppRoll)




# And get the LTIM_Valleys to use to subset for toy models at scale, but not enormous scale
LTIM_Valleys <- read_sf(dsn = file.path(datDir, 'ANAE/MDB_ANAE_Aug2017/MDB_ANAE.gdb'), layer = 'LTIM_Valleys') %>%
  st_cast("MULTIPOLYGON") %>% # cleans up an issue with multisurfaces
  st_make_valid()
LTIM_Valleys <- st_transform(LTIM_Valleys, 3577)

# and the basin boundary, might be useful, especially for clipping rasters
basin <- read_sf(dsn = file.path(datDir, 'ANAE/MDB_ANAE_Aug2017/MDB_ANAE.gdb'), layer = 'MDB_Boundary') %>%
  st_cast("MULTIPOLYGON") %>% # cleans up an issue with multisurfaces
  st_make_valid() %>%
  select(LEVEL2NAME) # no need for other info
basin <- st_transform(basin, 3577) #GDA94 / Australian Albers


# generated in wetlandBoundaries.R
load(file.path(datOut, 'WetlandBoundaries', "ramsarMDB.rdata"))

load(file.path(datOut, 'WetlandBoundaries', "ramsarBoundsMDB.rdata"))

# create a zoom box to see what's going on. 

AvocaMarshesBounds <- ramsarBoundsMDB %>% 
  filter(Wetland == "First Marsh (The Marsh)" |
           Wetland == "Second Marsh (Middle Marsh)" |
           Wetland == "Third Marsh (Top Marsh)")

AvocaMarshBB <- st_bbox(AvocaMarshesBounds)
source("./Functions/BBoxAdj.R")
AvocaMarshBB <- BBoxAdj(AvocaMarshBB, 0.5,0.5)

# gplot + coord_sf(xlim = st_coordinates(bbox_new)[c(1,2),1], # min & max of x values
#                  ylim = st_coordinates(bbox_new)[c(2,3),2]) # min & max of y values


AvocaMarshBB <- st_as_sf(AvocaMarshBB)

# plot subset of ANAE strict stars


ggplot()+  # can't call stars data on this line
  theme(axis.text.x = element_text(angle = 90))+
  geom_stars(data = ANAEstrict[AvocaMarshBB][,,1])+
  ggtitle("Area of polygon with depth suitable for breeding")
# adding 50% of range to left and right with [] stars includes an extra long polygon
# which extends the area a lot more

ggplot()+  # can't call stars data on this line
  theme(axis.text.x = element_text(angle = 90))+
  geom_stars(data = ANAEstrict[AvocaMarshBB][,,1])+
  coord_sf(xlim = st_coordinates(AvocaMarshBB)[c(1,2),1], # min & max of x values
           ylim = st_coordinates(AvocaMarshBB)[c(2,3),2])+ # min & max of y values
  ggtitle("Area of polygon with depth suitable for breeding")
# this forces to crop to bbox not polygon. 

plot(ANAEstrict[AvocaMarshBB][,,1])

  
# ---- limit to wetland boundaries

# breedWetlands <- (st_contains(st_geometry(ANAEstrict), st_geometry(ramsarBoundsMDB))) # no matches
# list object 
# use map to sum across list elements and see if there are non-zero entries. 
# Which to covert to true/false?
# map(breedWetlands, sum)# sums within each list element

# breedWetlandsInv <- (st_contains(st_geometry(ramsarBoundsMDB), st_geometry(ANAEstrict)))

# breedWetlandsInd <- (st_covered_by(st_geometry(ANAEstrict), 
                                        # st_geometry(ramsarBoundsMDB))) 

# making sf object still returns list object 
breedWetlandsInd <- (st_covered_by(st_geometry(st_as_sf(ANAEstrict)), 
                                   st_geometry(ramsarBoundsMDB))) 

# df$indicator <- st_within(df, box) %>% lengths > 0

#  using a logical test converts the sparse geometry binary predicate to logical
breedWetlandsInd2 <- st_covered_by(((ANAEstrict)), 
                                   (ramsarBoundsMDB)) %>% lengths > 0

RamsarStrict <- ANAEstrict*breedWetlandsInd2

lapply(breedWetlandsInd, sum)



# a <- unlist(breedWetlandsInd)


breedWetlands <- (st_intersects(st_geometry(ANAEstrict), st_geometry(ramsarBoundsMDB)))
# also a list but with additional attributes
# can use sparse = false arg to st_intersects to get a logical instead of sgbp

breedWetlandsTF <- lengths(breedWetlands)>0
# logical vector.  Can I assume this hasn't been shuffled?!! 



# Set non breeding wetland to zero
ANAEstrict2 <- ANAEstrict*breedWetlandsTF 
# TODO check product operations (*) are applied across slices = times

# plot to see if this subsets





ggplot()+  # can't call stars data on this line
  theme(axis.text.x = element_text(angle = 90))+
  geom_stars(data = ANAEstrict2[AvocaMarshBB][,,1])+
  coord_sf(xlim = st_coordinates(AvocaMarshBB)[c(1,2),1], # min & max of x values
           ylim = st_coordinates(AvocaMarshBB)[c(2,3),2])+ # min & max of y values
  ggtitle("Area of polygon with depth suitable for breeding")

# Doesn't appear to subset in AvocaMarshBB

ggplot()+  # can't call stars data on this line
  theme(axis.text.x = element_text(angle = 90))+
  geom_stars(data = ANAEstrict2[,,1])+
  # coord_sf(xlim = st_coordinates(AvocaMarshBB)[c(1,2),1], # min & max of x values
           # ylim = st_coordinates(AvocaMarshBB)[c(2,3),2])+ # min & max of y values
  ggtitle("Area of polygon with depth suitable for breeding")

ggplot()+  # can't call stars data on this line
  theme(axis.text.x = element_text(angle = 90))+
  geom_stars(data = ANAEstrict[,,1])+
  # coord_sf(xlim = st_coordinates(AvocaMarshBB)[c(1,2),1], # min & max of x values
  # ylim = st_coordinates(AvocaMarshBB)[c(2,3),2])+ # min & max of y values
  ggtitle("Area of polygon with depth suitable for breeding")


#could remove zeros for plotting...



#check that boundaries are correctly aligned in wetlandBoundaries.R

# roll across bimonthly periods, ID those that have Area >0 in 3 consecutive (pr)

areaTF <- breedStrict>0
areaTF[,,1:3]
sum(areaTF$Area) # 3819
timeStrict <- areaTF

timeStrict[,,1]

timeStrict[[1]] <- t(RcppRoll::roll_sum(t(areaTF[[1]]), n= 3, align = "right", fill = c(NA, NA, NA)))
# might be worth setting fill = zero not actually true but may smooth operations later
# 
timeStrict[[1]] <- timeStrict$Area == 3

seasonStrict <- timeStrict
seasonStrict[[1]] <- if(st_geometry(seasonStrict))
  
# area n/s of mac marshes 
  # are those polygons wet in the right season - lubridate 
# Galen 
# - test with his timeroll function see if get same answer 
# - reducing memory usage - use function or overwrite previous objects
#         - probably still using same processing, just better on memory

  
# getting slice labels = dates so I can test on them. previously I've converted to sf??
a <- (seasonStrict[,,"1988-01-01 11:00:00"])
# keeps all 
b <- seasonStrict[,,1]
dim(seasonStrict)

b$Area$time$values # doesn't work on atomic ..'
b[[1]][2] # doesn't give $time

times <- st_get_dimension_values(seasonStrict, "time")


# breeding season S-E australia Oct-march
# north of macquarie Sept-April (inclusive)

# ALERT: 1988-01-01 11:00:00 time represents bimonthly period covering jan and feb
# thus there is a mismatch between the breeding window Oct-March and inundation data

breedSeasonSth <- interval() 

lubridate::month(times[2])

lubridate::month(times[2])-as.Date.POSIXct(6, "%m", tz='', origin = "1970-01-01")



t1 <- dplyr::slice(timeStrict, time, c(1, 61, 121, 181))
t1 <- dplyr::slice(timeStrict, time, time < times[3] ) # nope 


d = c(10, 10, 150)
a = array(rnorm(prod(d)), d) # pure noise
times = Sys.Date() + seq(1, 2000, length.out = d[3])
m = as.numeric(format(times, "%m"))

#  for southern basin there is only one 6month period so the 'time' must be march. 

breedTimes <- which(lubridate::month(times)== 03)

seasonStrict[[1]] <-0
seasonStrict[[1]][,breedTimes]<-1 



seasonStrict <- seasonStrict*timeStrict #  Times and polygon combinations where 3 bimonthly and season strictures are TRUE


# seasonStrict <- aggregate(breedDepth,
breedDepth3bimos <- breedDepth
breedDepth3bimos[[1]] <- t(RcppRoll::roll_max(t(breedDepth[[1]]), n= 3, align = "right", fill = c(NA, NA, NA)))

breedStrict <- breedDepth3bimos*seasonStrict #max area of inundation across 3 bimos in the breeding season 


# --- sum up areas to wetland complex scale

# there are only 16 Ramsar wetlands in the basin so I can go one by one if necessary

# check 



f <- function(x){sum(x, na.rm = TRUE)}
breedAreaByWetland <- aggregate(breedStrict, ramsarBoundEdwa,f )

breedAreaByWetland1 <- aggregate(breedStrict, ramsarBoundsMDB,f ) #stars obj, 202 polys
dim(breedAreaByWetland)==dim(breedAreaByWetland1)
# 
