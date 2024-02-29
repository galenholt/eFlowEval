# import and plot Ramsar wetlands 

# Libraries and system setup
source('directorySet.R')

# Let's get libraries here
library(here)
library(tidyverse)
library(sf)
library(stars)
library(ggrepel)
library(tmap)


# Directory to export TO
scriptOut <- file.path(datOut, 'WetlandBoundaries')
if (!dir.exists(scriptOut)) {dir.create(scriptOut, recursive = TRUE)}

# optional load of previous script outputs
# This seems to need stricture outputs, but not to do the wetlands, just data exploration. Needs to clean up.
load(file.path(datOut, 'Strictures', 'Spoonbill', 'spoonbill_strictures.rdata'))

# This gets overwritten below. Not sure what it's doing here.
# load(file.path(file.path(datOut, 'WetlandBoundaries', "ramsarMDB.rdata")))  # ramsar polygons

# The only place this gets made anywhere is below, so hopefully we can delete it here.
# load(file.path(file.path(datOut, 'WetlandBoundaries',"ramsarBoundsMDB.rdata"))) # boundary of each set of polygons in each wetland complex``



# https://data.gov.au/data/dataset/04cd73cc-24d9-4ae9-aeaa-046a022cb592
# metadata html in folder
Ramsar <- read_sf(dsn = file.path(datDir, "RAMSARwetlandBoundaries/important_wetlands.shp")) %>%
  st_cast("MULTIPOLYGON") %>% # cleans up an issue with multi-surfaces
  st_make_valid()
# there is one fewer unique WNAME values (202) than REFCODE values??

# read in ANAEv3 

# wetlands <- read_sf(dsn = file.path(datDir, 
#                                     'ANAE/ANAE_Wetlands_v3_24mar2021/Wetlands_ANAE_v3_24mar2021/Wetlands_ANAE_v3_24mar2021.shp')) %>%
#   st_cast("MULTIPOLYGON") %>% # cleans up an issue with multisurfaces
#   st_make_valid()

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


#crop Ramsar to basin 
# RamsarMDB <- Ramsar[basin] # crops to bounding box
Ramsar <- st_transform(Ramsar, st_crs(basin))
ramsarMDB <- st_intersection(Ramsar, basin)


save(ramsarMDB, file = file.path(scriptOut, "ramsarMDB.rdata"))
# load(file.path(scriptOut, "ramsarMDB.rdata"))

# Pushing the watercourses down here, so don't have to run them if all we want to do is the processing
st_layers(dsn = file.path(datDir, 'ANAE/MDB_ANAE_Aug2017/MDB_ANAE.gdb'))

#read in watercourses for context, plots
# Watercourses_ANAE created 2013 by brooks et al. 
watercourses <- read_sf(dsn = file.path(datDir, 'ANAE/MDB_ANAE_Aug2017/MDB_ANAE.gdb'), layer = 'watercourses_ANAE')

#filter to major rivers 
M_D <- watercourses%>% filter(., Name == c("MURRAY RIVER", "DARLING RIVER"))
# could filter by length to get a more manageable subset

plot(st_geometry(basin), bg = NA)
plot(st_geometry(LTIM_Valleys), add = TRUE, bg = NA)
# plot(st_geometry(watercourses), add = TRUE, col = "green"))  #thousands, of streams!!
plot(st_geometry(ramsarMDB), col = "blue", add = TRUE)

plot(st_geometry(M_D), add = TRUE, col = "light blue", lwd = 2)


# filter into wetland complexes and combine each into single polygon
# ALERT this cuts down from 8819 to 202 but there are only 16 Ramsar sites. 

ramsarBoundsMDB <- ramsarMDB %>%
  group_by(WNAME)%>%
  summarise()

names(ramsarBoundsMDB)[1] <- "Wetland"
ramsarBoundsMDB <- st_transform(ramsarBoundsMDB, 3577)

save(ramsarBoundsMDB, file = file.path(scriptOut,"ramsarBoundsMDB.rdata"))
# load(file.path(scriptOut,"ramsarBoundsMDB.rdata"))
plot(ramsarBoundsMDB)


ggplot() +
  geom_sf(data = ramsarBoundsMDB, aes(fill = Wetland, colour = Wetland), show.legend = FALSE) +
  geom_sf(data = LTIM_Valleys, fill = NA, color = "grey") 

plot(st_geometry(filter(LTIM_Valleys, ValleyName == "Avoca")))
plot(ramsarBoundsMDB, add = TRUE)

# plot to see if the bounds are working

AvocaBound <- filter(LTIM_Valleys, ValleyName == "Avoca")
st_bbox(AvocaBound)
# xmin     ymin     xmax     ymax 
# 951850 -4122165  1063308 -3837980 


AvocaWetlands <- ggplot()+
  geom_sf(data = filter(LTIM_Valleys, ValleyName == "Avoca"))+
  geom_sf(data = st_intersection(ramsarBoundsMDB, AvocaBound), 
          aes(fill = Wetland, colour = Wetland), show.legend = FALSE)+
  # geom_sf_label(data = st_intersection(ramsarBoundsMDB, AvocaBound), aes(label = Wetland))+
  # geom_text(aes(label =  "Wetland"),nudge_x = 1)+
  theme(axis.text.x = element_text(angle = 90))+
  coord_sf(crs = 3577)+  theme_bw()+
  ggtitle("Ramsar wetlands of Avoca Catchment")

  
# -- expanding bounding box method 
bbox_new <- st_bbox(AvocaBound)
xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
yrange <- bbox_new$ymax - bbox_new$ymin # range of y values

bbox_new[1] <- bbox_new[1] - (0.5 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.5 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.5 * yrange) # ymin - bottom
# bbox_new[4] <- bbox_new[4] + (0.5 * yrange) # ymax - top 

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc()

AvocaWetlands +
  coord_sf(xlim = st_coordinates(bbox_new)[c(1,2),1], # min & max of x values
           ylim = st_coordinates(bbox_new)[c(2,3),2]) # min & max of y values
  # geom_text_repel(data = st_coordinates(st_centroid(st_intersection(ramsarBoundsMDB, AvocaBound))), 
  #                 aes(label = "Wetland"))  

ggsave(filename = file.path(scriptOut, "AvocaRamsarWetlands.pdf" ))

# -- zoom to wetland polys and add labels

bbox_new <- st_bbox(st_intersection(ramsarBoundsMDB, AvocaBound))

ggplot()+
  geom_sf(data = filter(LTIM_Valleys, ValleyName == "Avoca"))+
  geom_sf(data = st_intersection(ramsarBoundsMDB, AvocaBound), 
          aes(fill = Wetland, colour = Wetland), show.legend = TRUE)+
  # geom_sf_label(data = st_intersection(ramsarBoundsMDB, AvocaBound), aes(label = Wetland))+
  # geom_text(aes(label =  "Wetland"),nudge_x = 1)+
  theme(axis.text.x = element_text(angle = 90))+
  coord_sf(crs = 3577, 
           ylim = c(bbox_new$ymin, bbox_new$ymax))+  
  theme_bw()+
  ggtitle("Ramsar wetlands of Avoca Catchment")
 
ggsave(filename = file.path(scriptOut, "AvocaRamsarWetlandsZoomed2wetlandsWithLegend.pdf" ) ) 

AvocaMarshesBounds <- ramsarBoundsMDB %>% 
  filter(Wetland == "First Marsh (The Marsh)" |
           Wetland == "Second Marsh (Middle Marsh)" |
           Wetland == "Third Marsh (Top Marsh)")

AvocaMarshBB <- st_bbox(AvocaMarshesBounds)
save(AvocaMarshBB, file = file.path(datDir, "dataStrict/AvocaMarshBB.rdata"))

AvocaMarshPolys <- st_crop(ramsarMDB, AvocaMarshBB)



ggMarshes <- ggplot()+
  # geom_sf(data = AvocaMarshesBounds, 
  #         aes(fill = Wetland, colour = Wetland), show.legend = TRUE)+
  geom_sf(data = AvocaMarshPolys, aes(fill = "None" ))

## There are multiple polygons for the same ramsar wetland unit. 
# same refcode (e.g. VIC045) but different areas etc. dates
# lake bael bael is included - could be a bounding box effect,  

# Action: ignore for now, the combining of different polys with same Wetland name
# should cover it i.e. wetlandBound

# plot the ANAE polygons over bounds i.e. are they contained within?
# should I add a buffer? 

load(file.path(datOut, 'ANAEprocessed', 'AvocaANAE.rdata'))
AvocaANAE <- st_transform(AvocaANAE, st_crs(AvocaMarshBB))
AvocaMarshANAE <- st_crop(AvocaANAE, AvocaMarshBB)

gplot <- ggplot()+
geom_sf(data = AvocaMarshANAE, aes(fill = ANAE_DESC), alpha = 0.5)+
  geom_sf(data = AvocaMarshesBounds, 
                  colour = "red", fill = NA, show.legend = FALSE)

  # geom_sf(data = AvocaMarshANAE, alpha = 0.5)
bbox_new <- BBoxAdj(AvocaMarshBB, 0.5,0.5)

gplot + coord_sf(xlim = st_coordinates(bbox_new)[c(1,2),1], # min & max of x values
                 ylim = st_coordinates(bbox_new)[c(2,3),2]) # min & max of y values

# so there are ANAE adjacent to ramsar bounds but not contained within


# adding buffer to bound and plot
  ggplot()+
  geom_sf(data = AvocaMarshANAE, aes(fill = ANAE_DESC), alpha = 0.5, show.legend = FALSE)+
  geom_sf(data = st_buffer(AvocaMarshesBounds, 250), 
          colour = "red", fill = NA, show.legend = FALSE)+ 
  coord_sf(xlim = st_coordinates(bbox_new)[c(1,2),1], # min & max of x values
           ylim = st_coordinates(bbox_new)[c(2,3),2]) # min & max of y values


  
# plot star 

AvocaMarshBreedD1 <- st_crop(st_as_sf(breedDepth[,,1]),AvocaMarshBB )
# 39 obs, 2 cols: area at depth, geometry. 3 with depth > 0
# includes some multipolygons
names(AvocaMarshBreedD1)[1] <- "Area"

AvocaMarshBreedD2 <- st_crop(st_as_sf(breedDepth[,,2]),AvocaMarshBB )
# 1988-03-01 11:00:00 so bimonthly slices as expected. 
names(AvocaMarshBreedD2)[1] <- "Area"


ggplot() + 
  geom_sf(data =   AvocaMarshBreedD1)
#just gives outlines

ggplot()+
  geom_sf(data = filter(AvocaMarshBreedD1,names(AvocaMarshBreedD1)[1] > 0))
# doesn't remove the 'depth' = 0 features

ggplot()+
  geom_sf(data = AvocaMarshBreedD1)+
  geom_sf(data = filter(AvocaMarshBreedD1, Area >0), aes(colour = "red"), 
          show.legend = FALSE)+
  theme_bw()+ 
  theme(axis.text.x = element_text(angle = 90))+#gets ignored if before a different theme() call
  ggtitle("Avoca marsh wetlands")
  
ggplot()+
  geom_sf(data = AvocaMarshBreedD1)+
  geom_sf(data = filter(AvocaMarshBreedD1, Area >0), aes(fill = Area))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("Avoca marsh wetlands, 1 Jan 1988")

# not sure how to get a new line in title. below doesn't work
# ggtitle(paste(cat("Avoca marsh wetlands","\n", "1 Jan 1988")))


ggsave(filename = file.path(scriptOut, "AvocaMarshlandsAreaOfBreedingDepth1_1_1988.png"))

ggplot() + 
  geom_stars(data= breedDepth[,,1]) +
# get polygons with depth as gradient

#how to zoom in? 

# set coordinates

xlim(0,100)+ylim(10,20) #?? works on geomstars?


# plot ANAEstrict = only breeding ANAE

# AvocaMarshANAEstrictD1 never gets created anywhere, so I'm commenting it out.
# ggplot()+
#   # geom_sf(data = AvocaMarshBreedD1)+ #cropped breedDepth
#   geom_sf(data =AvocaMarshANAEstrictD1, aes(fill = Area))+
#   theme_bw()+
#   theme(axis.text.x = element_text(angle = 90))+
#   ggtitle("Avoca marsh wetlands, 1 Jan 1988")

# eyeballing it, it doesn't seem like multiplying by breedANAE 
# (i.e. those matching the Heather's breeding ANAE types)
# changes the number of polygons. however the total area does drop.

# sum(AvocaMarshANAEstrictD1$Area)  # 5114081
sum(AvocaMarshBreedD1$Area)  # 5250824

# Even more pronounced across the whole catchment...
sum(breedDepth$depth) # 921764681
# never gets created
# sum(ANAEstrict$depth) # 347382030


# ---- Label Ramsar polygons with Ramsar names of wetland complexes (vs individual polys)

# import ramsar centroids
#  use buffer to select polys within a range and label? 

# LTIM_Valleys <- read_sf(dsn = file.path(datDir, 'ANAE/MDB_ANAE_Aug2017/MDB_ANAE.gdb'), layer = 'LTIM_Valleys') %>%
#   st_cast("MULTIPOLYGON") %>% # cleans up an issue with multisurfaces
#   st_make_valid()
# LTIM_Valleys <- st_transform(LTIM_Valleys, 3577)

ramsarCentroids <- read_sf(dsn = file.path(datDir, 'RAMSARcentroids/features_centroid_publishedPoint.shp'))
ramsarCentroids <- ramsarCentroids %>%  filter(country_en =='Australia')  # 66  7
# ramsar sites australia 2018 has 67... 
# https://www.awe.gov.au/sites/default/files/env/pages/d3389750-50fc-4ed3-9e2a-0652b74913f8/files/ramsar-sites-australia.pdf
ramsarCentroids <- st_transform(ramsarCentroids, 3577) #GDA94 / Australian Albers
ramsarCentroids <- st_intersection(ramsarCentroids, basin)

plot(ramsarCentroids$geometry)


ggplot()+
  geom_sf(data = AvocaMarshBreedD1)+
  geom_sf(data = filter(AvocaMarshBreedD1, Area >0), aes(fill = Area))+
  geom_sf(data = st_crop(ramsarCentroids,AvocaMarshBB), aes(colour = "red"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("Avoca marsh wetlands, 1 Jan 1988")

st_crop(ramsarCentroids,AvocaMarshBB)
# there doesn't appear to be any ramsar sites within avoca marsh BBox


# ---- plot ramsar centroids and buffer on ANAE and Ramsar bounds


ggplot()+
  geom_sf(data = ramsarMDB, aes(colour = "polygon"))+
  geom_sf(data = ramsarBoundsMDB, aes(colour = "boundary"))+
  geom_sf(data = basin, fill = NA)+
  geom_sf(data = LTIM_Valleys, fill = NA)+
  geom_sf(data = ramsarCentroids)+
  geom_sf_label(aes())


# p <- ggplot(nc3) +
#   geom_sf(aes(fill = AREA))
# p + geom_sf_text(aes(label = NAME), colour = "white")
# p + geom_sf_label(aes(label = NAME))



#check what this does
# ramsarBoundsMDB <- ramsarMDB %>%
#   group_by(WNAME)%>%
#   summarise()

Edwa <- filter(LTIM_Valleys, ValleyName =="Edward Wakool")
ramsarEdwa <- st_intersection(ramsarMDB, Edwa)
ramsarBoundEdwa <- st_intersection(ramsarBoundsMDB, Edwa)

ggplot()+
  geom_sf(data = Edwa, fill = NA)+
  geom_sf(data = ramsarEdwa, aes(fill = SPECIFIC_N))+
  ggtitle("Edward Wakool Ramsar 'SPECIFIC_N'")
  
ggplot()+
  geom_sf(data = Edwa, fill = NA)+
  geom_sf(data = ramsarEdwa, aes(fill = WNAME))+
  ggtitle("Edward Wakool Ramsar 'WNAME'")


ggplot()+
  geom_sf(data = Edwa, fill = NA)+
  geom_sf(data = ramsarBoundEdwa, aes(fill = Wetland))+
  ggtitle("Edward Wakool Ramsar summarised to 'WNAME'")

# Okay so this suggests that there are some spots where Ramsar poly do go across a bunch of ANAE polys
# i.e. Could use them to sum up
# Also SPECIFIC_N has only a few extra names = polys than WNAME. For EddyWak 1662/1667 are the same.
# In contrast in Avoca catchment ramsar dataset has different names for first, second and third marsh 
# Rather than them being group together with Kerang wetlands, some of which are in Loddon(?)
# Leave Avoca for now but will need addressing to go basin-scale
# TODO: Check all catchments   
  



#quick plot for valley names
tm_shape(LTIM_Valleys)+tm_borders()+tm_text("ValleyName")

# So... for eddyWak current polys will be okay.  Intersect with ANAE to create grouping variable
# Make a separate 'UID | Wetland complex'   table for each catchment that can be read in. 

# https://stackoverflow.com/questions/47600466/using-r-intersections-to-create-a-polygons-inside-a-polygon-key-using-two-shapef

# ramsarTF <- st_covered_by(((breedANAEstrict)), 
#                           (st_buffer(ramsarBoundsMDB,250)))

# THIS NEVER GETS USED
# wetlands <- read_sf(dsn = file.path(datDir,
#                                     'ANAE/ANAE_Wetlands_v3_24mar2021/Wetlands_ANAE_v3_24mar2021/Wetlands_ANAE_v3_24mar2021.shp')) %>%
#   st_cast("MULTIPOLYGON") %>% # cleans up an issue with multisurfaces
#   st_make_valid()

# why are we just reading in one?
load(file.path(datOut, 'Inundationprocessed', 'areaSpoonbillBreed', 'EdwardWakool_areaSpoonbillBreed.rdata'))

ramsarLabels <- EdwardWakool_areaSpoonbillBreed_index
# ramsarLabels$ramsar <- if{}

a <- st_covered_by(ramsarLabels, ramsarBoundsMDB)
a <- a %>% lengths >0
head(a)
sum(a)
which(a>0)
# in Avoca 47 ANAE within ramsar (without buffer)
# EddyWak 1472 of 14158

b <- st_covered_by(ramsarBoundsMDB, EdwardWakool_areaSpoonbillBreed_index)
length(b)#202 
print(b[[1]])


c <- st_intersects(ramsarBoundsMDB, EdwardWakool_areaSpoonbillBreed_index)
c[[1]]

# Which of elements of ramsar bounds have ANAE inside? 
cInd <- which(lengths(c)>0)  #  77 125 183 190
ramsarBoundsMDB$Wetland[cInd] # names of the wetland complexes containing ANAE
# [1] "Koondrook and Perricoota Forests"    "Millewa Forest"                      "Wakool-Tullakool Evaporation Basins"
# [4] "Werai Forest"

# what are the indices of the ANAE contained with each Ramsar polygon?
# there are many. 

# first 5 contained in Koondrock 
d <- (c[[cInd[[1]]]])
d[1:5]
# And their UID | geometry
EdwardWakool_areaSpoonbillBreed_index[d[1:5],]

ramsarLabels$wetland <- NA

ramsarLabels$wetland[d] <- ramsarBoundsMDB$Wetland[cInd[1]]
ramsarLabels$wetland[c[[cInd[[2]]]]] <- ramsarBoundsMDB$Wetland[cInd[2]]
ramsarLabels$wetland[c[[cInd[[3]]]]] <- ramsarBoundsMDB$Wetland[cInd[3]]
ramsarLabels$wetland[c[[cInd[[4]]]]] <- ramsarBoundsMDB$Wetland[cInd[4]]


sum( !is.na(ramsarLabels$wetland)) # 2831

unique(ramsarLabels$wetland)

EdwardWakoolRamsarLabels <- ramsarLabels

save(EdwardWakoolRamsarLabels, file = file.path(scriptOut, "EdwardWakoolRamsarLabels.rdata"))
# ~70MB 
# TODO store more cheaply by using a ramsar ID number and join with a smaller ramsar ID | wetland name table for plotting

#TODO turn above into a function and run on either ind'l valleys or entire basin. 

#  could just use the indexes directly e.g. sum( ANAE)
# but risk of shuffling?

#aggregate according to st_intersects call? could that work? 

# ---- try aggregate to ramsar wetland complex

f <- function(x){sum(x, na.rm = TRUE)}
breedAreaByWetland <- aggregate(breedStrict, ramsarBoundEdwa,f )
# I think this worked! That is for each timestep it summed area values across
# polygons that are contained in ramsar wetland complex polys
# however, it is now a list, NOT stars... however the geometry of original polys is there...
# shoudl be 4 values (1 each wetland complex) per time = slice

summary(breedAreaByWetland$Area) # we have non-zero Areas!

# is it going to be pain working with this list?

dim(breedAreaByWetland) # 4, 197 
names(breedAreaByWetland)
dim(breedStrict)
# can I force it to stars?
breedAreaByWetland <-  st_as_stars(breedAreaByWetland)
#yes. No? still 
class(breedAreaByWetland) #stars
#still list in environment. list of 4 stars objects?
# seems to act life a stars object unless I'm missing something. 
ggplot()+
  geom_stars(data = breedAreaByWetland[,,194])

#TODO test whether adding a buffer changes number of ANAE included. 
# also plot to check that it is behaving. 

#---- test against minimum total area stricture ---- 

# create stand-in value of, say, 75% total wetland area. see what happens

# total area of polys in each ramsar wetland
# ~ size of ramsar wetland poly

totalWetlandAreas <- st_area(st_geometry(breedAreaByWetland))
# [1] 281978748.9  21193552.4    647395.8 367070235.7   m^2

# times with non-zero Areas: c(8, 20, 14, 32, 38, 44, 50, 56, 62, 68, 74, 80, 86, 92, 98, 
# 104,  110, 116, 122, 128, 134, 140, 146, 152, 158, 164, 170,176, 182, 188, 194)

# f <- function(x){sum(st_area(x))}
# totalANAEareaByWetland <- aggregate(st_geometry(breedStrict), st_geometry(ramsarBoundEdwa),f )
# doesn't work. Wrong approach anyhow. 

# use totalWetlandArea for now

areaPC <- 0.1
wetlandAreaThreholds <- areaPC*totalWetlandAreas
# [1] 197385124.3  14835486.7    453177.1 256949165.0

areaStricture <- breedAreaByWetland
areaStricture[,,194][[1]]
#            [,1]
# [1,] 1161290.69
# [2,]   78925.46
# [3,]       0.00
# [4,] 1296191.49



areaStricture[[1]] <- ifelse(breedAreaByWetland[[1]] > (areaPC* as.numeric(st_area(st_geometry(breedAreaByWetland)))),1,0)
# need as.numeric() to cancel units (m^2) from st_area() 

areaStricture[,,194][[1]] #0,0,0,0 
# So either less than threshold or error...
# Lower.  Maybe thresholds are too. High. Try max of actual areas?



aggregate(breedAreaByWetland, by = "year", max)
# gets max for each year. I want max across all times. 
a <- aggregate(breedAreaByWetland, by = time, max)
dim(a) #geometry 197, geometry 4?? What is it doing?
tim <- st_get_dimension_values(breedAreaByWetland, "time")
tim <- last(tim) #tim[length(tim)]
f <- function(x){max(x, na.rm = TRUE)}
a <- aggregate(breedAreaByWetland, by = tim, f )
a[[1]] # NA NA NA NA??
# TODO figure out why not working. or do a workaround. or do the analysis of actual breed events
a <- aggregate(breedAreaByWetland, by = tim, max, na.rm = TRUE ) # nppe

a <- aggregate(breedAreaByWetland, by = "394 months", max, na.rm = TRUE ) #
by_t <- dim(breedAreaByWetland)["time"]*2 #number of months in dataset
a <- aggregate(breedAreaByWetland, by = paste(by_t,"months"), max, na.rm = TRUE ) # hooray!
# This gives one Area per ramsar wetland



ggplot()+
  geom_stars(data = areaStricture[,,194])



areaStricture <- aggregate(areaStricture, by = "year", sum)
dim(areaStricture)
t <- st_get_dimension_values(areaStricture,"time")
head(t)
# [1] "1988-01-01 AEDT" "1989-01-01 AEDT" "1990-01-01 AEDT" "1991-01-01 AEDT" "1992-01-01 AEDT" "1993-01-01 AEDT"

areaStricture <- aggregate(areaStricture, basin, sum)

areaStricture[[1]]



# COMMENTING OUT BECAUSE FULL OF ERRORS AND DON'T THINK IT GETS USED --------
# 
# 
# # ---- test why not working
# # try on a small subset
# # areaStricture[[1]] <- ifelse(breedAreaByWetland[[1]][1,] > (areaPC* as.numeric(st_area(st_geometry(breedAreaByWetland[[1]][1,194])))),1,0)
# # need as.numeric() to cancel units (m^2) from st_area() 
# # areaPC* as.numeric(st_area(st_geometry(breedAreaByWetland)))
# 
# # 
# # max(breedAreaByWetland[[1]][1,]) # [1] 2002358
# # max(breedAreaByWetland[[1]][2,]) # [1] 93221.95
# # max(breedAreaByWetland[[1]][3,]) # [1] 0
# # max(breedAreaByWetland[[1]][4,]) # [1] 1573478
# 
# # try directly summarise max Area for each wetland
# 
# by_t <- dim(breedAreaByWetland)["time"]*2 #number of months in dataset
# maxWetlandInunArea <- aggregate(breedAreaByWetland, by = paste(by_t,"months"), max, na.rm = TRUE )
# 
# 
# areaPC <- 0.7
# 
# areaStricture <- breedAreaByWetland
# areaStricture[[1]] <- ifelse(breedAreaByWetland[[1]] > 
#                                (areaPC*aggregate(breedAreaByWetland[[1]], by = paste(by_t,"months"), max, na.rm = TRUE )) ,1,0)
# # doesn't work.
# areaStricture[[1]] <- ifelse(breedAreaByWetland[[1]] > 
#                                (areaPC*maxWetlandInunArea[[1]]) ,1,0)
# 
# areaStricture[[1]] <- breedAreaByWetland > 
#                                areaPC*maxWetlandInunArea
# 
# ggplot()+
#   geom_stars(data = areaStricture[,,194])
# 
# # Calculate total number and proportion of breeding events in a year. 
# 
# 
# numBreedEvents <- aggregate(areaStricture, ramsarBoundEdwa, sum, na.rm = TRUE )
# # not working as expected 
# 
# numBreedEvents$Area 
# 
# 
# numBreedEvents$Area[194]
# 
# #all zeros so something is not working. 
# 
# # stuff it switch to sf is fine. 
# areaStrictureSF <- st_as_sf(areaStricture)
# 
# taxa2subB <- taxa2sub %>% st_apply(., MARGIN = 1, sum )  # across time ()
# # If MARGIN=1, the function accepts each row of X as a vector argument, and returns a vector of the results
# 
# # numBreedEvents <- areaStrictureSF %>% st_apply(., MARGIN = 2, sum) # only for arrays
# 
# # -- get sum of 
# #stack columns
# # a %>%
# #   pivot_longer(cols = everything())
# 
# numBreedEvents <- areaStrictureSF 
# numBreedEvents <- numBreedEvents %>% st_drop_geometry()
# numBreedEvents <- numBreedEvents %>% pivot_longer(names_to = "Date", values_to = "Breed", cols = everything())
# numBreedEvents <- numBreedEvents %>%  group_by(Date)%>% summarise(NumBreed = sum(Breed)) %>% ungroup()
# 
# sum(numBreedEvents$NumBreed)
# 
# y <- c("2014", "2015", "2016", "2017", "2018")
# 
# subDates <- st_get_dimension_values(areaStricture, "time")
# subDates <- which(lubridate::year(subDates) %in% y )
# 
# # lubridate::month(times)== 03)
# plotDat <- numBreedEvents %>% filter( lubridate::month(Date)== 03)
# 
# 
# ggplot(data = filter(numBreedEvents, lubridate::year(Date)%in% y))+
#   geom_line(aes(Date, NumBreed))
# 
# ggplot(data = filter(numBreedEvents[subDates,]), aes(Date, NumBreed))+
#   geom_point()
# 
# ggplot(data = plotDat, aes(Date, NumBreed))+
#   geom_point()+
#   labs(y = "Number of potential breeding events", x = "Year")+
#   scale_x_discrete(labels = lubridate::year(plotDat$Date))+
#   theme(axis.text.x = element_text(angle = 90))
#   
# 
# #histogram
# ggplot(data = numBreedEvents, aes(x= NumBreed))+
#   geom_bar()
# 
# ggplot(data = ungroup(numBreedEvents), aes(x= Date, y = NumBreed))+
#   geom_step()
# 
# 
# table(numBreedEvents$NumBreed)
# # 0   1   2   3 
# # 172   1  17   7 
# 



