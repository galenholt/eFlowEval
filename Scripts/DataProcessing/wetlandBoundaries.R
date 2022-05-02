# import and plot Ramsar wetlands 

# Libraries and system setup
source('directorySet.R')

# Let's get libraries here
library(here)
library(tidyverse)
library(sf)
library(stars)
library(ggrepel)

# Directory to export TO
scriptOut <- file.path(datOut, 'WetlandBoundaries')
if (!dir.exists(scriptOut)) {dir.create(scriptOut, recursive = TRUE)}

# optional load of previous script outputs

load(file.path(scriptOut, "ramsarMDB.rdata"))  # ramsar polygons
load(file.path(scriptOut,"ramsarBoundsMDB.rdata")) # boundary of each set of polygons in each wetland complex``



# https://data.gov.au/data/dataset/04cd73cc-24d9-4ae9-aeaa-046a022cb592
# metadata html in folder
Ramsar <- read_sf(dsn = file.path(datDir, "RAMSARwetlandBoundaries/important_wetlands.shp")) %>%
  st_cast("MULTIPOLYGON") %>% # cleans up an issue with multi-surfaces
  st_make_valid()


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
ramsarMDB <- st_intersection(Ramsar, basin)
ramsarMDB <- st_transform(ramsarMDB, 3577)

save(ramsarMDB, file = file.path(scriptOut, "ramsarMDB.rdata"))
load(file.path(scriptOut, "ramsarMDB.rdata"))

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

ramsarBoundsMDB <- ramsarMDB %>%
  group_by(WNAME)%>%
  summarise()

names(ramsarBoundsMDB)[1] <- "Wetland"
ramsarBoundsMDB <- st_transform(ramsarBoundsMDB, 3577)

save(ramsarBoundsMDB, file = file.path(scriptOut,"ramsarBoundsMDB.rdata"))
load(file.path(scriptOut,"ramsarBoundsMDB.rdata"))
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

ggsave(filename = "AvocaRamsarWetlands.pdf" )  

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
 
ggsave(filename = "AvocaRamsarWetlandsZoomed2wetlandsWithLegend.pdf" )  

AvocaMarshesBounds <- ramsarBoundsMDB %>% 
  filter(Wetland == "First Marsh (The Marsh)" |
           Wetland == "Second Marsh (Middle Marsh)" |
           Wetland == "Third Marsh (Top Marsh)")

AvocaMarshBB <- st_bbox(AvocaMarshesBounds)
save(AvocaMarshBB, file = "C:/Users/amacq/Deakin University/QAEL - MER/Model/dataStrict/AvocaMarshBB.rdata")

AvocaMarshPolys <- st_crop(ramsarMDB, AvocaMarshBB)

AvocaMarshPolysPlus <- Avoca

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


ggsave(filename = "AvocaMarshlandsAreaOfBreedingDepth1_1_1988.png")

ggplot() + 
  geom_stars(data= breedDepth[,,1])
# get polygons with depth as gradient

#how to zoom in? 

# set coordinates

+xlim(0,100)+ylim(10,20) #?? works on geomstars?


# plot ANAEstrict = only breeding ANAE


ggplot()+
  # geom_sf(data = AvocaMarshBreedD1)+ #cropped breedDepth
  geom_sf(data =AvocaMarshANAEstrictD1, aes(fill = Area))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("Avoca marsh wetlands, 1 Jan 1988")

# eyeballing it, it doesn't seem like multiplying by breedANAE 
# (i.e. those matching the Heather's breeding ANAE types)
# changes the number of polygons. however the total area does drop.

sum(AvocaMarshANAEstrictD1$Area)  # 5114081
sum(AvocaMarshBreedD1$Area)  # 5250824

# Even more pronounced across the whole catchment...
sum(breedDepth$depth) # 921764681
sum(ANAEstrict$depth) # 347382030


# ---- Label Ramsar polygons with Ramsar names of wetland complexes (vs individual polys)

# import ramsar centroids
#  use buffer to select polys within a range and label? 

LTIM_Valleys <- read_sf(dsn = file.path(datDir, 'ANAE/MDB_ANAE_Aug2017/MDB_ANAE.gdb'), layer = 'LTIM_Valleys') %>%
  st_cast("MULTIPOLYGON") %>% # cleans up an issue with multisurfaces
  st_make_valid()
LTIM_Valleys <- st_transform(LTIM_Valleys, 3577)

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


#---- plot max extent area of inundaition across years (facet)
# try avoca





