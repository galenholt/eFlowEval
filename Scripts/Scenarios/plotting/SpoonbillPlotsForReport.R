# royal spoonbill plots

# Libraries and system setup
source('directorySet.R')

# Let's get libraries here
library(here)
library(tidyverse)
library(sf)
library(stars)
library(ggrepel)
library(viridis)

# # Directory to export TO
# scriptOut <- file.path(datOut, 'WetlandBoundaries')
# if (!dir.exists(scriptOut)) {dir.create(scriptOut, recursive = TRUE)}

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


# handy bounding box for avoca marshes. 
load("C:/Users/amacq/Deakin University/QAEL - MER/Model/dataStrict/AvocaMarshBB.rdata")

# select months = march = only one relevent for southern basin
times <- st_get_dimension_values(breedStrict, "time")
breedTimes <- which(lubridate::month(times)== 03) 

# galen's figs show years 2014-2018
y <- c("2014", "2015", "2016", "2017", "2018")


marshBreed <- breedStrict[,,breedTimes]
subDates <- st_get_dimension_values(marshBreed, "time")
subDates <- which(lubridate::year(subDates) %in% y )

marshBreed <- marshBreed[,,subDates]

AvocaMarshPolys <- st_crop(ramsarMDB, AvocaMarshBB)
# error when trying to crop stars with sf object 
# crop only crops regular grids
# documentation crop a stars RASTER


ggplot() + 
  geom_stars(data = marshBreed[AvocaMarshBB])+
  coord_sf() +
  facet_wrap(~as.character(as.Date(time))) +
  theme_void() +
  scale_fill_viridis(option = 'viridis')

bbox_new <- BBoxAdj(AvocaMarshBB, 0.5,0.5,0,0.5)

ggplot() + 
  geom_stars(data = marshBreed[bbox_new])+
  coord_sf() +
  facet_wrap(~as.character(as.Date(time))) +
  theme_void() +
  scale_fill_viridis(option = 'viridis')+
  coord_sf(xlim = st_coordinates(bbox_new)[c(1,2),1], # min & max of x values
           ylim = st_coordinates(bbox_new)[c(2,3),2]) # min & max of y values
# Limiting the data to the bbox_new make plotting faster, though includes long polys
# setting coords as well limits actual plot window to bbox_new

ggplot() + 
  geom_stars(data = marshBreed[bbox_new])+
  coord_sf() +
  facet_grid(~as.character(as.Date(time))) +
  scale_fill_viridis(option = 'viridis')+
# guides(fill = guide_legend(title.position = 'top')) +
  theme_grey(base_size = 8) + 
  theme(
    legend.position = 'bottom',
        legend.background = element_blank(),
        legend.key.size = unit(0.3, 'cm'))+
  coord_sf(xlim = st_coordinates(bbox_new)[c(1,2),1], # min & max of x values
           ylim = st_coordinates(bbox_new)[c(2,3),2]) # min & max of y values

ggsave("AvocaMarshesAreaBreedInundation6monthsInSeason.pdf")

# TODO fix units, labels date = years, remove decimals on x-axis or rotate


# ---- Forage area plots ----

# For plot put Area from buffered area into original geometry

forageAreaBufferPlot <- forageAreaByWetland
forageAreaBufferPlot[[1]] <- forageAreaByWetlandBuffer[[1]]


forageAreaBufferPlot <- aggregate(forageAreaBufferPlot, "year", sum, na.rm = TRUE)
# for some reason this changes the order of dimensions 
forageAreaBufferPlot <- aperm(forageAreaBufferPlot, c(2,1))



y <- c("2014", "2015", "2016", "2017", "2018")

subDates <- st_get_dimension_values(forageAreaBufferPlot, "time")
subDates <- which(lubridate::year(subDates) %in% y )



ggplot()+
  geom_stars(data = forageAreaByWetland[,,194])

ggplot()+
  geom_stars(data = forageAreaByWetlandBuffer[,,194])

ggplot()+
  geom_stars(data = forageAreaBufferPlot[,,194])



# stars2 <- aperm(stars2, c(2,1))
forageAreaBufferPlot <- aperm(forageAreaBufferPlot, c(2,1))
# convert to Ha from m2
forageAreaBufferPlot[[1]] <- forageAreaBufferPlot[[1]]/10000

ggplot()+
  geom_stars(data = forageAreaBufferPlot[,,subDates]) +
  # geom_stars(data = st_geometry(forageAreaByWetlandBuffer))
  coord_sf() +
  facet_grid(~as.character(as.Date(time))) +
  scale_fill_viridis(option = 'plasma', limits = c(0,7e+08))+
  # guides(fill = guide_legend(title.position = 'top')) +
  theme_grey(base_size = 8)

ggsave("EdwardWakoolAreaForageInlcude10kBuffer.pdf")

# Line graph of forage area ----
wetlandName <- ramsarBoundEdwa$Wetland

forageAreaSF <- forageAreaBufferPlot %>% st_as_sf() 
forageAreaSF <- forageAreaSF %>% st_drop_geometry()
forageAreaSF <- forageAreaSF %>% pivot_longer(names_to = "Date", values_to = "Breed", cols = everything())
# forageAreaSF <- forageAreaSF %>%  group_by(Date)%>% summarise(NumBreed = sum(Breed)) %>% ungroup()
# forageAreaSF$Wetland <- rep(c(1:4), each = length(unique(forageAreaSF$Date)))
forageAreaSF$Wetland <- rep(wetlandName, each = length(unique(forageAreaSF$Date)))
forageAreaSF <- forageAreaSF %>% filter(Wetland != "Wakool-Tullakool Evaporation Basins")

ggplot(data = forageAreaSF, aes(x=lubridate::year(Date), y = Breed, group = Wetland, colour = Wetland))+
  geom_point()+ geom_line()+  # combine two geom types
  # hide x-axis label
  labs(x=element_blank(), y = "Forgaing habitat (ha)")+
  # place legend over the top of plot area to save space
  # values are x and y position as a proportion e.g. 0.5 is halfway across
  theme(legend.position = c(0.55, 0.8))

ggsave("EdwardWakoolAreaForageInlcude10kBufferPointAndLineLegend.png")

  
# ---- redo breed plots for eddywak ----

# breedAreaByWetland = #max area of inundation across 3 bimos in the breeding season aggregated to wetland complex




ggplot()+
  geom_stars(data = (aggregate(breedAreaByWetland/10000, by = "year", sum, na.rm = TRUE)[,subDates,])) +  # time first dim for some reason
  coord_sf() +
  facet_grid(~as.character(as.Date(time))) +
  scale_fill_viridis(option = 'viridis')+
  # scale_fill_viridis(option = 'viridis', limits = c(0,7e+08) )+ # limit works 
  # guides(fill = guide_legend(title.position = 'top')) +
  theme_grey(base_size = 8)

ggsave("EdwardWakoolAreaBreedInundation6monthsInSeason.pdf")

# get consistent gradient across plots - actually forage isn't strictly area

# Line graph would be better. What's the 

sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(1, 8))

max(breedAreaByWetland$Area)
max(forageAreaBufferPlot$Area)


# raster plot of breeding opportunities ---- 
numBreedEvents <- areaStricture %>% st_as_sf() 

# numBreedEventsByWetland_yr <- aggregate(numBreedEvents, by = "year", sum, na.rm = TRUE)

numBreedEvents <- numBreedEvents %>% st_drop_geometry()
numBreedEvents <- numBreedEvents %>% pivot_longer(names_to = "Date", values_to = "Breed", cols = everything())

wetlandName <- ramsarBoundEdwa$Wetland

# numBreedEvents$Wetland <- rep(c(1:4), each = length(unique(numBreedEvents$Date)))
numBreedEvents$Wetland <- rep(wetlandName, each = length(unique(numBreedEvents$Date)))

numBreedEvents[195:200,]
# numBreedEvents <- numBreedEvents %>%  group_by(Date)%>% summarise(NumBreed = sum(Breed)) %>% ungroup()

numBreedEventsByWetland_yr <- numBreedEvents %>% 
  group_by(Wetland, Year = lubridate::year(Date)) %>% 
  summarise(Breed_yr = sum(Breed))%>% filter(Wetland != "Wakool-Tullakool Evaporation Basins")



# ggplot() +
#   geom_raster(data = data, aes(x = samples, y = organism, 
#                                fill = cut(value, breaks = c(0, 3, 6, 9)))) +
#   scale_fill_manual(name = "value", 
#                     values = c("green", "yellow", "red"),
#                     labels = c("0-3", "4-6", "7-9"))

ggplot()+
  geom_raster(data = numBreedEventsByWetland_yr, aes(x = Year, y = Wetland, fill = Breed_yr))+
  labs(x=element_blank(), y = element_blank())+
  theme(legend.position = "none")
# theme(legend.position = "none", axis.text.x=element_blank(), axis.text.y = element_blank()) # hides tick labels

# what are the names of the wetlands in EddyWakool


# p = ggplot(dat, aes(x=Var1, y=Var2, fill=value)) +
#   geom_tile(colour="grey20") +
#   scale_fill_gradientn(colours = terrain.colors(10))

ggplot(data = numBreedEventsByWetland_yr, aes(x = Year, y = Wetland, fill = Breed_yr))+
  geom_tile(colour="grey20")+
  labs(x=element_blank(), y = element_blank())+
  theme(legend.position = "none")

ggsave("tile_plot.png", height=2, width=10, dpi=300)

ggplot()+
  geom_sf( data = ramsarBoundEdwa, aes(fill = Wetland))
  
ggplot()+
  geom_stars(data = areaStricture[,3,194], )

# 1 = Koondrook
# 2 = Millewa
# 3 = Evap ponds
# 4 = Werai

#  rasterplot breeding opportunities including foraging ----

numBreedEvents <- forageBreedStricture %>% st_as_sf() 
numBreedEvents <- numBreedEvents %>% st_drop_geometry()
numBreedEvents <- numBreedEvents %>% pivot_longer(names_to = "Date", values_to = "Breed", cols = everything())
wetlandName <- ramsarBoundEdwa$Wetland
numBreedEvents$Wetland <- rep(wetlandName, each = length(unique(numBreedEvents$Date)))

numBreedEvents[195:200,] #check labels in correct place

numBreedEventsByWetland_yr <- numBreedEvents %>% 
  group_by(Wetland, Year = lubridate::year(Date)) %>% 
  summarise(Breed_yr = sum(Breed))%>% filter(Wetland != "Wakool-Tullakool Evaporation Basins")
# get multiple e.g. 4 breed events per year, per wetland because unlike breed strictures forage is year round atm. 

ggplot(data = numBreedEventsByWetland_yr, aes(x = Year, y = Wetland, fill = Breed_yr))+
  geom_tile(colour="grey20")+
  labs(x=element_blank(), y = element_blank())+
  theme(legend.position = "none")

ggsave("EdwardWakForage5pcBreed70pc.png", height=2, width=10, dpi=300)


#  graph to demonstrate lookaround on foraging ---- 
# a. Individual polygons with foraging areas
# b. area scaled to wetland
# c. Area in buffer outlines
# Total area mapped onto wetlands 

# a.

#subset polygons to wetlands and plot area as fill. - why is this so hard!
forageANAEstrictPlot <- forageANAEstrict[,,194]
forageANAEstrictPlot <- st_as_sf(forageANAEstrictPlot)
forageANAEstrictPlot <- st_intersection(forageANAEstrictPlot, st_geometry(ramsarBoundEdwa) )

ggplot()+
  geom_sf(data = forageANAEstrictPlot, fill = forageANAEstrictPlot$`2020-03-01 11:00:00`)
sum(forageANAEstrictPlot$`2020-03-01 11:00:00`) # [1] 94557.42


#b. 

ggplot()+
  geom_stars(data = forageAreaByWetland[,,194])


ggplot()+
  geom_stars(data = forageAreaByWetlandBuffer[,,194])

ggplot()+
  geom_stars(data = forageAreaBufferPlot[,,194])
  

  





