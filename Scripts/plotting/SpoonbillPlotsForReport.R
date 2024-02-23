# royal spoonbill plots

# Libraries and system setup
source('directorySet.R')

# This seems to require that wetlandBoundaries.R and RoyalSpoonbill.R get run first. 
# We need to actually save the needed outputs from those and read them in.

# This is superseded by notebooks/spoonbill_plots.qmd

# Let's get libraries here
library(here)
library(tidyverse)
library(sf)
library(stars)
library(ggrepel)
library(viridis)
library(patchwork)

# # Directory to export TO
scriptOut <- file.path('strictOut', 'spoonbill')
if (!dir.exists(scriptOut)) {dir.create(scriptOut, recursive = TRUE)}


# Wetland boundaries from wetlandBoundaries.R
load(file.path(datOut, 'WetlandBoundaries', 'ramsarBoundsMDB.rdata'))
load(file.path(datOut, 'WetlandBoundaries', 'ramsarMDB.rdata'))

# Stricture results
load(file.path(datOut, 'Strictures', 'Spoonbill', 'EdwardWakool_spoonbill_strictures.rdata'))


# And get the ltimNoNorth to use to subset for toy models at scale, but not enormous scale
load(file.path(datOut, 'ANAEprocessed', 'ltimNoNorth.rdata'))
ltimNoNorth <- ltimNoNorth %>% st_transform(3577)

# and the basin boundary, might be useful, especially for clipping rasters
basin <- read_sf(dsn = file.path(datDir, 'ANAE/MDB_ANAE_Aug2017/MDB_ANAE.gdb'), layer = 'MDB_Boundary') %>%
  st_cast("MULTIPOLYGON") %>% # cleans up an issue with multisurfaces
  st_make_valid() %>%
  select(LEVEL2NAME) # no need for other info
basin <- st_transform(basin, 3577) #GDA94 / Australian Albers


# handy bounding box for avoca marshes. 
load(file.path(datDir, "dataStrict/AvocaMarshBB.rdata"))

# select months = march = only one relevent for southern basin
times <- st_get_dimension_values(spoonbillstricts$breedStrict, "time")
breedTimes <- which(lubridate::month(times)== 03) 

# galen's figs show years 2014-2018
y <- c("2014", "2015", "2016", "2017", "2018")


marshBreed <- spoonbillstricts$breedStrict[,,breedTimes]
subDates <- st_get_dimension_values(marshBreed, "time")
subDates <- which(lubridate::year(subDates) %in% y )

marshBreed <- marshBreed[,,subDates]

AvocaMarshPolys <- st_crop(ramsarMDB, AvocaMarshBB)
# error when trying to crop stars with sf object 
# crop only crops regular grids
# documentation crop a stars RASTER



# ---- Forage area plots ----

# For plot put Area from buffered area into original geometry

forageAreaBufferPlot <- spoonbillstricts$forageAreaByWetland
forageAreaBufferPlot[[1]] <- spoonbillstricts$forageAreaByWetlandBuffer[[1]]


forageAreaBufferPlot <- aggregate(forageAreaBufferPlot, "year", sum, na.rm = TRUE)
# for some reason this changes the order of dimensions 
forageAreaBufferPlot <- aperm(forageAreaBufferPlot, c(2,1))



y <- c("2014", "2015", "2016", "2017", "2018")

subDates <- st_get_dimension_values(forageAreaBufferPlot, "time")
subDates <- which(lubridate::year(subDates) %in% y )



ggplot()+
  geom_stars(data = spoonbillstricts$forageAreaByWetland[,,194])

ggplot()+
  geom_stars(data = spoonbillstricts$forageAreaByWetlandBuffer[,,194])

# aggressively skipping errors
# ggplot()+
#   geom_stars(data = forageAreaBufferPlot[,,194])



# stars2 <- aperm(stars2, c(2,1))
# forageAreaBufferPlot <- aperm(forageAreaBufferPlot, c(2,1))
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

ggsave(file.path(scriptOut, "EdwardWakoolAreaForageInlcude10kBuffer.pdf"))

# Line graph of forage area ----
ramsarBoundEdwa <- st_intersection(ramsarBoundsMDB, filter(ltimNoNorth, ValleyName =="Edward Wakool"))
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
  theme_pub(legend.position = c(0.55, 0.8))

ggsave(file.path(scriptOut, "EdwardWakoolAreaForageInlcude10kBufferPointAndLineLegend.png"),
       height = 8, width = 16, units = 'cm', dpi = 300)

  
# ---- redo breed plots for eddywak ----

# breedAreaByWetland = #max area of inundation across 3 bimos in the breeding season aggregated to wetland complex




breed_area <- ggplot()+
  geom_stars(data = (aggregate(spoonbillstricts$breedAreaByWetland/10000, by = "year", sum, na.rm = TRUE)[,subDates,])) +  # time first dim for some reason
  coord_sf() +
  facet_grid(~as.character(as.Date(time))) +
  scale_fill_viridis(option = 'viridis')+
  labs(fill = 'Area (ha)') +
  # scale_fill_viridis(option = 'viridis', limits = c(0,7e+08) )+ # limit works 
  # guides(fill = guide_legend(title.position = 'top')) +
  theme_grey(base_size = 8)
breed_area

ggsave(plot = breed_area, 
       filename = file.path(scriptOut, "EdwardWakoolAreaBreedInundation6monthsInSeason.png"),
       height=5, width=18, units = 'cm', dpi=300)
ggsave(plot = breed_area, 
       filename = file.path(scriptOut, "EdwardWakoolAreaBreedInundation6monthsInSeason.pdf"),
       height=5, width=18, units = 'cm', dpi=300)
# get consistent gradient across plots - actually forage isn't strictly area

# Line graph would be better. What's the 

# sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(1, 8))

max(spoonbillstricts$breedAreaByWetland$Area, na.rm = T)
max(forageAreaBufferPlot$Area)


# raster plot of breeding opportunities ---- 
numBreedEvents_area <- spoonbillstricts$breedAreaStricture %>% st_as_sf() 

# numBreedEvents_areaByWetland_yr <- aggregate(numBreedEvents_area, by = "year", sum, na.rm = TRUE)

numBreedEvents_area <- numBreedEvents_area %>% st_drop_geometry()
numBreedEvents_area <- numBreedEvents_area %>% 
  pivot_longer(names_to = "Date", values_to = "Breed", cols = everything()) |> 
  mutate(Date = stringr::str_remove_all(Date, ' .*'), Date = lubridate::ymd(Date)) |> 
  # make the NA in the first two periods FALSE
  mutate(Breed = ifelse(is.na(Breed), FALSE, Breed))

wetlandName <- ramsarBoundEdwa$Wetland

# numBreedEvents_area$Wetland <- rep(c(1:4), each = length(unique(numBreedEvents_area$Date)))
numBreedEvents_area$Wetland <- rep(wetlandName, each = length(unique(numBreedEvents_area$Date)))

numBreedEvents_area[195:200,]
# numBreedEvents_area <- numBreedEvents_area %>%  group_by(Date)%>% summarise(NumBreed = sum(Breed)) %>% ungroup()

numBreedEvents_areaByWetland_yr <- numBreedEvents_area %>% 
  group_by(Wetland, Year = lubridate::year(Date)) %>% 
  summarise(Breed_yr = sum(Breed, na.rm = TRUE))%>% 
  filter(Wetland != "Wakool-Tullakool Evaporation Basins")



# ggplot() +
#   geom_raster(data = data, aes(x = samples, y = organism, 
#                                fill = cut(value, breaks = c(0, 3, 6, 9)))) +
#   scale_fill_manual(name = "value", 
#                     values = c("green", "yellow", "red"),
#                     labels = c("0-3", "4-6", "7-9"))

ggplot()+
  geom_raster(data = numBreedEvents_areaByWetland_yr, aes(x = Year, y = Wetland, fill = Breed_yr))+
  labs(x=element_blank(), y = element_blank())+
  theme(legend.position = "none")
# theme(legend.position = "none", axis.text.x=element_blank(), axis.text.y = element_blank()) # hides tick labels

# what are the names of the wetlands in EddyWakool


# p = ggplot(dat, aes(x=Var1, y=Var2, fill=value)) +
#   geom_tile(colour="grey20") +
#   scale_fill_gradientn(colours = terrain.colors(10))

breed_tile <- ggplot(data = numBreedEvents_areaByWetland_yr, aes(x = Year, y = Wetland, fill = Breed_yr))+
  geom_tile(colour="grey20")+
  labs(x=element_blank(), y = element_blank())+
  theme_pub(legend.position = "none") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0))
breed_tile

ggsave(plot = breed_tile, filename = file.path(scriptOut, "breed_tile.png"), height=2, width=10, dpi=300)

yrc <- c('NA', 'grey20') |> setNames(c('internal', 'yearbreak'))
breedbim_tile <- ggplot(data = numBreedEvents_area |> 
                          filter(Wetland != "Wakool-Tullakool Evaporation Basins"),
                        aes(x = Date, y = Wetland, fill = as.numeric(Breed)))+
  geom_tile(colour="grey20")+
  # geom_tile() +
  # scale_color_manual(values = yrc) +
  labs(x=element_blank(), y = element_blank())+
  theme_pub(legend.position = "none") +
  scale_x_date(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0))
breedbim_tile


ggplot()+
  geom_sf( data = ramsarBoundEdwa, aes(fill = Wetland))
  
ggplot()+
  geom_stars(data = spoonbillstricts$breedAreaStricture[,3,194], )

# 1 = Koondrook
# 2 = Millewa
# 3 = Evap ponds
# 4 = Werai

#  rasterplot breeding opportunities including foraging ----

numBreedEvents_forage <- spoonbillstricts$forageBreedStricture %>% st_as_sf() 
numBreedEvents_forage <- numBreedEvents_forage %>% st_drop_geometry()
numBreedEvents_forage <- numBreedEvents_forage %>% pivot_longer(names_to = "Date", values_to = "Breed", cols = everything())
wetlandName <- ramsarBoundEdwa$Wetland
numBreedEvents_forage$Wetland <- rep(wetlandName, each = length(unique(numBreedEvents_forage$Date)))

numBreedEvents_forage[195:200,] #check labels in correct place

numBreedEvents_forageByWetland_yr <- numBreedEvents_forage %>% 
  group_by(Wetland, Year = lubridate::year(Date)) %>% 
  summarise(Breed_yr = sum(Breed, na.rm = TRUE)) %>%
  filter(Wetland != "Wakool-Tullakool Evaporation Basins")

forage_breed_tile <- ggplot(data = numBreedEvents_forageByWetland_yr, aes(x = Year, y = Wetland, fill = Breed_yr))+
  geom_tile(colour="grey20")+
  labs(x=element_blank(), y = element_blank())+
  theme_pub(legend.position = "none") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0))
forage_breed_tile

ggsave(plot = forage_breed_tile, filename = file.path(scriptOut, "forage_breed_5pcBreed70pcForage.png"), height=2, width=10, dpi=300)



# Just forage -------------------------------------------------------------

# get multiple e.g. 4 forage events per year, per wetland because unlike breed strictures forage is year round. (the breed*forage above) So we need to just ask if there were any.

foragealone <- spoonbillstricts$forageAreaStricture %>% 
  st_as_sf() %>%
  st_drop_geometry() %>%
  pivot_longer(names_to = "Date", values_to = "Forage", cols = everything()) |> 
  mutate(Forage = as.numeric(Forage > 0)) |> # Forage is in area, but for this simplification, we just want yes/no.
  mutate(Date = stringr::str_remove_all(Date, ' .*'), Date = lubridate::ymd(Date))

foragealone$Wetland <- rep(wetlandName, each = length(unique(foragealone$Date)))

foragealone[195:200,] #check labels in correct place

foragealoneByWetland_yr <- foragealone %>% 
  group_by(Wetland, Year = lubridate::year(Date)) %>% 
  summarise(Forage_yr = as.numeric(sumna(Forage) > 0)) %>%
  filter(Wetland != "Wakool-Tullakool Evaporation Basins")
# get multiple e.g. 4 breed events per year, per wetland because unlike breed strictures forage is year round atm. 

forage_tile <- ggplot(data = foragealoneByWetland_yr, aes(x = Year, y = Wetland, fill = Forage_yr))+
  geom_tile(colour="grey20")+
  labs(x=element_blank(), y = element_blank())+
  theme_pub(legend.position = "none") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0))
forage_tile

ggsave(plot = forage_tile, filename = file.path(scriptOut, "forage_70pcForage.png"), height=2, width=10, dpi=300)



foragebim_tile <- ggplot(data = foragealone |> 
                           filter(Wetland != "Wakool-Tullakool Evaporation Basins"),
                         aes(x = Date, y = Wetland, fill = Forage, 
                             color = yearblock))+
  geom_tile(colour="grey20")+
  # geom_tile() +
  # scale_color_manual(values = yrc)+
  labs(x=element_blank(), y = element_blank())+
  theme_pub(legend.position = "none") +
  scale_x_date(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0))
foragebim_tile

# Combine the tiles

# ggpubr::ggarrange(breed_tile, forage_breed_tile, nrow = 2, labels = c('a', 'b'))
breed_tile/forage_breed_tile + plot_annotation(tag_levels = 'a')

ggsave(plot = breed_tile/forage_breed_tile + plot_annotation(tag_levels = 'a'), 
       filename = file.path(scriptOut, "spoonbill_tile_panels.png"), 
       height=6, width=12, units = 'cm', dpi=300)

ggsave(plot = breed_tile/forage_breed_tile + plot_annotation(tag_levels = 'a'), 
       filename = file.path(scriptOut, "spoonbill_tile_panels.pdf"), 
       height=6, width=12, units = 'cm')

# ggpubr::ggarrange(breed_tile, forage_breed_tile, nrow = 2, labels = c('a', 'b'))
breed_tile/forage_tile/forage_breed_tile + plot_annotation(tag_levels = 'a')

ggsave(plot = breed_tile/forage_tile/forage_breed_tile + plot_annotation(tag_levels = 'a'), 
       filename = file.path(scriptOut, "spoonbill_tile_all_panels.png"), 
       height=6, width=12, units = 'cm', dpi=300)

ggsave(plot = breed_tile/forage_tile/forage_breed_tile + plot_annotation(tag_levels = 'a'), 
       filename = file.path(scriptOut, "spoonbill_tile_all_panels.pdf"), 
       height=6, width=12, units = 'cm')


# the bimonth/overall
all_bimtiles <- breedbim_tile/foragebim_tile/forage_breed_tile + plot_annotation(tag_levels = 'a')
all_bimtiles

ggsave(plot = all_bimtiles, 
       filename = file.path(scriptOut, "spoonbill_tilebim_all_panels.png"), 
       height=9, width=12, units = 'cm', dpi=300)

ggsave(plot = all_bimtiles, 
       filename = file.path(scriptOut, "spoonbill_tilebim_all_panels.pdf"), 
       height=9, width=12, units = 'cm')
#  graph to demonstrate lookaround on foraging ---- 
# a. Individual polygons with foraging areas
# b. area scaled to wetland
# c. Area in buffer outlines
# Total area mapped onto wetlands 

# a.

#subset polygons to wetlands and plot area as fill. - why is this so hard!
forageANAEstrictPlot <- spoonbillstricts$forageANAEstrict[,,194]
forageANAEstrictPlot <- st_as_sf(forageANAEstrictPlot)
forageANAEstrictPlot <- st_intersection(forageANAEstrictPlot, st_geometry(ramsarBoundEdwa) )

# these are the individual ANAEs
ggplot()+
  geom_sf(data = forageANAEstrictPlot, aes(fill = `2020-03-01 11:00:00`))

sum(forageANAEstrictPlot$`2020-03-01 11:00:00`) # [1] 94557.42


#b. 

# forageAreabyWetland is the sum of area within each ramsar
ramsar_forage_area <- ggplot()+
  geom_stars(data = spoonbillstricts$forageAreaByWetland[,,194])

# forageAreaByWetlandBuffer puts a 10k buffer around each
ramsar_forage_buffer <- ggplot()+
  geom_stars(data = spoonbillstricts$forageAreaByWetlandBuffer[,,194])

# This one is aggregated to year from the others. It's unclear how this ever would have worked.
# Could change the index, but then we're not comparing the same things.
# ggplot()+
#   geom_stars(data = forageAreaBufferPlot[,,194])
  

  





