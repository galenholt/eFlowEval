# import and plot Ramsar wetlands 


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

# and the basin boundary, might be useful, especially for clipping rasters
basin <- read_sf(dsn = file.path(datDir, 'ANAE/MDB_ANAE_Aug2017/MDB_ANAE.gdb'), layer = 'MDB_Boundary') %>%
  st_cast("MULTIPOLYGON") %>% # cleans up an issue with multisurfaces
  st_make_valid() %>%
  select(LEVEL2NAME) # no need for other info

st_layers(dsn = file.path(datDir, 'ANAE/MDB_ANAE_Aug2017/MDB_ANAE.gdb'))

#read in watercourses for context, plots
# Watercourses_ANAE created 2013 by brooks et al. 
watercourses <- read_sf(dsn = file.path(datDir, 'ANAE/MDB_ANAE_Aug2017/MDB_ANAE.gdb'), layer = 'watercourses_ANAE')

#filter to major rivers 
M_D <- watercourses%>% filter(., Name == c("MURRAY RIVER", "DARLING RIVER"))
# could filter by length to get a more manageable subset

#crop Ramsar to basin 
# RamsarMDB <- Ramsar[basin] # crops to bounding box
ramsarMDB <- st_intersection(Ramsar, basin)

save(ramsarMDB, file = file.path(myhome, "Source/datOut/WetlandBoundaries/ramsarMDB.rdata"))


plot(st_geometry(basin), bg = NA)
plot(st_geometry(LTIM_Valleys), add = TRUE, bg = NA)
# plot(st_geometry(watercourses), add = TRUE, col = "green"))  #thousands, of streams!!
plot(st_geometry(ramsarMDB), col = "blue", add = TRUE)

plot(st_geometry(M_D), add = TRUE, col = "light blue", lwd = 2)





