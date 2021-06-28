# load and analyze test data so don't have to make it every time
library(tidyverse)
library(sf)
# Data is in QAEL - MER/Model/Data/ANAE
# tempted to go with a Here, but should really have a library structure
# use here for now
# library(here)

# Argh. sort all this crap out later


myhome <- str_remove(path.expand("~"), "/Documents")
datDir <- file.path(myhome, "Deakin University/QAEL - MER/Model/dataBase") # "C:/Users/Galen/Deakin University/QAEL - MER/Model/dataBase"

datOut <- "datOut"

# load the data
load(file.path(datOut, 'LachlanANAE.rdata'))

# To allow plotting the ltim zones (otherwise their polygons get lost)
# And get the LTIM_Valleys to use to subset for toy models at scale, but not enormous scale
LTIM_Valleys <- read_sf(dsn = file.path(datDir, 'ANAE/MDB_ANAE_Aug2017/MDB_ANAE.gdb'), layer = 'LTIM_Valleys') %>%
  st_cast("MULTIPOLYGON") # cleans up an issue with multisurfaces

# LTIM areas
ltimNoNorth <- LTIM_Valleys %>%
  select(ValleyName, ValleyID, ValleyCode) # Three different ways to reference, basically

lachPlot <- ggplot() + 
  geom_sf(data = filter(ltimNoNorth, ValleyName %in% c("Lachlan")), 
          aes(fill = ValleyName, color = ValleyName), alpha = 0.5) +
  geom_sf(data = LachlanANAE, aes(fill = ANAE_DESC, color = ANAE_DESC)) +
  # Fill doesn't work without closed shape, as happens with the coord_sf call below
  # coord_sf(xlim = c(145.65, 145.71),
  #          ylim = c(-35.94, -35.89)) +
  theme_bw() + theme(legend.position = 'none')

lachPlot

