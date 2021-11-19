# file to adjust inundation volume

# let's do this by taking the inundation volumes and adding 10%

# need to read in the inundations in the ANAEs, add 10%, and save somewhere else

# Header from the temperature file to retain all the directories,  --------
source('directorySet.R')

# Let's get libraries here, then sort out git then sort out making this a
# library so we don't have to deal with all the library crap
library(here)
library(tidyverse)
library(sf)
# library(RNetCDF) # Probably not, raster can handle geographic netCDF
library(stars)
library(foreach)
library(doFuture)

# source('Functions/rastPolyJoin.R')

# Set up parallel backend
registerDoFuture()
plan(sequential) # no need to parallelize, I don't think


# modify -------------------------------------------------------------
add10p <- function(x) {
  x2 <- x*1.1
}


system.time(modifyANAE(outerDir = file.path(datOut, 'Inundationprocessed'),
                       summaryFuns = c('volInun'),
                       returnDir = 'vol10p',
                       FUN = add10p))

