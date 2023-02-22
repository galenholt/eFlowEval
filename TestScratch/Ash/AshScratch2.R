
# testing script path
# C:\Users\amacq\source\MER_cc2\Scripts\DataProcessing\testingandscratch



datDir <- "C:/Users/amacq/Deakin University/QAEL - MER/Model/dataBase"

setwd("c:/Users/amacq/source/MER_cc2")

library("here")

setwd(path.expand("C:/Users/amacq/Documents/MER/CC2"))

datOut <- "datOut"

library(tidyverse)
library(stars)

library(raster)
library(sp)
library(ncdf4)
library(lubridate)


source(here('R',"strictlyFun.R"))

load(file.path(datOut, 'lachAll.rdata'))
load(file = file.path(datOut, 'kanDemo.rdata'))


whichcrs <- st_crs(lachAll)
bb = st_bbox(c(xmin = 144.4, ymin = -33.8, xmax = 144.6, ymax = -33.6), crs = whichcrs)

subAvgSM <- dailyPolySMavg[st_as_sfc(bb)]



b <- threshSM(avgSM = subAvgSM, dailyThresh = 0.01, 
              startDay = "2020-01-01", endDay = "2020-01-31", windowThresh = 20) 

c <- threshSM(subAvgSM, 0.01, "2020-01-01", "2020-01-31", 15)
d <- threshSM(subAvgSM, 0.01, "2020-01-01", "2020-01-31", 10)





