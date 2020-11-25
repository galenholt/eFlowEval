

# strictures will be manually input as formula i.e. hard-coded
# the predictive variables required will be hard-coded as necessary




## - check conditions at time t meet strictures or not.
#        - sequentially from most to least important
#  if yes, update 'persistence' variable
# - if conditions are favourable with regards to stricture persistent = Y
#         - or number (proportion) of strictures met
#  make above a function and apply to each polygon at each timestep
#        - apply persist <- strict(taxa)
#       - persist_taxa1

#  ***Are the most important strictures for persistence in poly overcome? ***
#  = first pass.  Later passes include % cover etc.
# (probability of immigration?)
#  Scaling up 
#  output = number of timesteps that meet conditions in 5 year period
#  proportion of taxa across themes that are persistent across year?




# slice filters to specific layers (= times) by index
# star1[polgygon1] clips to poly extent

datDir <- "C:/Users/amacq/Deakin University/QAEL - MER/Model/dataBase"

setwd(path.expand("C:/Users/amacq/Documents/MER/CC2"))


datOut <- "datOut"

library(tidyverse)
library(stars)

library(raster)
library(sp)
library(ncdf4)


soilMstars <- read_ncdf(file.path(datDir, 'soilmoisture/sm_pct_2020_Actual_day.nc'))


basin <- read_sf(dsn = file.path(datDir, 'ANAE/MDB_ANAE.gdb'), layer = 'MDB_Boundary') %>%
  st_cast("MULTIPOLYGON")  %>% # cleans up an issue with multisurfaces
  dplyr::select(LEVEL2NAME)

basinMatch <- st_transform(basin, st_crs(soilMstars)) # datum transformation

soilBasin <- soilMstars[basinMatch]

rm(soilMstars)


soilBasinT4 <- dplyr::slice(soilBasin, time, c(1, 61, 121, 181))
plot(soilBasinT4)
#  nested lists
#  sm_pct is an attr = stars_raster and  contains 3 lists num [1:280, 1:263, 1:4]
#        $long, $lat, $time, 
#         each with their own sublists and attributes(which can also have sublists)

# aggregate to valleys
# this is where Galen had problem of adjacent polygons squabbling over pixels they share.
# do aggregate on raster then convert to sf?

soilRas <- raster(file.path(datDir, 'soilmoisture/sm_pct_2020_Actual_day.nc'))
basinSP <- SpatialPolygons()


# soilMstarsCut <- soilMstars[,,,2:208]
weekmeans <- aggregate(soilMstars, by = 'weeks', FUN = mean, na.rm = TRUE)


# aperm --------------
# interchange the first two subscripts on a 3-way array x
x  <- array(1:24, 2:4) 
xt <- aperm(x, c(2,1,3)) # 3 x 2 x 4
xt2 <- aperm(x, c(1,3,2)) #2 x 4 x 3

dim(x)
dim(xt)
x[2,3,4]
xt[2,3,4] # out  of bounds

#what is the position of value Y in array X 

which(x==23) #23
which(xt==23) #21

23/(dim(xt))

stopifnot(t(xt[,,2]) == x[,,2],
          t(xt[,,3]) == x[,,3],
          t(xt[,,4]) == x[,,4])

UCB <- aperm(UCBAdmissions, c(2,1,3))
UCB[1,,]
summary(UCB) # UCB is still a contingency table

# ------ load lachAll subset for testing ----

load(file.path(datOut, 'lachAll.rdata'))

load(file = file.path(datOut, 'kanDemo.rdata'))

# variable currently available
# wieghted mean soil moist / poly / day    = dailyPolySMavg
# maximum soil moisture past 10 days /poly / day     = polyTimeSmax_10
# soil moisture averaged over 7 days per raster cell, converted to / poly / day  = weeklyRastavgSMpoly
# weighted mean soil moisture /poly (intersected to create new polys) /day

# list of ANAE types OR list of individual polys where it is present

table(anaeSub$ANAE_DESC)
# Riparian or floodplain shrubland  Pt1.8.1   84
# Freshwater meadow    Pt2.3.2     37


# might need to filter by ANAE before creating predictor variable brick e.g. soil moisture
# but then you would have to do bricks for each taxa if they were in different subsets of ANAE type
# better to preserve ANAE_CODE? 

#right now just work with what I've got. 

# do I save stricture x time brick? Or just roll across them in time. 

# 1> make a stricture x time brick for soil moisture > 0.01

# replicate brick and strip, aside from ID and Shape

names(lachAll)
whichcrs <- st_crs(lachAll)

bb = st_bbox(c(xmin = 144.4, ymin = -33.8, xmax = 144.6, ymax = -33.6), crs = whichcrs)


taxa1 <- lachAll%>%
  select(SYS2,SYSID,ANAE_CODE, Shape) 

taxa1Sub <- st_crop(taxa1, st_as_sfc(bb))

# subAvgSM <- st_crop(dailyPolySMavg, st_as_sfc(bb)) #fails
subAvgSM <- dailyPolySMavg[st_as_sfc(bb)]

plot(subAvgSM[ , , 1])
subAvgSMt10 <- subAvgSM[ , , 10]

## ---- deprecated ----
# taxa1Sub <- taxa1Sub %>%
#   mutate(str1 = if_else(subAvgSMt1$sm_pct>0.002, 1, 0))
# 
# #counting the number of strictures passed
# taxa1Sub <- taxa1Sub %>%
#   mutate(str1 = if_else(subAvgSMt1$sm_pct>0.002, 1, 0))%>%
#   mutate(str1 = if_else(subAvgSMt1$sm_pct>0.002, str1+1, str1))
# -------


# keep coming back to the timestep. the different strictures will have to align
# yearly timestep. e.g. if soil surface temp exceed 50degress for 1 week at any point location fails for year 


plot(taxa1Sub)

summary(subAvgSMt1$sm_pct)

# I guess this is a slow way to do this. 

# Galen mentioned something about crop? cutting off polygons at edge vs []?
# plot(anaeSub[,'ANAE_DESC'])

plot(taxa1Sub[,,10:13])

plot(taxa1Sub$SYS2)

head(taxa1Sub)
dim(taxa1Sub)

# probably failing because I taxa1 is only 2 D and lacks time dimension
#try on 1D rick

#  demonstrate that i can test multiple strictures and combine into single score
# Dont worry too much about the best / fastest way to do it just yet. 
#  **scale of test. do I put is on each 
# how many daily passes of test = passing stricture? write stricutre in terms of years.

# make a time x poly matrix replace with 1/0
# can delete it after - automatic if in function

# test on subset 4 x t then time how long for daily


SMthresh <-  0.08

taxa2sub <- subAvgSM



taxa2sub$sm_pct <- if_else(subAvgSM$sm_pct>SMthresh, 1, 0)

plot(taxa2sub[,,60])

head(taxa2sub[[2]])

names(taxa2sub)
# stars = list of arrays


dim(subAvgSMt1)
length(subAvgSM$sm_pct)/240 #208 = Time
# a stars object = list of arrays
# only 1 array in list, but has 2 dimensions: shape and time ~labels?

array1 <- array(c(-5.5,6,3),dim = c(3,4,3)) 
array1[,,2]

apply(array1, MARGIN=c( 2), sum)



st_dimensions(taxa2sub)[2] 



a<- filter(taxa2sub, st_dimensions(taxa2sub)[2]  == "2020-01-01 12:00:00")
a<- filter(taxa2sub, st_dimensions(taxa2sub)[2]  == "2020-01-01 12:00:00")
a <- filter(st_as_sf(taxa2sub), time == "2020-01-01 12:00:00")

a <- taxa2sub[,,"2020-01-01 12:00:00"] # does nothing

a<- st_as_sf(taxa2sub) #lose 

# # change the time dimension
# st_dimensions(avgPRStars)[2] <- st_dimensions(rastst)[3]
# # st_dimensions(avgPRtars)[2] # yup, though it's still called X1?
# names(st_dimensions(avgPRStars))[2] <- names(st_dimensions(rastst))[3]
# # and change the name of the attribute
# names(avgPRStars) <- names(rastst)

# how do I use attr to e.g. filter, subset
# how do I apply operations across attributes

# could convert to sf? then use dplyr
# purrr? - about working with list elements
# st_apply


taxa2subA <- taxa2sub %>% st_apply(., MARGIN = 2, sum )  #across Shape (gives 1d obj, length (time))
taxa2subB <- taxa2sub %>% st_apply(., MARGIN = 1, sum )  # across time ()
# If MARGIN=1, the function accepts each row of X as a vector argument, and returns a vector of the results
dim(taxa2sub)

# all_of used to indicate variable external to function call e.g.
# group_by(across(all_of(grouper)))



# make a function

# filter to SYS2 at taxa level
tax1SM <- intPR %>% filter(ANAE_CODE == "Pt1.8.1")

table(intPR$ANAE_CODE) # really slow 
# F.1.8    F1.2    F1.6    F2.2    F2.4    F3.2   Lp1.1   Lt1.1 Pp2.1.2 Pp2.2.2 Pp2.3.2   Pp4.2    Pps5 
# 36       3       1       8      29       2     359     211      11       7      28     292       7 
# Pst2.2 Pt1.1.1 Pt1.1.2 Pt1.2.1 Pt1.2.2 Pt1.4.1 Pt1.6.1 Pt1.6.2 Pt1.7.1 Pt1.7.2 Pt1.8.1 Pt1.8.2 Pt2.1.2 
# 987   15619     307   12025     383       1     812      87    1381     137    2833     378      74 
# Pt2.2.2 Pt2.3.2 Pt3.1.2   Pt4.2   Rp1.1   Rp1.2   Rp1.3   Rp1.4   Rt1.2   Rt1.3   Rt1.4 
# 2222    1481     153      55     421      71       6     935       4      14     874 
table(lachSub$SYS2)

startDay <- "2020-01-01"
endDay <- "2020-01-31"
avgSM <- subAvgSM
dailyThresh <- 0.01 # soil moisture required 
windowThresh <- 10 # days required above threshold

# return stars with single slice == whether soil moisture stayed above a critical threshold for total X day during
# window startDay:EndDay

# Galen note: I think this can be done with aggregate, feeding it intervals instead of somethign like 'years'
  # But would need to be a stars, not an sf

threshSM <- function(avgSM = avgSM, dailyThresh = 0.01, startDay = NULL, endDay = NULL, windowThresh = NULL){
  
  dayz <- st_get_dimension_values(avgSM, "time")
  dayz <- date(dayz)
  if(is.null(startDay))  startDay <- dayz[1]
  if(is.null(endDay))  endDay <- dayz[length(dayz)]
  if(is.null(windowThresh)) windowThresh <- length(dayz)
  
  indDayz <- which(dayz == date(startDay)):which(dayz == endDay)
  
  if(windowThresh>length(indDayz)) 
  {stop("windowThresh must be < or =  number of days in time window")}
  # print(startDay)
  # print(endDay)
  
  avgSMdayz <- avgSM %>% slice(time, indDayz)
  
  avgSMdayz$sm_pct <- if_else(avgSMdayz$sm_pct > dailyThresh, 1, 0)
  
  #number of days soil moisture above threshold
  sumSMdayz <- st_apply(avgSMdayz, MARGIN = 1, sum)
  SMstr <- sumSMdayz %>% mutate( str = if_else(sum > windowThresh, 1,0),
                                 sum = NULL) 
  # print(paste0("number of days:",  date(endDay)-date(startDay)+1))
  return(SMstr)
}

g <- threshSM(subAvgSM, 0.01, "2020-01-01", "2020-01-31", 20)
g <- threshSM(subAvgSM, 0.01, endDay = "2020-01-31", windowThresh = 20)
g <- threshSM(subAvgSM, 0.01, endDay = "2020-01-31", windowThresh = 30)


system.time(f <-  threshSM(dailyPolySMavg, 0.01, "2020-01-01", "2020-01-31", 20)
)



  

  
  
# list of strictures = list of functions.  parameter values = sublist? 

#double brackets gives it as an array 
  

# Combine the outcome stars obj into a list or stack where dim 3 = str? sum across to get number passesd
# or 3/3 = pass
# stricutres could be weighted by rank. 
#  

# ** If I am subsetting to taxa distributions to ANAE_CODE need to do that before rastPolyJoin as it returns stars minus ANAE_CODE
  # G note: I'd keep all ANAE codes for consistency/safety, and zeros are real, and so better to keep than discard


# ------- Whats the benefit of st_dim_to_attr ?? ----
tif = system.file("tif/L7_ETMs.tif", package = "stars")
x1 = read_stars(tif)
(x = st_dim_to_attr(x1))
plot(x)
(x = st_dim_to_attr(x1, 2:3))
plot(x)
(x= st_dim_to_attr(x1, 3))
plot(x)


# ------ Combine multiples strictures into one object


# combine multiple slices together

b <- threshSM(subAvgSM, 0.01, "2020-01-01", "2020-01-31", 20)
b <- threshSM(avgSM = subAvgSM, dailyThresh = 0.01, 
              startDay = "2020-01-01", endDay = "2020-01-31", windowThresh = 20) 

c <- threshSM(subAvgSM, 0.01, "2020-01-01", "2020-01-31", 15)
d <- threshSM(subAvgSM, 0.01, "2020-01-01", "2020-01-31", 10)

# create an empty stars object and replace values?
# convert to sf and do cbind there?

# ------ Deprecated -----
# taxa1str <- subAvgSM[,,1:3]
# 
# names(taxa1str) <- "strictTF"
# 
# taxa1str$strictTF <- c(b$str, c$str, d$str) #works but second dim still has time attributes
# attributes(taxa1str)
# taxa1str <- st_set_dimensions(taxa1str,"time", values = c(1,2,3), names = "strNum") #fixed!
# ----

# could also try c(b,c)  # https://r-spatial.github.io/stars/reference/c.stars.html
# or merge/split?

#next combine into an overall assessment across strictures
# actually probably better to have each stricutre in a separate star with year as the second dim
# add on extra slices year by year. 
# if we want year Y strictures to depend on answer for Y-1 then need to run each stricutre before moving to the next year
# therefore we will have a summary star


yrs <- unique(year(st_get_dimension_values(subAvgSM, "time")))  # eventually set by umbrella function which extracts yearly chunks from dependent variable bricks
#year() call drops posix class.  Not sure how to get it back...
# yrs <- as.Date("yrs", format= "%Y")  #nope

taxa1str <- subAvgSM[,,1:length(yrs)]
taxa1str <- st_set_dimensions(taxa1str,"time", values = yrs, names = "yrs") # yrs not posix


# G comment: this can get huge. If we keep the strictures separate but with same
# dimensions, we can also avoid the applys, and just do element-wise operations,
# which will all be C-optimized

e <- c(b,c,d, along = "strNum") # THIS combines 3 stars object into a stack with each as a slice 


# for taxa summarise across strictures 

# # proportion of strictures met
f <- st_apply(e, MARGIN = 1, function(x){sum(x)/length(x)}) 

# All strictures met?
g <- st_apply(e, MARGIN = 1, function(x){ifelse(sum(x)/length(x)==1,1,0)})  #

head(g)
head(g$str)

for(yr in yrs){
  
  x <- strict()
  
  
  
}


  
strList <-  list(threshSM(subAvgSM, 0.01, "2020-01-01", "2020-01-31", 20), 
                 threshSM(subAvgSM, 0.01, "2020-01-01", "2020-01-31", 15),
                 threshSM(subAvgSM, 0.01, "2020-01-01", "2020-01-31", 10))
# runs threshSM rather than storing call and ingredients




# lapply(function.list, do.call, list()) #list apply - returns list


# strict <- function(strList, yrs, ){
#   
#   
#   
#   
# } 
  

# units::set_units()



# figure out how to filter to Veg type (or poly list) using SYSID | 

#make table =~ sf table minus geometry?

# use lachAll for now. can trim unused variables when we go big. 

Taxa1ANAE <- c("Pt1.2.1","Pt1.8.1" )
x <- left_join(st_as_sf())
x <- lachAll %>% filter(ANAE_CODE == Taxa1ANAE)

# solution for bounding discrepancy = extract list of sysids from larger (subDailySM), filter lachAll
# then left_join filtered lachAll to subDaily

# SISIDs seem to be gone from the stars object. Did Galen paste them back on 
# and that was the concern about the possible misalignment?

bbIDs <- st_get_dimension_values(subAvgSM, "SYS2") #NA


#sub lachAlll to bounding box

# ANAE
anaeSub <- st_crop(lachAll, st_as_sfc(bb))
plot(anaeSub[,'ANAE_DESC'])

# Bounds of these not the same... 
# subAvgSM1 fits within subAvgSM

subAvgSM <- dailyPolySMavg[st_as_sfc(bb)]
subAvgSM1 <- st_crop(st_as_sf(dailyPolySMavg),  st_as_sfc(bb))

dailyPolySub <- st_crop(st_as_sf(dailyPolySMavg[,,10:13]), st_as_sfc(bb))



plot(subAvgSM[,,10:13]) #stars
plot(subAvgSM1) #sf
dim(subAvgSM1)

plot(dailyPolySub[,,1])

plot(dailyPolySMavg[,,10:13])


# centroids vs boundry inclosed in box leads to some polygons to be excluded from box
# conversely some polys which extend well beyond the box can be included, pushing our the spatial extent


# ---- shortcutting rastpolyjoin to return SF ----

system.time(dailySMpolyavg <- rastPolyJoinSFout(polysf = lachAll, rastst = lachSoil, grouper = 'SYS2', maintainPolys = TRUE))

avgSMsf <-  dailySMpolyavg[[1]]
avgSMsfInd <-  dailySMpolyavg[[2]]
all(avgSMsfInd$SYS2 == lachAll$SYS2)

save(avgSMsf, 
     avgSMsfInd,
     file = file.path(datOut, 'avgSMsf.rdata'))

load(file.path(datOut, 'avgSMsf.rdata'))


# ---- re-write thresh SM to work on SF ----
startDay <- NULL
endDay <- NULL



threshSMsf <- function(avgSMsf = avgSMsf, dailyThresh = 0.01, startDay = NULL, endDay = NULL, windowThresh = NULL){
  # source(strictlyFun)
  # strip off hh:mm:ss  # put outside function later 
  names(avgSMsf)[2:(length(names(avgSMsf))-1)]  <- str_sub(names(avgSMsf)[2:(length(names(avgSMsf))-1)],  0,-10)
  # which select starts_with

  # extract dates from column names
  dayz <- col2date(avgSMsf)  #really only need start and end dates here
  
  if(is.null(startDay))  startDay <- dayz[1]
  if(is.null(endDay))  endDay <- dayz[length(dayz)]
  if(is.null(windowThresh)) windowThresh <- length(dayz)/2
  
  if(windowThresh>length(dayz)) 
  {stop("windowThresh must be < or =  number of days in time window")}
  # print(startDay)
  # print(endDay)
  startDay <- as.Date(startDay, origin = "1970-01-01")
  endDay <- as.Date(endDay, origin = "1970-01-01")
  
  # days of interest
  dayx <- as.Date(startDay:endDay, origin = "1970-01-01")
  # dayx <- startDay:endDay
  
  dayx <- date2col(dayx) 
  
  avgSMdayx <- avgSMsf[, c("SYS2", dayx)] 

  # remove geometry to apply tests
  
  avgSMdayx <- st_drop_geometry(avgSMdayx[dayx]) > dailyThresh
 # number of days above threshold
  SMstr <- apply(avgSMdayx, 1, sum)
  # sufficient no. of days above threshold?
  SMstr <- as.numeric(SMstr >= windowThresh) 
  # add on SYS and geometry
  SMstr <- cbind(avgSMsf["SYS2"],SMstr) 

  # print(paste0("number of days:",  date(endDay)-date(startDay)+1))
  return(SMstr) #return 1 column 1/0 (+sys2 and geom) stricture sf
}

# G note: So, fundamentally, this is asking whether > 20 days in Jan 2020 are above soil moisture of 1%
  # Then the plan would be to loop over months?
  # Basically, this is windowed rather than rolling, and then summed to check.
  # Just thinking through implementation both ways, either are possible in the
  # stars method too, but rolling 30-day (or 28, or whatever) wouldn't impose
  # arbitrary breakpoints at month transitions. Then could have the roll sum up
  # days with > 0.01 over 30 days, and the stricture test ask if that is > 20
b <- threshSMsf(avgSMsf = avgSMsf, dailyThresh = 0.01, 
              startDay = "2020-01-01", endDay = "2020-01-31", windowThresh = 20) 




# avgSMdayz[dayx] <- lapply(avgSMdayz[dayx], function(x) {(x *2)})
# c <- lapply(avgSMdayz[dayx], function(x) {(x *2)})
# By2 <- function(x) {(x +2)}
# d <- apply(avgSMdayz[], 2, By2)


# ---- scracting sturcture ----

# load a full data set 

load(file.path("C:/Users/amacq/Deakin University/QAEL - MER/Model/Outputs/lachSoilMoistprocessedAllOut.rdata"))
# lachAll, 
# deProxySoil, = ignore
# dailyPolySMavg, means in polys
# soilMoistMin42, = rolling minimum with 42 day lookback
# soilMoistMin5, 
rm(deProxySoil)

# lachAll,dailyPolyTempavg,
# soilTempMax28 =  rolling max temp lookback 28 days


#cutt to BB
# SMmin42bb <- st_crop(soilMoistMin42, bb)
SMmin42bb <- soilMoistMin42[st_as_sfc(bb)]
class(SMmin42bb)

plot(SMmin42bb[,,1])

str(soilMoistMin42)

crs(bb)
crs(soilMoistMin42)

# -----

lachbb <- st_crop(lachAll,bb)

# G note: I've been thinking these would be logical checks as well: Give 1s to
# the ANAE codes and 0s elsewhere. Easy to do to either stars or sf

Taxa1ANAE <- c("Pt1.2.1","Pt1.8.1")

taxa1ind <- which(lachAll$ANAE_CODE == Taxa1ANAE)
taxa1indbb <- which(lachbb$ANAE_CODE == Taxa1ANAE)

Str <- lachAll[,]

x <- soilMoistMin42[,taxa1ind,1]
plot(x) # works with errors no non-missing args to min...etc.
x <- SMmin42bb[,taxa1ind,1]
plot(x) # doesnpt plot, with same errors 


dim(soilMoistMin42[[1]])

plot(SMmin42bb[,taxa1ind,1])



#initilise stricutre stars

# make blank lach star
strX <- SMmin42bb[,,1]
strX$sm_pct <- NULL  # remove attr   1.3 MB
strX$str <- 0  # add attribute
strX[1]
strX[,taxa1indbb,] <- 2

strX$str[taxa1indbb]<-1  #works
plot(strX) 



  
lachNull <- soilMoistMin42[,,1]
lachNull[[1]] <- NULL
save(lachNull, file = file.path(datOut, 'lachNull.rdata'))
lachBB <- lachNull[st_as_sfc(bb)]

str1 <- lachBB
str1_1 <- c(str1, strX)  # works - adds attribute to Null 

str1 <- c(str1, 
          threshSM(avgSM = subAvgSM, dailyThresh = 0.01, 
                   startDay = "2020-01-01", endDay = "2020-01-31", windowThresh = 20)
          )

b <- threshSM(avgSM = subAvgSM, dailyThresh = 0.01, 
              startDay = "2020-01-01", endDay = "2020-01-31", windowThresh = 20) 

str1 <- c(str1, b)
# str1$str <- 1

# output of b is 1D (dim = Shape), whereas str1 has 2D, (shape, time)
# why does str have a time element??



# one seed set event = 1 day in moist10 =1 
moist10 <- soilMoistMin42 > 0.1
seedSet <- sum(moist10$sm_pct)>1  
# surely you want the germination event to be followed in time by good soil moisture 
# i.e. 2 strictures passed in time sequence. 

sum(moist10$sm_pct)
table(moist10)


#what years do we have
summary(st_get_dimension_values(moist10, "time"))
# 2014-01-01 00:00:00, 2020-10-27 00:00:00



#----
#slice moist to 

startYr <- ""
yrs <- unique(year(st_get_dimension_values(SMmin42bb, "time")))
dim(SMmin42bb)
yr <- 2014
SMmin42bb <- soilMoistMin42[st_as_sfc(bb)]
SMmin5bb <- soilMoistMin5[st_as_sfc(bb)]


library(cubelyr)




SMmin42yr <- SMmin42bb %>% filter(year(time) == yr)

moist10 <- SMmin42yr > 0.1

m10yL <- aggregate(moist10, by = "years", FUN = max, na.rm = TRUE)



moist80 <- SMmin5bb > 0.80
m80yL <- aggregate(moist80, by = "years", FUN = max, na.rm = TRUE)

strMet <- m10yL + m80yL





