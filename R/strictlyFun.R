# Repo for strictures functions



# return stars with single slice == whether soil moisture stayed above a critical threshold for total X day during
# window startDay:EndDay

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
  
  avgSMdayz <- avgSM |> slice(time, indDayz)
  
  avgSMdayz$sm_pct <- if_else(avgSMdayz$sm_pct > dailyThresh, 1, 0)
  
  #number of days soil moisture above threshold
  sumSMdayz <- st_apply(avgSMdayz, MARGIN = 1, sum)
  SMstr <- sumSMdayz |> mutate( str = if_else(sum > windowThresh, 1,0),
                                 sum = NULL) 
  # print(paste0("number of days:",  date(endDay)-date(startDay)+1))
  return(SMstr) # 1D stars object
}

#  transform column names to dates  
col2date <- function(sf){
  dayz <- names(sf)
  dayz <- dayz |> 
    str_subset("X") |>
    str_sub(2) |>
    str_replace_all('[.]', "-")|>
    as.Date()
  return(dayz)
}

# transform vector of dates into operable column names  
date2col <- function(dateV){
  colChar <- dateV |> 
    str_replace_all('-', '.') |> 
    str_c("X",.)
}

threshSMsf <- function(avgSMsf = avgSMsf, dailyThresh = 0.01, startDay = NULL, endDay = NULL, windowThresh = NULL){
  # source(here('Functions', 'strictlyFun'))
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


