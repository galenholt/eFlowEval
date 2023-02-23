# This is kind of silly- should just define these as actual named functions. 
get_data_proc_function <- function(summaryFun) {
  if (summaryFun == 'areaInun') {
    # Area of inundation
    chosenSummary <- function(x, area) {
      sum(ifelse(x > 0, area, 0))
    }
  } else if (summaryFun == 'volInun') {
    # area*depth for each raster, so sum(depth, area)
    chosenSummary <- function(z, area) {
      sum(z*area)
    }
  } else if (summaryFun == 'volLimit') {
    # Volume of water <= 10cm (or arbitrary photic limit)
    # depth up to 10cm * area, then summed
    # So, get the minimum of depth or photic limit, multiply by area, and
    # summarise with sum to get all depths
    # Have to sort of write a min() out of ifelse or it does min() over the column, not the rows
    chosenSummary <- function(x, area, limit = 0.1) {
      sum(ifelse(x > limit, limit*area, x * area))
    }
  } else if (summaryFun == 'avgInunDepth') {
    # mean depth of the inundated area- this potentially much different than mean
    # depth across the polygon
    # give 0 weights to those that aren't inundated
    chosenSummary <- function(x, area) {
      areaifinun <- ifelse(x > 0, area, 0)
      weighted.mean(x, areaifinun)
    }
  } else if (summaryFun == 'weightedMean') {
    # Weighted mean, ignoring NA by default
    chosenSummary <- function(x, area) {
      weighted.mean(x, area, na.rm = TRUE)
    }
  } else if (summaryFun == 'areaSpoonbillForage') {
    # area of water <= 40cm and > 0
    # Have to sort of write a min() out of ifelse or it does min() over the column, not the rows
    chosenSummary <- function(x, area, limitShallow = 0, limitDeep = 0.4) {
      sum(ifelse(x > limitShallow & x <= limitDeep, area, 0))
    }
  } else if (summaryFun == 'areaSpoonbillBreed') {
    # area of water <= 150cm and >= 50cm
    # Have to sort of write a min() out of ifelse or it does min() over the column, not the rows
    chosenSummary <- function(x, area, limitShallow = 0.5, limitDeep = 1.5) {
      sum(ifelse(x >= limitShallow & x <= limitDeep, area, 0))
    }
  } else if (summaryFun == 'maxInunDepth') {
    # get the maximum depth. so far just used for testing
    chosenSummary <- function(x, area) {
      max(x, na.rm = TRUE)
    }
  } else if (summaryFun == 'lippiaAdultSurvive') {
    # area of water <= 30cm
    chosenSummary <- function(x, area, limitShallow = 0, limitDeep = 0.3) {
      sum(ifelse((x <= limitDeep)|(is.na(x)), area, 0))
    }
  } else {
    stop('need to choose a summary function')
  }
  
  return(chosenSummary)
}