# This is kind of silly- should just define these as actual named functions.

# Area of inundation
areaInun <- function(x, area) {
  sum(ifelse(x > 0, area, 0))
}

# area*depth for each raster, so sum(depth, area)
volInun <- function(z, area) {
  sum(z*area)
}

# Volume of water <= 10cm (or arbitrary photic limit)
# depth up to 10cm * area, then summed
# So, get the minimum of depth or photic limit, multiply by area, and
# summarise with sum to get all depths
# Have to sort of write a min() out of ifelse or it does min() over the column, not the rows
volLimit <- function(x, area, limit = 0.1) {
      sum(ifelse(x > limit, limit*area, x * area))
    }

# mean depth of the inundated area- this potentially much different than mean
# depth across the polygon
# give 0 weights to those that aren't inundated
  avgInunDepth <- function(x, area) {
      areaifinun <- ifelse(x > 0, area, 0)
      weighted.mean(x, areaifinun)
  }

  # Weighted mean, ignoring NA by default
weightedMean <- function(x, area) {
      weighted.mean(x, area, na.rm = TRUE)
}

# area of water <= 40cm and > 0
# Have to sort of write a min() out of ifelse or it does min() over the column, not the rows
areaSpoonbillForage <- function(x, area, limitShallow = 0, limitDeep = 0.4) {
      sum(ifelse(x > limitShallow & x <= limitDeep, area, 0))
}

# area of water <= 150cm and >= 50cm
# Have to sort of write a min() out of ifelse or it does min() over the column, not the rows
 areaSpoonbillBreed <- function(x, area, limitShallow = 0.5, limitDeep = 1.5) {
      sum(ifelse(x >= limitShallow & x <= limitDeep, area, 0))
 }

 # get the maximum depth. so far just used for testing
  maxInunDepth <- function(x, area) {
      max(x, na.rm = TRUE)
  }

  # area of water <= 30cm
 lippiaAdultSurvive <- function(x, area, limitShallow = 0, limitDeep = 0.3) {
      sum(ifelse((x <= limitDeep)|(is.na(x)), area, 0))
 }

 # area of moisture > 0.1. Using a max of Inf, though the max should be 1.
 # Have to sort of write a min() out of ifelse or it does min() over the column, not the rows
 areaCentipedaSurvive <- function(x, area, limitdry = 0.1, limitwet = Inf) {
      sum(ifelse(x > limitdry & x <= limitwet, area, 0))
 }
