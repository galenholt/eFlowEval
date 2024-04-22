#' multiply one stars by another at a different temporal scale, possibly with a lag
#'
#' ie daily for one species depends on another ever succeeding in the previous year
#'
#' @param fineStars stars with a fine timestep
#' @param coarseStars stars on a coarser timestep
#' @param lag lag from fine to coarse; i.e. 0 is multiply day by coarse same year, 1 is day by previous year
#' @param invertCoarseLogic flip the logicals on the coarse, ie if they are a competitor species
#'
#' @return multiplied stars with same dimensions as fineStars
#' @export
#'
unevenTimeMult <- function(fineStars, coarseStars, lag, invertCoarseLogic = FALSE) {

  # barf if too many dimensions
  if (length(dim(fineStars)) != 2 | length(dim(coarseStars)) != 2) {
    stop('expects a time dimension and one other (likely shape). Sort out for higher dimensions later if needed')
  }

  # Flip logic if asked
  if (invertCoarseLogic) {
    coarseStars <- !coarseStars
  }
  # Get the time values in each
  timesheets <- st_get_dimension_values(fineStars, 'time')
  yrtimes <- st_get_dimension_values(coarseStars, 'time')

  # We need to handle the situation of fine times beyond the end of coarse. To
  # do that, we need the end of the last coarse period

  yrtimes <- c(yrtimes, yrtimes[length(yrtimes)] + diff(yrtimes[(length(yrtimes)-1):length(yrtimes)]))

  # This is the interval (in the coarse) each sheet of the fine data is in
  # So, we really just need to grab the whichInt[t]-1 yearsheet for the mult
  whichInt <- findInterval(timesheets, yrtimes, rightmost.closed = TRUE)

  # set up the output stars; will be same as the fine input
  # fineStars <- fineStars
  # But we don't actually use the input except immediately, so could just overwrite to save data
  # If this becomes more dynamic, might need to

  # Extract the matrices
  coarseMat <- coarseStars[[1]]
  fineMat <- fineStars[[1]]

  # Dim check and arrange
  coarsetimeDim <- which(names(dim(coarseStars)) == 'time')
  finetimeDim <- which(names(dim(fineStars)) == 'time')

  # flip each if needed to be what I expect
  if (coarsetimeDim != 1) {
    coarseMat <- t(coarseMat)
  }

  if (finetimeDim != 2) {
    fineMat <- t(fineMat)
  }

  if (any(diff(unique(whichInt)) > 1)) {
    rlang::warn("Missing time data in fineStars, can cause unexpected behaviour in the multiplication, since some intervals are missing.")
  }
  # Loop over each coarse time unit, multiplying it by the matrix of fine time units applying to it (as determined by lag)
  for (t in min(whichInt):max(whichInt)) {

    # Get the indexes for the fine that fall within this t
    thiscoarseIndex <- which(whichInt == t)

    # multiply the coarse vector by the fine matrix (this is element-wise but expanded, NOT matrix-mult)
    # Set output to NA if it's off the bottom (< 1) or the top (in the last interval, whihc is > the last data + one interval size)
    if (((t-lag) < 1) | ((t-lag) >= max(whichInt))) {
      # set to NA if there is no coarse value for the desired lag
      fineMat[ ,thiscoarseIndex] <- NA * fineMat[ , thiscoarseIndex]
    } else {
      fineMat[ ,thiscoarseIndex] <- coarseMat[(t-lag), ] * fineMat[ , thiscoarseIndex]
    }

  }

  # If the daily matrix was flipped vs what expected, flip it back
  if (finetimeDim != 2) {
    fineMat <- t(fineMat)
  }

  # Replace the mat
  fineStars[[1]] <- fineMat

  return(fineStars)

}
