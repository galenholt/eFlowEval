#' concatenate lists of stars objects and indices
#'
#' Because the list structure means we can't just c() the stars and bind_rows the indices, since they're items 1 and 2 in a list
#'
#' @param starindexlist a list of lists with the stars in 1 and indices in 1
#' @param dimension dimension along which to [c.stars()]
#' @param starname name to give the stars object. Defaults to the name of the first one
#' @param indexname name to give the index, defaults to the name of the first one
#'
#' @return a list with stars in 1 and indices in 2, concatenated together over the starindexlist
#' @export
#'
concat_star_index <- function(starindexlist, dimension,
                              starname = names(starindexlist[[1]][1]),
                              indexname = names(starindexlist[[1]][2])) {
  # I really should define these stars + index objects as a class, and then turn this into a method for `c`

  # There's got to be a purrr method for this, but map keeps stuffing up c.stars.
  tempAns <- foreach::foreach(l = 1:length(starindexlist),
                     .combine=function(...) stars:::c.stars(..., along = dimension), # Pass dimension argument to c.stars
                     .multicombine=TRUE) %do% {
                       starindexlist[[l]][[1]]
                     }

  # For some reason the time dimension sometimes loses times.
    # WE SHOULD ALSO SORT SO TIMES ARE IN THE RIGHT ORDER
    # AND TEST FOR DUPLICATE TIMES
  if (dimension == 'time') {
    # unfortunately, the safest way to do this is with another foreach
    alltimes <- foreach::foreach(l = 1:length(starindexlist),
                                 .combine = c, .multicombine = TRUE) %do% {
                  st_get_dimension_values(starindexlist[[l]][[1]], 'time')
                                 }

    if (any(duplicated(alltimes))) {
      rlang::abort("Duplicated times. Check your data.")
    }

    if (any(alltimes != sort(alltimes))) {
      rlang::abort("Times out of order. Need to figure out a robust sorting method.")
    }

    tempAns <- st_set_dimensions(tempAns, 'time', values = alltimes)


  }

  # glue together the geom indices if geometry, but not if time
  if (grepl('geometry|shape', dimension, ignore.case = TRUE)) {
    tempIndex <- foreach::foreach(l = 1:length(starindexlist),
                                  .combine=dplyr::bind_rows,
                                  .multicombine=TRUE) %do% {
                                    starindexlist[[l]][[2]]
                                  }
  } else if (grepl('time', dimension, ignore.case = TRUE)) {
    tempIndex <- starindexlist[[1]][[2]]
  } else {
    rlang::warn("dimension not defined, attemtpting to return input geometries")
    tempIndex <- starindexlist[[1]][[2]]
  }

  templist <- list(tempAns, tempIndex) |>
    setNames(c(starname, indexname))

  return(templist)

}
