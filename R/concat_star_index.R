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
                     .combine=function(...) c(..., along = dimension), # Pass dimension argument to c.stars
                     .multicombine=TRUE) %do% {
                       starindexlist[[l]][[1]]
                     }

  tempIndex <- foreach::foreach(l = 1:length(starindexlist),
                       .combine=dplyr::bind_rows,
                       .multicombine=TRUE) %do% {
                         starindexlist[[l]][[2]]
                       }
  templist <- list(tempAns, tempIndex) |>
    setNames(c(starname, indexname))

  return(templist)

}
