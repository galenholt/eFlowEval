#' Time-roll stars
#'
#' @param stardat a stars object or a single attribute, e.g. stars_obj[[1]]
#' @param attribute_number the attribute number if a stars object
#' @param tDim the number of the time dimension, default NULL chooses final dimension
#' @param FUN function to apply. Typically RcppRoll::roll_*
#' @param rolln rolling window. see RcppRoll
#' @param align see RcppRoll
#' @param fill see RcppRoll
#' @param na.rm see RcppRoll
#'
#' @return an array or stars object, depending on what was given.
#' @export
#'
timeRoll <- function(stardat, attribute_number = 1,
                     tDim = NULL, FUN, rolln = 1L,
                     align = 'center', fill = c(NA, NA, NA), na.rm = FALSE) {

  # This takes stardat, which can be JUST a single starsobj[[attribute]], or a
  # stars object, with the attribute_number extracted in here. That allows
  # easier calling (the stars), but also more flexibility. It returns just the
  # array though, so we can easily add to stars as a new attribute.

  instars <- ifelse(inherits(stardat, 'stars'), TRUE, FALSE)
  if (instars) {
    starref <- stardat
    stardat <- stardat[[attribute_number]]
  }
  # The data for the attribute may be a matrix or array

  ndims <- length(dim(stardat))
  if (ndims > 3) {
    stop('code not written for arrays with > 3 dimensions. would need another level of looping (or a different approach- purrr?)')
  }

  # if no tDim set, make it the last dimension, since that's usually where it is
  if (is.null(tDim)) {
    tDim <- ndims
  }

  # permute if needed
  if (tDim == 1) {
    starperm <- stardat # allow doing this even if time is dim 1, still might want to roll
  } else {
    if (length(dim(stardat)) == 2) {
      starperm <- t(stardat) # t() is faster than aperm if we can use it
    } else {
      alldims <- 1:ndims
      # permutation order: doesn't really matter, as long as tDim is rows
      permorder <- c(tDim, alldims[-tDim])
      # get the reverser here too
      revPerm <- Matrix::invPerm(permorder)

      # Permute
      starperm <- aperm(stardat, perm = permorder)
    }
  }

  # loop over 3rd dimension if necessary
    # this is hacky. I'm sure there's a purrr way to do this
  if (ndims == 2) {
    starperm <- FUN(starperm, n = rolln, align = align, fill = fill, na.rm = na.rm)
  } else {
    for (i in 1:dim(starperm)[3]) {
      starperm[,,i] <-FUN(starperm[,,i], n = rolln, align = align, fill = fill, na.rm = na.rm)
    }
  }

  # unpermute
  if (tDim == 1) {
    starroll <- starperm # allow doing this even if time is dim 1, still might want to roll
  } else {
    if (length(dim(stardat)) == 2) {
      starroll <- t(starperm) # t() is faster than aperm if we can use it
    } else {
      # Permute
      starroll <- aperm(starperm, perm = revPerm)
    }
  }

  # Deal with issues of e.g. max(NA, NA): from help
  # The minimum and maximum of a numeric empty set are +Inf and -Inf (in this
  # order!) which ensures transitivity, e.g., min(x1, min(x2)) == min(x1, x2).
  # For numeric x max(x) == -Inf and min(x) == +Inf whenever length(x) == 0
  # (after removing missing values if requested).
  starroll[is.infinite(starroll)] <- NA

  # if it came in as stars, put it back together.
  if (instars) {
    starref[[1]] <- starroll
    starroll <- starref
  }

 return(starroll)

}
