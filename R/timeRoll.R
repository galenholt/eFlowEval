# timeRoll function

timeRoll <- function(stardat, attribute_number = 1,
                     tDim = NULL, FUN, rolln = 1L,
                     align = 'center', fill = c(NA, NA, NA), na.rm = FALSE) {
  
  # This takes stardat, which can be JUST a single starsobj[[attribute]], or a
  # stars object, with the attribute_number extracted in here. That allows
  # easier calling (the stars), but also more flexibility. It returns just the
  # array though, so we can easily add to stars as a new attribute.
  
  if (inherits(stardat, 'stars')) {
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
  test <- 1
  
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
  
 return(starroll)
  
}