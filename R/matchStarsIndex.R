#' Function to sort stars according to their index files and deal with any duplicated ANAEs
#'
#' Particularly important while parallel processing. Often used with anaes as index1 to sort stars relative to them. In typical use, this assumes the stars2 and index2 are not shuffled relative to each other, and sorts them relative to index1. If `testfinal = TRUE`, it does a check of the stars2 and index2 spatial matching, but that is slow.
#'
#'
#' @param index1 sf with an index in column 1 and geometry
#' @param stars1 a stars belonging to index1. Default NULL
#' @param index2 sf with an index in column 1 and geometry
#' @param stars2 a stars belonging to index2
#' @param indexcol columns of the index values. Default c(1,1) if both index1 and 2 have the column as 1
#' @param testfinal do a spatial check of the stars itself. This takes a very long time.
#' @param return1 whether to return index1 or not. Default FALSE
#' @param as_test Use this as a test of whether things needed to be sorted (ie emits error if sorting happens). Default FALSE
#'
#' @return a list of stars and index (and if return1, index1 also)
#' @export
#'
matchStarsIndex <- function(index1, stars1 = NULL, index2, stars2,
                            indexcol = c(1, 1), testfinal = TRUE,
                            return1 = FALSE, as_test = FALSE) {
  # index1 and index2 expected to be an sf with an index in the first column and geometry
  # testfinal does a big st_intersects on the stars, and so can take FOREVER. Turn it off for speed
  # If stars1 is NULL, this just matches to index1. particularly useful for matching to the ANAE in sf form,
    # e.g. resortFromANAE <- matchStarsIndex(index1 = EdwardWakoolANAE, stars1 = NULL,
              # index2 = predictIndices, stars2 = catchPredict, indexcol = c(1, 1))

  ##
  # Fix single indexcol to match for both datasets
  if (length(indexcol) == 1) {
    indexcol <- rep(indexcol, 2)
  }

  # deal with the time and geometry dimensions being flopped
  # Note that this assumes there are only time and geometry dimensions
  # do it off the bat to catch the flopped issue even if they're sorted correctly
  if (attributes(stars::st_dimensions(stars2))$name[1] != 'geometry') {
    stars2 <- aperm(stars2, c(2,1))
    warning('re-set geometry dimension in stars2 to be first.
            Probably should do that earlier (e.g. aperm(stars2, c(2,1)))
            to avoid other hidden problems with this dataset')
  }

  # if stars1 is null, skip
  if(!is.null(stars1)) {
    # To test, need geometry of stars1 to be correct too- this might really slow things down though
    if (attributes(stars::st_dimensions(stars1))$name[1] != 'geometry') {
      stars1 <- aperm(stars1, c(2,1))
      warning('stars1 has geometry dimension not first.
    DIMENSION SWAP FOR TESTING HERE, DOES NOT GET RETURNED.
            Probably should swap dims earlier (e.g. aperm(stars1, c(2,1)))
            to avoid other hidden problems with this dataset')
    }

  }


  # can have any index column, but typically it will be 1 and named UID. Going
  # to rename it though in case it ever isn't, but throw a warning
  if(names(index1)[indexcol[1]] != 'UID' |
     names(index2)[indexcol[2]] != 'UID') {
    warning('indexcol is not UID, it is ', names(index2)[indexcol[2]])
  }

  origname1 <- names(index1)[indexcol[1]]
  origname2 <- names(index2)[indexcol[2]]

  names(index1)[indexcol[1]] <- 'INDEX'
  names(index2)[indexcol[2]] <- 'INDEX'

  # expected behaviour is that this will sort 2 to match 1, and so only returns
  # the sorted index and stars for 2

  # First, do a check, and if they already match, don't need to do any more
  if (length(index1$INDEX) == length(index2$INDEX) &&
      all(index1$INDEX == index2$INDEX)) {
    names(index2)[indexcol[2]] <- origname2
    if (return1) {return(tibble::lst(index1, index2, stars2))}
    return(tibble::lst(stars2, index2))
  }


# Deal with potential duplicates ------------------------------------------

  # need to make a second index column to deal with potential duplicates
    # Do it out here so we can use that column name even when there aren't duplicates
  index1$INDUP <- index1$INDEX
  index2$INDUP <- index2$INDEX

  if (any(duplicated(index1$INDEX)) | any(duplicated(index2$INDEX))) {

    if (any(duplicated(index1)) != any(duplicated(index2))) {
      stop('Duplicated UIDs in only one of the stars.
           This suggests you are comparing different sets of ANAEs,
           and this function may not yield expected results')
    }

    # identify where the duplicates are
    dup1 <- index1[which(duplicated(index1$INDEX)), ]
    bothdup1 <- which(index1$INDEX %in% dup1$INDEX)

    dup2 <- index2[which(duplicated(index2$INDEX)), ]
    bothdup2 <- which(index2$INDEX %in% dup2$INDEX)

    # sometimes duplicates happen because geometries have the same index, but
    # are not actually the same. We want to handle and retain those. But
    # sometimes there are true duplicates, in which case we should remove them.
    truedup2 <- unlist(sf::st_equals(index2[bothdup2, ], retain_unique = TRUE))
    drop2 <- bothdup2[truedup2]

    # Suppress warnings because it can throw a warning when calculating the
    # moments to print(), and that screws up my warning/error catcher.
    if (length(drop2) > 0) {
      index2 <- index2[-drop2, ]
      stars2 <- stars2[,-drop2, ]
    }


    truedup1 <- unlist(sf::st_equals(index1[bothdup1, ], retain_unique = TRUE))
    drop1 <- bothdup1[truedup1]

    if (length(drop1) > 0) {
      index1 <- index1[-drop1, ]
      if (!is.null(stars1)) {stars1 <- stars1[,-drop1, ]}
    }


    # and now we need to re-do the dup-finding. This is getting a bit silly, but I don't have time to refactor.
    # identify where the duplicates are
    dup1 <- index1[which(duplicated(index1$INDEX)), ]
    bothdup1 <- which(index1$INDEX %in% dup1$INDEX)

    dup2 <- index2[which(duplicated(index2$INDEX)), ]
    bothdup2 <- which(index2$INDEX %in% dup2$INDEX)

    if (!all(dup1$INDEX %in% dup2$INDEX)) {
      stop('duplicate indices present in both indices,
      but they are not the same polygons.
      This suggests you are comparing different sets of ANAEs,
           and this function may not yield expected results')
    }

    # looking just at the duplicates, get the indices of 2 that match 1 based on the geometry itself
    matchindex <- as.numeric(sf::st_equals_exact(index1[bothdup1, ],
                                             index2[bothdup2, ],
                                             par = 1, sparse = TRUE))

    # There's a weird edge case sometimes where they don't `equal_exact`, but do
    # match, it just can't sort it out. Bypass and warn, I guess. I could
    # probably be more clever to find the closest, but at this point they're
    # functionally identical- I think it's likely an ANAE problem.
    if (all(is.na(matchindex))) {
      #

      # We need to do this over potentially multiple sets of duplicates
      # We've already checked the dup1 and dup2 match
      itstart <- 0
      matchindex <- NULL
      for (i in 1:nrow(dup1)) {
        bd1 <- which(index1$INDEX %in% dup1$INDEX[i])
        bd2 <- which(index2$INDEX %in% dup1$INDEX[i])

        # The `diag` is here because that's asking if each pair of indices
        # matches- ie even if they aren't true duplicates, do each of them
        # intersect? It effectively is an `sf::st_intersects` version of the
        # `truedup` stuff above with `sf::st_equals`, just more approximate.
        if (all(diag(sf::st_intersects(index1[bd1, ],
                      index2[bd2, ], sparse = FALSE)))) {
          rlang::warn(glue::glue("duplicates of UIDs {dup1$INDEX[i]} are failing to match exactly, but intersect well. Assigning based on position"))
          mi <- seq(itstart + 1, itstart + length(bd1))
          itstart <- itstart + length(bd1)
          matchindex <- c(matchindex, mi)
        } else {
          rlang::abort(glue::glue("Error: duplicates don't spatially match between index1 and index2 for UIDs {dup1$INDEX[i]}"))
        }

      }
    }

    # TESTING
    # ggplot() + geom_sf(data = index1[bd1, 'INDEX']) + geom_sf(data = index2[bd2, 'INDEX'])

    # Modify the relevant second index- for the reference set, just append 1:index
    index1$INDUP[bothdup1] <- paste0(index1[bothdup1, ]$INDUP, 1:length(bothdup1))
    # For the matching set, append the index match we just saved
    index2$INDUP[bothdup2] <- paste0(index2[bothdup2, ]$INDUP, matchindex)
  }

# Matching and sorting ----------------------------------------------------
  # I think this sorts the first argument to the second. So if I want to have
  # the first be the reference, need to reverse order here
  ordermatcher <- match(index2$INDUP, index1$INDUP)

  # Do the sort on the indices and check
  index2 <- index2[order(ordermatcher), ]
  # Check
  if (!all(index1$INDUP == index2$INDUP)) {
    stop('the sorting is still failing')
  }

  # Now, finally, sort the stars

  # sort the geometry dimension in the second to match the first
  stars2 <- stars2[ ,order(ordermatcher), ]

  # Test the geometries at least intersect. This is't perfect because the
  # geometries may not be exact after a few transforms, but catches most thigns
  if (testfinal) {
    print('testfinal is on, might take a long time')
    # If there is a stars1, test on that for best safety, otherwise use the index
    if (is.null(stars1)) {
      inter3 <- diag(sf::st_intersects(sf::st_geometry(index1),
                                   sf::st_geometry(stars2), sparse = FALSE))
    } else {
      inter3 <- diag(sf::st_intersects(sf::st_geometry(stars1),
                                   sf::st_geometry(stars2), sparse = FALSE))
    }

    if (sum(!inter3) != 0) {
      warning(sum(!inter3), " geometries still unmatched")
    } else {
      print('stars match, at least at the level of st_intersects')
    }
  }

  # return the output.
    # I'm not going to return the dup-fixed index. It would be dangerous because
    # the matching is only correct for sets that reference the same index1,
    # which is likely to get lost as we move through analyses. Will be better to
    # avoid dups in the first place in processANAE. Basically, the dup issue
    # will be sorted here with the sorting that needs to be done to compare
    # anyway
    names(index2)[indexcol[2]] <- origname2
    index2$INDUP <- NULL


    # if we're using this as a test and not re-saving the return object, check whether the orders were wrong.
    if (as_test) {
      if (ordermatcher != order(ordermatcher)) {
        rlang::abort("The two sets of polygons are not in the same order. Need to run matchStarsIndex on the inputs.")
      }
    }

    if (return1) {
      names(index1)[indexcol[2]] <- origname1
      index1$INDUP <- NULL
      return(tibble::lst(index1, index2, stars2))
    }

   if (!return1) {return(tibble::lst(stars2, index2))}

}


