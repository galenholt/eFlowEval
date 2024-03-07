
#' Intersect raster and polygons and get spatial average and return stars
#'
#' @param polysf polygons in sf
#' @param rastst rasters in stars or stars_proxy
#' @param grouper a single grouping variable to identify single polygons in polysf
#' @param FUN the aggregation function to apply. Should be a bare function name
#'   (tricky to get deep in the stack), a character, or a list, e.g. `list(testf
#'   = \(x) sum(ifelse(x > 0.005, area, 0), na.rm = TRUE))`. Due to changes in
#'   dplyr 1.1, if functions need 'area', it should be internally hardcoded, not
#'   a second argument. e.g. the easiest way to use weighted.mean is to write a
#'   new function `wm <- function(x) {weighted.mean(x, area, na.rm = T)}` and
#'   pass 'wm'. There are other solutions, but that seems to be the cleanest.
#' @param maintainPolys TRUE averages back into them. FALSE leaves them split, and makes a new grouping variable to keep it distinct
#' @param na.replace allows replacing NA with numerical values (e.g. 0)
#' @param whichcrs Default here is the Australian Albers 3577. If geographic (unprojected) crs is used, the intersection jumbles up for sf version >=1.0, and so I'm forcing a projection as default
#' @param maxPixels maximum number of pixels before internally chunking
#' @param pixelsize size of the pixels in the raster
#' @param rastRollArgs allows passing a list of arguments to timeRoll to roll the  raster after cropping. use the `attribute_number` format, e.g. `rastRollArgs  = list(attribute_number = 1, tDim = 3, FUN = RcppRoll::roll_max, rolln = 2, align = 'right', na.rm = TRUE)`. NULL (the default) just bypasses
#'
#' @return
#' @export
#'
#' @examples
rastPolyJoin <- function(polysf, rastst, grouper = 'UID', FUN = weighted.mean,
                         maintainPolys = TRUE, na.replace = NA, whichcrs = 3577,
                         maxPixels = 100000,
                         pixelsize = NA,
                         rastRollArgs = NULL) {

    # Handle the case where we feed it a null dataframe (or, more generally, where
  # the grouper doesn't exist)
    # The [1] index is because it returns FALSE for the geometry, and we only
    # want to check grouper
  if (is.na(polysf[ ,grouper])[1] | nrow(polysf) == 0) {
    warning("NA or NULL polygon df, returning NULL joined list")
    return(list(avgPRStars = NULL, avgPRindex = NULL))
  }

  # If functions come in as a character, we need to mget them
  if (is.character(FUN)) {
    FUN <- mget(FUN, inherits = TRUE)
  }
  # turn raster into polygon sf object, time as columns
    # This can take a really long time

  # The conditional here avoids using parallel overhead and the grid
  # intersection if smaller than maxPix. It would work with just the second
  # part, but it's about 0.15 seconds slower per polygon
  # Figure out how many raster pixels we're about to read in
    # stars reads in a raster matching the bbox of the whole polygon dataframe (whether that's one or many polygons)

    bbarea <- as.numeric(sf::st_area(sf::st_as_sfc(sf::st_bbox(polysf))))
    pixneeded <- bbarea/pixelsize


    # This whole thing just needs to be its own function, I think. Getting to be so many tweaks
  # If less than maxPixels are needed, just get the intersection directly
      # also if I didn't pass in a pixelsize or maxpixels
    # rpintersect is just a wrapper of st_intersection with a bunch of read-in and transform boilerplate so it works consistently
  if (is.na(pixneeded) | is.na(maxPixels) | (pixneeded <= maxPixels)) {
    intPR <- rpintersect(singlesf = polysf, singleraster = rastst,
                         na.replace = na.replace, whichcrs = whichcrs,
                         rastRollArgs = rastRollArgs)
  } else if (pixneeded > maxPixels) {
    # make a grid that breaks up the polygon (or is just the bb if the number of pixels is small enough)
    grid <- sf::st_make_grid(polysf, n = ceiling(sqrt(pixneeded/maxPixels)))
    # Now, the intersection gets the polysf split by grids (and drops grids with no polysf in them)
    # sf::st_agr(polysf) = "constant"
    # sf::st_agr(grid) = "constant"
    gridsf <- sf::st_intersection(polysf, grid)

    # get rid of linestrings and points that occasionally appear when we make
    # the grid- they don't make sense in this context and cause the crop to fail
    if (any(!sf::st_is(gridsf, c('POLYGON', 'MULTIPOLYGON')))) {
      gridsf <- sf::st_collection_extract(gridsf, 'POLYGON') # Extracts both poly and multi
    }

    gridsf <- sf::st_make_valid(gridsf)
    # Now, foreach over each of the polygon chunks and bind_rows back together
    # The group_by below for the stats means it doesn't matter that the gridding puts in extra 'cuts'
    # and we put it back together here (rather than keep going all the way
    # through the stats) because averages would require careful weighting if we
    # aren't averaging over the whole anae polygon

    ## THE second crop in here is breaking with proxys. Sort that out. I htink
    intPR <- foreach::foreach(r = 1:nrow(gridsf),
                     .combine = dplyr::bind_rows,
                     .multicombine = TRUE,
                     .options.future = list(seed = TRUE)) %dofuture% {
                       # Get the grid-cut polygon r
                       thissmall <- gridsf[r,]
                       # # crop the raster to JUST this grid-cut polygon. That
                       # # currently fails with a nested st_crop on a proxy. So
                       # # two options for a workaround- read the whole thing in,
                       # # or pass in the uncropped proxy and just do the crop
                       # # here. The whole point is it's too big to read in, so
                       # # try the latter. But do it in a conditional so we can switch back if we fix the bug
                       # if (is.null(uncropraster)) {
                       #   smallcrop <- st_crop(rastst, thissmall, as_points = FALSE)
                       # } else if (inherits(uncropraster, 'stars_proxy')) {
                       #   smallcrop <- st_crop(uncropraster, thissmall, as_points = FALSE)
                       #  } else {stop("the double crop isn't working and hasn't been end-run")}



                       # Use the core rpintersect function to do the intersection
                       intPRsmall <- rpintersect(singlesf = thissmall, singleraster = rastst,
                                                 na.replace = na.replace, whichcrs = whichcrs,
                                                 rastRollArgs = rastRollArgs)
                       # Return to be row-bound
                       intPRsmall
                     } # end foreach
  }


  # test <- 1
  if (maintainPolys) {
    # Get the averages into each ANAE ID at each time
    # This strips off the other ANAE cols, which is fine. Could do it earlier, but don't see a need?
    # Updated to drop geometry and add back later, because it is ~6-10x faster

    # since we now need to maybe do this twice, do some processing first
    # in case there are any stray 'Shape' columns hanging around.
    names(intPR)[names(intPR) == 'Shape'] <- 'geometry'

    intPR <- intPR |>
      dplyr::mutate(area = as.numeric(sf::st_area(geometry))) |>
      sf::st_drop_geometry()

    # Deal with the new dplyr as I did in WERP. It's really annoying.
    avgPR <- try(intPR |>
      dplyr::group_by(dplyr::across(tidyselect::all_of(grouper))) |>
      dplyr::summarize(dplyr::across(tidyselect::starts_with("X"), {{FUN}})) |>
      dplyr::ungroup(),
      silent = TRUE)

    if (inherits(avgPR, 'try-error')) {
      # turn the list into characters. Eval can't handle `return`, so make an
      # attempt to get rid of it, though this is likely fragile.
      if (is.character(FUN)) {
        charfun <- FUN
      } else {
        charfun <- paste0(deparse(FUN), collapse = '')
      }

      # add rlang::quo unless it's already there.
      if (!grepl("quo", charfun)) {
        charfun <- paste0(c("rlang::quo(", charfun, ")"), collapse = '')
      }
      charfun <- stringr::str_remove_all(charfun, 'return\\([A-z]\\)')
      # FUNS2 <- eval(parse(text = charfun)) # base R
      FUNS_quo <- rlang::eval_tidy(rlang::parse_expr(charfun)) # rlang claims to be faster?

      # go again
      avgPR <- intPR |>
        dplyr::group_by(dplyr::across(tidyselect::all_of(grouper))) |>
        dplyr::summarize(dplyr::across(tidyselect::starts_with("X"), {{FUNS_quo}})) |>
        dplyr::ungroup()
    }

    # Save the indexer column (is it possible to glue this back on as a dimension? Probably not, because it's parallel to shape)
      # need to make the sf have the right transform. Can't just do this above because the stars can't be shifted until after rpintersect
    # ensure the polygons match
    if (sf::st_crs(polysf)$epsg != whichcrs) {
      polysf <- sf::st_transform(polysf, whichcrs) |>
        sf::st_make_valid()
    }

    avgPRindex <- polysf[ ,grouper] # leave the geometry ON this one, maybe?

    # Put the geometry back on the summarised data
    avgPR <- dplyr::left_join(avgPRindex, avgPR, by = grouper)
  }

  if (!maintainPolys) {
      # here, the values are already in there. we don't need to average into
      # SYSIDs, we instead need to make sure the sysids are unique
      # There's probably a straight-up mutate we could do, but this is maybe better to ensure the groups are set up right
    avgPR <- intPR |>
      dplyr::group_by(dplyr::across(all_of(grouper))) |>
      dplyr::mutate(subgroup = dplyr::row_number()) |>
      dplyr::ungroup() |>
      dplyr::mutate(dplyr::across(tidyselect::all_of(grouper), ~stringr::str_c(., subgroup, sep = '_'))) |>
      dplyr::select(all_of(grouper), tidyselect::starts_with('X')) # to have same format as the summarize, drop everything but the grouper and times

    # Save the indexer column (is it possible to glue this back on as a dimension? Probably not, because it's parallel to shape)
    avgPRindex <- avgPR[ ,grouper] # leave the geometry ON this one, maybe?
  }


  # It has been re-sorted for some unknown stupid dplyr reason. Make sure it is sorted back
  # avgPR <- avgPR |> arrange(across(all_of(grouper)))
  # avgPRindex <- avgPRindex |> arrange(across(all_of(grouper)))
  # Those sorts were causing more problems than they were solving

  # Check (unless we KNOW it's wrong)
  if (maintainPolys) {
    if (!all(sf::st_drop_geometry(avgPR[ ,grouper]) == sf::st_drop_geometry(polysf[ ,grouper]))) {
      warning('the returned object got re-sorted')
    }
  }

  # This is particularly an issue if the re-sort happens between avgPR and the index
  if (!all(sf::st_drop_geometry(avgPR[ ,grouper]) == sf::st_drop_geometry(avgPRindex[ ,grouper]))) {
    stop('re-sorted data relative to index')
  }


  # at the least, it'll allow me to see if things have gotten jumbled between shape and sys2

  # Turn back into a stars
  avgPRStarstemp <- avgPR |>
    dplyr::select(-tidyselect::all_of(grouper)) |>
    stars::st_as_stars() # |>
    # merge() # this is where it fails with more than ~800 days

  nbreaks <- ceiling(length(names(avgPRStarstemp))/800) + 1
  breaks <- round(seq(from = 0, to = length(names(avgPRStarstemp)), length.out = nbreaks))
  avgPRStars <- foreach::foreach(l = 1:(length(breaks)-1),
                      .combine=function(...) c(..., along = 2), # Pass dimension argument to c.stars
                      .multicombine=TRUE) %do% {
                        bottom <- breaks[l]+1
                        top <- breaks[l+1]
                        avgPRStarstemp[bottom:top, ] |>
                          merge()
                      }

  # change the time dimension
  stars::st_dimensions(avgPRStars)[2] <- stars::st_dimensions(rastst)[3]
  # st_dimensions(avgPRtars)[2] # yup, though it's still called X1?
  names(stars::st_dimensions(avgPRStars))[2] <- names(stars::st_dimensions(rastst))[3]

  # and change the name of the attribute
  if (is.function(FUN)) {
    funname <- deparse(substitute(FUN))
  } else if (is.character(FUN)) {
    funname <- FUN
  } else if (is.list(FUN)) {
    funname <- names(FUN)[1]
    } else {
    rlang::inform("not using named function, ignoring for attribute names")
  }
  names(avgPRStars) <- paste0(funname, '_from_', names(rastst))

  return(list(avgPRStars, avgPRindex))

}




# The core intersection function used by rastPolyJoin ------------------------------------------

# THIS IS THE CORE FUNCTION THAT does a single polygon x raster intersect (or a
# whole sf dataframe) and ensures transforms, etc

# It is agnostic to cropping- it should work (with sufficient memory or
# stars_proxy) if it's fed a raster that's been cropped or a raster of the whole
# world
rpintersect <- function(singlesf, singleraster,
                        na.replace = NA, whichcrs = 3577,
                        rastRollArgs = NULL) {

  # Crop for memory and speed- mostly useful for stars_proxy, but also
  # elsewhere. Fine to have pre-cropped as well, I think. But proxies can get in
  # here on their own just fine, and then crop at the last minute
  singleraster <- sf::st_crop(singleraster, singlesf, as_points = FALSE)

  # time-roll the raster if needed
  if (!is.null(rastRollArgs)) {
    singleraster <- stars::st_as_stars(singleraster)
    singleraster[[rastRollArgs$attribute_number]] <- rlang::exec(timeRoll, singleraster, !!!rastRollArgs)
  }
  # read in the stars as sf polys
    # I want to use merge = TRUE to reduce the number of polygons, but it ALSO merges the time dimension, which is bad.
  rastSF <- sf::st_as_sf(singleraster, as_points = FALSE, merge = FALSE, na.rm = FALSE)

  # replace NA?
  if (!is.na(na.replace)) {
    rastSF[is.na(rastSF)] <- na.replace
  }

  # Transform to correct crs.
  # Doing this here, because if rastst is a proxy, we won't be able to transform until it's read in as st_as_sf

  if (sf::st_crs(rastSF)$epsg != whichcrs) {
    rastSF <- sf::st_transform(rastSF, whichcrs) |>
      sf::st_make_valid()
  }
  # ensure the polygons match
  if (sf::st_crs(singlesf)$epsg != whichcrs) {
    singlesf <- sf::st_transform(singlesf, whichcrs) |>
      sf::st_make_valid()
  }

  # Have to intersect with the polygons to get average.
  # Less fiddly (because it's one-to-one), and it ensures the averages are area
  # weighted (they're not with aggregate; see timePolyRastScratch for testing)

  # trying to avoid warnings https://github.com/r-spatial/sf/issues/406
  # sf::st_agr(singlesf) = "constant"
  # sf::st_agr(rastSF) = "constant"
  intersectedPR <- sf::st_intersection(singlesf, rastSF)

  return(intersectedPR)
}


