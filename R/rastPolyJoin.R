# Function to intersect raster and polygons and get spatial average and return stars

rastPolyJoin <- function(polysf, rastst, grouper = 'UID', FUN = weighted.mean,  
                         maintainPolys = TRUE, na.replace = NA, whichcrs = 3577, 
                         maxPixels = 100000,
                         pixelsize = NA,
                         rastRollArgs = NULL) {
  # polysf is a spatial polygon simple feactures object
  # rastst is a raster brick in stars or stars_proxy (more likely) format
  # grouper is a single grouping variable to identify single polygons in polysf
  # maintainPolys TRUE averages back into them. FALSE leaves them split, and
  # makes a new grouping variable to keep it distinct
  # na.replace allows replacing NA with numerical values (e.g. 0)
  # crs sets the crs. Default here is the Australian Albers 3577. If geographic
  # (unprojected) crs is used, the intersection jumbles up for sf version >=1.0,
  # and so I'm forcing a projection as default
  # FUN allows calculating arbitrary functions.
  # Note that the second argument to FUN HAS TO BE area of the raster
  # cells, while ... are additional arguments. There's almost certainly a way to
  # specify something other than area for the second argument programatically,
  # but the ... syntax isn't playing nice for column names in the data frame
  # that are calculated internal to this function, so this assumption is a
  # workaround
  # maxPixels and pixelsize are used to bring the raster in in chunks if it's too big
  # rastRollArgs allows passing a list of arguments to timeRoll to roll the
  # raster after cropping. use the `attribute_number` format, e.g. `rastRollArgs
  # = list(attribute_number = 1, tDim = 3, FUN = RcppRoll::roll_max, rolln = 2,
  # align = 'right', na.rm = TRUE)`. NULL (the default) just bypasses

    # Handle the case where we feed it a null dataframe (or, more generally, where
  # the grouper doesn't exist)
    # The [1] index is because it returns FALSE for the geometry, and we only
    # want to check grouper
  if (is.na(polysf[ ,grouper])[1] | nrow(polysf) == 0) {
    warning("NA or NULL polygon df, returning NULL joined list")
    return(list(avgPRStars = NULL, avgPRindex = NULL))
  }
  
  # turn raster into polygon sf object, time as columns
    # This can take a really long time
  
  # The conditional here avoids using parallel overhead and the grid
  # intersection if smaller than maxPix. It would work with just the second
  # part, but it's about 0.15 seconds slower per polygon
  # Figure out how many raster pixels we're about to read in
    # stars reads in a raster matching the bbox of the whole polygon dataframe (whether that's one or many polygons)
  
    bbarea <- as.numeric(st_area(st_as_sfc(st_bbox(polysf))))
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
    grid <- st_make_grid(polysf, n = ceiling(sqrt(pixneeded/maxPixels)))
    # Now, the intersection gets the polysf split by grids (and drops grids with no polysf in them)
    gridsf <- st_intersection(polysf, grid) 
    
    # get rid of linestrings and points that occasionally appear when we make
    # the grid- they don't make sense in this context and cause the crop to fail
    if (any(!st_is(gridsf, c('POLYGON', 'MULTIPOLYGON')))) {
      gridsf <- st_collection_extract(gridsf, 'POLYGON') # Extracts both poly and multi
    }
    
    gridsf <- st_make_valid(gridsf)
    # Now, foreach over each of the polygon chunks and bind_rows back together
    # The group_by below for the stats means it doesn't matter that the gridding puts in extra 'cuts'
    # and we put it back together here (rather than keep going all the way
    # through the stats) because averages would require careful weighting if we
    # aren't averaging over the whole anae polygon
    
    ## THE second crop in here is breaking with proxys. Sort that out. I htink 
    intPR <- foreach(r = 1:nrow(gridsf),
                     .combine = bind_rows,
                     .multicombine = TRUE) %dopar% {
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
    avgPR <- intPR %>%
      mutate(area = as.numeric(st_area(.))) %>%
      st_drop_geometry() %>%
      group_by(across(all_of(grouper))) %>%
      summarize(across(starts_with("X"), ~FUN(., area))) %>% # averages across the cols with x in their name and gets weighted mean
      # TODO:: is it faster to pre-allocate these cols? IE just cut to
      # only those cols and then summarize everything?
      # Tried it and it crashed, but maybe moving out of dplyr syntax
      # entirely would speed it up, but be a pain. across() finding
      # the indices shouldn't be that slow, really, though. so the
      # only issue is if it can't parallelize or something
      # Same as above, but keeps all the other columns. Way slower
      # summarize(across(starts_with("X"), ~weighted.mean(.x, as.numeric(area))), 
      #           across(-c(starts_with("X"), Shape), first)) %>% # st_area returns a units object, which is good, but breaks weighted.mean
      ungroup()
    
    # Save the indexer column (is it possible to glue this back on as a dimension? Probably not, because it's parallel to shape)
      # need to make the sf have the right transform. Can't just do this above because the stars can't be shifted until after rpintersect
    # ensure the polygons match
    if (st_crs(polysf)$epsg != whichcrs) {
      polysf <- st_transform(polysf, whichcrs) %>%
        st_make_valid()
    }
    
    avgPRindex <- polysf[ ,grouper] # leave the geometry ON this one, maybe?
    
    # Put the geometry back on the summarised data
    avgPR <- left_join(avgPRindex, avgPR, by = grouper)
  }
  
  if (!maintainPolys) {
      # here, the values are already in there. we don't need to average into
      # SYSIDs, we instead need to make sure the sysids are unique
      # There's probably a straight-up mutate we could do, but this is maybe better to ensure the groups are set up right
    avgPR <- intPR %>%
      group_by(across(all_of(grouper))) %>%
      mutate(subgroup = row_number()) %>%
      ungroup() %>%
      mutate(across(all_of(grouper), ~str_c(., subgroup, sep = '_'))) %>%
      select(all_of(grouper), starts_with('X')) # to have same format as the summarize, drop everything but the grouper and times
    
    # Save the indexer column (is it possible to glue this back on as a dimension? Probably not, because it's parallel to shape)
    avgPRindex <- avgPR[ ,grouper] # leave the geometry ON this one, maybe?
  }
 
  
  # It has been re-sorted for some unknown stupid dplyr reason. Make sure it is sorted back
  # avgPR <- avgPR %>% arrange(across(all_of(grouper)))
  # avgPRindex <- avgPRindex %>% arrange(across(all_of(grouper)))
  # Those sorts were causing more problems than they were solving
  
  # Check (unless we KNOW it's wrong)
  if (maintainPolys) {
    if (!all(st_drop_geometry(avgPR[ ,grouper]) == st_drop_geometry(polysf[ ,grouper]))) {
      warning('the returned object got re-sorted')
    }
  }
  
  # This is particularly an issue if the re-sort happens between avgPR and the index
  if (!all(st_drop_geometry(avgPR[ ,grouper]) == st_drop_geometry(avgPRindex[ ,grouper]))) {
    stop('re-sorted data relative to index')
  }

  
  # at the least, it'll allow me to see if things have gotten jumbled between shape and sys2
  
  # Turn back into a stars
  avgPRStarstemp <- avgPR %>% 
    select(-all_of(grouper)) %>%
    st_as_stars() # %>%
    # merge() # this is where it fails with more than ~800 days
  
  nbreaks <- ceiling(length(names(avgPRStarstemp))/800) + 1
  breaks <- round(seq(from = 0, to = length(names(avgPRStarstemp)), length.out = nbreaks))
  avgPRStars <- foreach(l = 1:(length(breaks)-1),
                      .combine=function(...) c(..., along = 2), # Pass dimension argument to c.stars
                      .multicombine=TRUE) %do% {
                        bottom <- breaks[l]+1
                        top <- breaks[l+1]
                        avgPRStarstemp[bottom:top, ] %>%
                          merge()
                      }

  # change the time dimension
  st_dimensions(avgPRStars)[2] <- st_dimensions(rastst)[3]
  # st_dimensions(avgPRtars)[2] # yup, though it's still called X1?
  names(st_dimensions(avgPRStars))[2] <- names(st_dimensions(rastst))[3]
  # and change the name of the attribute
  names(avgPRStars) <- names(rastst)
  
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
  singleraster <- st_crop(singleraster, singlesf, as_points = FALSE)
  
  # time-roll the raster if needed
  if (!is.null(rastRollArgs)) {
    singleraster <- st_as_stars(singleraster)
    singleraster[[rastRollArgs$attribute_number]] <- rlang::exec(timeRoll, singleraster, !!!rastRollArgs)
  }
  # read in the stars as sf polys
    # I want to use merge = TRUE to reduce the number of polygons, but it ALSO merges the time dimension, which is bad.
  rastSF <- st_as_sf(singleraster, as_points = FALSE, merge = FALSE, na.rm = FALSE)
  
  # replace NA?
  if (!is.na(na.replace)) {
    rastSF[is.na(rastSF)] <- na.replace
  }
  
  # Transform to correct crs.
  # Doing this here, because if rastst is a proxy, we won't be able to transform until it's read in as st_as_sf
  if (st_crs(rastSF)$epsg != whichcrs) {
    rastSF <- st_transform(rastSF, whichcrs) %>%
      st_make_valid()
  }
  # ensure the polygons match
  if (st_crs(singlesf)$epsg != whichcrs) {
    singlesf <- st_transform(singlesf, whichcrs) %>%
      st_make_valid()
  }
  
  # Have to intersect with the polygons to get average.
  # Less fiddly (because it's one-to-one), and it ensures the averages are area
  # weighted (they're not with aggregate; see timePolyRastScratch for testing)
  intersectedPR <- st_intersection(singlesf, rastSF)
} 

