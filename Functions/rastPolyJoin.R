# Function to intersect raster and polygons and get spatial average and return stars

rastPolyJoin <- function(polysf, rastst, grouper = 'SYS2', maintainPolys = TRUE) {
  # polysf is a spatail polygon simple feactures object
  # rastst is a raster brick in stars format
  # grouper is a single grouping variable to identify single polygons in polysf
  # maintainPolys TRUE averages back into them. FALSE leaves them split, and
  # makes a new grouping variable to keep it distinct
  # This function calculates a mean. wouldn't be too hard to expand to other metrics.
  
  # turn raster into polygon sf object, time as columns
    # This can take a really long time
  rastSF <- st_as_sf(rastst, as_points = FALSE, merge = FALSE, na.rm = FALSE)
  
  # Have to intersect with the polygons to get average.
  # Less fiddly (because it's one-to-one), and it ensures the averages area area
  # weighted (they're not with aggregate; see timePolyRastScratch for testing)
  intPR <- st_intersection(polysf, rastSF)
  
  if (maintainPolys) {
    # Get the averages into each ANAE ID at each time
    # This strips off the other ANAE cols, which is fine. Could do it earlier, but don't see a need?
    # TODO:: Again, rename to reduce size later on
    # 255 seconds with 221 cols
    avgPR <- intPR %>%
      mutate(area = st_area(.)) %>%  
      group_by(across(all_of(grouper))) %>%
      summarize(across(starts_with("X"), ~weighted.mean(.x, as.numeric(area)))) %>% # averages across the cols with x in their name and gets weighted mean
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
      select(grouper, starts_with('X')) # to have same format as the summarize, drop everything but the grouper and times
  }
 
  
  # It has been re-sorted for some unknown stupid dplyr reason. Make sure it is sorted back
  avgPR <- avgPR %>% arrange(across(all_of(grouper)))
  # Check (unless we KNOW it's wrong)
  if (maintainPolys) {
    if (!all(st_drop_geometry(avgPR[ ,grouper]) == st_drop_geometry(polysf[ ,grouper]))) {
      warning('the returned object got re-sorted')
    }
  }

  
  # And, still, save the indexer column (is it possible to glue this back on as a dimension? Probably not, because it's parallel to shape)
  avgPRindex <- avgPR[ ,grouper] # leave the geometry ON this one, maybe?
  
  # at the least, it'll allow me to see if things have gotten jumbled between shape and sys2
  
  # Turn back into a stars
  avgPRStars <- avgPR %>% 
    select(-grouper) %>%
    st_as_stars() %>%
    merge()
  # avgA_Stars
  
  # change the time dimension
  st_dimensions(avgPRStars)[2] <- st_dimensions(rastst)[3]
  # st_dimensions(avgPRtars)[2] # yup, though it's still called X1?
  names(st_dimensions(avgPRStars))[2] <- names(st_dimensions(rastst))[3]
  # and change the name of the attribute
  names(avgPRStars) <- names(rastst)
  
  return(list(avgPRStars, avgPRindex))
  
}