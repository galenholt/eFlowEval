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