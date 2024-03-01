# adds extra space to your plot bounding box by adding a proportion of the x and y axes as required for 
# ggplot + coord_sf(xlim = st_coordinates(bbox_new)[c(1,2),1], # min & max of x values
#                   ylim = st_coordinates(bbox_new)[c(2,3),2]) # min & max of y values

BBoxAdj <- function(bbox, expandLeft = 0, expandRight = 0, expandTop = 0, expandBot = 0){
  # require(sf)
  # require(ggplot2)
  bbox_new <- st_bbox(bbox) 
  xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
  yrange <- bbox_new$ymax - bbox_new$ymin # range of y values
  
  bbox_new[1] <- bbox_new[1] - (expandLeft * xrange) # xmin - left
  bbox_new[3] <- bbox_new[3] + (expandRight * xrange) # xmax - right
  bbox_new[2] <- bbox_new[2] - (expandBot * yrange) # ymin - bottom
  bbox_new[4] <- bbox_new[4] + (expandTop * yrange) # ymax - top
  
  bbox_new <- bbox_new |>  # take the bounding box ...
    st_as_sfc()
  return(bbox_new)
  # sfPlotObj +
  #   coord_sf(xlim = st_coordinates(bbox_new)[c(1,2),1], # min & max of x values
  #            ylim = st_coordinates(bbox_new)[c(2,3),2]) # min & max of y values
  
}
# -- expanding bounding box method 
# boundAdj <- function(sfPlotObj, expandLeft = 1, expandRight = 1, expandTop = 1, expandBot = 1){
#   require(sf)
#   require(ggplot2)
#   bbox_new <- st_bbox(sfPlotObj) 
#   xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
#   yrange <- bbox_new$ymax - bbox_new$ymin # range of y values
#   
#   bbox_new[1] <- bbox_new[1] - (expandLeft * xrange) # xmin - left
#   bbox_new[3] <- bbox_new[3] + (expandRight * xrange) # xmax - right
#   bbox_new[2] <- bbox_new[2] - (expandBot * yrange) # ymin - bottom
#   bbox_new[4] <- bbox_new[4] + (expandTop * yrange) # ymax - top
#   
#   bbox_new <- bbox_new |>  # take the bounding box ...
#     st_as_sfc()
#   
#   sfPlotObj +
#     coord_sf(xlim = st_coordinates(bbox_new)[c(1,2),1], # min & max of x values
#              ylim = st_coordinates(bbox_new)[c(2,3),2]) # min & max of y values
#   
# }

