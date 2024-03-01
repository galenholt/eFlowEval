get_pixarea <- function(onefile, crs, sub = TRUE) {
  rasterio = list(nXOff = 1, nYOff = 1, nXSize = 10, nYSize = 10)

  # Proxy = TRUE fails in the st_as_sf for proxys. This is therefore potentially
  # a major bottleneck for data with high numbers of sheets in the 3rd+
  # dimensions
  data1010 <- onefile |> # filename
    stars::read_stars(RasterIO = rasterio, sub = sub, proxy = FALSE)

  extradims <- names(stars::st_dimensions(data1010))[!(names(stars::st_dimensions(data1010)) %in% c("x", 'y'))]

  # Not sure this will actually work for dims > 3
  if (length(extradims) > 0) {
    data1010 <- data1010 |>
      slice(!!extradims, 1)
  }

  pixarea <- data1010 |>
    sf::st_set_crs(crs) |>
    sf::st_as_sf(as_points = FALSE, merge = FALSE, na.rm = FALSE) |> # Read in as sf polygons
    dplyr::mutate(area = sf::st_area(geometry)) |>
    # st_drop_geometry() |>
    dplyr::summarize(area = mean(area, na.rm = TRUE))

  return(pixarea$area)
}

