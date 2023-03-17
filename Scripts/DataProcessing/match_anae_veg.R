# goal here is to find the ANAE types where Lippia and Centipeda have been seen

source('directorySet.R')
# ALA package
library(galah)
library(sf)

vegdir <- file.path(datOut, 'Vegmapping')

# setup
galah_config(email = "g.holt@deakin.edu.au", atlas = "Australia")


# Data acquisition --------------------------------------------------------

# according to the website, lippi has a synonym- phyla nodiflora. Get both
lippia_phyla <- galah_call() |>
  galah_identify(c("lippia", 'phyla nodiflora')) |>
  atlas_occurrences()

# lippia_phyla

# just get cunninghamii, though there are other centipedas.
centipeda <- galah_call() |>
  galah_identify("Centipeda cunninghamii") |>
  atlas_occurrences()

# bring in the ANAE to match
load(file.path(datOut, 'ANAEprocessed', 'ANAEbasinclim.rdata'))
ANAEbasinclim <- st_make_valid(ANAEbasinclim)

# Geointersection ---------------------------------------------------------

# Make the lippia and centipeda geographic
lippia_phyla_geo <- lippia_phyla %>% 
  tidyr::drop_na(decimalLongitude, decimalLatitude) %>% 
  st_as_sf(coords = c('decimalLongitude', 'decimalLatitude')) %>% 
  st_set_crs("WGS84") %>% 
  st_transform(st_crs(ANAEbasinclim))

# Make the lippia and centipeda geographic
centipeda_geo <- centipeda %>% 
  tidyr::drop_na(decimalLongitude, decimalLatitude) %>% 
  st_as_sf(coords = c('decimalLongitude', 'decimalLatitude')) %>% 
  st_set_crs("WGS84") %>% 
  st_transform(st_crs(ANAEbasinclim))

# check it looks right
plot(lippia_phyla_geo['scientificName'])
plot(centipeda_geo['scientificName'])

# intersect with ANAE
# The left = FALSE returns an inner join, e.g. only matching records
# These take a while (10ish minutes each?)
lippia_anae <- st_join(lippia_phyla_geo, ANAEbasinclim, left = FALSE)

cent_anae <- st_join(centipeda_geo, ANAEbasinclim, left = FALSE)


# Get unique anae types ---------------------------------------------------

# I was going to just grab vectors, but maybe better to provide a bit more info
# lippia_anae_types <- unique(lippia_anae$ANAE_CODE)
# centipeda_anae_types <- unique(cent_anae$ANAE_CODE)

lippia_anae_types <- lippia_anae %>% 
  st_drop_geometry() %>% 
  select(scientificName, ANAE_CODE, ANAE_DESC) %>% 
  distinct()

centipeda_anae_types <- cent_anae %>% 
  st_drop_geometry() %>% 
  select(scientificName, ANAE_CODE, ANAE_DESC) %>% 
  distinct()

# out of curiostity, what are those names?


# I kind of want to save these with rds, but I think I actually do want them to
# retain names on read-in
if (!dir.exists(vegdir)) {dir.create(vegdir, recursive = TRUE)}
save(lippia_anae_types, file = file.path(vegdir, 'lippia_anae.rdata'))
save(centipeda_anae_types, file = file.path(vegdir, 'centipeda_anae.rdata'))
