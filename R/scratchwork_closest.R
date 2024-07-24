library(sf)
librray(dplyr)
library(ggplot2)

coping_spaces <- readr::read_csv("data/All_Coping_Spaces_07-04-2024.csv") |>
  dplyr::select(-geometry) |>
  suppressMessages()

coping_spaces_shp <- coping_spaces |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84")

coping_spaces_shp |>
  ggplot() +
  ggspatial::annotation_map_tile() +
  geom_sf(aes(colour = facility_type))
