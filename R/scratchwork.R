library(sf)
library(dplyr)
library(ggplot2)
coping_spaces <- readr::read_csv("data/All_Coping_Spaces_07-04-2024.csv")

coping_spaces_shp <- coping_spaces |>
  sf::st_as_sf(coords=c("lon","lat"), crs="WGS84")

coping_spaces_shp |>
  ggplot() +
  ggspatial::annotation_map_tile() +
  geom_sf(aes(colour = facility_type))


phhs <- neighbourhoodstudy::ottawa_phhs |>
  dplyr::filter(dbpop > 0)

phhs_foranalysis <- dplyr::bind_cols(phhs, sf::st_coordinates(phhs)) |>
  sf::st_drop_geometry() |>
  dplyr::rename(lon=X, lat=Y)

ggplot(phhs) + geom_sf()


arenas <- coping_spaces |>
  dplyr::filter(facility_type == "arena")

tos <- dplyr::select(arenas, Parent_ID, lat, lon)
froms <- dplyr::select(phhs_foranalysis, phh_id, lat, lon)

test <- valhallr::od_table(tos = tos,
                           to_id_col = "Parent_ID",
                           froms = froms,
                           from_id_col = "phh_id",
                           costing = "auto",
                           hostname = "192.168.0.150",
                           verbose = TRUE )

iso <- valhallr::isochrone(from = arenas[1,], hostname = "192.168.0.150")

iso
valhallr::map_isochrone(iso)
# start with community centres and libraries

arenas$Parent_ID |> unique() |> length()

phhs
