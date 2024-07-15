library(ggplot2)
library(dplyr)
library(sf)
targets::tar_load(coping_isochrones)
targets::tar_load(coping_spaces)
targets::tar_load(ons_shp)

ons_shp


get_phhs <- function() {
phhs <- neighbourhoodstudy::ottawa_phhs |>
  sf::st_join(ons_shp) |>
  dplyr::filter(dbpop > 0) |>
  dplyr::select(phh_id, dbpop, ONS_ID, ONS_Name, rurality)
}


facilities <- c("arena",
                "cmmt_garden",
                "library",
                "outdoor_cafe",
                "shade_structure",
                "beach",
                "water_fountain",
                "mall",
                "indoor_pool",
                "splash_pad",
                "chc",
                "fastfood",
                "museum",
                "outdoor_pool",
                "movie_theatre",
                "cmmt_centre",
                "gallery",
                "other",
                "religious_centre",
                "wading_pool",
                "medical",
                "pharmacy",
                "emergency_service")

# URBAN

facility <- facilities[[12]]

test <- purrr::map_dfr(facilities, function(facility) {

  message(facility)
  phhs <- phhs |>
    dplyr::filter(rurality == "Urban")

  isos <- coping_isochrones |>
    dplyr::left_join(coping_spaces) |>
    dplyr::select(facility_type, costing, metric, contour) |>
    dplyr::filter(facility_type == facility ) |>
    dplyr::filter(costing == "pedestrian") |>
    sf::st_union() |>
    sf::st_make_valid() |>
    suppressMessages()



  phhs$covered <- sf::st_covered_by(phhs, isos) |>
    purrr::map_lgl(length)

 # phhs

  #ggplot() + geom_sf(data = isos) + geom_sf(data = phhs, mapping = aes(colour = covered))

  phhs |>
    sf::st_drop_geometry() |>
    dplyr::group_by(ONS_ID, ONS_Name, rurality) |>
    dplyr::summarise(pct_covered = sum(dbpop * covered) / sum(dbpop),
                     .groups = "drop") |>
    dplyr::mutate(facility_type = facility)

})





## isos not valid?? NEEDED TO SWITCH OFF s2
library(ggplot2)
targets::tar_load(coping_isochrones)

isos <- sf::st_make_valid(coping_isochrones)

isos$valid <- sf::st_is_valid(isos)

isos |>
  dplyr::filter(!valid) |>
  #head(n=1) |>
  sf::st_make_valid()  |> sf::st_is_valid()
  ggplot() + geom_sf()
