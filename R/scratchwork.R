library(sf)
library(dplyr)
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


phhs <- neighbourhoodstudy::ottawa_phhs |>
  dplyr::filter(dbpop > 0)

phhs_foranalysis <- dplyr::bind_cols(phhs, sf::st_coordinates(phhs)) |>
  sf::st_drop_geometry() |>
  dplyr::rename(lon = X, lat = Y)

ggplot(phhs) +
  geom_sf()


arenas <- coping_spaces |>
  dplyr::filter(facility_type == "arena")

tos <- dplyr::select(arenas, Parent_ID, lat, lon)
froms <- dplyr::select(phhs_foranalysis, phh_id, lat, lon)

test <- valhallr::od_table(
  tos = tos,
  to_id_col = "Parent_ID",
  froms = froms,
  from_id_col = "phh_id",
  costing = "auto",
  hostname = "192.168.0.150",
  verbose = TRUE
)

iso <- valhallr::isochrone(from = arenas[1, ], hostname = "192.168.0.150")

iso
valhallr::map_isochrone(iso)
# start with community centres and libraries

arenas$Parent_ID |>
  unique() |>
  length()

phhs


test <- coping_spaces |>
  dplyr::select(-geometry) |>
  # dplyr::group_by(Parent_ID) |>
  tidyr::nest(data = -Parent_ID) |>
  head() |>
  dplyr::mutate(isochrones = purrr::map(
    data,
    function(x) {
      drive <- valhallr::isochrone(from = x, costing = "auto", contours = c(10, 15), hostname = "192.168.0.150")
      walk <- valhallr::isochrone(from = x, costing = "pedestrian", contours = 10, hostname = "192.168.0.150")

      dplyr::bind_rows(drive, walk) |>
        dplyr::select(-fill.opacity, -fillColor, -opacity, -fill, -fillOpacity, -color)
    }
  )) |>
  tidyr::unnest(isochrones) |>
  tidyr::unnest(data)

test |>
  tidyr::unnest(isochrones) |>
  tidyr::unnest(data)

library(ggplot2)

test |>
  tidyr::unnest(isochrones) |>
  tidyr::unnest(data) |>
  sf::st_as_sf() |>
  ggplot() +
  geom_sf(aes(fill = costing))




coping_spaces |>
  head(n = 10) |>
  tidyr::nest(data = -Parent_ID) |>
  dplyr::mutate(isochrones = purrr::map(
    data,
    function(x) {
      drive <- valhallr::isochrone(from = x, costing = "auto", contours = c(10, 15), hostname = "192.168.0.150")
      walk <- valhallr::isochrone(from = x, costing = "pedestrian", contours = 10, hostname = "192.168.0.150")

      dplyr::bind_rows(drive, walk) |>
        dplyr::select(-fill.opacity, -fillColor, -opacity, -fill, -fillOpacity, -color)
    },
    .progress = TRUE
  ))




### travel distance analysis

calculate_phh_coping_travel <- function(coping_spaces) {
  tos <- coping_spaces |>
    dplyr::select(Parent_ID, lon, lat)

  froms <- neighbourhoodstudy::ottawa_phhs |>
    dplyr::bind_cols(sf::st_coordinates(neighbourhoodstudy::ottawa_phhs)) |>
    sf::st_drop_geometry() |>
    dplyr::select(phh_id, lat = Y, lon = X) |>
    head(n = 10)


  # errors <- c()
  results <- dplyr::tibble()
  to_batch_size <- 100
  to_batches <- ceiling(nrow(tos) / to_batch_size)


  # also batch the tos
  for (i in 0:(to_batches - 1)) {
    message(i + 1, "/", to_batches)

    batch_rows <- ((i * to_batch_size) + 1):min(((i + 1) * to_batch_size), nrow(tos))

    z <- tryCatch(valhallr::od_table(
      froms = froms, from_id_col = "phh_id",
      tos = tos[batch_rows, ], to_id_col = "Parent_ID",
      batch_size = 100,
      hostname = "192.168.0.150", verbose = TRUE
    ))

    if ("try-error" %in% class(z)) {
      message(i, " ERROR")
      errors <- c(errors, i)
    } else {
      results <- dplyr::bind_rows(results, z)
    }
  } # end for

  results
}



#### mapping dbs to hoods for avg distance
library(sf)
library(dplyr)

targets::tar_load(db_top_bottom_distances)
targets::tar_load(coping_spaces)

# get neighbourhood-level population-weighted average travel times to
# top 5 and bottom 5 heat coping spaces
get_hood_times <- function(coping_spaces, db_top_bottom_distances) {
  stats <- dplyr::left_join(
    db_top_bottom_distances,
    coping_spaces,
    by = "Parent_ID"
  ) |>
    dplyr::select(DBUID, distance, time, facility_type)

  stats <- stats |>
    dplyr::left_join(neighbourhoodstudy::ottawa_dbs_pop2021, by = "DBUID") |>
    dplyr::filter(dbpop2021 > 0) |>
    dplyr::mutate(DAUID = substr(DBUID, 1, 8)) |>
    dplyr::left_join(neighbourhoodstudy::sli_das_gen3_mape, by = "DAUID") |>
    dplyr::group_by(DBUID, facility_type) |>
    dplyr::arrange(time) |>
    dplyr::slice_head(n = 1)


  results <- stats |>
    dplyr::group_by(ONS_ID, facility_type) |>
    #  dplyr::filter(ONS_ID == "3001", facility_type == "fastfood")
    # dplyr::arrange(ONS_ID, facility_type)
    dplyr::summarise(
      popwt_avg_time = sum(time * dbpop2021) / sum(dbpop2021),
      .groups = "drop"
    )
  results
}



## check results

targets::tar_load(hood_times)
targets::tar_load(ons_shp)

test_type <- "library"

test_ons_shp <- dplyr::left_join(
  ons_shp,
  dplyr::filter(hood_times, facility_type == test_type)
)

test_spaces_shp <- coping_spaces |>
  dplyr::filter(facility_type == test_type) |>
  dplyr::select(lat, lon) |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84")

ggplot2::ggplot() +
  ggspatial::annotation_map_tile(zoomin = -1) +
  ggplot2::geom_sf(data = test_ons_shp, mapping = ggplot2::aes(fill = popwt_avg_time_s)) +
  ggplot2::geom_sf(data = test_spaces_shp) +
  ggplot2::labs(title = sprintf("Facility type: %s", test_type))







### ANALYSIS 3

targets::tar_load(coping_isochrones)
targets::tar_load(coping_spaces)
targets::tar_load(phhs)
run_analysis_3 <- function(coping_isochrones, coping_spaces, phhs) {
  facility_types <- c(
    "cmmt_centre",
    "shade_structure",
    "library",
    "splash_pad",
    "mall",
    "wading_pool",
    "cmmt_garden",
    "beach",
    "outdoor_pool",
    "religious_centre"
  )

  sf::sf_use_s2(FALSE)
  union_isos <- coping_isochrones |>
    dplyr::left_join(coping_spaces, by = "Parent_ID") |>
    dplyr::filter(facility_type %in% facility_types) |>
    sf::st_make_valid() |>
    dplyr::select(facility_type, costing, contour) |>
    dplyr:::group_by(contour, costing, facility_type) |>
    tidyr::nest() |>
    dplyr::mutate(data = purrr::map(data, sf::st_union)) |>
    tidyr::unnest() |>
    sf::st_as_sf() |>
    dplyr::mutate(rurality = dplyr::case_when(
      costing == "auto" & contour == 15 ~ "rural",
      costing == "pedestrian" ~ "urban",
      TRUE ~ "suburban/town"
    ))

  # URBAN
  iso_now <- dplyr::filter(union_isos, rurality == "urban")
  urban_coverage <- phhs |>
    dplyr::filter(rurality == "urban") |>
    dplyr::mutate(covered_by = sf::st_intersects(geometry, iso_now) |> purrr::map_dbl(length))

  # SUBURBAN / TOWN
  iso_now <- dplyr::filter(union_isos, rurality == "suburban/town")
  suburban_town_coverage <- phhs |>
    dplyr::filter(rurality %in% c("suburban", "town")) |>
    dplyr::mutate(covered_by = sf::st_intersects(geometry, iso_now) |> purrr::map_dbl(length))


  # RURAL
  iso_now <- dplyr::filter(union_isos, rurality == "rural")
  rural_coverage <- phhs |>
    dplyr::filter(rurality == "rural") |>
    dplyr::mutate(covered_by = sf::st_intersects(geometry, iso_now) |> purrr::map_dbl(length))

  dplyr::bind_rows(urban_coverage, rural_coverage, suburban_town_coverage) |>
    sf::st_drop_geometry() |>
    dplyr::mutate(threshold_met = covered_by >= 2) |>
    dplyr::mutate(covered_pop = dbpop * threshold_met) |>
    dplyr::group_by(ONS_ID, ONS_Name, rurality) |>
    dplyr::summarise(pct_covered = sum(covered_pop) / sum(dbpop))
} # end function run_analysis_3()
