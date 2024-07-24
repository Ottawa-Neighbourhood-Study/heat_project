load_coping_spaces <- function(filepath = "data/All_Coping_Spaces_07-04-2024.csv") {
  readr::read_csv(filepath) |>
    dplyr::select(-geometry) |>
    suppressMessages()
}


calculate_coping_isochrones <- function(coping_spaces) {
  coping_spaces |>
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
    )) |>
    tidyr::unnest(isochrones) |>
    dplyr::select(-data) |>
    sf::st_as_sf()
}




calculate_phh_coping_travel <- function(coping_spaces) {
  tos <- coping_spaces |>
    dplyr::select(Parent_ID, lon, lat)

  froms <- neighbourhoodstudy::ottawa_phhs |>
    dplyr::bind_cols(sf::st_coordinates(neighbourhoodstudy::ottawa_phhs)) |>
    sf::st_drop_geometry() |>
    dplyr::filter(dbpop > 0) |>
    dplyr::select(phh_id, lat = Y, lon = X) #|> head(n=10)


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



# return facility types as a character vector
get_facilities <- function() {
  c(
    "arena",
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
    "emergency_service"
  )
}


# get coverage for all urban hoods
get_facility_coverage <- function(facilities, phhs, coping_isochrones, coping_spaces, rurality) {
  purrr::map_dfr(facilities, function(facility) facility_coverage_one(facility, phhs, coping_isochrones, coping_spaces, rurality))
}

facility_coverage_one <- function(facility,
                                  phhs,
                                  coping_isochrones,
                                  coping_spaces,
                                  rurality_filter = c("Urban", "Rural", "Suburban Walk", "Suburban Drive")) {
  message(rurality_filter, "- ", facility)


  # extract the name of the rurality if it's a two-word filter parameter
  rurality_name <- stringr::str_extract(rurality_filter, ".*?(?=\\s|$)")

  phhs <- phhs |>
    dplyr::filter(tolower(rurality) == tolower(rurality_name))


  isos <- coping_isochrones |>
    dplyr::left_join(coping_spaces, by = "Parent_ID") |>
    dplyr::select(facility_type, costing, metric, contour) |>
    dplyr::filter(facility_type == facility)



  # differential filtering based on rurality
  if (rurality_filter == "Urban") {
    costing_name <- "pedestrian"
    travel_limit <- 10
  } else if (rurality_filter == "Rural") {
    costing_name <- "auto"
    travel_limit <- 15
  } else if (rurality_filter == "Suburban Walk") {
    costing_name <- "pedestrian"
    travel_limit <- 10
  } else if (rurality_filter == "Suburban Drive") {
    costing_name <- "auto"
    travel_limit <- 10
  } else {
    stop("invalid rurality_filter parameter")
  }

  # filter isochrones based on conditions
  # (switch spherical geometry off/on so that the results are valid and fast)
  sf::sf_use_s2(FALSE)

  isos <- isos |>
    dplyr::filter(
      costing == costing_name,
      contour == travel_limit
    ) |>
    sf::st_union() |>
    sf::st_make_valid() |>
    suppressMessages()

  sf::sf_use_s2(TRUE)

  phhs$covered <- sf::st_covered_by(phhs, isos) |>
    purrr::map_lgl(length)

  # phhs

  # ggplot2::ggplot() + ggplot2::geom_sf(data = isos) + ggplot2::geom_sf(data = phhs, mapping = ggplot2::aes(colour = covered))

  phhs |>
    sf::st_drop_geometry() |>
    dplyr::group_by(ONS_ID, ONS_Name, rurality) |>
    dplyr::summarise(
      pct_covered = sum(dbpop * covered) / sum(dbpop),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      facility_type = facility,
      costing = costing_name,
      travel_limit_mins = travel_limit
    )
}


get_phhs <- function(ons_shp) {
  phhs <- neighbourhoodstudy::ottawa_phhs |>
    sf::st_join(ons_shp) |>
    dplyr::filter(dbpop > 0) |>
    dplyr::select(phh_id, dbpop, ONS_ID, ONS_Name, rurality)
}


save_pct_coverage <- function(facility_rural_coverage, facility_suburban_drive_coverage, facility_suburban_walk_coverage, facility_urban_coverage) {
  all <- dplyr::bind_rows(facility_rural_coverage, facility_suburban_drive_coverage, facility_suburban_walk_coverage, facility_urban_coverage)
  readr::write_csv(all, paste0("output/analysis_1_pct_coverage-", Sys.Date(), ".csv"))
  TRUE
}



get_snapped_db_centroids <- function() {
  neighbourhoodstudy::ottawa_dbs_shp2021 |>
    dplyr::left_join(neighbourhoodstudy::ottawa_dbs_pop2021, by = "DBUID") |>
    sf::st_make_valid() |>
    sf::st_centroid() |>
    suppressWarnings() |>
    # head(n=1) |>
    snap_to_roads()
}


snap_to_roads <- function(input) {
  # message("Loading Ontario roads...")
  # ontario_roads <-
  #   sf::read_sf("~/datascience/data/spatial/lrnf000r21a_e/lrnf000r21a_e.shp",
  #     query = 'SELECT NGD_UID,NAME,RANK,CLASS FROM "lrnf000r21a_e" WHERE
  #                            ("PRNAME_L" = \'Ontario\' OR "PRNAME_R" = \'Ontario\') AND
  #                            (
  # 							("CLASS" IN (\'20\',\'21\',\'22\',\'23\')) OR
  # 							("RANK" IN (\'4\',\'5\') AND "CLASS" IN (\'12\', \'13\')) OR
  # 							("RANK" = \'1\' AND NAME NOT LIKE \'4__%\')
  # 						 ) AND NAME IS NOT NULL'
  #   )

  roads <- pseudohouseholds::ottawa_roads_shp
  roads <- sf::st_union(roads)
  roads <- sf::st_transform(roads, crs = sf::st_crs(input))

  # from https://stackoverflow.com/questions/51292952/snap-a-point-to-the-closest-point-on-a-line-segment-using-sf
  message("Snapping to road segments...")
  input %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      linestring = sf::st_nearest_points(geometry, roads),
      closest_point = sf::st_cast(linestring, "POINT")[seq(2, nrow(.) * 2, 2)]
    )
}





# get distances from snapped db centroids top/bottom coping spaces
get_db_top_bottom_distances <- function(
    db_centroids_snapped,
    coping_spaces,
    facilities_top,
    facilities_bottom) {
  froms <- db_centroids_snapped |>
    dplyr::bind_cols(sf::st_coordinates(db_centroids_snapped)) |>
    sf::st_drop_geometry() |>
    dplyr::select(DBUID, lat = Y, lon = X)

  tos <- coping_spaces |>
    dplyr::filter(facility_type %in% c(facilities_top, facilities_bottom)) |>
    dplyr::select(Parent_ID, lat, lon)


  # errors <- c()
  results <- dplyr::tibble()
  to_batch_size <- 100
  to_batches <- ceiling(nrow(tos) / to_batch_size)


  # also batch the tos
  for (i in 0:(to_batches - 1)) {
    message(i + 1, "/", to_batches)

    batch_rows <- ((i * to_batch_size) + 1):min(((i + 1) * to_batch_size), nrow(tos))

    z <- tryCatch(valhallr::od_table(
      froms = froms, from_id_col = "DBUID",
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
