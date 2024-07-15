

load_coping_spaces <- function(filepath = "data/All_Coping_Spaces_07-04-2024.csv"){
  readr::read_csv(filepath) |>
    dplyr::select(-geometry) |>
    suppressMessages()
}


calculate_coping_isochrones <- function(coping_spaces){

  coping_spaces |>
    tidyr::nest(data = -Parent_ID) |>
    dplyr::mutate(isochrones = purrr::map(
      data,
      function(x) {

        drive <- valhallr::isochrone(from = x, costing = "auto", contours = c(10, 15), hostname = "192.168.0.150")
        walk  <- valhallr::isochrone(from = x, costing = "pedestrian", contours = 10, hostname = "192.168.0.150")

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
    dplyr::select(phh_id, lat=Y, lon=X) #|> head(n=10)


  # errors <- c()
  results <- dplyr::tibble()
  to_batch_size <- 100
  to_batches <- ceiling(nrow(tos)/to_batch_size)


  # also batch the tos
  for (i in 0:(to_batches-1)){
    message(i+1, "/", to_batches)

    batch_rows <- ((i * to_batch_size) + 1):min(((i+1)* to_batch_size), nrow(tos))

    z <- tryCatch(valhallr::od_table(froms = froms, from_id_col = "phh_id",
                                     tos = tos[batch_rows,], to_id_col = "Parent_ID",
                                     batch_size=100,
                                     hostname = "192.168.0.150", verbose = TRUE))

    if ("try-error" %in% class(z) ){
      message(i," ERROR")
      errors <- c(errors, i)
    } else {
      results <- dplyr::bind_rows(results, z)
    }

  } # end for

  results
}



# return facility types as a character vector
get_facilities <- function() {
  c("arena",
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
}


# get coverage for all urban hoods
get_facility_coverage <- function(facilities, phhs, coping_isochrones, coping_spaces, rurality) {
  purrr::map_dfr(facilities, function(facility) facility_coverage_one(facility, phhs, coping_isochrones, coping_spaces, rurality))
}

facility_coverage_one <- function(facility,
                                  phhs,
                                  coping_isochrones,
                                  coping_spaces,
                                  rurality_filter = c("Urban", "Rural", "Suburban")) {

  message(rurality_filter, "- ", facility)


  phhs <- phhs |>
    dplyr::filter(rurality == rurality_filter)


  isos <- coping_isochrones |>
    dplyr::left_join(coping_spaces, by = "Parent_ID") |>
    dplyr::select(facility_type, costing, metric, contour) |>
    dplyr::filter(facility_type == facility )

  sf::sf_use_s2(FALSE)
  # differential filtering based on rurality
  if (rurality_filter == "Urban"){
    isos <- isos |>
      dplyr::filter(costing == "pedestrian") |>
      sf::st_union() |>
      sf::st_make_valid() |>
      suppressMessages()
  } else if (rurality_filter == "Rural"){
    isos <- isos |>
      dplyr::filter(costing == "auto",
                    contour == 15) |>
      sf::st_union() |>
      sf::st_make_valid() |>
      suppressMessages()
  } else if (rurality_filter == "Suburban") {
    isos <- isos |>
      sf::st_make_valid() |>
      dplyr::filter(contour == 10) |> # 10-minute walk or 10-minute drive
      sf::st_make_valid() |>
      sf::st_union() |>
      sf::st_make_valid() |>
      suppressMessages()
  }

  sf::sf_use_s2(TRUE)

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

}


get_phhs <- function(ons_shp) {
  phhs <- neighbourhoodstudy::ottawa_phhs |>
    sf::st_join(ons_shp) |>
    dplyr::filter(dbpop > 0) |>
    dplyr::select(phh_id, dbpop, ONS_ID, ONS_Name, rurality)
}


save_pct_coverage <- function(facility_rural_coverage, facility_suburban_coverage, facility_urban_coverage){
  all <- dplyr::bind_rows(facility_rural_coverage, facility_suburban_coverage, facility_urban_coverage)
  readr::write_csv(all, paste0("output/analysis_1_pct_coverage-",Sys.Date(),".csv"))
  TRUE
}
