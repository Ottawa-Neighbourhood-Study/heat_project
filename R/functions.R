

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
