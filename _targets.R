library(targets)
library(sf)
library(dplyr)
library(ggplot2)
library(leaflet)

source("R/functions.R")

# disable s2 so that isochrones will be valid
# sf::sf_use_s2(FALSE)


list(
  targets::tar_target(coping_spaces, load_coping_spaces(filepath = "data/All_Coping_Spaces_07-04-2024.csv")),
  targets::tar_target(coping_isochrones, calculate_coping_isochrones(coping_spaces)),
  targets::tar_target(ons_shp, dplyr::filter(neighbourhoodstudy::ons_gen3_shp, ONS_Region == "OTTAWA")),
  targets::tar_target(facilities, get_facilities()),
  targets::tar_target(phhs, get_phhs(ons_shp)),
  # targets::tar_target(phh_coping_travel, calculate_phh_coping_travel(coping_spaces)),


  targets::tar_target(facility_urban_coverage, get_facility_coverage(facilities, phhs, coping_isochrones, coping_spaces, rurality = "Urban")),
  targets::tar_target(facility_rural_coverage, get_facility_coverage(facilities, phhs, coping_isochrones, coping_spaces, rurality = "Rural")),
  targets::tar_target(facility_suburban_walk_coverage, get_facility_coverage(facilities, phhs, coping_isochrones, coping_spaces, rurality = "Suburban Walk")),
  targets::tar_target(facility_suburban__drivecoverage, get_facility_coverage(facilities, phhs, coping_isochrones, coping_spaces, rurality = "Suburban Drive")),
  targets::tar_target(save_pct_coverage_results, save_pct_coverage(facility_rural_coverage, facility_suburban_walk_coverage, facility_suburban__drivecoverage, facility_urban_coverage)),


  ## ANALYSIS 2
  targets::tar_target(db_centroids_snapped, get_snapped_db_centroids()),
  targets::tar_target(
    db_top_bottom_distances,
    get_db_top_bottom_distances(
      db_centroids_snapped,
      coping_spaces,
      facilities_top = c(
        "splash_pad",
        "cmmt_centre",
        "library",
        "mall",
        "shade_structure"
      ),
      facilities_bottom = c(
        "fastfood",
        "museum",
        "gallery",
        "arena",
        "movie_theatre"
      )
    )
  ),
  targets::tar_target(hood_times, get_hood_times(coping_spaces, db_top_bottom_distances)),
  targets::tar_target(save_hood_times, {
    readr::write_csv(
      hood_times,
      sprintf("output/analysis_2_hood_travel_times-%s.csv", Sys.Date())
    )
    TRUE
  }),

  ## ANALYSIS 3 - % pop covered by 2 or more different top-10 facilities
  targets::tar_target(
    analysis_3_results,
    run_analysis_3(
      coping_isochrones = coping_isochrones,
      coping_spaces = coping_spaces,
      phhs = phhs
    )
  ),
  targets::tar_target(
    save_analysis_3,
    readr::write_csv(analysis_3_results, paste0("output/analysis_3_hood_coverage_pct-", Sys.Date(), ".csv"))
  ),
  NULL
)
