library(targets)
library(sf)
library(dplyr)
library(ggplot2)
library(leaflet)

source ("R/functions.R")

# disable s2 so that isochrones will be valid
#sf::sf_use_s2(FALSE)


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

  NULL
)
