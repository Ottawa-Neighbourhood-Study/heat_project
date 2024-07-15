library(targets)
library(sf)
library(dplyr)
library(ggplot2)
library(leaflet)

source ("R/functions.R")

list(
  targets::tar_target(coping_spaces, load_coping_spaces(filepath = "data/All_Coping_Spaces_07-04-2024.csv")),
  targets::tar_target(coping_isochrones, calculate_coping_isochrones(coping_spaces)),
  # targets::tar_target(phh_coping_travel, calculate_phh_coping_travel(coping_spaces)),




  NULL
)
