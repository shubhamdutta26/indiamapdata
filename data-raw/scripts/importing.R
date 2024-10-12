library(tidyverse)
library(sf)

# INDIA COUNTRY-----------------------------------------------------------------
country_i <- sf::st_read("data-raw/shapefiles/India_Country_Boundary.shp") |>
  dplyr::rename(geom = geometry)
# Convert shp file into gpkg file
sf::st_write(country_i, "inst/extdata/india_country.gpkg")

country <- sf::st_read("inst/extdata/india_country.gpkg")

# INDIA STATES------------------------------------------------------------------
state_i <- sf::st_read("data-raw/shapefiles/States/Admin2.shp") |>
  dplyr::rename(geom = geometry, full = ST_NM)

sf::st_write(state_i, "inst/extdata/india_states.gpkg")

states_id <- read.csv("data-raw/state_id.csv")

state <- sf::st_read("inst/extdata/india_states.gpkg")

state_joined <- dplyr::full_join(state, states_id, by = "full")

sf::st_write(state_joined, "inst/extdata/india_states.gpkg")

state <- sf::st_read("inst/extdata/india_states.gpkg")

centroids_state <- sf::st_centroid(state)

sf::st_write(centroids_state, "inst/extdata/india_states_centroid.gpkg")

# INDIA DISTRICTS---------------------------------------------------------------
district <- sf::st_read("data-raw/shapefiles/Districts/Census_2011/2011_Dist.shp") |>
  dplyr::rename(geom = geometry, full = ST_NM) |>
  dplyr::mutate(full = ifelse(full == "NCT of Delhi", "Delhi", full)) |>
  dplyr::mutate(full = ifelse(full == "Arunanchal Pradesh", "Arunachal Pradesh", full))

# telangana
telangana_data <- sf::st_read("data-raw/shapefiles/Telangana/TS_Dist_DISS.shp") |>
  dplyr::rename(DISTRICT = New_Dist_4, geom = geometry)

sf::st_write(telangana_data, "inst/extdata/telangana_data.gpkg")

telangana <- sf::st_read("inst/extdata/telangana_data.gpkg") |>
  dplyr::mutate(
    full = "Telangana", .after = 1
  ) |>
  dplyr::mutate(
    ST_CEN_CD = NA, .after = 2
  ) |>
  dplyr::mutate(
    DT_CEN_CD = NA, .after = 3
  ) |>
  dplyr::mutate(
    censuscode = NA, .after = 4) |>
  dplyr::select(-Area)

#The  Ensure they have the same CRS before binding
if (sf::st_crs(telangana) != sf::st_crs(district)) {
  telangana <- sf::st_transform(telangana, sf::st_crs(district))
}

districts_full <- dplyr::bind_rows(list(district, telangana))

sf::st_write(districts_full, "inst/extdata/india_districts.gpkg")

districts <- sf::st_read("inst/extdata/india_districts.gpkg")
districts_id <- read.csv("data-raw/state_id_2.csv")

sf::st_write(districts_full, "inst/extdata/india_districts.gpkg")

districts <- sf::st_read("inst/extdata/india_districts.gpkg")

centroids_district <- sf::st_centroid(districts)

invalid_geoms <- districts[!sf::st_is_valid(districts), ]
print(invalid_geoms)
valid_districts <- sf::st_make_valid(districts)

centroids_district <- sf::st_centroid(valid_districts)
sf::st_write(centroids_district, "inst/extdata/india_districts_centroid.gpkg")


# Plot--------------------------------------------------------------------------
country <- sf::st_read("inst/extdata/india_country.gpkg")
states <- sf::st_read("inst/extdata/india_states.gpkg")
districts <- sf::st_read("inst/extdata/india_districts.gpkg")
ggplot2::ggplot(data = districts) +
  ggplot2::geom_sf() +
  ggplot2::theme_minimal()


# US centroids

