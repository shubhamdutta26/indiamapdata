library(tidyverse)
library(sf)

# INDIA COUNTRY-----------------------------------------------------------------
country_i <- sf::st_read("data-raw/shapefiles/India_Country_Boundary.shp") |>
  dplyr::rename(geom = geometry)
# Convert shp file into gpkg file
sf::st_write(country_i, "inst/extdata/india_country.gpkg")

country <- sf::st_read("inst/extdata/india_country.gpkg")

# Districts---------------------------------------------------------------------
states_abbr_dt <- readxl::read_excel("data-raw/state_abbr_for_district.xlsx")

districts <- sf::read_sf("data-raw/shapefiles/INDIA_DISTRICTS.geojson")

new_districts <- dplyr::full_join(districts, states_abbr_dt, by = "stcode11")

sf::st_write(new_districts, "inst/extdata/india_districts.gpkg")

# Plot--------------------------------------------------------------------------
states_abbr <- readxl::read_excel("data-raw/state_abbr.xlsx")

country <- sf::st_read("inst/extdata/india_country.gpkg")

states <- sf::st_read("inst/extdata/india_states.gpkg")
states_cen <- sf::st_read("inst/extdata/india_states_centroids.gpkg")

districts <- sf::st_read("inst/extdata/india_districts.gpkg")


palette_brewer <- RColorBrewer::brewer.pal(12, "Paired")


ggplot2::ggplot(data = states) +
  ggplot2::geom_sf(ggplot2::aes(fill = STNAME)) +
  ggplot2::scale_fill_manual(values = rep(sample(palette_brewer, 12),
                                          length.out = 37)) +
  ggplot2::theme_minimal()
