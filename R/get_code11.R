#' Retrieve state and district codes
#'
#' @param regions The region breakdown for the map, can be one of
#'   (\code{"states"}, \code{"state"}, \code{"districts"}, \code{"district"}).
#'   The default is \code{"states"}.
#'
#' @return An data frame of codes of the desired \code{regions}.
#'
#' @examples
#' str(get_code11())
#'
#' state_codes <- get_code11()
#' county_codes <- get_code11(regions = "districts")
#'
#' @export
get_code11 <- function(
    regions = c("states", "state", "districts", "district")
) {
  regions <- match.arg(regions)

  map_data <- indiamapdata::india_map(regions)
  sf::st_geometry(map_data) <- NULL
  if (regions %in% c("states", "state")) {
    map_data <- dplyr::rename(map_data, code11 = "stcode11")
  } else if (regions %in% c("districts", "district")) {
    map_data$code11 <- paste0(map_data$stcode11, map_data$dtcode11)
  }
  map_data
}
