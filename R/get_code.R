#' Retrieve state and district codes
#'
#' @param regions The region breakdown for the map, can be one of
#'   (\code{"states"}, \code{"state"}, \code{"districts"}, \code{"district"}).
#'   The default is \code{"states"}.
#'
#' @return An data frame of codes of the desired \code{regions}.
#'
#' @examples
#' str(get_code())
#'
#' state_codes <- get_code()
#' county_codes <- get_code(regions = "districts")
#'
#' @export
get_code <- function(
    regions = c("states", "state", "districts", "district")
) {
  regions <- match.arg(regions)

  map_data <- indiamapdata::india_map(regions)
  sf::st_geometry(map_data) <- NULL
  map_data
}
