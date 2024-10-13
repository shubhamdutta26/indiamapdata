#' Retrieve Indian mapping data
#'
#' @param regions The region breakdown for the map, can be one of
#'   (\code{"states"}, \code{"state"}, \code{"districts"}, \code{"district"}).
#'   The default is \code{"states"}.
#' @param include The regions to include in the resulting map. If \code{regions} is
#'  \code{"states"}/\code{"state"}, the value can be either a state name, abbreviation or code.
#'  If states are provided in the district map, only ditricts in the included states
#'  will be returned.
#' @param exclude he regions to exclude in the resulting map. If \code{regions} is
#'  \code{"states"}/\code{"state"}, the value can be either a state name, abbreviation or code.
#'  The regions listed in the \code{include} parameter are applied first and the
#'  \code{exclude} regions are then removed from the resulting map. Any excluded regions
#'  not present in the included regions will be ignored.
#'
#' @return An `sf` data frame of indian map coordinates divided by the desired \code{regions}.
#' @export
#'
#' @examples
#' str(india_map())
#'
#' df <- india_map(regions = "districts")
#'
#' include_states <- india_map(include = c("WB", "NL", "AP"))
#'
#' exclude_states <- india_map(exclude = c("WB", "NL", "AP"))
india_map <- function(
    regions = c("states", "state", "districts", "district"),
    include = c(),
    exclude = c()
) {
  regions <- match.arg(regions)

  if (regions == "state") regions <- "states"
  else if (regions == "district") regions <- "districts"

  df <- sf::read_sf(
    system.file("extdata", paste0("india_", regions, ".gpkg"),
                package = "indiamapdata")
  )

  if (regions == "states") {
    if (length(include) > 0) {
      df <- df[df$stname %in% include |
                 df$abbr %in% include |
                 df$stcode11 %in% include, ]
    }

    if (length(exclude) > 0) {
      df <- df[!(df$stname %in% exclude |
                   df$abbr %in% exclude |
                   df$stcode11 %in% exclude), ]
    }
  } else if (regions == "districts") {
    if (length(include) > 0) {
      df <- df[df$stname %in% include |
                 df$abbr %in% include |
                 df$dtcode11 %in% include, ]
    }

    if (length(exclude) > 0) {
      df <- df[!(df$stname %in% exclude |
                   df$abbr %in% exclude |
                   df$dtcode11 %in% exclude), ]
    }
  }


  df[order(df$abbr), ]
}

#' Retrieve centroid labels
#'
#' @param regions The region breakdown for the map, can be one of
#'   (\code{"states"}, \code{"districts"}, as specified by the internal file names.
#'   The default is \code{"states"}.
#'
#' @return An `sf` data frame of state or district centroid labels and positions
#'   relative to the coordinates returned by the \code{india_map} function.
#'
#' @export
centroid_labels <- function(
    regions = c("states")
) {

  regions <- match.arg(regions)

  sf::read_sf(
    system.file("extdata", paste0("india_", regions, "_centroids.gpkg"),
                package = "indiamapdata")
  )
}
