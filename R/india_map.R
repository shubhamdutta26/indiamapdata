#' Xxx
#'
#' @param regions xxx
#' @param include xxx
#' @param exclude xxx
#'
#' @return xxx
#' @export
#'
#' @examples
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

  if (length(include) > 0) {
    df <- df[df$full %in% include | df$abbr %in% include %in% include, ]
  }

  if (length(exclude) > 0) {
    df <- df[!(df$full %in% exclude | df$abbr %in% exclude  %in% exclude), ]
  }

  df[order(df$abbr), ]
}

#' Retrieve centroid labels
#'
#' @param regions The region breakdown for the map, can be one of
#'   (\code{"states"}, \code{"counties"}, as specified by the internal file names.
#'   The default is \code{"states"}.
#'
#' @return An `sf` data frame of state or county centroid labels and positions
#'   relative to the coordinates returned by the \code{us_map} function.
#'
#' @export
centroid_labels <- function(
    regions = c("states", "districts")
) {

  regions <- match.arg(regions)

  sf::read_sf(
    system.file("extdata", paste0("india_", regions, "_centroids.gpkg"),
                package = "indiamapdata")
  )
}
