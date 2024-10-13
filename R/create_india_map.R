#' Internal map creation tools
#'
#' @description
#' `create_india_map()` creates the modified shapefiles used by the
#' \link[indiamap]{indiamap} package.
#'
#' `ea_crs()` returns the World Geodetic System 1984 projection method
#' (CRS) used by this package and `indiamap`.
#'
#' `compute_centroids()` computes the modified centroids for each state or
#' district polygon using a center-of-mass technique on the largest polygon in
#' the region.
#'
#' @note
#' Using these functions externally is not recommended since they make certain
#' undocumented assumptions that may not work with all inputs.
#'
#' It is strongly recommend that the \link[indiamap]{indiamap} package is used
#' directly.
#'
#' In some cases where the raw data is required, the \link{india_map} and
#' \link{centroid_labels} functions located in this package can be used instead.
#'
#' @references {
#'  Gert (2017). “How to calculate
#'  polygon centroids in R (for
#'  non-contiguous shapes).”
#'  <https://gis.stackexchange.com/a/265475>.
#' }
#'
#'
#' @keywords internal
create_india_map <- function(
    type = c("states", "districts"),
    input_file,
    output_file
) {
  # check for dplyr
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("`dplyr` must be installed to use `create_india_map()`.
         Use: install.packages(\"dplyr\") and try again.")
  }

  type <- match.arg(type)

  # import map file
  india <- sf::read_sf(input_file)

  # ea: 4326
  india_ea <- sf::st_transform(india, ea_crs())

  # delete unused columns
  cols <- c()
  if (type == "states") {
    cols <- c("STNAME", "STCODE11", "geometry")
  } else if (type == "districts") {
    cols <- c("dtname", "stname", "stcode11", "dtcode11", "geometry")
  }
  india_ea <- dplyr::select(india_ea, dplyr::all_of(cols))

  # rename remaining columns
  new_cols <- c()
  if (type == "states") {
    new_cols <- c(stcode11 = "STCODE11", geom = "geometry", stname = "STNAME")
  } else if (type == "districts") {
    new_cols <- c(geom = "geometry")
  }
  india_ea <- dplyr::rename(india_ea, dplyr::all_of(new_cols))

  # join with abbr
  if (type == "states") {
    state_abbr <- readxl::read_excel("data-raw/state_abbr.xlsx")
    india_ea <- dplyr::full_join(india_ea, state_abbr, by = "stcode11")
  } else if (type == "districts") {
    state_abbr_for_dt <- readxl::read_excel("data-raw/state_abbr_for_district.xlsx")
    india_ea <- dplyr::full_join(india_ea, state_abbr_for_dt, by = "stcode11")
  }

  # sort output
  if (type == "states") {
    india_ea <- dplyr::arrange(india_ea, .data$abbr)
  } else if (type == "districts") {
    india_ea <- dplyr::arrange(india_ea, .data$abbr, .data$dtname)
  }

  # export modified shape file
  sf::st_write(india_ea, output_file, quiet = TRUE, append = FALSE)

  # compute centroids
  centroids <- compute_centroids(india_ea)

  # determine centroids file path
  centroids_output_file <- file.path(
    dirname(output_file),
    paste0(
      tools::file_path_sans_ext(basename(output_file)),
      "_centroids.",
      tools::file_ext(output_file)
    )
  )

  # export centroids
  sf::st_write(centroids, centroids_output_file, quiet = TRUE, append = FALSE)
}

#' @rdname create_india_map
#' @keywords internal
ea_crs <- function() {
  sf::st_crs(4326)  # World Geodetic System 1984
}

#' @rdname create_india_map
#' @keywords internal
compute_centroids <- function(polygons, iterations = 3, initial_width_step = 10) {
  if (iterations < 1) {
    stop("`iterations` must be greater than or equal to 1.")
  }

  if (initial_width_step < 1) {
    stop("`initial_width_step` must be greater than or equal to 1.")
  }

  new_polygons <- sf::st_as_sf(polygons)

  # Iterate through each provided polygon
  for (i in seq_len(nrow(polygons))) {
    width <- -initial_width_step
    area <- as.numeric(sf::st_area(polygons[i, ]))
    current_polygon <- polygons[i, ]

    isEmpty <- FALSE
    for (j in 1:iterations) {
      # Stop if buffer polygon becomes empty
      if (!isEmpty) {
        buffer <- sf::st_buffer(current_polygon, dist = width)

        # Repeatedly increase buffer size until non-empty if needed
        subtract_width <- width / 20
        while (sf::st_is_empty(buffer)) {
          width <- width - subtract_width
          buffer <- sf::st_buffer(current_polygon, dist = width)
          isEmpty <- TRUE
        }

        new_area <- as.numeric(sf::st_area(buffer))

        # Determine width needed to reduce area to 1/4 of current
        # for next iteration
        slope <- (new_area - area) / width
        width <- (area / 4 - area) / slope

        # Set values for next iteration
        area <- new_area
        current_polygon <- buffer
      }
    }

    # Determine biggest polygon in case of multiple polygons
    d <- sf::st_geometry(current_polygon)

    if (length(d) > 1) {
      biggest_area <- sf::st_area(d[1, ])

      which_polygon <- 1
      for (k in 2:length(d)) {
        if (sf::st_area(d[k, ]) > biggest_area) {
          biggest_area <- sf::st_area(d[k, ])
          which_polygon <- k
        }
      }

      current_polygon <- d[which_polygon, ]
    }

    # Replace existing polygon with new polygon
    new_polygons[i, ] <- current_polygon
  }

  # Return centroids of newly computed polygons
  sf::st_agr(new_polygons) <- "constant"
  sf::st_centroid(new_polygons)
}
