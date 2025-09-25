#' @title Calculate Distance to Coastline
#'
#' @description
#' This function calculates the shortest distance from the centroid of each
#' planning unit in an `sf` object to the nearest coastline. It can use either
#' a default coastline from the `rnaturalearth` package or a custom-provided
#' coastline `sf` object.
#'
#' @details
#' The function adds a new column named `coastDistance_km` to the input `sf`
#' object, containing the calculated distances in kilometers. The CRS of the
#' input data is preserved. It is crucial to ensure the input `sf` object has
#' a suitable projected CRS for accurate distance calculations.
#'
#' @param dat_sf `[sf]` \cr An `sf` object containing polygon or point features
#'   representing the planning units. Must have a valid CRS.
#' @param custom_coast `[sf]` \cr An optional `sf` object representing a
#'   custom coastline. If `NULL` (the default), the coastline is downloaded
#'   from `rnaturalearth`.
#' @param res `[character(1)]` \cr The resolution of the `rnaturalearth`
#'   coastline to use. Options are `"small"`, `"medium"` (default), or
#'   `"large"`. This parameter is ignored if `custom_coast` is provided.
#'
#' @family cost_features
#'
#' @return An `sf` object identical to `dat_sf` but with an added column
#'   `coastDistance_km` representing the distance to the nearest coastline in
#'   kilometers.
#'
#' @importFrom assertthat assert_that is.flag
#' @importFrom units set_units drop_units
#' @importFrom rnaturalearth ne_coastline
#' @importFrom sf st_crs st_centroid st_geometry st_distance st_transform
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: Calculate distance to coast for a simple grid
#' bbox <- sf::st_bbox(c(xmin = 0, ymin = 0, xmax = 3, ymax = 3))
#' grid <- sf::st_as_sf(sf::st_make_grid(bbox, n = c(3, 3)))
#' grid_with_dist <- splnr_get_distCoast(grid)
#' plot(grid_with_dist["coastDistance_km"])
#'
#' # Example 2: Using a specific resolution for the coastline
#' # Note: Requires the 'dat_sf' object to be created first, e.g., using
#' # splnr_get_planning_units()
#' if (exists("dat_sf")) {
#'   dat_sf_dist <- splnr_get_distCoast(dat_sf, res = "large")
#'   summary(dat_sf_dist$coastDistance_km)
#' }
#'
#' # Example 3: Using a custom coastline
#' # First, create a custom coastline (e.g., from a country polygon)
#' landmass <- rnaturalearth::ne_countries(
#'   scale = "medium",
#'   returnclass = "sf"
#' )
#'
#' if (exists("dat_sf") && exists("landmass")) {
#'    # Transform landmass to the same CRS as the planning units
#'   landmass_proj <- sf::st_transform(landmass, sf::st_crs(dat_sf))
#'   dat_sf_custom_coast <- splnr_get_distCoast(dat_sf, custom_coast = landmass_proj)
#'   summary(dat_sf_custom_coast$coastDistance_km)
#' }
#' }
splnr_get_distCoast <- function(dat_sf, custom_coast = NULL, res = "medium") {
  # Input validation using assertthat
  assertthat::assert_that(
    inherits(dat_sf, "sf"),
    msg = "'dat_sf' must be an 'sf' object."
  )
  assertthat::assert_that(
    !is.na(sf::st_crs(dat_sf)),
    msg = "'dat_sf' must have a valid Coordinate Reference System (CRS)."
  )
  assertthat::assert_that(
    is.null(custom_coast) || inherits(custom_coast, "sf"),
    msg = "'custom_coast' must be either NULL or an 'sf' object."
  )
  assertthat::assert_that(
    is.character(res) && length(res) == 1 && res %in% c("small", "medium", "large"),
    msg = "'res' must be a single character string: 'small', 'medium', or 'large'."
  )

  # Load or prepare the coastline data
  if (is.null(custom_coast)) {
    message(paste0("Downloading coastline data from rnaturalearth at '", res, "' resolution."))
    # If no custom coast is provided, download from rnaturalearth
    coast <- rnaturalearth::ne_coastline(scale = res, returnclass = "sf") %>%
      # Transform the coastline to match the CRS of the input data
      sf::st_transform(crs = sf::st_crs(dat_sf))
  } else {
    message("Using custom coastline data.")
    # If a custom coast is provided, use it
    coast <- custom_coast %>%
      # Ensure the custom coastline has the same CRS as the input data
      sf::st_transform(crs = sf::st_crs(dat_sf))
  }

  # Calculate centroids of the planning units
  # Using centroids is a standard approach to represent the location of each planning unit
  message("Calculating centroids for planning units.")
  grid_centroid <- sf::st_centroid(sf::st_geometry(dat_sf))

  # Calculate the distance matrix between each planning unit centroid and the coastline
  message("Calculating distances to coastline.")
  dist_mat <- sf::st_distance(grid_centroid, coast) %>%
    # Explicitly set the distance units to kilometers
    units::set_units("km") %>%
    # Drop the units class to get a numeric matrix for easier computation
    units::drop_units()

  # Find the minimum distance for each planning unit (each row in the matrix)
  # This identifies the shortest distance from each centroid to any part of the coastline
  message("Finding minimum distances and adding to dataframe.")
  dat_sf$coastDistance_km <- do.call(pmin, as.data.frame(dist_mat))

  # Return the original sf object with the new distance column
  message("Distance calculation complete.")
  return(dat_sf)
}
