#' @title Create a Planning Region Boundary
#'
#' @description
#' This function generates a spatial boundary for the planning region as an `sf`
#' polygon object. The boundary can be defined in several ways:
#' 1. A simple rectangular bounding box using numeric coordinates.
#' 2. A global boundary spanning the entire world.
#' 3. A complex shape based on marine ecoregions from `rnaturalearth`.
#'
#' @details
#' A planning region boundary is the foundational first step for most spatial
#' conservation planning exercises. All subsequent analyses and data preparation
#' steps within the `spatialplanr` package rely on a defined boundary. The
#' coordinate reference system (CRS) of the returned object is projected by
#' default (Mollweide), which is suitable for equal-area calculations.
#'
#' @param Limits A required input that defines the spatial extent. This can be:
#'   \itemize{
#'     \item A named numeric vector of four elements: `c("xmin" = ..., "xmax" = ..., "ymin" = ..., "ymax" = ...)`.
#'     \item The string `"Global"` to create a worldwide boundary.
#'     \item A character vector of ocean/sea names (e.g., `"North Atlantic Ocean"`) to be used with `Type = "Ocean"`.
#'   }
#' @param Type `r lifecycle::badge("deprecated")` The type of Limits being provided. This is only required if `Limits` is a character vector of ocean names, in which case it should be `"Ocean"`. It is no longer required and will be removed in a future version.
#' @param res `[numeric(1)]`\cr The resolution (in decimal degrees) used to
#'   construct the polygon vertices when `Limits` is numeric or `"Global"`.
#'   Defaults to `1`. Must be a positive number.
#' @param cCRS `[character(1)]`\cr The coordinate reference system (CRS) for the
#'   output `sf` object. Can be a PROJ4 string or an EPSG code. Defaults to
#'   `"ESRI:54009"` (Mollweide).
#'
#' @family planning_region
#'
#' @return An `sf` object containing a single polygon feature representing the
#'   planning boundary.
#'
#' @export
#'
#' @importFrom assertthat assert_that is.string is.flag
#' @importFrom dplyr tibble bind_rows filter
#' @importFrom rnaturalearth ne_download
#' @importFrom sf st_crs st_join st_as_sf st_union st_transform st_set_crs st_polygon st_sfc
#' @importFrom lifecycle deprecate_warn
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' # Example 1: Create a boundary from an ocean name.
#' # This fetches polygon data for the specified ocean.
#' bndry_ocean <- splnr_get_boundary(Limits = "North Atlantic Ocean", Type = "Ocean")
#' plot(bndry_ocean)
#'
#' # Example 2: Create a global boundary.
#' bndry_global <- splnr_get_boundary(Limits = "Global")
#' plot(bndry_global)
#'
#' # Example 3: Create a boundary from a numeric bounding box.
#' bndry_coords <- splnr_get_boundary(
#'   Limits = c("xmin" = 150, "xmax" = 170, "ymin" = -40, "ymax" = -20)
#' )
#' plot(bndry_coords)
#' }
splnr_get_boundary <- function(Limits,
                               Type = NULL,
                               res = 1,
                               cCRS = "ESRI:54009" # Mollweide
) {
  # Input validation using assertthat
  assertthat::assert_that(
    !missing(Limits),
    msg = "'Limits' is a required argument."
  )
  assertthat::assert_that(
    is.numeric(res) && length(res) == 1 && res > 1e-6,
    msg = "'res' must be a single positive numeric value."
  )
  assertthat::assert_that(
    is.character(cCRS) && length(cCRS) == 1,
    msg = "'cCRS' must be a single character string."
  )

  # Validate 'Limits' based on its type
  is_numeric_limits <- is.numeric(Limits) && length(Limits) == 4 && !is.null(names(Limits)) && all(sort(names(Limits)) == sort(c("xmin", "xmax", "ymin", "ymax")))
  is_global_limit <- is.character(Limits) && length(Limits) == 1 && identical(Limits, "Global")
  is_ocean_limits <- is.character(Limits) && !identical(Limits, "Global")
  is_character_limits <- is.character(Limits)

  assertthat::assert_that(
    is_numeric_limits || is_global_limit || is_ocean_limits,
    msg = paste0(
      "'Limits' must be either:\n",
      "  - A named numeric vector of four: c(xmin=..., xmax=..., ymin=..., ymax=...)\n",
      "  - The string 'Global'\n",
      "  - A character vector of ocean/sea names (e.g., 'North Atlantic Ocean')"
    )
  )

  # Validate 'Type' in relation to 'Limits'
  if (is_character_limits && !is_global_limit) { # Only validate Type if Limits is a character vector and not "Global"
    assertthat::assert_that(
      is.character(Type) && length(Type) == 1,
      msg = "`Type` must be a single character string when `Limits` is a character vector (other than 'Global')."
    )
    assertthat::assert_that(
      Type %in% c("Oceans", "Ocean", "EEZ"), # Include EEZ as per original function, even if commented out later.
      msg = paste0("When `Limits` is an ocean name, `Type` must be 'Ocean' or 'Oceans' (or 'EEZ').")
    )
  }

  # Check that Type is not provided for numeric or "Global" limits and warn
  if ((is_numeric_limits || is_global_limit) && !is.null(Type)) {
    warning("`Type` is ignored when `Limits` is a numeric bounding box or 'Global'.")
  }

  # # Deprecation warning for Type argument
  # if (!is.null(Type)){
  #   lifecycle::deprecate_warn(
  #     when = "0.2.0",
  #     what = "splnr_get_boundary(Type)",
  #     details = "The `Type` argument is no longer necessary and will be removed in a future release."
  #   )
  # }

  # Handle numeric limits to create a rectangular boundary
  if (is_numeric_limits) {
    # Check that min is less than max
    assertthat::assert_that(Limits["xmin"] < Limits["xmax"], msg = "'xmin' must be less than 'xmax'.")
    assertthat::assert_that(Limits["ymin"] < Limits["ymax"], msg = "'ymin' must be less than 'ymax'.")

    # Construct the boundary polygon from coordinates
    Bndry <- dplyr::tibble(x = seq(Limits["xmin"], Limits["xmax"], by = res), y = Limits["ymin"]) %>%
      dplyr::bind_rows(dplyr::tibble(x = Limits["xmax"], y = seq(Limits["ymin"], Limits["ymax"], by = res))) %>%
      dplyr::bind_rows(dplyr::tibble(x = seq(Limits["xmax"], Limits["xmin"], by = -res), y = Limits["ymax"])) %>%
      dplyr::bind_rows(dplyr::tibble(x = Limits["xmin"], y = seq(Limits["ymax"], Limits["ymin"], by = -res))) %>%
      # Convert the points to an sf polygon
      splnr_create_polygon(cCRS) %>%
      # Convert to an sf object
      sf::st_sf()

    return(Bndry)
  }

  # Handle the "Global" limit case
  if (is_global_limit) {
    # Construct a global boundary polygon
    Bndry <- dplyr::tibble(x = seq(-180, 180, by = res), y = -90) %>%
      dplyr::bind_rows(dplyr::tibble(x = 180, y = seq(-90, 90, by = res))) %>%
      dplyr::bind_rows(dplyr::tibble(x = seq(180, -180, by = -res), y = 90)) %>%
      dplyr::bind_rows(dplyr::tibble(x = -180, y = seq(90, -90, by = -res))) %>%
      # Convert the points to an sf polygon
      splnr_create_polygon(cCRS) %>%
      # Convert to an sf object
      sf::st_sf()

    return(Bndry)
  }

  ## TODO Disable EEZ until offshoredatr publicly online.
  # if (Type == "EEZ"){
  #   # This part relies on an external package not universally available or stable.
  #   # It's commented out but kept for structural completeness if offshoredatr becomes stable.
  #   Bndry <- offshoredatr::get_area(area_name = Limits) %>%
  #     dplyr::filter(.data$territory1 %in% Limits) %>%
  #     sf::st_union() %>%
  #     sf::st_transform(cCRS)
  #   return(Bndry)
  # }

  # Handle Ocean limits using rnaturalearth
  if (is_ocean_limits) {
    # Download marine polygons from rnaturalearth
    Bndry <- rnaturalearth::ne_download(
      scale = "large",
      category = "physical",
      type = "geography_marine_polys",
      returnclass = "sf"
    ) %>%
      # Filter for the specified ocean(s)
      dplyr::filter(.data$name %in% Limits) %>%
      # Unify the polygons into a single feature
      sf::st_union() %>%
      # Transform to the specified CRS
      sf::st_transform(cCRS) %>%
      # Convert to an sf object
      sf::st_sf()

    return(Bndry)
  }
  # Fallback for unexpected Limits/Type combinations
  stop("Invalid 'Limits' or 'Type' combination provided. Please check the function documentation.")
}
