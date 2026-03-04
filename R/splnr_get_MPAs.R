#' @title Get Marine Protected Areas (MPAs) from WDPA
#'
#' @description
#' This function serves as a wrapper for the `wdpar` package, facilitating the
#' retrieval of Marine Protected Areas (MPAs) from the World Database on Protected
#' Areas (WDPA) and intersecting them with provided planning units.
#' The result is an `sf` object indicating the area of planning units covered by
#' the selected marine protected areas.
#'
#' @details
#' This function leverages the robust capabilities of the `wdpar` package by
#' Jeffrey O. Hanson to access and process WDPA data. It allows filtering of MPAs
#' based on country, status, designation type, and IUCN category, and then
#' spatially intersects these MPAs with your defined planning units.
#'
#' For a comprehensive understanding of the WDPA data fields:
#' \itemize{
#'   \item \strong{Status}: Refers to the establishment, designation, or proposal
#'         status of a protected area at the time of data submission. Valid options
#'         include "Designated", "Established", "Inscribed", "Proposed", and "Adopted".
#'   \item \strong{Desig} (Designation Type): Categorizes the legal or official
#'         designation of the protected area. Valid options include "National",
#'         "Regional", "International", and "Not Applicable".
#'   \item \strong{Category} (IUCN Protected Area Management Categories): Represents
#'         the IUCN management categories for protected areas. Valid options include
#'         "Ia", "Ib", "II", "III", "IV", "V", "VI", "Not Reported", "Not Applicable",
#'         and "Not Assigned".
#' }
#'
#' @param PlanUnits An `sf` object representing the planning units to be used for intersection.
#'   This object should have a valid CRS defined.
#' @param Countries A character vector specifying the countries for which to extract MPAs.
#'   To retrieve all global MPAs, use the value `"global"`. Country names should match
#'   those recognized by the WDPA database.
#' @param Status A character vector specifying the desired status of protected areas
#'   to include. Defaults to `c("Designated", "Established", "Inscribed")`.
#' @param Desig A character vector specifying the desired designation types of
#'   protected areas. Defaults to `c("National", "Regional", "International", "Not Applicable")`.
#' @param Category A character vector specifying the desired IUCN Protected Area
#'   Management Categories. Defaults to `c("Ia", "Ib", "II", "III", "IV")`.
#' @param ... Other arguments that are passed directly to the `wdpa_fetch()` function
#'   from the `wdpar` package (e.g., `verbose = TRUE`).
#'
#' @return An `sf` object. This object contains the planning units, with an
#'   additional `wdpa` column (set to 1) for areas that intersect with the
#'   selected MPAs.
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter select mutate
#' @importFrom purrr map
#' @importFrom rlang .data
#' @importFrom rappdirs user_data_dir
#' @importFrom sf st_as_sf
#' @importFrom spatialgridr get_data_in_grid
#'
#' @examples
#' \dontrun{
#' # Assuming 'dat_PUs' is an existing sf object of planning units in your package.
#'
#' # Example: Get MPAs for Australia and intersect with planning units.
#' dat_mpas <- splnr_get_MPAs(PlanUnits = dat_PUs, Countries = "Australia", force = TRUE)
#'
#' # Example: Get MPAs for multiple countries with specific status and categories.
#' dat_mpas_specific <- splnr_get_MPAs(
#'   PlanUnits = dat_PUs,
#'   Countries = c("Australia", "New Zealand"),
#'   Status = c("Designated", "Proposed"),
#'   Category = c("II", "IV")
#' )
#'
#' # Example: Visualize the result using ggplot2.
#' # Assuming 'aust' is an sf object representing Australia's coastline,
#' # perhaps loaded from rnaturalearth::ne_countries.
#' aust <- rnaturalearth::ne_countries(country = "Australia", returnclass = "sf")
#'
#' gg <- ggplot2::ggplot() +
#'   ggplot2::geom_sf(data = dat_mpas, ggplot2::aes(fill = wdpa)) +
#'   ggplot2::geom_sf(data = aust, fill = "grey50") +
#'   ggplot2::labs(title = "Marine Protected Areas in Australia") +
#'   ggplot2::theme_minimal()
#' print(gg)
#' }
splnr_get_MPAs <- function(PlanUnits,
                           Countries,
                           Status = c("Designated", "Established", "Inscribed"),
                           Desig = c("National", "Regional", "International", "Not Applicable"),
                           Category = c("Ia", "Ib", "II", "III", "IV"),
                           ...) {

  # Assertions to validate input parameters.
  assertthat::assert_that(
    inherits(PlanUnits, "sf"),
    msg = "The 'PlanUnits' parameter must be an 'sf' object."
  )
  assertthat::assert_that(
    is.character(Countries),
    msg = "The 'Countries' parameter must be a character vector."
  )
  assertthat::assert_that(
    all(Status %in% c("Designated", "Established", "Inscribed", "Proposed", "Adopted")),
    msg = "Invalid 'Status' provided. Must be one or more of: 'Designated', 'Established', 'Inscribed', 'Proposed', 'Adopted'."
  )
  assertthat::assert_that(
    all(Desig %in% c("National", "Regional", "International", "Not Applicable")),
    msg = "Invalid 'Desig' provided. Must be one or more of: 'National', 'Regional', 'International', 'Not Applicable'."
  )
  assertthat::assert_that(
    all(Category %in% c("Ia", "Ib", "II", "III", "IV", "V", "VI", "Not Reported", "Not Applicable", "Not Assigned")),
    msg = "Invalid 'Category' provided. Must be one or more of valid IUCN categories (e.g., 'Ia', 'Ib', 'II', 'III', 'IV', 'V', 'VI', 'Not Reported', 'Not Applicable', 'Not Assigned')."
  )


  # TODO Add a check for wdpar package


  # Set chromote timeout option to prevent issues with web scraping for WDPA data.
  options(chromote.timeout = 120)

  # Fetch WDPA data for the specified countries and then process it.
  # Note: Chromote may produce benign "Unhandled promise error: Browser.close" messages
  # that can be safely ignored (see wdpar documentation and
  # https://github.com/rstudio/chromote/pull/111)
  wdpa_data <- suppressWarnings(Countries %>%
    # Use purrr::map to fetch WDPA data for each country in the 'Countries' vector.
    # 'wait = TRUE' ensures sequential downloads, and 'download_dir' specifies where to cache the data.
    purrr::map(wdpar::wdpa_fetch,
               wait = TRUE,
               download_dir = rappdirs::user_data_dir("wdpar"),
               ...) %>%
    # Bind all fetched data frames into a single data frame.
    dplyr::bind_rows() %>%
    # Filter for marine and coastal protected areas only
    dplyr::filter(.data$REALM %in% c("Coastal", "Marine")) %>%
    # Filter by the specified IUCN Protected Area Management Categories.
    dplyr::filter(.data$IUCN_CAT %in% Category) %>%
    # Filter by the specified Designation Types.
    dplyr::filter(.data$DESIG_TYPE %in% Desig) %>%
    # Filter by the specified Status of the protected area.
    dplyr::filter(.data$STATUS %in% Status) %>%
    # Clean the protected area data using wdpar::wdpa_clean, removing any invalid geometries
    # or other issues. 'retain_status = NULL' means all statuses are considered for cleaning.
    # 'erase_overlaps = FALSE' means overlapping polygons are not removed at this stage.
    wdpar::wdpa_clean(retain_status = NULL, erase_overlaps = FALSE) %>%
    # Dissolve the protected area polygons to merge adjacent or overlapping areas into single geometries.
    wdpar::wdpa_dissolve() %>%
    # Select only the 'geometry' column, discarding other attributes after dissolving.
    dplyr::select("geometry") %>%
    # Add a new column 'wdpa' and set its value to 1, indicating it's a WDPA area.
    dplyr::mutate(wdpa = 1))

  # Intersect the cleaned WDPA data with the provided planning units using spatialgridr::get_data_in_grid.
  # This function identifies which planning units overlap with the WDPA areas.
  wdpa_data <- spatialgridr::get_data_in_grid(spatial_grid = PlanUnits,
                                              dat = wdpa_data,
                                              cutoff = NULL)

  return(wdpa_data)
}
