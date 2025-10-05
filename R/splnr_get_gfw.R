#' @title Retrieve Global Fishing Watch Data
#'
#' @description
#' The `splnr_get_gfw` function retrieves Global Fishing Watch (GFW) data and
#' returns it as an `sf` (simple features) object. This function allows for
#' flexible data queries based on geographical region, time range, and desired
#' spatial and temporal resolutions.
#'
#' @details
#' The possibilities offered by this function are extensively explained in
#' `vignette("GlobalFishingWatch")`.
#'
#' This function shares many parameters with the `get_raster` function from the
#' `gfwr` package, with the addition of `cCRS` for specifying the Coordinate
#' Reference System of the output `sf` object.
#'
#' Fishing activity data can be aggregated (`group_by`) by "FLAGANDGEARTYPE"
#' by default, combining flags and gear types.
#'
#' \strong{Notes:}
#' \itemize{
#'   \item Currently, the function is primarily designed for data within
#'         Exclusive Economic Zones (EEZs), but it can potentially be
#'         extended to specific Marine Protected Areas (MPAs) or RFMOs.
#'   \item Days specified in the `start_date` and `end_date` variables are
#'         inclusive in the data recovery.
#' }
#'
#' @param region A character string specifying the name of the region (e.g., an EEZ name)
#'   or a numeric ID for the region, or an `sf` object if `region_source` is set
#'   to "USER_SHAPEFILE".
#' @param start_date The start date for data retrieval, expected in "%Y-%m-%d" format (e.g., "2021-01-01").
#' @param end_date The end date for data retrieval, expected in "%Y-%m-%d" format (e.g., "2022-12-31").
#' @param temp_res The desired temporal resolution for the data. Must be one of:
#'   "DAILY", "MONTHLY", or "YEARLY".
#' @param spat_res The desired spatial resolution for the data. Must be one of:
#'   "LOW" (0.1 degree) or "HIGH" (0.01 degree). Defaults to "LOW".
#' @param region_source The source of the region definition. Must be one of:
#'   'EEZ', 'MPA', 'RFMO', or 'USER_SHAPEFILE'. Defaults to "EEZ".
#' @param key Your API token for the GFW API. If not provided, it attempts to
#'   authenticate using `gfwr::gfw_auth()`. See the GlobalFishingWatch vignette
#'   for details on obtaining a key.
#' @param cCRS The Coordinate Reference System (CRS) to which the output `sf` object
#'   will be transformed. Defaults to "EPSG:4326".
#' @param compress A logical value. If `TRUE`, the data will be compressed (aggregated)
#'   by coordinates, summing fishing hours for each unique location. If `FALSE`,
#'   the raw data points are returned. Defaults to `FALSE`.
#'
#' @return An `sf` object containing the requested GFW data. The structure of
#'   the `sf` object will vary depending on the `compress` and `temp_res`
#'   parameters.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example: Retrieve yearly GFW data for Australia, transformed to a
#' # Mollweide projection (ESRI:54009) and compressed (aggregated) by location.
#' gfw_data <- splnr_get_gfw(
#'   region = 'Australia',
#'   start_date = "2021-01-01",
#'   end_date = "2022-12-31",
#'   temp_res = "YEARLY",
#'   cCRS = "ESRI:54009",
#'   compress = TRUE
#' )
#'
#' # Example: Retrieve monthly GFW data for a specific EEZ ID,
#' # keeping individual time ranges and locations.
#' # Note: Replace 1000 with an actual EEZ ID if needed for testing.
#' gfw_data_monthly <- splnr_get_gfw(
#'   region = 1000, # Example numeric EEZ ID
#'   start_date = "2022-01-01",
#'   end_date = "2022-03-31",
#'   temp_res = "MONTHLY",
#'   region_source = "EEZ",
#'   compress = FALSE
#' )
#' }
splnr_get_gfw <- function(region,
                          start_date,
                          end_date,
                          temp_res,
                          spat_res = "LOW",
                          region_source = "EEZ",
                          key = gfwr::gfw_auth(),
                          cCRS = "EPSG:4326",
                          compress = FALSE) {

  # Assertions for input parameters to ensure correct types and values
  assertthat::assert_that(
    (is.character(region) || is.numeric(region) || (region_source == "USER_SHAPEFILE" && inherits(region, "sf"))),
    msg = "The 'region' parameter must be a character (name), numeric (ID), or an 'sf' object if 'region_source' is 'USER_SHAPEFILE'."
  )
  assertthat::assert_that(
    inherits(start_date, "character") && !is.na(as.Date(start_date, "%Y-%m-%d")),
    msg = "The 'start_date' parameter must be a character string in 'YYYY-MM-DD' format."
  )
  assertthat::assert_that(
    inherits(end_date, "character") && !is.na(as.Date(end_date, "%Y-%m-%d")),
    msg = "The 'end_date' parameter must be a character string in 'YYYY-MM-DD' format."
  )
  assertthat::assert_that(
    temp_res %in% c("DAILY", "MONTHLY", "YEARLY"),
    msg = "The 'temp_res' parameter must be one of 'DAILY', 'MONTHLY', or 'YEARLY'."
  )
  assertthat::assert_that(
    spat_res %in% c("LOW", "HIGH"),
    msg = "The 'spat_res' parameter must be one of 'LOW' or 'HIGH'."
  )
  assertthat::assert_that(
    region_source %in% c('EEZ', 'MPA', 'RFMO', 'USER_SHAPEFILE'),
    msg = "The 'region_source' parameter must be one of 'EEZ', 'MPA', 'RFMO', or 'USER_SHAPEFILE'."
  )
  assertthat::assert_that(
    is.character(key),
    msg = "The 'key' parameter must be a character string (your GFW API token)."
  )
  assertthat::assert_that(
    is.character(cCRS),
    msg = "The 'cCRS' parameter must be a character string representing a valid CRS (e.g., 'EPSG:4326')."
  )
  assertthat::assert_that(
    is.logical(compress),
    msg = "The 'compress' parameter must be a logical value (TRUE or FALSE)."
  )

  # Define an internal helper function to fetch GFW data for a single region.
  get_gfw_byRegion <- function(region){

    # Determine the region ID based on the region_source and region type.
    if (region_source == "EEZ" & is.character(region)){
      region_id <- gfwr::get_region_id(region_name = region, region_source = region_source, key = key)$id
    } else if (region_source == "EEZ" & is.numeric(region)){
      # If region is numeric for EEZ, assume it's already an ID.
      region_id <- region
    } else if (region_source == "RFMO"){
      # For RFMO, pass the region as is; handles potential gfwr package quirks.
      region_id <- region
    } else if (region_source == "USER_SHAPEFILE"){
      # If region_source is USER_SHAPEFILE, use the provided region (assumed to be an sf object).
      region_id <- region
    }

    # Convert start_date and end_date strings to Date objects.
    start_date <- as.Date(start_date, format = "%Y-%m-%d")
    end_date <- as.Date(end_date, format = "%Y-%m-%d")

    # Define a nested helper function to obtain data for a specific date range within the loop.
    get_data_for_range <- function(start_date, end_date, rid) {

      # Call the gfwr::get_raster function to retrieve GFW raster data.
      data <- gfwr::get_raster(
        spatial_resolution = spat_res,
        temporal_resolution = temp_res,
        group_by = 'FLAGANDGEARTYPE', # Group by flag and geartype.
        start_date = start_date,
        end_date = end_date,
        region = rid,
        region_source = region_source,
        key = key)

      # Mutate and rename columns for consistency and clarity.
      data <- data %>%
        dplyr::mutate(GFWregionID = rid) %>% # Add a column for the GFW region ID.
        dplyr::rename(TimeRange = .data$`Time Range`,
                      VesselID = .data$`Vessel IDs`,
                      ApparentFishingHrs = .data$`Apparent Fishing Hours`)

      return(data)
    }

    # Create an expanded grid of dates and regions to iterate over, splitting long date ranges.
    # This helps in fetching data in manageable chunks, as GFW API might have limitations on date ranges.
    eg <- tidyr::expand_grid(
      Date = seq(start_date, end_date, by = "366 days"), # Step by 366 days to ensure yearly chunks, adapting to leap years.
      Region = region_id
    )

    # Use purrr::map2 to apply the get_data_for_range function to each date chunk and region.
    # Then, drop empty list elements and bind all data frames into a single data frame.
    data_df <- purrr::map2(eg$Date, eg$Region, ~ get_data_for_range(.x, min(.x + 365, end_date), .y)) %>%
      vctrs::list_drop_empty() %>%
      dplyr::bind_rows()

    # Check if the resulting data frame is empty and stop with an informative message if no data is found.
    if(rlang::is_empty(data_df)){
      stop(paste0("No data found at all for the requested area of ", region, " between ", start_date, " and ", end_date))
    }

    # Process data based on the 'compress' parameter.
    if (isTRUE(compress)){

      # Group data by Lon and Lat and summarise (sum) Apparent Fishing Hours for compression.
      data_df <- data_df %>%
        dplyr::group_by(.data$Lon, .data$Lat) %>%
        dplyr::summarise("ApparentFishingHrs" = sum(.data$ApparentFishingHrs, na.rm = TRUE),
                         GFWregionID = dplyr::first(.data$GFWregionID)) %>%
        dplyr::ungroup()

      # Convert the aggregated data frame to a 'terra' raster, then to polygons, and finally to an 'sf' object.
      data_sf <- data_df %>%
        terra::rast(type = "xyz", crs = "EPSG:4326") %>% # Convert to a raster from XYZ data with WGS84 CRS.
        terra::as.polygons(trunc = FALSE, dissolve = FALSE, na.rm = TRUE, round = FALSE) %>% # Convert raster cells to polygons.
        sf::st_as_sf() %>% # Convert the 'terra' polygons to an 'sf' object.
        dplyr::mutate(GFWregionID = as.factor(.data$GFWregionID)) # Ensure GFWregionID is a factor.

      # Verify that the dimensions of the data frame and sf object match after conversion.
      if (dim(data_df)[1] != dim(data_sf)[1]){
        stop("Data dimensions of data_df and data_sf do not match after conversion to polygon")
      }

    } else if (isFALSE(compress)){

      # Process data without compression, separating 'TimeRange' based on temporal resolution.
      if (temp_res == "YEARLY") {
        # If temporal resolution is yearly, create a 'Year' column and convert to sf.
        data_sf <- data_df %>%
          dplyr::mutate(Year = .data$TimeRange) %>%
          sf::st_as_sf(coords = c("Lon", "Lat"), crs ="EPSG:4326")
      } else {
        # Otherwise, separate 'TimeRange' into 'Year', 'Month', and/or 'Day' columns.
        if (temp_res == "MONTHLY") {
          data_sf <- data_df %>%
            tidyr::separate("TimeRange", into = c("Year", "Month"), sep = "-", remove = FALSE) %>%
            sf::st_as_sf(coords = c("Lon", "Lat"), crs = "EPSG:4326")
        } else if (temp_res == "DAILY") {
          data_sf <- data_df %>%
            tidyr::separate("TimeRange", into = c("Year", "Month", "Day"), sep = "-", remove = FALSE) %>%
            sf::st_as_sf(coords = c("Lon", "Lat"), crs = "EPSG:4326")
        }
      }
    }

    # Transform the CRS of the sf object if the requested cCRS is different from the default "EPSG:4326".
    if (isFALSE(cCRS == "EPSG:4326")){
      data_sf <- data_sf %>%
        sf::st_transform(crs = cCRS)
    }
    return(data_sf)
  }

  # Apply the internal get_gfw_byRegion function to each specified region.
  out <- purrr::map(region, function(x) get_gfw_byRegion(x))

  # Combine the results from multiple regions based on the 'compress' setting.
  if (isFALSE(compress)){
    # If not compressed, simply bind rows of the sf objects.
    out <- out %>%
      dplyr::bind_rows()
  } else if (isTRUE(compress)){
    # If compressed, bind rows and then re-summarise to handle duplicate cells on boundaries,
    # summing fishing hours and combining GFWregionIDs.
    out <- out %>%
      dplyr::bind_rows() %>%
      dplyr::group_by(.data$geometry) %>%
      dplyr::summarise("ApparentFishingHrs" = sum(.data$ApparentFishingHrs, na.rm = TRUE),
                       GFWregionID = toString(.data$GFWregionID)) %>%
      dplyr::ungroup()
  }

  return(out)
}
