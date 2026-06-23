#' Convert a world sf object to a Pacific-centred one
#' Defaults to assuming Robinson projection
#'
#' Written by Jason D. Everett
#' UQ/CSIRO/UNSW
#' Last edited 8th Sept 2021
#'
#' @param df An sf dataframe
#' @param buff The buffer too apply to features that cross after merge
#' @param cCRS The crs to use for the output.
#'
#' @return An sf object in the Robinson projection
#' @export
#'
#' @examples
#' df_rob <- rnaturalearth::ne_coastline(returnclass = "sf") %>%
#'   splnr_convert_toPacific()
splnr_convert_toPacific <- function(df,
                                    buff = 0,
                                    cCRS) {
  assertthat::assert_that(
    inherits(df, "sf"),
    is.numeric(buff) && buff >= 0,
    is.character(cCRS)
  )

  # TODO add a warning if df doesn't cross the pacific dateline
  longlat <- "EPSG:4326"

  # Define a long & slim polygon that overlaps the meridian line & set its CRS to match
  # that of world Adapted from here:
  # https://stackoverflow.com/questions/56146735/visual-bug-when-changing-robinson-projections-central-meridian-with-ggplot2

  polygon <- sf::st_polygon(x = list(rbind(
    c(-0.0001, 90),
    c(0, 90),
    c(0, -90),
    c(-0.0001, -90),
    c(-0.0001, 90)
  ))) %>%
    sf::st_sfc() %>%
    sf::st_set_crs(longlat)

  # Modify world dataset to remove overlapping portions with world's polygons
  # TODO add a warning if the input df is not unprojected
  suppressWarnings({
    df_proj <- df %>%
      sf::st_transform(longlat) %>% # The input needs to be unprojected.
      sf::st_make_valid() %>% # Just in case....
      sf::st_difference(polygon) %>%
      sf::st_transform(crs = cCRS) # Perform transformation on modified version of polygons
    rm(polygon)
  })

  # # notice that there is a line in the middle of Antarctica. This is because we have
  # # split the map after reprojection. We need to fix this:
  bbox <- sf::st_bbox(df_proj)
  bbox[c(1, 3)] <- c(-1e-5, 1e-5)
  polygon_proj <- sf::st_as_sfc(bbox)

  crosses <- df_proj %>%
    sf::st_intersects(polygon_proj) %>%
    sapply(length) %>%
    as.logical() %>%
    which()

  # # Adding buffer (usually 0)
  df_proj <- df_proj[crosses, ] %>%
    sf::st_buffer(buff)

  return(df_proj)
}
