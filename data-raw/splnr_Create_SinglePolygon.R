# Create one polygon that we can use to populate with PUs
#
# splnr_Create_SinglePolygon <- function (df, res){
#
#   # Creating a empty raster
#   rs <- raster::raster(ncol = 360*(1/res), nrow = 180*(1/res))
#   rs[] <- 1:raster::ncell(rs)
#   raster::crs(rs) <- sf::st_crs(df) # Make raster crs the same as the sf object.
#
#   # Fasterize the land object
#   df_rs <- fasterize::fasterize(df, rs)
#
#   pol <- stars::as(df_rs,  "SpatialPolygonsDataFrame")
#   pol$layer <- seq(1, length(pol))
#
#   # Now to a sf object and create ONE BIG polygon that we can use to populate with PUs
#   pol_sf <- sf::st_as_sf(pol) %>%
#     dplyr::select(.data$layer) %>%
#     dplyr::summarise(total_layer = sum(.data$layer, do_union = TRUE))
# }
