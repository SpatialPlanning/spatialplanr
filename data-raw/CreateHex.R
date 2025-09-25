# Some code to create a hex sticker for the spatialplanr package
#
# Last updated: Friday 13th June 2025
#
# Jason D. Everett (UQ/CSIRO/UNSW)
#
# For installation instructions see here:
# https://github.com/GuangchuangYu/hexSticker

# devtools::install_github("GuangchuangYu/hexSticker")

library(spatialplanr)

cCRS <- "EPSG:4326"

create_hexagon <- function(center_x, center_y, size, top_type) {
  if (top_type == "flat" || top_type == "flat_topped") {
    angles <- seq(0, 300, by = 60)
  } else if (top_type == "pointy" || top_type == "pointed" || top_type == "pointed_top") {
    angles <- seq(30, 360, by = 60)
  } else {
    stop("Invalid top_type. Must be 'flat' or 'pointy'.")
  }

  vertices <- matrix(0, nrow = length(angles), ncol = 2)

  for (i in 1:length(angles)) {
    angle <- angles[i]
    vertices[i, 1] <- center_x + size * cos(angle * pi / 180)
    vertices[i, 2] <- center_y + size * sin(angle * pi / 180)
  }

  # Close the polygon by duplicating the first vertex
  vertices <- rbind(vertices, vertices[1, ])

  out <- list()
  for (i in 1:length(angles)) {
    int <- sf::st_linestring(vertices[i:(i+1),]) %>%
      sf::st_segmentize(units::set_units(0.1, km)) %>%
      sf::st_coordinates()

    out[[i]] <- int[,1:2]

  }

  out2 <- list(do.call(rbind, out))

  # Create an sf polygon
  polygon <- sf::st_polygon(out2)

  return(polygon)
}

Bndry <- create_hexagon(70, -5, 70*1.1547, "pointed") %>%
  sf::st_polygon() %>%
  sf::st_sfc(crs = "EPSG:4326") %>%
  sf::st_sf() %>%
  sf::st_make_valid()

landmass <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  sf::st_make_valid() %>%
  sf::st_intersection(Bndry) %>%
  sf::st_make_valid()

PUs <- sf::st_make_grid(Bndry,
  square = FALSE,
  cellsize = c(4, 4),
  what = "polygons"
) %>%
  sf::st_sf() %>%
  sf::st_intersection(Bndry) %>%
  sf::st_make_valid()

PUs <- PUs %>%
  dplyr::mutate(Prob = runif(dim(PUs)[1]))

(gg <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = PUs, colour = "#4F4F51", fill = "#46718C", linewidth = 0.1, show.legend = FALSE) +
    ggplot2::geom_sf(data = landmass, colour = "grey70", fill = "#4F4F51", alpha = 1, linewidth = 0.05, show.legend = FALSE) +
    ggplot2::coord_sf(xlim = sf::st_bbox(PUs)$xlim, ylim = sf::st_bbox(PUs)$ylim) +
    ggplot2::theme_void())


hexSticker::sticker(gg,
                    package = "spatialplanr",
                    p_x = 1,
                    p_y = 0.98,
                    p_color = "white",
                    p_family = "Aller_Rg",
                    p_fontface = "bold",
                    p_size = 80,
                    s_x = 1,
                    s_y = 1,
                    s_width = 2.2,
                    s_height = 2.2,
                    # h_fill = "#9FE2BF",
                    h_color = "black", # "grey40",
                    url = "spatialplanning.github.io/spatialplanr",
                    u_color = "grey90",
                    # u_family = "sans",
                    u_size = 15,
                    u_x = 0.98,
                    u_y = 0.055,
                    dpi = 1000,
                    asp = 1,
                    filename = file.path("data-raw", "spatialplanr.png"))

usethis::use_logo(img = file.path("data-raw", "spatialplanr.png"))


# Create favicons for the site
pkgdown::build_favicons(pkg = ".", overwrite = TRUE)
