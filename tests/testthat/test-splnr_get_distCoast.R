testthat::test_that("Correct function output", {
  crs <- sf::st_crs("+proj=longlat +datum=WGS84 +no_defs")
  bbox <- sf::st_bbox(c(xmin = 0, ymin = 0, xmax = 3, ymax = 3))
  grid <- sf::st_make_grid(bbox, n = c(3, 3), what = "polygons")
  grid <- sf::st_sf(geometry = grid) %>%
    sf::st_set_crs(crs)
  result <- splnr_get_distCoast(grid)

  expect_s3_class(result, "sf")
})
