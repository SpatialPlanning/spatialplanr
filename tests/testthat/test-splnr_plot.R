# Continuous plot of bathymetry


Region <- "Coral Sea" # "Australia"
Type <- "Oceans" # "EEZ"
PU_size <- 107460 # m2 (10,000 km2)
cCRS <- "ESRI:54009"

Bndry <- splnr_get_boundary(Limits = Region, Type = Type, cCRS = cCRS)

landmass <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
) %>%
  sf::st_transform(cCRS)

PUs <- spatialgridr::get_grid(boundary = Bndry,
                              crs = cCRS,
                              output = "sf_hex",
                              resolution = PU_size)

splnr_theme <- list(
  ggplot2::theme_bw(),
  ggplot2::theme(
    legend.position = "right",
    legend.direction = "vertical",
    text = ggplot2::element_text(size = 9, colour = "black"),
    axis.text = ggplot2::element_text(size = 9, colour = "black"),
    plot.title = ggplot2::element_text(size = 9),
    axis.title = ggplot2::element_blank()
  )
)


distance <- splnr_get_distCoast(dat_PUs)

# Binary plot of species distribution
testthat::test_that("Correct function output", {
  expect_s3_class(
    splnr_plot(df = dat_species_bin,
               colNames = "Spp1",
               legendTitle = "Legend",
               legendLabels = c("Absent", "Present"))
    , "gg"
  )
})

# Logical plot of species distribution
testthat::test_that("Correct function output", {
  expect_s3_class(
    splnr_plot(df = dat_species_bin %>% dplyr::mutate(dplyr::across(tidyselect::starts_with("Spp"), as.logical)),
               colNames = "Spp1",
               legendTitle = "Legend",
               legendLabels = c("Absent", "Present"))
    , "gg"
  )
})


testthat::test_that("Correct function output", {
  expect_s3_class(
    splnr_plot(df = distance,
               colNames = "coastDistance_km",
               plotTitle = "Distance to Coast",
               legendTitle = "Distance (km)")
    , "gg"
  )
})

# Plot Planning Units
testthat::test_that("Correct function output", {
  expect_s3_class(
    splnr_plot(df = dat_PUs)
    , "gg"
  )
})

# Multi binary features
testthat::test_that("Correct function output", {
  expect_s3_class(
    splnr_plot(df = dat_species_bin,
               colNames = colnames(dat_species_bin %>%
                                      sf::st_drop_geometry() %>%
                                      dplyr::select(
                                        tidyselect::starts_with("Spp"))),
               legendTitle = "Number of features")

    , "gg"
  )
})


testthat::test_that("Correct function output", {
  expect_s3_class(
    ggPU <- splnr_plot(PUs) +
      splnr_gg_add(
        Bndry = Bndry, overlay = landmass,
        cropOverlay = PUs, ggtheme = splnr_theme
      )
    , "gg"
  )
})



testthat::test_that("Correct function output", {
  expect_s3_class(
    ggPU <- splnr_plot(df = PUs) +
      splnr_gg_add(
        Bndry = Bndry, overlay = landmass,
        cropOverlay = PUs, ggtheme = "Default"
      )
    , "gg"
  )
})

testthat::test_that("Correct function output", {
  expect_s3_class(
    ggPU <- splnr_plot(df = PUs) +
      splnr_gg_add(
        Bndry = Bndry,
        overlay = landmass,
        overlay2 = landmass,
        overlay3 = landmass,
        cropOverlay = PUs, ggtheme = "Default",
        lockIn  = dat_mpas, nameLockIn = "wdpa",
        typeLockIn = "Contours",
        alphaLockIn = 0.5, colorLockIn = "red",
        legendLockIn = "", labelLockIn = "MPAs"
      )
    , "gg"
  )
})


# overlay2 = NULL, colorOverlay2 = "grey30",
# overlay3 = NULL, colorOverlay3 = "grey40",
# contours = NULL, colorConts = "black",

# testthat::test_that("Correct function output", {
#   expect_s3_class(
#
#     , "gg"
#   )
# )
# })


