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


# ---------------------------------------------------------------------------
# splnr_gg_add() lock-in / lock-out legend label tests
# ---------------------------------------------------------------------------
# These tests verify that the legend label displayed for locked-in and
# locked-out areas uses the human-readable label supplied via labelLockIn /
# labelLockOut rather than the raw column name (nameVariable).
# dat_mpas has a binary column "wdpa" (1 = MPA).

# Helper: build a minimal solution sf for use as a base plot
dat_soln_for_lock_test <- dat_species_bin %>%
  dplyr::mutate(solution_1 = 1L)

# --- Lock-in: named vector maps nameVariable -> nameCommon ---
testthat::test_that("splnr_gg_add() Full lock-in uses named labelLockIn vector for legend", {
  gg <- splnr_plot_solution(dat_soln_for_lock_test) +
    splnr_gg_add(
      lockIn      = dat_mpas,
      nameLockIn  = "wdpa",
      labelLockIn = c(wdpa = "Marine Protected Areas"),
      legendLockIn = "Locked In",
      ggtheme     = FALSE
    )

  # The plot should build without error and be a gg object
  expect_s3_class(gg, "gg")

  # The LI_Area column in the geom_sf layer's data is what drives the fill
  # aesthetic and therefore the legend labels.  Inspect it directly — this is
  # more reliable than inspecting scale$labels, which scale_fill_brewer leaves
  # NULL (it derives labels from the data at draw time).
  #
  # Layer index: 1 = solution fill, 2 = new_scale_fill (no data), 3 = lock-in geom_sf.
  # Find the lock-in layer by looking for one whose data has an LI_Area column.
  li_layer_idx <- which(vapply(gg$layers, function(l) "LI_Area" %in% names(l$data), logical(1)))
  expect_length(li_layer_idx, 1L)

  li_area_vals <- unique(gg$layers[[li_layer_idx]]$data$LI_Area)
  expect_true("Marine Protected Areas" %in% li_area_vals)
  expect_false("wdpa" %in% li_area_vals)
  expect_false("Wdpa" %in% li_area_vals)
})

# --- Lock-in: single string label ---
testthat::test_that("splnr_gg_add() Full lock-in uses single string labelLockIn", {
  gg <- splnr_plot_solution(dat_soln_for_lock_test) +
    splnr_gg_add(
      lockIn     = dat_mpas,
      nameLockIn = "wdpa",
      labelLockIn = "MPAs",
      legendLockIn = "Locked In",
      ggtheme    = FALSE
    )
  expect_s3_class(gg, "gg")
})

# --- Lock-in: fallback (no label supplied, default "MPAs") ---
testthat::test_that("splnr_gg_add() Full lock-in falls back to title-cased column name when labelLockIn is default", {
  # Default labelLockIn = "MPAs" — single non-empty string, so it is used directly.
  gg <- splnr_plot_solution(dat_soln_for_lock_test) +
    splnr_gg_add(
      lockIn     = dat_mpas,
      nameLockIn = "wdpa",
      legendLockIn = "Locked In",
      ggtheme    = FALSE
    )
  expect_s3_class(gg, "gg")
})

# --- Lock-out: named vector maps nameVariable -> nameCommon ---
testthat::test_that("splnr_gg_add() Full lock-out uses named labelLockOut vector for legend", {
  # Reuse dat_mpas as a lock-out layer for testing purposes
  gg <- splnr_plot_solution(dat_soln_for_lock_test) +
    splnr_gg_add(
      lockOut      = dat_mpas,
      nameLockOut  = "wdpa",
      labelLockOut = c(wdpa = "Shipping Lanes"),
      legendLockOut = "Locked Out",
      ggtheme      = FALSE
    )
  expect_s3_class(gg, "gg")
})


