targets <- dat_species_bin %>%
  sf::st_drop_geometry() %>%
  colnames() %>%
  data.frame() %>%
  setNames(c("feature")) %>%
  dplyr::mutate(target = 0.3)

# Tibble version of targets — replicates what fget_targets_with_bioregions()
# returns via tibble::enframe(). The CPA assignTargets function previously
# extracted trgt with [, "target"] which returns a 1-row tibble (not a scalar)
# when the input is a tibble, causing NA targets after bind_rows().
targets_tbl <- tibble::tibble(
  feature = dat_species_bin %>% sf::st_drop_geometry() %>% colnames(),
  target  = 0.3
)

feat_names <- dat_species_bin %>%
  sf::st_drop_geometry() %>%
  colnames()


# --- Climate Priority Area (CPA) approach -----------------------------------

testthat::test_that("splnr_climate_priorityAreaApproach() returns correct structure", {
  result <- splnr_climate_priorityAreaApproach(
    features      = dat_species_bin,
    metric        = dat_clim,
    targets       = targets,
    direction     = -1
  )

  # Return value is a list with named elements
  expect_true(rlang::is_list(result))
  expect_named(result, c("Features", "Targets"))

  # Features is an sf object with the same number of rows as the input
  expect_s3_class(result$Features, "sf")
  expect_equal(nrow(result$Features), nrow(dat_species_bin))

  # Features contains _CS and _NCS columns for every input feature
  cs_cols <- paste0(feat_names, "_CS")
  ncs_cols <- paste0(feat_names, "_NCS")
  expect_true(all(cs_cols %in% names(result$Features)))
  expect_true(all(ncs_cols %in% names(result$Features)))

  # Targets is a data.frame with feature and target columns
  expect_s3_class(result$Targets, "data.frame")
  expect_true(all(c("feature", "target") %in% names(result$Targets)))

  # Targets has 2 rows per input feature (_CS and _NCS)
  expect_equal(nrow(result$Targets), 2L * length(feat_names))
})

testthat::test_that("splnr_climate_priorityAreaApproach() produces no NA targets when targets is a tibble", {
  # Regression test: when targets is a tibble (as returned by shinyplanr's
  # fget_targets_with_bioregions()), the [, "target"] subsetting inside
  # splnr_climate_priorityArea_assignTargets() previously returned a 1-row
  # tibble instead of a scalar. This caused targetCS to be a data frame,
  # c(targetCS, targetNCS) to be a list, and dplyr::bind_rows() to produce
  # NA in the target column — crashing prioritizr::add_relative_targets().
  # The fix uses dplyr::pull() to always extract a scalar.
  result <- splnr_climate_priorityAreaApproach(
    features      = dat_species_bin,
    metric        = dat_clim,
    targets       = targets_tbl, # tibble, not plain data.frame
    direction     = -1,
    percentile    = 5 # small percentile → prop_cs > trgt for most features
  )

  # The target column must be a plain numeric vector with no NA values.
  # NA targets cause prioritizr::add_relative_targets() to error.
  expect_true(is.numeric(result$Targets$target))
  expect_false(any(is.na(result$Targets$target)))
  expect_false(any(!is.finite(result$Targets$target)))
})


# --- Feature approach -------------------------------------------------------

testthat::test_that("splnr_climate_featureApproach() returns correct structure", {
  result <- splnr_climate_featureApproach(
    features      = dat_species_bin,
    metric        = dat_clim,
    targets       = targets,
    direction     = 1
  )

  # Return value is a list with named elements
  expect_true(rlang::is_list(result))
  expect_named(result, c("Features", "Targets"))

  # Features is an sf object with the same number of rows as the input
  expect_s3_class(result$Features, "sf")
  expect_equal(nrow(result$Features), nrow(dat_species_bin))

  # Features contains a climate_layer column
  expect_true("climate_layer" %in% names(result$Features))

  # Targets is a data.frame with feature and target columns
  expect_s3_class(result$Targets, "data.frame")
  expect_true(all(c("feature", "target") %in% names(result$Targets)))

  # Targets has one row per input feature plus one row for climate_layer
  expect_equal(nrow(result$Targets), length(feat_names) + 1L)
})


# --- Percentile approach ----------------------------------------------------

testthat::test_that("splnr_climate_percentileApproach() returns correct structure", {
  result <- splnr_climate_percentileApproach(
    features  = dat_species_bin,
    metric    = dat_clim,
    targets   = targets,
    direction = 1
  )

  # Return value is a list with named elements
  expect_true(rlang::is_list(result))
  expect_named(result, c("Features", "Targets"))

  # Features is an sf object with the same number of rows as the input
  expect_s3_class(result$Features, "sf")
  expect_equal(nrow(result$Features), nrow(dat_species_bin))

  # Features contains the same column names as the input (values are filtered
  # to climate-smart areas only; the column names are not renamed)
  expect_true(all(feat_names %in% names(result$Features)))

  # Targets is a data.frame with feature and target columns
  expect_s3_class(result$Targets, "data.frame")
  expect_true(all(c("feature", "target") %in% names(result$Targets)))

  # Targets has one row per input feature
  expect_equal(nrow(result$Targets), length(feat_names))
})


# ---------------------------------------------------------------------------
# splnr_climate_percentile_preprocess() NA metric warning (line 1004-1009)
# ---------------------------------------------------------------------------
# When the metric column contains NAs, a warning is issued before the
# per-feature percentile loop.  We inject NAs into a copy of dat_clim.

testthat::test_that("splnr_climate_percentileApproach() warns when metric contains NAs", {
  # Introduce NAs into the metric column for a handful of planning units.
  dat_clim_na <- dat_clim %>%
    dplyr::mutate(metric = dplyr::if_else(dplyr::row_number() <= 10L, NA_real_, .data$metric))

  expect_warning(
    splnr_climate_percentileApproach(
      features  = dat_species_bin,
      metric    = dat_clim_na,
      targets   = targets,
      direction = 1
    ),
    "NAs present in the metric data"
  )
})


# ---------------------------------------------------------------------------
# splnr_climate_percentile_preprocess() direction = -1 branch (lines 1047-1048)
# ---------------------------------------------------------------------------
# When direction = -1, the climate-smart filter keeps planning units whose
# metric value is <= the percentile threshold (cold/low-stress refugia).
# The existing test only uses direction = 1; this test exercises direction = -1.

testthat::test_that("splnr_climate_percentileApproach() works with direction = -1", {
  result <- splnr_climate_percentileApproach(
    features   = dat_species_bin,
    metric     = dat_clim,
    targets    = targets,
    direction  = -1,
    percentile = 35
  )

  # Basic structure checks
  expect_true(rlang::is_list(result))
  expect_named(result, c("Features", "Targets"))
  expect_s3_class(result$Features, "sf")
  expect_equal(nrow(result$Features), nrow(dat_species_bin))
  expect_true(all(feat_names %in% names(result$Features)))
  expect_equal(nrow(result$Targets), length(feat_names))
})
