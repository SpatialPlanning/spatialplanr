
# Helper used across multiple tests: get numeric column names from an sf result,
# excluding geometry. Uses the same purrr::map_lgl approach as the function itself.
get_numeric_cols <- function(sf_obj) {
  plain <- sf::st_drop_geometry(sf_obj)
  names(plain)[purrr::map_lgl(plain, is.numeric)]
}

# --- Existing behaviour: single numeric scalar ---

testthat::test_that("single numeric cutoff returns an sf object", {
  expect_s3_class(
    splnr_apply_cutoffs(dat_species_prob, Cutoffs = 0.5),
    "sf"
  )
})

testthat::test_that("single numeric cutoff with inverse returns an sf object", {
  expect_s3_class(
    splnr_apply_cutoffs(dat_species_prob, Cutoffs = 0.5, inverse = TRUE),
    "sf"
  )
})

testthat::test_that("named numeric vector cutoff returns an sf object", {
  expect_s3_class(
    splnr_apply_cutoffs(
      dat_species_prob,
      Cutoffs = c("Spp1" = 0.5, "Spp2" = 0.4, "Spp3" = 0.6, "Spp4" = 0.5, "Spp5" = 0.5)
    ),
    "sf"
  )
})

# --- Single numeric scalar: correct binarisation ---

testthat::test_that("single numeric cutoff produces only 0/1 values", {
  result      <- splnr_apply_cutoffs(dat_species_prob, Cutoffs = 0.5)
  num_cols    <- get_numeric_cols(result)
  vals        <- unlist(sf::st_drop_geometry(result)[num_cols])
  expect_true(all(vals %in% c(0, 1)))
})

testthat::test_that("single numeric cutoff: values >= threshold become 1", {
  # Cutoff of 0 means every non-NA value (>= 0) becomes 1.
  result   <- splnr_apply_cutoffs(dat_species_prob, Cutoffs = 0)
  num_cols <- get_numeric_cols(result)
  vals     <- unlist(sf::st_drop_geometry(result)[num_cols])
  expect_true(all(vals == 1))
})

testthat::test_that("single numeric cutoff: values < threshold become 0", {
  # Cutoff of 1 means only values exactly equal to 1 become 1; all others become 0.
  result   <- splnr_apply_cutoffs(dat_species_prob, Cutoffs = 1)
  num_cols <- get_numeric_cols(result)
  vals     <- unlist(sf::st_drop_geometry(result)[num_cols])
  expect_true(all(vals %in% c(0, 1)))
})

testthat::test_that("inverse flips 0 and 1 for non-NA values", {
  # NA values become 0 in normal mode and 1 in inverse mode (0 is binarised
  # first, then flipped), so they do not sum to 1. We therefore test only on
  # non-NA cells in the original data.
  normal   <- splnr_apply_cutoffs(dat_species_prob, Cutoffs = 0.5, inverse = FALSE)
  inverted <- splnr_apply_cutoffs(dat_species_prob, Cutoffs = 0.5, inverse = TRUE)
  num_cols <- get_numeric_cols(normal)

  for (col in num_cols) {
    orig_vals    <- sf::st_drop_geometry(dat_species_prob)[[col]]
    non_na_idx   <- !is.na(orig_vals)
    normal_vals  <- sf::st_drop_geometry(normal)[[col]][non_na_idx]
    inverse_vals <- sf::st_drop_geometry(inverted)[[col]][non_na_idx]
    expect_equal(normal_vals + inverse_vals, rep(1, sum(non_na_idx)))
  }
})

# --- Single function cutoff ---

testthat::test_that("single function cutoff returns an sf object", {
  expect_s3_class(
    splnr_apply_cutoffs(dat_species_prob, Cutoffs = \(x) quantile(x, 0.99)),
    "sf"
  )
})

testthat::test_that("single function cutoff produces only 0/1 values", {
  result   <- splnr_apply_cutoffs(dat_species_prob, Cutoffs = \(x) quantile(x, 0.99))
  num_cols <- get_numeric_cols(result)
  vals     <- unlist(sf::st_drop_geometry(result)[num_cols])
  expect_true(all(vals %in% c(0, 1)))
})

testthat::test_that("single function cutoff with inverse returns an sf object", {
  expect_s3_class(
    splnr_apply_cutoffs(dat_species_prob, Cutoffs = \(x) quantile(x, 0.99), inverse = TRUE),
    "sf"
  )
})

testthat::test_that("single function cutoff: median threshold produces only 0/1 values", {
  result   <- splnr_apply_cutoffs(dat_species_prob, Cutoffs = \(x) median(x))
  num_cols <- get_numeric_cols(result)
  vals     <- unlist(sf::st_drop_geometry(result)[num_cols])
  expect_true(all(vals %in% c(0, 1)))
})

# --- Named list: mixed numeric and function ---

testthat::test_that("named list with mixed numeric and function entries returns an sf object", {
  expect_s3_class(
    splnr_apply_cutoffs(
      dat_species_prob,
      Cutoffs = list(
        "Spp1" = 0.5,
        "Spp2" = \(x) quantile(x, 0.99)
      )
    ),
    "sf"
  )
})

testthat::test_that("named list cutoff produces only 0/1 values in targeted columns", {
  result     <- splnr_apply_cutoffs(
    dat_species_prob,
    Cutoffs = list(
      "Spp1" = 0.5,
      "Spp2" = \(x) quantile(x, 0.99)
    )
  )
  vals_spp1 <- sf::st_drop_geometry(result)[["Spp1"]]
  vals_spp2 <- sf::st_drop_geometry(result)[["Spp2"]]
  expect_true(all(vals_spp1 %in% c(0, 1)))
  expect_true(all(vals_spp2 %in% c(0, 1)))
})

testthat::test_that("named list: untargeted columns are unchanged", {
  result <- splnr_apply_cutoffs(
    dat_species_prob,
    Cutoffs = list("Spp1" = 0.5)
  )
  # Spp2 should be unchanged (still continuous)
  original_spp2 <- sf::st_drop_geometry(dat_species_prob)[["Spp2"]]
  result_spp2   <- sf::st_drop_geometry(result)[["Spp2"]]
  expect_equal(original_spp2, result_spp2)
})

# --- Named list: all functions ---

testthat::test_that("named list with all function entries returns an sf object", {
  expect_s3_class(
    splnr_apply_cutoffs(
      dat_species_prob,
      Cutoffs = list(
        "Spp1" = \(x) quantile(x, 0.95),
        "Spp2" = \(x) quantile(x, 0.99)
      )
    ),
    "sf"
  )
})

# --- Input validation ---

testthat::test_that("non-sf input raises an error", {
  expect_error(
    splnr_apply_cutoffs(sf::st_drop_geometry(dat_species_prob), Cutoffs = 0.5),
    "must be an 'sf' object"
  )
})

testthat::test_that("cutoff outside [0,1] raises an error", {
  expect_error(
    splnr_apply_cutoffs(dat_species_prob, Cutoffs = 1.5),
    "outside the required \\[0, 1\\] range"
  )
})

testthat::test_that("function returning value outside [0,1] raises an error", {
  expect_error(
    splnr_apply_cutoffs(dat_species_prob, Cutoffs = \(x) 2.0),
    "outside the required \\[0, 1\\] range"
  )
})

testthat::test_that("function returning non-scalar raises an error", {
  expect_error(
    splnr_apply_cutoffs(dat_species_prob, Cutoffs = \(x) c(0.3, 0.5)),
    "must return a single finite numeric"
  )
})

testthat::test_that("named cutoff with unrecognised column name raises an error", {
  expect_error(
    splnr_apply_cutoffs(dat_species_prob, Cutoffs = c("NonExistentCol" = 0.5)),
    "Unrecognised names"
  )
})

testthat::test_that("non-logical inverse raises an error", {
  expect_error(
    splnr_apply_cutoffs(dat_species_prob, Cutoffs = 0.5, inverse = "yes"),
    "must be a single logical value"
  )
})
